%%% #995: concurrent TLS 1.3 handshakes to one host with automatic session
%%% resumption used to fail now and then with `bad_record_mac'. OTP's client
%%% ticket store can hand the same single-use ticket to two handshakes; the
%%% server accepts it on both and the second derives the wrong keys. hackney
%%% now lets one handshake per host offer a ticket at a time.
%%%
%%% The upgrades go through hackney_conn with `final' options, the path a
%%% pooled HTTPS request takes, so the resumption gate and the claim run
%%% exactly as they do in production. verify_none stands in for the default
%%% trust store, which would reject the test certificate.
-module(hackney_tls_resumption_race_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ROUNDS, 15).
-define(CONCURRENCY, 20).

race_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_LSock, Port}) ->
         [{timeout, 120,
           {"concurrent resumed handshakes to one host do not fail",
            fun() -> no_failed_handshakes(Port) end}}]
     end}.

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [binary, {active, false}, {reuseaddr, true}, {backlog, 128},
         {certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {versions, ['tlsv1.3']},
         {session_tickets, stateless}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    _ = spawn(fun() -> accept_loop(LSock) end),
    {LSock, Port}.

cleanup({LSock, _Port}) ->
    _ = ssl:close(LSock),
    ok.

ssl_opts() ->
    [{verify, verify_none}, {versions, ['tlsv1.3']},
     {server_name_indication, "localhost"}, {session_tickets, auto}].

no_failed_handshakes(Port) ->
    %% A full handshake first: it fills the ALPN memo, which the resumption
    %% gate needs, and stores the first tickets.
    {ok, C0} = upgrade(Port),
    timer:sleep(200),
    ok = hackney_conn:stop(C0),
    Results = lists:append([round(Port) || _ <- lists:seq(1, ?ROUNDS)]),
    Errors = [E || {error, E} <- Results],
    ?assertEqual([], Errors).

%% One burst of concurrent upgrades. Each finished conn is kept open long
%% enough to receive the server's tickets for the next round.
round(Port) ->
    Self = self(),
    Pids = [spawn(fun() -> Self ! {done, self(), upgrade(Port)} end)
            || _ <- lists:seq(1, ?CONCURRENCY)],
    Results = [receive {done, P, R} -> R after 20000 -> {error, lost} end
               || P <- Pids],
    timer:sleep(100),
    [ok = hackney_conn:stop(C) || {ok, C} <- Results],
    [case R of {ok, _} -> ok; Error -> Error end || R <- Results].

upgrade(Port) ->
    {ok, Conn} = hackney_conn_sup:start_conn(
                   #{host => "localhost", port => Port, transport => hackney_tcp,
                     connect_timeout => 5000, recv_timeout => 5000}),
    ok = hackney_conn:connect(Conn),
    case hackney_conn:upgrade_to_ssl(Conn, ssl_opts(), #{final => true}) of
        ok ->
            {ok, Conn};
        {error, _} = Error ->
            ok = hackney_conn:stop(Conn),
            Error
    end.

accept_loop(LSock) ->
    case ssl:transport_accept(LSock) of
        {ok, TSock} ->
            _ = spawn(fun() ->
                          case ssl:handshake(TSock, 5000) of
                              {ok, S} -> _ = ssl:recv(S, 0, 5000), ssl:close(S);
                              _ -> ok
                          end
                      end),
            accept_loop(LSock);
        {error, _} ->
            ok
    end.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
