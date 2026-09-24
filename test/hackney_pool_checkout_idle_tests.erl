%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Regression for the keepalive timer surviving pool checkout.
%%%
%%% connected(enter) arms {state_timeout, keepalive_timeout, idle_timeout} on a
%%% pooled conn. Checkout probes it with is_ready/1, which validates the socket
%%% but used to leave that timer running, so a conn could answer
%%% {ok, connected}, be handed to a requester, and then self-close because the
%%% timer had been armed almost a full keepalive_timeout earlier. The late
%%% {request, _} then fell through handle_common as {error, invalid_state}.
-module(hackney_pool_checkout_idle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BODY, <<"{\"ok\":true}">>).
-define(IDLE_MS, 300).

checkout_idle_test_() ->
    {setup,
     fun() -> _ = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"is_ready disarms the keepalive timer",
       {timeout, 30, fun checkout_disarms_idle_timer/0}},
      {"a conn checked out near its deadline still serves the request",
       {timeout, 30, fun request_after_checkout_succeeds/0}},
      {"the timer is re-armed once the conn goes back to the pool",
       {timeout, 30, fun idle_timer_rearmed_after_response/0}}]}.

%% A conn that answers {ok, connected} to the checkout probe must not self-close
%% afterwards on a timer armed before the probe.
checkout_disarms_idle_timer() ->
    {Server, Port} = start_server(),
    ConnPid = start_conn(Port),
    try
        {ok, 200, _} = hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>),
        {ok, ?BODY} = hackney_conn:body(ConnPid),
        %% Idle in connected, timer armed for ?IDLE_MS. Sit on most of it.
        timer:sleep(?IDLE_MS - 100),
        ?assertEqual({ok, connected}, hackney_conn:is_ready(ConnPid)),
        %% Past the original deadline. Before the fix the conn had closed here.
        timer:sleep(200),
        ?assertMatch({connected, _}, sys:get_state(ConnPid))
    after
        catch hackney_conn:close(ConnPid),
        stop_server(Server)
    end.

%% Same race, asserted through the caller-visible result rather than the state.
request_after_checkout_succeeds() ->
    {Server, Port} = start_server(),
    ConnPid = start_conn(Port),
    try
        {ok, 200, _} = hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>),
        {ok, ?BODY} = hackney_conn:body(ConnPid),
        timer:sleep(?IDLE_MS - 100),
        ?assertEqual({ok, connected}, hackney_conn:is_ready(ConnPid)),
        timer:sleep(200),
        %% {error, invalid_state} before the fix.
        ?assertMatch({ok, 200, _},
                     hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>)),
        ?assertEqual({ok, ?BODY}, hackney_conn:body(ConnPid))
    after
        catch hackney_conn:close(ConnPid),
        stop_server(Server)
    end.

%% Disarming on checkout must not leak: reading the response re-enters connected,
%% which re-arms the timer, so an abandoned conn still ages out of the pool.
idle_timer_rearmed_after_response() ->
    {Server, Port} = start_server(),
    ConnPid = start_conn(Port),
    try
        {ok, 200, _} = hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>),
        {ok, ?BODY} = hackney_conn:body(ConnPid),
        ?assertEqual({ok, connected}, hackney_conn:is_ready(ConnPid)),
        {ok, 200, _} = hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>),
        {ok, ?BODY} = hackney_conn:body(ConnPid),
        %% Now idle again with the timer re-armed; it must still fire.
        timer:sleep(?IDLE_MS + 200),
        ?assertNotMatch({connected, _}, sys:get_state(ConnPid))
    after
        catch hackney_conn:close(ConnPid),
        stop_server(Server)
    end.

start_conn(Port) ->
    {ok, ConnPid} = hackney_conn_sup:start_conn(
        #{host => "localhost", port => Port, transport => hackney_ssl,
          ssl_options => [{insecure, true}, {verify, verify_none}],
          recv_timeout => 3000, idle_timeout => ?IDLE_MS}),
    ok = hackney_conn:connect(ConnPid),
    ConnPid.

%%====================================================================
%% Minimal keep-alive HTTP/1.1 TLS server.
%%====================================================================

start_server() ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn(fun() -> accept_loop(LSock) end),
    {Pid, Port}.

stop_server(Pid) -> exit(Pid, shutdown), ok.

accept_loop(LSock) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() ->
                case ssl:handshake(TSock, 5000) of
                    {ok, Sock} -> serve(Sock);
                    _ -> ok
                end
            end),
            accept_loop(LSock);
        {error, timeout} -> accept_loop(LSock);
        {error, _} -> ok
    end.

serve(Sock) ->
    case read_request(Sock, <<>>) of
        {ok, _Path} ->
            ok = ssl:send(Sock, response()),
            serve(Sock);
        closed -> ok
    end.

read_request(Sock, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        nomatch ->
            case ssl:recv(Sock, 0, 30000) of
                {ok, Data} -> read_request(Sock, <<Acc/binary, Data/binary>>);
                {error, _} -> closed
            end;
        _ ->
            {ok, ok}
    end.

response() ->
    [<<"HTTP/1.1 200 OK\r\ncontent-type: application/json\r\ncontent-length: ">>,
     integer_to_list(byte_size(?BODY)), <<"\r\n\r\n">>, ?BODY].

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
