%%% Tests for HTTP/2 responses that signal end-of-stream with a trailing
%%% HEADERS frame (trailers) instead of an END_STREAM DATA flag, and for the
%%% per-stream recv_timeout watchdog.
%%%
%%% Regression for the production hang: an h2 response with no content-length
%%% whose body is closed by trailers (what proxies/ALBs emit for streamed
%%% bodies) left the body reader parked forever, because hackney ignored the
%%% {trailers,...} event and waited for an END_STREAM DATA frame that never
%%% came. The reader must complete as soon as the peer signals end-of-stream.
-module(hackney_http2_trailers_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BODY, (binary:copy(<<"x">>, 14000))).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

%%====================================================================
%% Fixture
%%====================================================================

http2_trailers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Ctx) ->
         [{"body closed by trailers completes promptly",
           {timeout, 30, fun() -> t_trailered_response(Ctx) end}},
          {"trailered + END_STREAM-on-DATA both work across a reused pooled conn",
           {timeout, 60, fun() -> t_pooled_reuse(Ctx) end}},
          {"a never-ending response fails fast on recv_timeout",
           {timeout, 30, fun() -> t_recv_timeout_safety_net(Ctx) end}}]
     end}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => fun server_handler/5,
        settings => #{max_concurrent_streams => unlimited}
    }),
    Port = h2:server_port(Server),
    #{server => Server, port => Port}.

cleanup(#{server := Server}) ->
    try h2:stop_server(Server) catch _:_ -> ok end,
    ok.

%%====================================================================
%% Server handler
%%====================================================================

%% /trailered : body, then END_STREAM via a trailing HEADERS frame.
%% /normal    : body with END_STREAM on the final DATA frame.
%% /stall     : headers + a partial body, then never end the stream.
server_handler(Conn, Sid, _Method, Path, _Headers) ->
    Json = [{<<"content-type">>, <<"application/json">>}],
    case Path of
        <<"/trailered">> ->
            ok = h2:send_response(Conn, Sid, 200, Json),
            ok = h2:send_data(Conn, Sid, ?BODY, false),
            ok = h2:send_trailers(Conn, Sid, [{<<"x-trailer">>, <<"end">>}]);
        <<"/normal">> ->
            ok = h2:send_response(Conn, Sid, 200, Json),
            ok = h2:send_data(Conn, Sid, ?BODY, true);
        <<"/stall">> ->
            ok = h2:send_response(Conn, Sid, 200, Json),
            ok = h2:send_data(Conn, Sid, <<"partial">>, false),
            %% Leave the stream open well past the client's recv_timeout.
            timer:sleep(5000),
            try h2:send_data(Conn, Sid, <<>>, true) catch _:_ -> ok end
    end.

%%====================================================================
%% Tests
%%====================================================================

%% Bug 1a: a response closed by trailers must return the full body, not hang.
t_trailered_response(#{port := Port}) ->
    {ok, 200, _Headers, Body} =
        hackney:request(get, url(Port, <<"/trailered">>), [], <<>>, opts(false)),
    ?assertEqual(?BODY, Body).

%% Reuse the single pooled h2 connection for many sequential requests,
%% alternating trailered and END_STREAM-on-DATA responses; every read must
%% return the full body with no lost bytes and no recv_timeout.
t_pooled_reuse(#{port := Port}) ->
    Pool = hackney_h2_trailers_pool,
    _ = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    try
        lists:foreach(fun(N) ->
            Path = case N rem 2 of
                0 -> <<"/trailered">>;
                1 -> <<"/normal">>
            end,
            {ok, 200, _H, Body} =
                hackney:request(get, url(Port, Path), [], <<>>, opts(Pool)),
            ?assertEqual(?BODY, Body)
        end, lists:seq(1, 40))
    after
        (try hackney_pool:stop_pool(Pool) catch _:_ -> ok end)
    end.

%% Bug 1b: with no trailers and no END_STREAM, the watchdog must surface
%% {error, timeout} quickly instead of blocking until the connection dies.
t_recv_timeout_safety_net(#{port := Port}) ->
    Opts = [{pool, false}, {protocols, [http2]}, {recv_timeout, 500},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    Started = erlang:monotonic_time(millisecond),
    Result = hackney:request(get, url(Port, <<"/stall">>), [], <<>>, Opts),
    Elapsed = erlang:monotonic_time(millisecond) - Started,
    ?assertEqual({error, timeout}, Result),
    ?assert(Elapsed < 3000).

%%====================================================================
%% Helpers
%%====================================================================

opts(Pool) ->
    [{pool, Pool},
     {protocols, [http2]},
     {recv_timeout, 5000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

url(Port, Path) ->
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), Path]).
