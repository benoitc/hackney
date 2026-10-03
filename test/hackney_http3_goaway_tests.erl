%%% An HTTP/3 GOAWAY must only fail the requests the peer did not accept.
%%%
%%% RFC 9114 5.2: requests on streams below the GOAWAY identifier may still be
%%% processed, the ones at or above it were refused. The connection leaves the
%%% pool, refused streams fail with {error, {goaway, no_error}} and are reset,
%%% accepted ones complete, and the connection closes once they have.
%%%
%%% The quic_h3 server always sends GOAWAY past the last stream it has seen, so
%%% it never refuses an open stream. The tests that need a refusal deliver the
%%% GOAWAY event to the connection as hackney_h3 would forward it.
-module(hackney_http3_goaway_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, h3_goaway_test_pool).

h3_goaway_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun(Ctx) -> {timeout, 60, fun() -> held(fun accepted_stream_completes/1, Ctx) end} end,
      fun(Ctx) -> {timeout, 60, fun() -> held(fun refused_stream_fails_and_is_reset/1, Ctx) end} end,
      fun(Ctx) -> {timeout, 60, fun() -> held(fun refused_upload_fails_on_next_call/1, Ctx) end} end]}.

%% The test process gets the server's held-request notifications.
held(Test, Ctx) ->
    true = register(hackney_h3_test_hold, self()),
    try Test(Ctx) after unregister(hackney_h3_test_hold) end.

setup() ->
    Ctx = hackney_h3_test_server:start(),
    try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end,
    ok = hackney_pool:start_pool(?POOL, [{max_connections, 5}]),
    Ctx.

cleanup(Ctx) ->
    try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end,
    hackney_h3_test_server:stop(Ctx).

%% The server sends GOAWAY while stream 0 is held. Stream 0 was accepted and
%% still completes, a request made during the drain gets a fresh connection,
%% and the drained connection closes once stream 0 is done.
accepted_stream_completes(Ctx) ->
    Conn = connect(Ctx),
    P1 = spawn_request(Conn, get),
    {Handler, ServerConn, 0} = await_held(),
    ok = quic_h3:goaway(ServerConn),
    ok = await_state(Conn, draining),
    ?assertMatch({ok, 200, _, _},
                 hackney:request(get, hackney_h3_test_server:url(Ctx, <<"/">>), [],
                                 <<>>, opts())),
    Handler ! release,
    ?assertEqual({ok, 200, <<"0">>}, await(P1)),
    ok = await_state(Conn, closed).

%% Streams 0 and 4 are held, then GOAWAY(4): stream 4 fails at once and is
%% reset, stream 0 still completes.
refused_stream_fails_and_is_reset(Ctx) ->
    Conn = connect(Ctx),
    P1 = spawn_request(Conn, get),
    {Handler0, _, 0} = await_held(),
    P2 = spawn_request(Conn, get),
    {_Handler4, _, 4} = await_held(),
    ok = goaway(Conn, 4),
    ?assertEqual({error, {goaway, no_error}}, await(P2)),
    ?assertEqual(ok, receive {held_reset, 4} -> ok after 10000 -> no_reset end),
    ?assertEqual({ok, draining}, hackney_conn:get_state(Conn)),
    Handler0 ! release,
    ?assertEqual({ok, 200, <<"0">>}, await(P1)),
    ok = await_state(Conn, closed).

%% Stream 0 is a held request, stream 4 a streamed upload, then GOAWAY(4): the
%% upload was refused and its next send says so, stream 0 still completes.
refused_upload_fails_on_next_call(Ctx) ->
    Conn = connect(Ctx),
    P1 = spawn_request(Conn, get),
    {Handler0, _, 0} = await_held(),
    ok = hackney_conn:send_request_headers(Conn, <<"POST">>, <<"/hold">>, []),
    {_Handler4, _, 4} = await_held(),
    ok = goaway(Conn, 4),
    ?assertEqual(ok, receive {held_reset, 4} -> ok after 10000 -> no_reset end),
    ?assertEqual({error, {goaway, no_error}},
                 hackney_conn:send_body_chunk(Conn, <<"part">>)),
    Handler0 ! release,
    ?assertEqual({ok, 200, <<"0">>}, await(P1)),
    ok = await_state(Conn, closed).

%%====================================================================
%% Helpers
%%====================================================================

opts() ->
    [{pool, ?POOL} | hackney_h3_test_server:hackney_opts()].

connect(Ctx) ->
    {ok, Conn} = hackney:connect(hackney_ssl, "127.0.0.1",
                                 hackney_h3_test_server:port(Ctx), opts()),
    Conn.

spawn_request(Conn, get) ->
    Self = self(),
    spawn_link(fun() ->
        R = case hackney:send_request(Conn, {get, <<"/hold">>, [], <<>>}) of
            {ok, Status, _Headers, Body} when is_binary(Body) -> {ok, Status, Body};
            {ok, Status, _Headers, Conn} ->
                {ok, Body} = hackney:body(Conn),
                {ok, Status, Body};
            {error, E} -> {error, E}
        end,
        Self ! {self(), R}
    end).

await(Pid) ->
    receive {Pid, R} -> R after 20000 -> {error, test_timeout} end.

await_held() ->
    receive {held, Handler, ServerConn, StreamId} -> {Handler, ServerConn, StreamId}
    after 20000 -> error(not_held)
    end.

%% Deliver a GOAWAY as hackney_h3 forwards it to the connection.
goaway(Conn, GoawayId) ->
    [ConnRef] = [Ref || {Ref, Pid} <- ets:tab2list(hackney_h3_conns),
                        h3_owner(Pid) =:= Conn],
    Conn ! {h3, ConnRef, {goaway, GoawayId}},
    ok.

%% The hackney_h3 process monitors its owner, the connection.
h3_owner(Pid) ->
    {monitors, Monitors} = erlang:process_info(Pid, monitors),
    case [P || {process, P} <- Monitors] of
        [Owner | _] -> Owner;
        [] -> undefined
    end.

%% Poll the connection state: GOAWAY handling and the drain end are driven by
%% messages the test cannot await directly.
await_state(Conn, State) ->
    await_state(Conn, State, 200).

await_state(_Conn, State, 0) ->
    {timeout, State};
await_state(Conn, State, N) ->
    case try hackney_conn:get_state(Conn) catch exit:_ -> gone end of
        {ok, State} -> ok;
        gone when State =:= closed -> ok;
        _ ->
            receive after 50 -> ok end,
            await_state(Conn, State, N - 1)
    end.
