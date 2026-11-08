%%% A request on an HTTP/3 connection at the server's limit on concurrent
%%% request streams waits for a stream instead of failing with stream_limit.
%%%
%%% RFC 9000 4.6: a client must not open more streams than the server's
%%% MAX_STREAMS allows. quic_h3 refuses such a request and, with
%%% notify_streams_available, tells hackney once one fits again; hackney
%%% parks the request until then.
%%%
%%% The local quic_h3 server allows one request stream at a time and holds
%%% each /hold request until the test releases it.
-module(hackney_http3_stream_limit_tests).

-include_lib("eunit/include/eunit.hrl").

h3_stream_limit_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun(Ctx) -> {timeout, 60, fun() -> held(fun waits_for_a_stream_to_end/1, Ctx) end} end,
      fun(Ctx) -> {timeout, 60, fun() -> held(fun wait_runs_out_with_checkout_timeout/1, Ctx) end} end,
      fun(Ctx) -> {timeout, 60, fun() -> held(fun goaway_fails_the_waiting_request/1, Ctx) end} end]}.

held(Test, Ctx) ->
    true = register(hackney_h3_test_hold, self()),
    try Test(Ctx) after unregister(hackney_h3_test_hold) end.

setup() ->
    hackney_h3_test_server:start(#{quic_opts => #{max_streams_bidi => 1}}).

cleanup(Ctx) ->
    hackney_h3_test_server:stop(Ctx).

%% The second request waits until the first stream ends, then goes out on the
%% same connection.
waits_for_a_stream_to_end(Ctx) ->
    Conn = connect(Ctx),
    P1 = request(Conn, []),
    {Handler0, _, 0} = await_held(),
    P2 = request(Conn, []),
    ok = wait_parked(Conn, P2),
    Handler0 ! release,
    ?assertEqual({ok, 200, <<"0">>}, await(P1)),
    {Handler4, _, 4} = await_held(),
    Handler4 ! release,
    ?assertEqual({ok, 200, <<"4">>}, await(P2)).

%% A request waits at most its checkout_timeout.
wait_runs_out_with_checkout_timeout(Ctx) ->
    Conn = connect(Ctx),
    P1 = request(Conn, []),
    {Handler0, _, 0} = await_held(),
    P2 = request(Conn, [{checkout_timeout, 100}]),
    ?assertEqual({error, checkout_timeout}, await(P2)),
    Handler0 ! release,
    ?assertEqual({ok, 200, <<"0">>}, await(P1)).

%% A GOAWAY fails the waiting request, which was never sent; the accepted
%% stream still completes.
goaway_fails_the_waiting_request(Ctx) ->
    Conn = connect(Ctx),
    P1 = request(Conn, []),
    {Handler0, ServerConn, 0} = await_held(),
    P2 = request(Conn, []),
    ok = wait_parked(Conn, P2),
    ok = quic_h3:goaway(ServerConn),
    ?assertEqual({error, {goaway, no_error}}, await(P2)),
    Handler0 ! release,
    ?assertEqual({ok, 200, <<"0">>}, await(P1)).

%%====================================================================
%% Helpers
%%====================================================================

connect(Ctx) ->
    {ok, Conn} = hackney:connect(hackney_ssl, "127.0.0.1",
                                 hackney_h3_test_server:port(Ctx),
                                 [{pool, false} | hackney_h3_test_server:hackney_opts()]),
    ?assertEqual(http3, hackney_conn:get_protocol(Conn)),
    Conn.

request(Conn, ReqOpts) ->
    Self = self(),
    spawn(fun() ->
              R = hackney_conn:request(Conn, <<"GET">>, <<"/hold">>, [], <<>>, 30000, ReqOpts),
              Self ! {self(), simplify(R)}
          end).

simplify({ok, Status, _Headers, Body}) when is_binary(Body) -> {ok, Status, Body};
simplify(Other) -> Other.

await(Pid) ->
    receive {Pid, R} -> R after 20000 -> {error, test_timeout} end.

await_held() ->
    receive {held, Handler, ServerConn, StreamId} -> {Handler, ServerConn, StreamId}
    after 20000 -> error(not_held)
    end.

%% A parked request is monitored by the connection: wait for the monitor.
wait_parked(Conn, Pid) ->
    wait_until(fun() ->
                   {monitors, M} = process_info(Conn, monitors),
                   lists:member({process, Pid}, M)
               end, 500).

wait_until(_Fun, 0) ->
    timeout;
wait_until(Fun, N) ->
    case Fun() of
        true -> ok;
        false -> timer:sleep(10), wait_until(Fun, N - 1)
    end.
