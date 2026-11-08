%%% A request on an HTTP/2 connection at the server's limit on concurrent
%%% streams waits for a stream instead of failing with max_streams_exceeded.
%%%
%%% RFC 9113 5.1.2: a client must not open more streams than the server's
%%% SETTINGS_MAX_CONCURRENT_STREAMS. h2 refuses such a request and, with
%%% notify_stream_available, tells hackney once one fits again; hackney parks
%%% the request until then.
%%%
%%% The server (hackney_h2_limit_server) holds every response until the test
%%% releases it, so each step is driven by an explicit message.
-module(hackney_http2_stream_limit_tests).

-include_lib("eunit/include/eunit.hrl").

stream_limit_test_() ->
    {setup,
     fun() ->
         _ = application:ensure_all_started(hackney),
         _ = application:ensure_all_started(h2),
         ok
     end,
     fun(_) -> ok end,
     [{timeout, 30, fun waits_for_a_stream_to_end/0},
      {timeout, 30, fun wait_runs_out_with_checkout_timeout/0},
      {timeout, 30, fun dead_caller_leaves_the_queue/0},
      {timeout, 30, fun goaway_fails_the_waiting_requests/0},
      {timeout, 30, fun raised_limit_lets_every_waiter_go/0},
      {timeout, 30, fun upload_holds_the_rest_until_it_ends/0}]}.

%% Limit 1: the second request waits until the first stream ends, then goes
%% out on the same connection.
waits_for_a_stream_to_end() ->
    with_conn(1, fun(Conn) ->
        P1 = request(Conn),
        {C, 1} = hackney_h2_limit_server:next_request(),
        P2 = request(Conn),
        ok = wait_parked(Conn, P2),
        C ! {release, 1},
        ?assertEqual({ok, 200, <<"1">>}, result(P1)),
        {C, 3} = hackney_h2_limit_server:next_request(),
        C ! {release, 3},
        ?assertEqual({ok, 200, <<"3">>}, result(P2))
    end).

%% A request waits at most its checkout_timeout and fails like a pool
%% checkout would.
wait_runs_out_with_checkout_timeout() ->
    with_conn(1, fun(Conn) ->
        P1 = request(Conn),
        {C, 1} = hackney_h2_limit_server:next_request(),
        P2 = request(Conn, [{checkout_timeout, 100}]),
        ?assertEqual({error, checkout_timeout}, result(P2)),
        C ! {release, 1},
        ?assertEqual({ok, 200, <<"1">>}, result(P1))
    end).

%% A waiting caller that exits is dropped; the one behind it gets the slot.
dead_caller_leaves_the_queue() ->
    with_conn(1, fun(Conn) ->
        P1 = request(Conn),
        {C, 1} = hackney_h2_limit_server:next_request(),
        P2 = request(Conn),
        ok = wait_parked(Conn, P2),
        P3 = request(Conn),
        ok = wait_parked(Conn, P3),
        exit(P2, kill),
        ok = wait_unparked(Conn, P2),
        C ! {release, 1},
        ?assertEqual({ok, 200, <<"1">>}, result(P1)),
        {C, 3} = hackney_h2_limit_server:next_request(),
        C ! {release, 3},
        ?assertEqual({ok, 200, <<"3">>}, result(P3))
    end).

%% A GOAWAY fails the waiting request, which was never sent; the accepted
%% stream still completes.
goaway_fails_the_waiting_requests() ->
    with_conn(1, fun(Conn) ->
        P1 = request(Conn),
        {C, 1} = hackney_h2_limit_server:next_request(),
        P2 = request(Conn),
        ok = wait_parked(Conn, P2),
        C ! goaway,
        ?assertEqual({error, {goaway, no_error}}, result(P2)),
        C ! {release, 1},
        ?assertEqual({ok, 200, <<"1">>}, result(P1))
    end).

%% The server raises its limit while two requests wait: both go out, though
%% no stream has ended.
raised_limit_lets_every_waiter_go() ->
    with_conn(1, fun(Conn) ->
        P1 = request(Conn),
        {C, 1} = hackney_h2_limit_server:next_request(),
        P2 = request(Conn),
        ok = wait_parked(Conn, P2),
        P3 = request(Conn),
        ok = wait_parked(Conn, P3),
        C ! {max_streams, 3},
        {C, 3} = hackney_h2_limit_server:next_request(),
        {C, 5} = hackney_h2_limit_server:next_request(),
        [C ! {release, S} || S <- [1, 3, 5]],
        ?assertEqual({ok, 200, <<"1">>}, result(P1)),
        ?assertEqual({ok, 200, <<"3">>}, result(P2)),
        ?assertEqual({ok, 200, <<"5">>}, result(P3))
    end).

%% A waiting upload goes out and keeps the connection while its body is sent;
%% the request behind it goes out once the upload is done.
upload_holds_the_rest_until_it_ends() ->
    with_conn(1, fun(Conn) ->
        P1 = request(Conn),
        {C, 1} = hackney_h2_limit_server:next_request(),
        Self = self(),
        U = spawn(fun() ->
                      ok = hackney_conn:send_request_headers(Conn, <<"POST">>, <<"/">>, [], []),
                      Self ! {upload_started, self()},
                      receive send_body -> ok end,
                      ok = hackney_conn:send_body_chunk(Conn, <<"body">>),
                      ok = hackney_conn:finish_send_body(Conn),
                      Self ! {self(), hackney_conn:start_response(Conn)}
                  end),
        ok = wait_parked(Conn, U),
        P3 = request(Conn),
        ok = wait_parked(Conn, P3),
        C ! {max_streams, 3},
        receive {upload_started, U} -> ok after 10000 -> error(upload_not_started) end,
        %% P3 still waits: the connection is busy with the upload body.
        ok = wait_parked(Conn, P3),
        U ! send_body,
        {C, 3} = hackney_h2_limit_server:next_request(),
        C ! {release, 3},
        ?assertMatch({ok, 200, _}, start_response_status(result(U))),
        {C, 5} = hackney_h2_limit_server:next_request(),
        [C ! {release, S} || S <- [1, 5]],
        ?assertEqual({ok, 200, <<"1">>}, result(P1)),
        ?assertEqual({ok, 200, <<"5">>}, result(P3))
    end).

%%====================================================================
%% Helpers
%%====================================================================

with_conn(MaxStreams, Fun) ->
    {Server, Url} = hackney_h2_limit_server:start(MaxStreams),
    {ok, Port} = port_of(Url),
    {ok, Conn} = hackney:connect(hackney_ssl, "localhost", Port,
                                 [{protocols, [http2]}, {pool, false},
                                  {ssl_options, [{insecure, true}, {verify, verify_none}]}]),
    try
        ?assertEqual(http2, hackney_conn:get_protocol(Conn)),
        Fun(Conn)
    after
        try hackney:close(Conn) catch _:_ -> ok end,
        hackney_h2_limit_server:stop(Server)
    end.

port_of(Url) ->
    {match, [P]} = re:run(Url, <<":([0-9]+)/">>, [{capture, all_but_first, binary}]),
    {ok, binary_to_integer(P)}.

request(Conn) ->
    request(Conn, []).

request(Conn, ReqOpts) ->
    Self = self(),
    spawn(fun() ->
              R = hackney_conn:request(Conn, <<"GET">>, <<"/">>, [], <<>>, 20000, ReqOpts),
              Self ! {self(), simplify(R)}
          end).

simplify({ok, Status, _Headers, Body}) -> {ok, Status, Body};
simplify(Other) -> Other.

result(Pid) ->
    receive {Pid, R} -> R after 15000 -> {error, test_timeout} end.

start_response_status({ok, Status, _Headers}) -> {ok, Status, x};
start_response_status({ok, Status, _Headers, _Body}) -> {ok, Status, x};
start_response_status(Other) -> Other.

%% A parked request is monitored by the connection: wait for the monitor.
wait_parked(Conn, Pid) ->
    wait_until(fun() -> lists:member({process, Pid}, monitors(Conn)) end).

wait_unparked(Conn, Pid) ->
    wait_until(fun() -> not lists:member({process, Pid}, monitors(Conn)) end).

monitors(Conn) ->
    {monitors, M} = process_info(Conn, monitors),
    M.

wait_until(Fun) ->
    wait_until(Fun, 500).

wait_until(_Fun, 0) ->
    timeout;
wait_until(Fun, N) ->
    case Fun() of
        true -> ok;
        false -> timer:sleep(10), wait_until(Fun, N - 1)
    end.
