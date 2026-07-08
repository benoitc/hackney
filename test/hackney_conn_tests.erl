%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024 Benoit Chesneau
%%%
%%% @doc Tests for hackney_conn gen_statem.

-module(hackney_conn_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8125).

%%====================================================================
%% Test fixtures
%%====================================================================

%% Unit tests - no network required
hackney_conn_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"start and stop", fun test_start_stop/0},
      {"initial state is idle", fun test_initial_state/0},
      {"pre-established socket starts connected", fun test_preestablished_socket/0},
      {"connect timeout", fun test_connect_timeout/0},
      {"connect to invalid host", fun test_connect_invalid/0},
      {"owner death stops connection", fun test_owner_death/0},
      {"set_owner on closed connection returns invalid_state (#850)",
       fun test_set_owner_closed_returns_error/0},
      {"set_owner_async stops a closed pooled connection (#850)",
       fun test_set_owner_async_closed_pooled_stops/0},
      {"request on a dead connection returns {error, closed} (#861)",
       fun test_request_dead_conn_returns_error/0},
      {"body on a dead connection returns {error, closed} (#861)",
       fun test_body_dead_conn_returns_error/0}
     ]}.

%% Integration tests - use embedded Cowboy server
hackney_conn_integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun teardown_integration/1,
     [
      {"connect to server", fun test_connect/0},
      {"reconnect after close", fun test_reconnect/0},
      {"simple GET request", {timeout, 30, fun test_get_request/0}},
      {"POST request with body", {timeout, 30, fun test_post_request/0}},
      {"stream body", {timeout, 30, fun test_stream_body/0}},
      {"stream body with stateless function", {timeout, 30, fun test_stream_body_stateless_fun/0}},
      {"stream body with stateful function", {timeout, 30, fun test_stream_body_stateful_fun/0}},
      {"stream body function returns error", {timeout, 30, fun test_stream_body_fun_error/0}},
      {"HEAD request", {timeout, 30, fun test_head_request/0}},
      {"GHSA-j9wq: CRLF in request target rejected", {timeout, 30, fun test_crlf_request_target_rejected/0}},
      {"request returns to connected state", {timeout, 30, fun test_request_state_cycle/0}},
      %% Async tests
      {"async request continuous", {timeout, 30, fun test_async_continuous/0}},
      {"async request once mode", {timeout, 30, fun test_async_once/0}},
      %% Mid-stream ownership reassignment
      {"set_owner while receiving body", {timeout, 30, fun test_set_owner_while_receiving/0}},
      {"set_owner while streaming async", {timeout, 30, fun test_set_owner_while_streaming/0}},
      %% 1XX response handling
      {"skip 1XX informational responses", {timeout, 30, fun test_skip_1xx_responses/0}}
     ]}.

setup() ->
    application:ensure_all_started(hackney),
    ok.

teardown(_) ->
    ok.

setup_integration() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Host = '_',
    Routes = [
        {"/[...]", test_http_resource, []}
    ],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    {ok, _} = cowboy:start_clear(conn_test_server, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}),
    ok.

teardown_integration(_) ->
    cowboy:stop_listener(conn_test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_start_stop() ->
    Opts = #{
        host => "localhost",
        port => ?PORT,
        transport => hackney_tcp
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    ok = hackney_conn:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

test_initial_state() ->
    Opts = #{
        host => "localhost",
        port => ?PORT,
        transport => hackney_tcp
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    {ok, State} = hackney_conn:get_state(Pid),
    ?assertEqual(idle, State),
    hackney_conn:stop(Pid).

test_preestablished_socket() ->
    %% Create a mock socket (we just need any port for the test)
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(ListenSock),

    %% Connect to ourselves to get a real socket
    {ok, ClientSock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    {ok, _ServerSock} = gen_tcp:accept(ListenSock, 1000),

    %% Start hackney_conn with pre-established socket
    Opts = #{
        host => "127.0.0.1",
        port => Port,
        transport => hackney_tcp,
        socket => ClientSock
    },
    {ok, Pid} = hackney_conn:start_link(Opts),

    %% Should start in connected state, not idle
    {ok, State} = hackney_conn:get_state(Pid),
    ?assertEqual(connected, State),

    hackney_conn:stop(Pid),
    gen_tcp:close(ListenSock).

test_connect() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ?assertEqual({ok, idle}, hackney_conn:get_state(Pid)),
    Result = hackney_conn:connect(Pid),
    ?assertEqual(ok, Result),
    ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),
    hackney_conn:stop(Pid).

test_connect_timeout() ->
    %% Use a non-routable IP to trigger timeout
    %% Use longer timeouts to avoid flakiness on slow CI systems
    Opts = #{
        host => "10.255.255.1",
        port => 12345,
        transport => hackney_tcp,
        connect_timeout => 500
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    Result = hackney_conn:connect(Pid, 2000),
    ?assertMatch({error, _}, Result),
    %% Process should have stopped
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

test_connect_invalid() ->
    %% Connect to a port that refuses connections
    Opts = #{
        host => "127.0.0.1",
        port => 1,  %% Usually not available
        transport => hackney_tcp,
        connect_timeout => 1000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    Result = hackney_conn:connect(Pid),
    ?assertMatch({error, _}, Result),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

test_reconnect() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        idle_timeout => 50  %% Short timeout for testing
    },
    {ok, Pid} = hackney_conn:start_link(Opts),

    %% First connect
    ok = hackney_conn:connect(Pid),
    ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),

    %% Wait for idle timeout
    timer:sleep(100),
    ?assertEqual({ok, closed}, hackney_conn:get_state(Pid)),

    %% Reconnect
    ok = hackney_conn:connect(Pid),
    ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),

    hackney_conn:stop(Pid).

test_owner_death() ->
    Opts = #{
        host => "localhost",
        port => ?PORT,
        transport => hackney_tcp
    },

    %% Start connection from a temporary process
    Self = self(),
    Spawner = spawn(fun() ->
        {ok, Pid} = hackney_conn:start_link(Opts),
        Self ! {conn_pid, Pid},
        receive
            die -> ok
        end
    end),

    Pid = receive
        {conn_pid, P} -> P
    after 1000 ->
        error(timeout)
    end,

    ?assert(is_process_alive(Pid)),

    %% Kill the owner
    Spawner ! die,
    timer:sleep(50),

    %% Connection should have stopped
    ?assertNot(is_process_alive(Pid)).

%% #850: when a checkout races a server-side close, the pool calls set_owner on
%% a connection that has just transitioned to `closed`. It must get
%% {error, invalid_state} back (so the pool can fall through to a fresh
%% connection) rather than crash. A non-pooled connection has no grace timer,
%% so it stays in `closed` to answer.
test_set_owner_closed_returns_error() ->
    {Pid, ListenSock} = connected_conn(#{}),
    ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),
    ok = hackney_conn:close(Pid),
    ?assertEqual({ok, closed}, hackney_conn:get_state(Pid)),
    ?assertEqual({error, invalid_state}, hackney_conn:set_owner(Pid, self())),
    hackney_conn:stop(Pid),
    gen_tcp:close(ListenSock).

%% #850: the async ownership handoff (pool checkin/prewarm) to a *pooled*
%% connection that already closed must stop it promptly so the pool's monitor
%% removes it from `available`, instead of lingering through the grace window.
%% Dying within ~10 ms (well under ?CLOSED_GRACE_MS = 50 ms) shows the cast
%% stopped it, not the grace timer.
test_set_owner_async_closed_pooled_stops() ->
    {Pid, ListenSock} = connected_conn(#{pool_pid => self()}),
    ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),
    ok = hackney_conn:close(Pid),
    hackney_conn:set_owner_async(Pid, self()),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)),
    gen_tcp:close(ListenSock).

%% #861: a pooled connection can stop between checkout and the call, so a
%% request to an already-dead connection must return {error, closed} rather
%% than letting exit:{normal,_}/noproc crash the caller.
test_request_dead_conn_returns_error() ->
    Pid = dead_conn_pid(),
    ?assertEqual({error, closed},
                 hackney_conn:request(Pid, <<"GET">>, <<"/">>, [], <<>>)).

test_body_dead_conn_returns_error() ->
    Pid = dead_conn_pid(),
    ?assertEqual({error, closed}, hackney_conn:body(Pid)).

%% A pid that has already terminated normally.
dead_conn_pid() ->
    {Pid, Ref} = spawn_monitor(fun() -> ok end),
    receive {'DOWN', Ref, process, Pid, _} -> ok after 1000 -> ok end,
    Pid.

%% Start a hackney_conn already in `connected` state via a pre-established
%% local socket pair (no server needed). Extra opts are merged in.
connected_conn(Extra) ->
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(ListenSock),
    {ok, ClientSock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    {ok, _ServerSock} = gen_tcp:accept(ListenSock, 1000),
    Opts = maps:merge(#{host => "127.0.0.1", port => Port,
                        transport => hackney_tcp, socket => ClientSock}, Extra),
    {ok, Pid} = hackney_conn:start_link(Opts),
    {Pid, ListenSock}.

%%====================================================================
%% Request/Response Tests
%%====================================================================

test_get_request() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Send GET request
    {ok, Status, Headers} = hackney_conn:request(
        Pid, <<"GET">>, <<"/get">>, [], <<>>
    ),
    ?assert(Status >= 200 andalso Status < 400),
    ?assert(is_list(Headers)),

    %% Read body
    {ok, Body} = hackney_conn:body(Pid),
    ?assert(is_binary(Body)),
    ?assert(byte_size(Body) > 0),

    hackney_conn:stop(Pid).

%% GHSA-j9wq: a request target carrying raw CR/LF (e.g. from an
%% unsanitised query string) must be refused before anything is written to
%% the socket, otherwise the bytes split the request line into extra header
%% lines.
test_crlf_request_target_rejected() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),
    Evil = <<"/get?q=x HTTP/1.1\r\nX-Injected: yes\r\nX:">>,
    ?assertEqual({error, {invalid_request_target, Evil}},
                 hackney_conn:request(Pid, <<"GET">>, Evil, [], <<>>)),
    %% Connection survives the rejection and still serves a clean request.
    {ok, Status, _} = hackney_conn:request(Pid, <<"GET">>, <<"/get">>, [], <<>>),
    ?assert(Status >= 200 andalso Status < 400),
    hackney_conn:stop(Pid).

test_post_request() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Send POST request with body
    ReqBody = <<"test data">>,
    ReqHeaders = [{<<"Content-Type">>, <<"text/plain">>}],
    {ok, Status, _Headers} = hackney_conn:request(
        Pid, <<"POST">>, <<"/post">>, ReqHeaders, ReqBody
    ),
    ?assert(Status >= 200 andalso Status < 400),

    %% Read body
    {ok, Body} = hackney_conn:body(Pid),
    ?assert(is_binary(Body)),

    hackney_conn:stop(Pid).

test_stream_body() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Request /get
    {ok, Status, _Headers} = hackney_conn:request(
        Pid, <<"GET">>, <<"/get">>, [], <<>>
    ),
    ?assert(Status >= 200 andalso Status < 400),

    %% Stream body
    Body = stream_all(Pid, <<>>),
    ?assert(is_binary(Body)),
    ?assert(byte_size(Body) > 0),

    hackney_conn:stop(Pid).

test_stream_body_stateless_fun() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Create stateless function using ets table for state
    %% (function runs in hackney_conn process, not test process)
    Chunks = [<<"chunk1">>, <<"chunk2">>, <<"chunk3">>],
    Tab = ets:new(stream_test, [public, set]),
    ets:insert(Tab, {chunks, Chunks}),
    Fun = fun() ->
        case ets:lookup(Tab, chunks) of
            [{chunks, []}] -> eof;
            [{chunks, [H | T]}] ->
                ets:insert(Tab, {chunks, T}),
                {ok, H}
        end
    end,

    %% Send headers for streaming body (Host header required for HTTP/1.1)
    Headers = [{<<"Host">>, <<"127.0.0.1:", (integer_to_binary(?PORT))/binary>>},
               {<<"Content-Type">>, <<"text/plain">>}],
    ok = hackney_conn:send_request_headers(Pid, <<"POST">>, <<"/post">>, Headers),

    %% Send body using function
    ok = hackney_conn:send_body_chunk(Pid, Fun),
    ok = hackney_conn:finish_send_body(Pid),

    %% Get response
    {ok, Status, _RespHeaders, _} = hackney_conn:start_response(Pid),
    ?assert(Status >= 200 andalso Status < 300),

    %% Read response body
    {ok, RespBody} = hackney_conn:body(Pid),
    ?assert(is_binary(RespBody)),

    hackney_conn:stop(Pid),
    ets:delete(Tab).

test_stream_body_stateful_fun() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Stateful function: fun(State) -> {ok, Data, NewState} | eof
    Chunks = [<<"hello ">>, <<"world">>, <<"!">>],
    Fun = fun
        ([]) -> eof;
        ([H | T]) -> {ok, H, T}
    end,

    %% Host header required for HTTP/1.1
    Headers = [{<<"Host">>, <<"127.0.0.1:", (integer_to_binary(?PORT))/binary>>},
               {<<"Content-Type">>, <<"text/plain">>}],
    ok = hackney_conn:send_request_headers(Pid, <<"POST">>, <<"/post">>, Headers),

    %% Send body using stateful function
    ok = hackney_conn:send_body_chunk(Pid, {Fun, Chunks}),
    ok = hackney_conn:finish_send_body(Pid),

    {ok, Status, _RespHeaders, _} = hackney_conn:start_response(Pid),
    ?assert(Status >= 200 andalso Status < 300),

    {ok, RespBody} = hackney_conn:body(Pid),
    ?assert(is_binary(RespBody)),

    hackney_conn:stop(Pid).

test_stream_body_fun_error() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Function that returns error immediately
    Fun = fun() -> {error, simulated_error} end,

    %% Host header required for HTTP/1.1
    Headers = [{<<"Host">>, <<"127.0.0.1:", (integer_to_binary(?PORT))/binary>>}],
    ok = hackney_conn:send_request_headers(Pid, <<"POST">>, <<"/post">>, Headers),

    %% Should return error
    {error, simulated_error} = hackney_conn:send_body_chunk(Pid, Fun),

    %% Connection should be in closed state after error
    timer:sleep(100),
    ?assertEqual({ok, closed}, hackney_conn:get_state(Pid)),
    hackney_conn:stop(Pid).

test_head_request() ->
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Send HEAD request
    {ok, Status, Headers} = hackney_conn:request(
        Pid, <<"HEAD">>, <<"/get">>, [], <<>>
    ),
    ?assert(Status >= 200 andalso Status < 400),
    ?assert(is_list(Headers)),

    %% Body should be empty for HEAD
    {ok, Body} = hackney_conn:body(Pid),
    ?assertEqual(<<>>, Body),

    hackney_conn:stop(Pid).

test_request_state_cycle() ->
    %% Tests that connection returns to connected state after request/response
    %% Note: If server sends Connection: close, connection will transition to closed
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),
    ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),

    %% First request
    {ok, Status1, _} = hackney_conn:request(Pid, <<"GET">>, <<"/get">>, [], <<>>),
    ?assert(Status1 >= 200 andalso Status1 < 400),
    ?assertEqual({ok, receiving}, hackney_conn:get_state(Pid)),

    {ok, _Body1} = hackney_conn:body(Pid),
    %% After body, should be connected (or closed if server sent Connection: close)
    {ok, State1} = hackney_conn:get_state(Pid),
    ?assert(State1 =:= connected orelse State1 =:= closed),

    hackney_conn:stop(Pid).

%%====================================================================
%% Async Tests
%%====================================================================

test_async_continuous() ->
    %% Test async mode with continuous streaming
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Send async request - returns connection PID as the ref
    {ok, Ref} = hackney_conn:request_async(
        Pid, <<"GET">>, <<"/get">>, [], <<>>, true
    ),
    ?assert(is_pid(Ref)),

    %% Receive messages
    Messages = receive_all_async(Ref, []),
    ?assert(length(Messages) >= 2), %% At least status, headers, done

    %% Should have status message
    ?assert(lists:any(fun({status, S, _}) -> S >= 200; (_) -> false end, Messages)),
    %% Should have headers message
    ?assert(lists:any(fun({headers, _}) -> true; (_) -> false end, Messages)),
    %% Should have done message
    ?assert(lists:member(done, Messages)),

    hackney_conn:stop(Pid).

test_async_once() ->
    %% Test async once mode with stream_next
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    %% Send async request in once mode - returns connection PID as the ref
    {ok, Ref} = hackney_conn:request_async(
        Pid, <<"GET">>, <<"/get">>, [], <<>>, once
    ),
    ?assert(is_pid(Ref)),

    %% Should receive status
    receive
        {hackney_response, Ref, {status, Status, _Reason}} ->
            ?assert(Status >= 200 andalso Status < 400)
    after 5000 ->
        ?assert(false)
    end,

    %% Should receive headers
    receive
        {hackney_response, Ref, {headers, Headers}} ->
            ?assert(is_list(Headers))
    after 5000 ->
        ?assert(false)
    end,

    %% Now we need to call stream_next to get body chunks
    hackney_conn:stream_next(Pid),

    %% Collect remaining messages
    Messages = receive_all_async_with_next(Ref, Pid, []),
    ?assert(lists:member(done, Messages)),

    hackney_conn:stop(Pid).

test_set_owner_while_receiving() ->
    %% HTTP/1.1: a short-lived worker starts the response and reads one chunk,
    %% then a third process reassigns ownership to a long-lived reader. The
    %% original owner exits and the reader drains the body to done, proving the
    %% connection is no longer torn down by the original owner's DOWN.
    Size = 200000,
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    Parent = self(),
    Path = <<"/chunked/", (integer_to_binary(Size))/binary>>,

    {Worker, WMon} = spawn_monitor(fun() ->
        receive go -> ok end,
        {ok, 200, _} = hackney_conn:request(Pid, <<"GET">>, Path, [], <<>>),
        {ok, C1} = hackney_conn:stream_body(Pid),
        Parent ! {chunk1, self(), C1},
        receive stop -> ok end
    end),

    %% Tie the connection lifecycle to the worker (connected state).
    ok = hackney_conn:set_owner(Pid, Worker),
    Worker ! go,

    FirstChunk = receive
        {chunk1, Worker, C} -> C
    after 5000 -> error(chunk1_timeout)
    end,
    ?assert(byte_size(FirstChunk) > 0),

    %% Reassign ownership mid-stream (receiving state) to a long-lived reader
    %% that stays alive as owner until we have verified.
    Reader = spawn(fun() ->
        Rest = stream_all(Pid, <<>>),
        Parent ! {rest, self(), Rest},
        receive stop -> ok end
    end),
    ?assertEqual(ok, hackney_conn:set_owner(Pid, Reader)),

    %% Original owner exits; the connection must survive.
    Worker ! stop,
    receive {'DOWN', WMon, process, Worker, _} -> ok after 5000 -> error(worker_down_timeout) end,
    ?assert(is_process_alive(Pid)),

    Rest = receive
        {rest, Reader, R} -> R
    after 10000 -> error(rest_timeout)
    end,

    ?assertEqual(Size, byte_size(FirstChunk) + byte_size(Rest)),
    Reader ! stop,
    hackney_conn:stop(Pid).

test_set_owner_while_streaming() ->
    %% Async continuous: the request runs with the worker as lifecycle owner and
    %% a separate collector as stream_to. Mid-stream (streaming state) a third
    %% process reassigns ownership away from the worker, which then exits without
    %% stopping the connection. stream_to is unchanged, so the collector still
    %% receives every message through done.
    Size = 2000000,
    Opts = #{
        host => "127.0.0.1",
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid),

    Parent = self(),
    Path = <<"/chunked/", (integer_to_binary(Size))/binary>>,

    {Owner, OMon} = spawn_monitor(fun() -> receive stop -> ok end end),
    %% Owner becomes the lifecycle owner (connected state).
    ok = hackney_conn:set_owner(Pid, Owner),

    %% Collector issues the async request as its own caller, so stream_to is the
    %% collector while owner stays Owner (do_request_async leaves owner unchanged
    %% when StreamTo == caller).
    Collector = spawn(fun() ->
        {ok, Ref} = hackney_conn:request_async(Pid, <<"GET">>, Path, [], <<>>, true),
        Parent ! {started, self()},
        Msgs = receive_all_async(Ref, []),
        Parent ! {collected, self(), Msgs}
    end),

    %% Wait until the request is issued (streaming has begun), then reassign
    %% ownership to the long-lived parent while the body is still draining.
    receive {started, Collector} -> ok after 5000 -> error(started_timeout) end,
    ?assertEqual(ok, hackney_conn:set_owner(Pid, self())),

    %% Original owner exits; the connection must survive.
    Owner ! stop,
    receive {'DOWN', OMon, process, Owner, _} -> ok after 5000 -> error(owner_down_timeout) end,
    ?assert(is_process_alive(Pid)),

    Msgs = receive
        {collected, Collector, M} -> M
    after 15000 -> error(collect_timeout)
    end,
    ?assert(lists:member(done, Msgs)),
    ?assertEqual(Size, iolist_size([B || B <- Msgs, is_binary(B)])),
    hackney_conn:stop(Pid).

%%====================================================================
%% 1XX Response Tests
%%====================================================================

test_skip_1xx_responses() ->
    %% Start a mock server that sends 103 Early Hints before 200 OK
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, MockPort} = inet:port(ListenSock),

    %% Spawn a process to handle the connection
    Self = self(),
    spawn_link(fun() ->
        {ok, Sock} = gen_tcp:accept(ListenSock, 5000),
        %% Read the HTTP request (simple approach - just read some data)
        _ = gen_tcp:recv(Sock, 0, 5000),
        %% Send 103 Early Hints followed immediately by 200 OK
        Response = <<"HTTP/1.1 103 Early Hints\r\n",
                     "Link: </style.css>; rel=preload; as=style\r\n",
                     "\r\n",
                     "HTTP/1.1 200 OK\r\n",
                     "Content-Type: text/plain\r\n",
                     "Content-Length: 13\r\n",
                     "\r\n",
                     "Hello, World!">>,
        ok = gen_tcp:send(Sock, Response),
        %% Wait for client to read before closing
        receive after 1000 -> ok end,
        gen_tcp:close(Sock),
        Self ! mock_server_done
    end),

    %% Use hackney directly (which uses hackney_conn internally)
    URL = iolist_to_binary(["http://127.0.0.1:", integer_to_list(MockPort), "/"]),
    {ok, Status, Headers, Body} = hackney:request(get, URL, [], <<>>, []),

    %% Verify we got the final 200 response, not the 103
    ?assertEqual(200, Status),
    ?assert(is_list(Headers)),

    %% Body is now returned directly
    ?assertEqual(<<"Hello, World!">>, Body),

    %% Cleanup
    gen_tcp:close(ListenSock),
    receive mock_server_done -> ok after 1000 -> ok end.

%%====================================================================
%% Helpers
%%====================================================================

receive_all_async(Ref, Acc) ->
    receive
        {hackney_response, Ref, done} ->
            lists:reverse([done | Acc]);
        {hackney_response, Ref, {error, _} = Error} ->
            lists:reverse([Error | Acc]);
        {hackney_response, Ref, Msg} ->
            receive_all_async(Ref, [Msg | Acc])
    after 5000 ->
        lists:reverse(Acc)
    end.

receive_all_async_with_next(Ref, Pid, Acc) ->
    receive
        {hackney_response, Ref, done} ->
            lists:reverse([done | Acc]);
        {hackney_response, Ref, {error, _} = Error} ->
            lists:reverse([Error | Acc]);
        {hackney_response, Ref, Msg} ->
            hackney_conn:stream_next(Pid),
            receive_all_async_with_next(Ref, Pid, [Msg | Acc])
    after 5000 ->
        lists:reverse(Acc)
    end.

stream_all(Pid, Acc) ->
    case hackney_conn:stream_body(Pid) of
        {ok, Chunk} ->
            stream_all(Pid, <<Acc/binary, Chunk/binary>>);
        done ->
            Acc
    end.
