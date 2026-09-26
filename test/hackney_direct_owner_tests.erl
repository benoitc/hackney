%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc A connection opened outside the pool is owned by the process that
%%% opened it. It is started under hackney_conn_sup, so without an explicit
%%% owner the supervisor owned it and the connection outlived a caller that
%%% died (idle_timeout defaults to infinity).
%%%
%%% Each test opens a connection from a throwaway process to a local server
%%% that holds the request, kills that process, then waits for the
%%% hackney_conn process to go down and for the server to see its socket
%%% close.
%%%
%%% An HTTP/1.1 conn waiting for a response reads in slices and checks for
%%% its owner's 'DOWN' between them, so those tests keep the server silent.
-module(hackney_direct_owner_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WAIT, 5000).

-define(FULL, <<"HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok">>).
%% Status, headers and one chunk; the body stays open.
-define(PARTIAL, <<"HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
                   "2\r\nok\r\n">>).

direct_owner_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(hackney),
         {ok, _} = application:ensure_all_started(h2),
         ok
     end,
     fun(_) -> ok end,
     [{"request/5 without a pool", {timeout, 30, fun t_request/0}},
      {"connect/2 without a pool", {timeout, 30, fun t_connect/0}},
      {"streamed request body", {timeout, 30, fun t_stream_body/0}},
      {"async response to the caller", {timeout, 30, fun t_async_self/0}},
      {"async response owned by stream_to", {timeout, 30, fun t_async_stream_to/0}},
      {"plain HTTP proxy", {timeout, 30, fun t_http_proxy/0}},
      {"HTTP CONNECT tunnel", {timeout, 30, fun t_connect_proxy/0}},
      {"SOCKS5 tunnel", {timeout, 30, fun t_socks5_proxy/0}},
      {"HTTP/2 request without a pool", {timeout, 30, fun t_h2_request/0}},
      {"h2_open stream process killed", {timeout, 30, fun t_h2_open/0}},
      {"HTTP/3 connect without a pool", {timeout, 60, fun t_h3_connect/0}},
      {"async body read blocked on a silent server", {timeout, 30, fun t_async_blocked/0}},
      {"pooled conn blocked on a silent server", {timeout, 30, fun t_pooled_blocked/0}},
      {"set_owner/2 still moves ownership", {timeout, 30, fun t_set_owner/0}}]}.

%%====================================================================
%% Tests
%%====================================================================

t_request() ->
    with_hold_server(fun(Srv) ->
        Url = url(Srv),
        kill_and_check(Srv, request_seen, none, fun() ->
            hackney:request(get, Url, [], <<>>, opts())
        end)
    end).

t_connect() ->
    with_hold_server(fun(Srv) ->
        Url = url(Srv),
        kill_and_check(Srv, accepted, none, fun() ->
            {ok, _Conn} = hackney:connect(Url, opts()),
            block()
        end)
    end).

t_stream_body() ->
    with_hold_server(fun(Srv) ->
        Url = url(Srv),
        kill_and_check(Srv, request_seen, none, fun() ->
            {ok, Conn} = hackney:request(post, Url, [], stream, opts()),
            ok = hackney:send_body(Conn, <<"partial">>),
            block()
        end)
    end).

%% {async, once}: after the headers the conn waits for stream_next/1 instead
%% of reading, so it would sit there forever once its consumer is gone.
t_async_self() ->
    with_hold_server(fun(Srv) ->
        Url = url(Srv),
        kill_and_check(Srv, request_seen, ?PARTIAL, fun() ->
            {ok, _Ref} = hackney:request(get, Url, [], <<>>,
                                         [{async, once} | opts()]),
            block()
        end)
    end).

%% With stream_to set to another process, that process owns the connection:
%% the requester may exit once the request is sent, and the connection closes
%% when the consumer dies.
t_async_stream_to() ->
    with_hold_server(fun(Srv) ->
        Url = url(Srv),
        Before = conn_pids(),
        Test = self(),
        Consumer = spawn(fun() -> forward(Test) end),
        ConsumerMon = monitor(process, Consumer),
        Requester = spawn(fun() ->
            {ok, _Ref} = hackney:request(get, Url, [], <<>>,
                                         [{async, once}, {stream_to, Consumer} | opts()]),
            Test ! {requested, self()},
            block()
        end),
        receive {requested, Requester} -> ok after ?WAIT -> error(no_request) end,
        H = wait_server(Srv, request_seen),
        Conn = new_conn(Before),
        ConnMon = monitor(process, Conn),
        kill_and_wait(Requester),
        H ! {send, ?PARTIAL},
        receive
            {consumer, {hackney_response, Conn, {headers, _}}} -> ok
        after ?WAIT ->
            error(no_headers)
        end,
        %% The requester is gone; the conn still serves the consumer.
        ?assertMatch({streaming_once, _}, sys:get_state(Conn)),
        kill_and_wait(Consumer, ConsumerMon),
        wait_down(ConnMon),
        wait_closed(Srv, H)
    end).

t_http_proxy() ->
    %% The hold server stands in for the proxy: the request goes to it with an
    %% absolute URL and is never answered.
    with_hold_server(fun(Srv) ->
        Opts = [{proxy, {"127.0.0.1", port(Srv)}} | opts()],
        kill_and_check(Srv, request_seen, none, fun() ->
            hackney:request(get, <<"http://example.invalid/">>, [], <<>>, Opts)
        end)
    end).

t_connect_proxy() ->
    {ok, Proxy, ProxyPort} = mock_proxy_server:start_connect_proxy(),
    try
        with_hold_server(fun(Srv) ->
            Url = url(Srv),
            Opts = [{proxy, {connect, "127.0.0.1", ProxyPort}} | opts()],
            kill_and_check(Srv, request_seen, none, fun() ->
                hackney:request(get, Url, [], <<>>, Opts)
            end)
        end)
    after
        mock_proxy_server:stop(Proxy)
    end.

t_socks5_proxy() ->
    {ok, Proxy, ProxyPort} = mock_proxy_server:start_socks5_proxy(),
    try
        with_hold_server(fun(Srv) ->
            Url = url(Srv),
            Opts = [{proxy, {socks5, "127.0.0.1", ProxyPort}} | opts()],
            kill_and_check(Srv, request_seen, none, fun() ->
                hackney:request(get, Url, [], <<>>, Opts)
            end)
        end)
    after
        mock_proxy_server:stop(Proxy)
    end.

t_h2_request() ->
    with_h2_server(fun(Srv) ->
        Url = h2_url(Srv),
        Opts = [{protocols, [http2]} | ssl_opts()],
        kill_and_check(Srv, request_seen, none, fun() ->
            hackney:request(get, Url, [], <<>>, Opts)
        end)
    end).

%% The h2_* stream process owns its dedicated connection. Killed outright its
%% terminate/3 never runs, so the connection must close on the owner monitor.
t_h2_open() ->
    with_h2_server(fun(Srv) ->
        Url = h2_url(Srv),
        Before = conn_pids(),
        Test = self(),
        _Caller = spawn(fun() ->
            {ok, Stream} = hackney:h2_open(Url, [], ssl_opts()),
            Test ! {stream, Stream},
            block()
        end),
        Stream = receive {stream, S} -> S after ?WAIT -> error(no_stream) end,
        H = wait_server(Srv, request_seen),
        Conn = new_conn(Before),
        ConnMon = monitor(process, Conn),
        kill_and_wait(Stream),
        wait_down(ConnMon),
        wait_closed(Srv, H)
    end).

%% QUIC has no socket the test can watch from the server side; the
%% connection process going down is the check (terminate/3 closes QUIC).
t_h3_connect() ->
    Server = hackney_h3_test_server:start(),
    hackney_altsvc:clear_all(),
    try
        Url = hackney_h3_test_server:url(Server, <<"/">>),
        Before = conn_pids(),
        Test = self(),
        Caller = spawn(fun() ->
            {ok, Conn} = hackney:connect(Url, [{pool, false} |
                                               hackney_h3_test_server:hackney_opts()]),
            Test ! {conn, Conn},
            block()
        end),
        Conn = receive {conn, C} -> C after 30000 -> error(no_conn) end,
        ?assertEqual(Conn, new_conn(Before)),
        ?assertEqual(http3, hackney_conn:get_protocol(Conn)),
        ConnMon = monitor(process, Conn),
        kill_and_wait(Caller),
        wait_down(ConnMon)
    after
        hackney_altsvc:clear_all(),
        hackney_h3_test_server:stop(Server)
    end.

%% {async, true}: after the first chunk the conn blocks reading the next one.
%% The consumer is killed only once that chunk has reached it.
t_async_blocked() ->
    with_hold_server(fun(Srv) ->
        Url = url(Srv),
        Before = conn_pids(),
        Test = self(),
        Consumer = spawn(fun() ->
            {ok, _Ref} = hackney:request(get, Url, [], <<>>, [async | opts()]),
            forward(Test)
        end),
        H = wait_server(Srv, request_seen),
        Conn = new_conn(Before),
        ConnMon = monitor(process, Conn),
        H ! {send, ?PARTIAL},
        receive
            {consumer, {hackney_response, Conn, <<"ok">>}} -> ok
        after ?WAIT ->
            error(no_chunk)
        end,
        kill_and_wait(Consumer),
        wait_down(ConnMon),
        wait_closed(Srv, H)
    end).

%% A pooled conn whose owner dies mid-read stops and gives back its slot:
%% with max_per_host 1, a second request only gets through once it has.
t_pooled_blocked() ->
    Pool = direct_owner_pool,
    ok = hackney_pool:start_pool(Pool, [{pool_size, 1}, {max_per_host, 1}]),
    try
        with_hold_server(fun(Srv) ->
            Url = url(Srv),
            Opts = [{pool, Pool}, {recv_timeout, infinity}],
            kill_and_check(Srv, request_seen, none, fun() ->
                hackney:request(get, Url, [], <<>>, Opts)
            end),
            Test = self(),
            Caller = spawn(fun() ->
                Test ! {second, hackney:request(get, Url, [], <<>>,
                                                [{checkout_timeout, ?WAIT} | Opts])}
            end),
            H = wait_server(Srv, request_seen),
            H ! {send, ?FULL},
            receive
                {second, Result} -> ?assertMatch({ok, 200, _, <<"ok">>}, Result)
            after ?WAIT ->
                exit(Caller, kill),
                error(slot_not_released)
            end
        end)
    after
        hackney_pool:stop_pool(Pool)
    end.

%% A caller that hands its connection to another process keeps working.
t_set_owner() ->
    with_hold_server(fun(Srv) ->
        Url = url(Srv),
        NewOwner = spawn(fun block/0),
        NewOwnerMon = monitor(process, NewOwner),
        Test = self(),
        Opener = spawn(fun() ->
            {ok, Conn} = hackney:connect(Url, opts()),
            ok = hackney_conn:set_owner(Conn, NewOwner),
            Test ! {conn, Conn},
            block()
        end),
        Conn = receive {conn, C} -> C after ?WAIT -> error(no_conn) end,
        H = wait_server(Srv, accepted),
        ConnMon = monitor(process, Conn),
        kill_and_wait(Opener),
        ?assertMatch({connected, _}, sys:get_state(Conn)),
        kill_and_wait(NewOwner, NewOwnerMon),
        wait_down(ConnMon),
        wait_closed(Srv, H)
    end).

%%====================================================================
%% Helpers
%%====================================================================

opts() ->
    [{pool, false}, {recv_timeout, infinity}].

ssl_opts() ->
    [{ssl_options, [{insecure, true}, {verify, verify_none}]} | opts()].

%% Run Fun in a new process, wait until the server reports Event, then kill
%% the process and check that its connection goes down and the server sees
%% the socket close. Reply, unless `none', is sent by the server after the
%% kill.
kill_and_check(Srv, Event, Reply, Fun) ->
    Before = conn_pids(),
    Caller = spawn(fun() -> _ = Fun(), block() end),
    H = wait_server(Srv, Event),
    Conn = new_conn(Before),
    ConnMon = monitor(process, Conn),
    kill_and_wait(Caller),
    _ = Reply =:= none orelse (H ! {send, Reply}),
    wait_down(ConnMon),
    wait_closed(Srv, H).

forward(Test) ->
    receive Msg -> Test ! {consumer, Msg} end,
    forward(Test).

block() ->
    receive after infinity -> ok end.

kill_and_wait(Pid) ->
    kill_and_wait(Pid, monitor(process, Pid)).

kill_and_wait(Pid, Mon) ->
    exit(Pid, kill),
    receive {'DOWN', Mon, process, Pid, _} -> ok end.

wait_down(ConnMon) ->
    receive
        {'DOWN', ConnMon, process, _, _} -> ok
    after ?WAIT ->
        error(connection_outlived_owner)
    end.

conn_pids() ->
    [Pid || {_, Pid, _, _} <- supervisor:which_children(hackney_conn_sup),
            is_pid(Pid)].

new_conn(Before) ->
    [Conn] = conn_pids() -- Before,
    Conn.

wait_server(#{tag := Tag}, Event) ->
    receive
        {Tag, Event, H} -> H
    after ?WAIT ->
        error({server_event_missing, Event})
    end.

wait_closed(#{tag := Tag}, H) ->
    receive
        {Tag, closed, H} -> ok
    after ?WAIT ->
        error(socket_left_open)
    end.

port(#{port := Port}) -> Port.

url(Srv) ->
    iolist_to_binary(["http://127.0.0.1:", integer_to_list(port(Srv)), "/"]).

%% TCP server that reads requests and answers only when told to
%% ({send, Bytes}). It reports each accepted socket, the first bytes read on
%% it, and its close.
with_hold_server(Fun) ->
    Tag = make_ref(),
    Test = self(),
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(L),
    Acceptor = spawn(fun() -> receive go -> accept_loop(L, Tag, Test) end end),
    ok = gen_tcp:controlling_process(L, Acceptor),
    Acceptor ! go,
    try
        Fun(#{tag => Tag, port => Port})
    after
        exit(Acceptor, kill)
    end.

accept_loop(L, Tag, Test) ->
    case gen_tcp:accept(L) of
        {ok, S} ->
            H = spawn(fun() -> receive go -> hold(S, Tag, Test) end end),
            ok = gen_tcp:controlling_process(S, H),
            H ! go,
            accept_loop(L, Tag, Test);
        {error, _} ->
            ok
    end.

hold(S, Tag, Test) ->
    Test ! {Tag, accepted, self()},
    ok = inet:setopts(S, [{active, true}]),
    hold_loop(S, Tag, Test, false).

hold_loop(S, Tag, Test, Seen) ->
    receive
        {tcp, S, _} when not Seen ->
            Test ! {Tag, request_seen, self()},
            hold_loop(S, Tag, Test, true);
        {tcp, S, _} ->
            hold_loop(S, Tag, Test, Seen);
        {send, Bytes} ->
            _ = gen_tcp:send(S, Bytes),
            hold_loop(S, Tag, Test, Seen);
        {tcp_closed, S} ->
            Test ! {Tag, closed, self()};
        {tcp_error, S, _} ->
            Test ! {Tag, closed, self()}
    end.

%% HTTP/2 server whose handler never answers. It reports the request and the
%% connection closing, like the hold server.
with_h2_server(Fun) ->
    Tag = make_ref(),
    Test = self(),
    Handler = fun(Conn, Sid, _Method, _Path, _Headers) ->
        ok = h2:set_stream_handler(Conn, Sid, self()),
        Test ! {Tag, request_seen, self()},
        h2_hold(Conn, Sid, Tag, Test)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    try
        Fun(#{tag => Tag, port => h2:server_port(Server)})
    after
        try h2:stop_server(Server) catch _:_ -> ok end
    end.

h2_hold(Conn, Sid, Tag, Test) ->
    receive
        {h2, Conn, {closed, _}} -> Test ! {Tag, closed, self()};
        {h2, Conn, {stream_reset, Sid, _}} -> Test ! {Tag, closed, self()};
        _ -> h2_hold(Conn, Sid, Tag, Test)
    end.

h2_url(Srv) ->
    iolist_to_binary(["https://localhost:", integer_to_list(port(Srv)), "/"]).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
