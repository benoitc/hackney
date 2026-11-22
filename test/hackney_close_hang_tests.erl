%%% Closing an HTTP/1.1 request that waits for its response, or the death of
%%% its owner, acts at once: the conn stops with a normal exit, the server
%%% sees the socket close and the caller gets no message for the request.
%%% close/1 used to wait for the passive read in progress, which on a pooled
%%% connection was never (#996), and then for up to one second, the slice
%%% that read was cut into.
-module(hackney_close_hang_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, hackney_close_hang_pool).
%% The sliced read took about a second. This bound stays well clear of it
%% and leaves room for a slow CI host.
-define(FAST, 250).
-define(WAIT, 5000).
%% Status, headers and one chunk; the body stays open.
-define(PARTIAL, <<"HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
                   "2\r\nok\r\n">>).

close_hang_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{timeout, 30,
       {Title ++ " (" ++ atom_to_list(Proto) ++ ")",
        fun() -> with_server(Proto, Fun) end}}
      || Proto <- [tcp, tls],
         {Title, Fun} <- [
            {"close while the response is awaited", fun close_awaiting/1},
            {"owner killed while the response is awaited", fun owner_killed/1},
            {"a pooled conn closed while waiting is not reused", fun close_pooled/1},
            {"close during an async body", fun close_async_body/1},
            {"close during an {async, once} body", fun close_async_once_body/1},
            {"close notice between two reads", fun notice_between_reads/1},
            {"close during a streamed upload", fun close_upload/1},
            {"close of a held conn with a sync request", fun close_held_sync/1}]]}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 2}]).

cleanup(_) ->
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    ok.

%%====================================================================
%% Tests
%%====================================================================

close_awaiting(Srv) ->
    {ok, Conn} = hackney:get(url(Srv), [], <<>>, opts([async, {pool, false}])),
    H = wait_server(Srv, request_seen),
    close_fast(Srv, H, Conn).

owner_killed(Srv) ->
    Test = self(),
    Owner = spawn(fun() ->
        {ok, Conn} = hackney:get(url(Srv), [], <<>>, opts([async, {pool, false}])),
        Test ! {conn, Conn},
        receive after infinity -> ok end
    end),
    Conn = receive {conn, C} -> C after ?WAIT -> error(no_conn) end,
    H = wait_server(Srv, request_seen),
    MRef = erlang:monitor(process, Conn),
    T0 = now_ms(),
    exit(Owner, kill),
    wait_down(MRef, Conn),
    ?assert(now_ms() - T0 < ?FAST),
    wait_closed(Srv, H, T0).

%% The closed conn is neither handed back to the pool nor counted in use,
%% and the next request dials a new socket.
close_pooled(Srv) ->
    Pool = list_to_atom("hackney_close_hang_" ++ atom_to_list(maps:get(proto, Srv))),
    ok = hackney_pool:start_pool(Pool, [{pool_size, 1}]),
    try
        Opts = opts([async, {pool, Pool}]),
        {ok, Conn} = hackney:get(url(Srv), [], <<>>, Opts),
        H = wait_server(Srv, request_seen),
        close_fast(Srv, H, Conn),
        ok = wait_until(fun() ->
            proplists:get_value(in_use_count, hackney_pool:get_stats(Pool)) =:= 0
        end),
        ?assertEqual(0, proplists:get_value(free_count, hackney_pool:get_stats(Pool))),
        {ok, Conn2} = hackney:get(url(Srv), [], <<>>, Opts),
        ?assertNotEqual(Conn, Conn2),
        H2 = wait_server(Srv, request_seen),
        ?assertNotEqual(H, H2),
        close_fast(Srv, H2, Conn2)
    after
        hackney_pool:stop_pool(Pool)
    end.

close_async_body(Srv) ->
    {ok, Conn} = hackney:get(url(Srv), [], <<>>, opts([async, {pool, false}])),
    H = wait_server(Srv, request_seen),
    H ! {send, ?PARTIAL},
    await_headers(Conn),
    await_chunk(Conn),
    close_fast(Srv, H, Conn).

%% The pull after the first chunk leaves the conn reading a body that has
%% nothing more to give.
close_async_once_body(Srv) ->
    {ok, Conn} = hackney:get(url(Srv), [], <<>>, opts([{async, once}, {pool, false}])),
    H = wait_server(Srv, request_seen),
    H ! {send, ?PARTIAL},
    await_headers(Conn),
    ok = hackney:stream_next(Conn),
    await_chunk(Conn),
    ok = hackney:stream_next(Conn),
    close_fast(Srv, H, Conn).

%% The notice lands while the conn is not reading and a pull comes after it,
%% as when the pull comes from another process than the one closing: the
%% conn stops on the notice instead of waiting in that read.
notice_between_reads(Srv) ->
    {ok, Conn} = hackney:get(url(Srv), [], <<>>, opts([{async, once}, {pool, false}])),
    H = wait_server(Srv, request_seen),
    H ! {send, ?PARTIAL},
    await_headers(Conn),
    ok = hackney:stream_next(Conn),
    await_chunk(Conn),
    MRef = erlang:monitor(process, Conn),
    T0 = now_ms(),
    ok = hackney_conn:abort_read(Conn),
    ok = hackney:stream_next(Conn),
    wait_down(MRef, Conn),
    ?assert(now_ms() - T0 < ?FAST),
    wait_closed(Srv, H, T0),
    no_stray_messages(Conn).

close_upload(Srv) ->
    {ok, Conn} = hackney:request(post, url(Srv), [], stream, opts([{pool, false}])),
    ok = hackney:send_body(Conn, <<"chunk">>),
    H = wait_server(Srv, request_seen),
    close_fast(Srv, H, Conn).

%% A sync request blocked on a conn held through connect/2, closed from
%% another process: the request fails the way a stopped conn always made
%% it fail.
close_held_sync(Srv) ->
    {ok, Conn} = hackney:connect(url(Srv), opts([{pool, ?POOL}])),
    Test = self(),
    Req = spawn(fun() ->
        Test ! {result, hackney:send_request(Conn, {get, <<"/">>, [], <<>>})}
    end),
    H = wait_server(Srv, request_seen),
    MRef = erlang:monitor(process, Conn),
    T0 = now_ms(),
    ok = hackney:close(Conn),
    ?assert(now_ms() - T0 < ?FAST),
    receive {result, R} -> ?assertMatch({error, _}, R)
    after ?WAIT -> exit(Req, kill), error(no_result)
    end,
    wait_down(MRef, Conn),
    wait_closed(Srv, H, T0).

%%====================================================================
%% Helpers
%%====================================================================

opts(Extra) ->
    [{insecure, true} | Extra].

%% close/1 returns at once, the conn exits normal, the server sees the
%% socket close and no message for the request reaches the caller.
close_fast(Srv, H, Conn) ->
    MRef = erlang:monitor(process, Conn),
    T0 = now_ms(),
    ok = hackney:close(Conn),
    ?assert(now_ms() - T0 < ?FAST),
    wait_down(MRef, Conn),
    wait_closed(Srv, H, T0),
    no_stray_messages(Conn).

wait_down(MRef, Conn) ->
    receive {'DOWN', MRef, process, Conn, Reason} -> ?assertEqual(normal, Reason)
    after ?WAIT -> error(conn_still_up)
    end.

wait_closed(#{tag := Tag}, H, T0) ->
    receive {Tag, closed, H} -> ?assert(now_ms() - T0 < ?FAST)
    after ?WAIT -> error(socket_left_open)
    end.

wait_server(#{tag := Tag}, Event) ->
    receive {Tag, Event, H} -> H
    after ?WAIT -> error({server_event_missing, Event})
    end.

await_headers(Conn) ->
    receive {hackney_response, Conn, {status, 200, _}} -> ok
    after ?WAIT -> error(no_status)
    end,
    receive {hackney_response, Conn, {headers, _}} -> ok
    after ?WAIT -> error(no_headers)
    end.

await_chunk(Conn) ->
    receive {hackney_response, Conn, <<"ok">>} -> ok
    after ?WAIT -> error(no_chunk)
    end.

no_stray_messages(Conn) ->
    receive {hackney_response, Conn, _} = M -> ?assertEqual(none, M)
    after 200 -> ok
    end.

wait_until(Fun) ->
    wait_until(Fun, 50).

wait_until(_Fun, 0) ->
    timeout;
wait_until(Fun, N) ->
    case Fun() of
        true -> ok;
        false -> receive after 20 -> wait_until(Fun, N - 1) end
    end.

now_ms() ->
    erlang:monotonic_time(millisecond).

url(#{proto := tcp, port := Port}) ->
    iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/"]);
url(#{proto := tls, port := Port}) ->
    iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), "/"]).

%%====================================================================
%% Server: reads a request and answers only when told to ({send, Bytes}).
%% It reports, with its handler pid, each request it has read and each
%% socket close, over TCP or TLS.
%%====================================================================

with_server(Proto, Fun) ->
    Tag = make_ref(),
    Test = self(),
    {L, Port} = listen(Proto),
    Acceptor = spawn(fun() -> receive go -> accept_loop(Proto, L, Tag, Test) end end),
    ok = listener_owner(Proto, L, Acceptor),
    Acceptor ! go,
    try
        Fun(#{tag => Tag, port => Port, proto => Proto})
    after
        exit(Acceptor, kill),
        _ = close(Proto, L)
    end.

listen(tcp) ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(L),
    {L, Port};
listen(tls) ->
    Certs = cert_dir(),
    {ok, L} = ssl:listen(0, [{certfile, filename:join(Certs, "server.pem")},
                             {keyfile, filename:join(Certs, "server.key")},
                             {versions, ['tlsv1.2', 'tlsv1.3']},
                             {ip, {127, 0, 0, 1}},
                             binary, {active, false}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(L),
    {L, Port}.

listener_owner(tcp, L, Pid) -> gen_tcp:controlling_process(L, Pid);
listener_owner(tls, L, Pid) -> ssl:controlling_process(L, Pid).

close(tcp, S) -> gen_tcp:close(S);
close(tls, S) -> ssl:close(S).

accept_loop(Proto, L, Tag, Test) ->
    case accept(Proto, L) of
        {ok, S} ->
            H = spawn(fun() -> receive go -> handle(Proto, S, Tag, Test) end end),
            ok = socket_owner(Proto, S, H),
            H ! go,
            accept_loop(Proto, L, Tag, Test);
        {error, closed} ->
            ok;
        {error, _} ->
            accept_loop(Proto, L, Tag, Test)
    end.

accept(tcp, L) ->
    gen_tcp:accept(L);
accept(tls, L) ->
    case ssl:transport_accept(L) of
        {ok, T} -> ssl:handshake(T, 5000);
        Error -> Error
    end.

socket_owner(tcp, S, Pid) -> gen_tcp:controlling_process(S, Pid);
socket_owner(tls, S, Pid) -> ssl:controlling_process(S, Pid).

handle(tcp, S, Tag, Test) ->
    ok = inet:setopts(S, [{active, true}]),
    handle_loop(tcp, S, Tag, Test, <<>>);
handle(tls, S, Tag, Test) ->
    ok = ssl:setopts(S, [{active, true}]),
    handle_loop(tls, S, Tag, Test, <<>>).

handle_loop(Proto, S, Tag, Test, Acc) ->
    receive
        {Data, S, Bytes} when Data =:= tcp; Data =:= ssl ->
            handle_loop(Proto, S, Tag, Test, request_seen(Acc, Bytes, Tag, Test));
        {send, Bytes} ->
            _ = case Proto of
                    tcp -> gen_tcp:send(S, Bytes);
                    tls -> ssl:send(S, Bytes)
                end,
            handle_loop(Proto, S, Tag, Test, Acc);
        {Closed, S} when Closed =:= tcp_closed; Closed =:= ssl_closed ->
            Test ! {Tag, closed, self()};
        {Error, S, _} when Error =:= tcp_error; Error =:= ssl_error ->
            Test ! {Tag, closed, self()}
    end.

%% Report the request once its headers are in; `seen' afterwards.
request_seen(seen, _Bytes, _Tag, _Test) ->
    seen;
request_seen(Acc, Bytes, Tag, Test) ->
    Acc1 = <<Acc/binary, Bytes/binary>>,
    case binary:match(Acc1, <<"\r\n\r\n">>) of
        nomatch ->
            Acc1;
        _ ->
            Test ! {Tag, request_seen, self()},
            seen
    end.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
