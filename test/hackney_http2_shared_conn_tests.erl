%%% Lifetime of pooled HTTP/2 connections shared between callers (#937).
%%%
%%% A pooled HTTP/2 connection used to stay owned by the caller that dialed
%%% it: when that caller exited, the connection stopped and every other
%%% caller's stream on it failed with {error, closed}. A shared connection
%%% now has no owner. Each stream is tied to its own caller, and the
%%% connection closes itself once it has been idle for the pool timeout.
-module(hackney_http2_shared_conn_tests).

-include_lib("eunit/include/eunit.hrl").

shared_conn_test_() ->
    [{timeout, 30, {Title, Fun}} || {Title, Fun} <- [
        {"dialer exiting does not close other callers' streams",
         fun dialer_exit_keeps_other_streams/0},
        {"a killed caller resets only its own stream",
         fun killed_caller_resets_its_stream/0},
        {"a dead async consumer resets only its own stream",
         fun dead_async_consumer/0},
        {"a dead uploader frees the connection for new requests",
         fun dead_uploader/0},
        {"stream monitors are removed when streams end",
         fun stream_monitors_removed/0},
        {"an idle shared connection closes and releases its slot",
         fun idle_conn_closes/0},
        {"the idle timer waits for open streams",
         fun idle_waits_for_streams/0},
        {"an unregistered connection still releases its slot",
         fun unregistered_conn_releases_slot/0},
        {"an unpooled connection is not shared",
         fun unpooled_conn_not_shared/0}
    ]].

%%====================================================================
%% Tests
%%====================================================================

dialer_exit_keeps_other_streams() ->
    with_server([], fun(URL, _Port, Opts) ->
        {First, FirstRef} = spawn_monitor(fun() ->
            Result = hackney:request(get, <<URL/binary, "/first">>, [], <<>>, Opts),
            exit({result, Result})
        end),
        {FirstHandler, SConn} = started(<<"/first">>),
        _ = sync_request(second, <<URL/binary, "/second">>, Opts),
        {SecondHandler, SConn2} = started(<<"/second">>),
        ?assertEqual(SConn, SConn2),
        FirstHandler ! respond,
        receive
            {'DOWN', FirstRef, process, First, {result, FirstResult}} ->
                ?assertMatch({ok, 200, _, <<"ok">>}, FirstResult)
        after 5000 -> error(no_first_result)
        end,
        SecondHandler ! respond,
        ?assertMatch({ok, 200, _, <<"ok">>}, result(second))
    end).

killed_caller_resets_its_stream() ->
    with_server([], fun(URL, Port, Opts) ->
        First = sync_request(first, <<URL/binary, "/first">>, Opts),
        {_FirstHandler, SConn} = started(<<"/first">>),
        Conn = shared_conn(Opts, Port),
        ?assert(lists:member(First, monitored(Conn))),
        exit(First, kill),
        ok = wait_until(fun() -> not lists:member(First, monitored(Conn)) end),
        _ = sync_request(second, <<URL/binary, "/second">>, Opts),
        {SecondHandler, SConn2} = started(<<"/second">>),
        ?assertEqual(SConn, SConn2),
        SecondHandler ! respond,
        ?assertMatch({ok, 200, _, <<"ok">>}, result(second)),
        ?assertEqual(Conn, shared_conn(Opts, Port))
    end).

dead_async_consumer() ->
    with_server([], fun(URL, Port, Opts) ->
        Parent = self(),
        Consumer = spawn(fun() ->
            {ok, _Ref} = hackney:request(get, <<URL/binary, "/async">>, [], <<>>,
                                         [async | Opts]),
            Parent ! async_sent,
            receive stop -> ok end
        end),
        receive async_sent -> ok after 5000 -> error(no_async_request) end,
        %% The handler never answers: a response crossing the client's
        %% RST_STREAM is a separate h2 HPACK issue, not what this covers.
        {_AsyncHandler, _} = started(<<"/async">>),
        Conn = shared_conn(Opts, Port),
        ?assert(lists:member(Consumer, monitored(Conn))),
        exit(Consumer, kill),
        ok = wait_until(fun() -> not lists:member(Consumer, monitored(Conn)) end),
        ?assertMatch({ok, 200, _, <<"ok">>},
                     hackney:request(get, <<URL/binary, "/fast">>, [], <<>>, Opts)),
        ?assertEqual(Conn, shared_conn(Opts, Port))
    end).

dead_uploader() ->
    with_server([], fun(URL, Port, Opts) ->
        Parent = self(),
        Uploader = spawn(fun() ->
            {ok, UploadConn} = hackney:request(post, <<URL/binary, "/upload">>, [],
                                               stream, Opts),
            ok = hackney:send_body(UploadConn, <<"chunk">>),
            Parent ! {uploading, UploadConn},
            receive stop -> ok end
        end),
        Conn = receive {uploading, C} -> C after 5000 -> error(no_upload) end,
        {_UploadHandler, _} = started(<<"/upload">>),
        ?assertEqual({ok, streaming_body}, hackney_conn:get_state(Conn)),
        exit(Uploader, kill),
        ok = wait_until(fun() ->
            hackney_conn:get_state(Conn) =:= {ok, connected}
        end),
        ?assertMatch({ok, 200, _, <<"ok">>},
                     hackney:request(get, <<URL/binary, "/fast">>, [], <<>>, Opts)),
        ?assertEqual(Conn, shared_conn(Opts, Port))
    end).

stream_monitors_removed() ->
    with_server([], fun(URL, Port, Opts) ->
        {ok, 200, _, <<"ok">>} =
            hackney:request(get, <<URL/binary, "/fast">>, [], <<>>, Opts),
        Conn = shared_conn(Opts, Port),
        Baseline = monitored(Conn),
        Callers = [begin
                       Pid = sync_request({fast, N}, <<URL/binary, "/fast">>, Opts),
                       ?assertMatch({ok, 200, _, _}, result({fast, N})),
                       Pid
                   end || N <- lists:seq(1, 20)],
        %% The recv_timeout watchdog path drops the stream monitor too.
        TimeoutOpts = lists:keystore(recv_timeout, 1, Opts, {recv_timeout, 100}),
        ?assertEqual({error, timeout},
                     hackney:request(get, <<URL/binary, "/never">>, [], <<>>,
                                     TimeoutOpts)),
        ?assertEqual(Baseline, monitored(Conn)),
        ?assertEqual([], [P || P <- Callers, lists:member(P, monitored(Conn))]),
        ?assertEqual(Conn, shared_conn(Opts, Port))
    end).

idle_conn_closes() ->
    with_server([{timeout, 100}], fun(URL, Port, Opts) ->
        {ok, 200, _, <<"ok">>} =
            hackney:request(get, <<URL/binary, "/fast">>, [], <<>>, Opts),
        Conn = shared_conn(Opts, Port),
        Ref = erlang:monitor(process, Conn),
        receive {'DOWN', Ref, process, Conn, _} -> ok
        after 5000 -> error(conn_did_not_idle_out)
        end,
        ok = wait_until(fun() ->
            hackney_load_regulation:current("localhost", Port) =:= 0
        end),
        ?assertMatch({ok, 200, _, <<"ok">>},
                     hackney:request(get, <<URL/binary, "/fast">>, [], <<>>, Opts))
    end).

idle_waits_for_streams() ->
    with_server([{timeout, 100}], fun(URL, Port, Opts) ->
        _ = sync_request(slow, <<URL/binary, "/slow">>, Opts),
        {Handler, _} = started(<<"/slow">>),
        Conn = shared_conn(Opts, Port),
        %% Hold the stream open well past the idle timeout.
        receive after 400 -> ok end,
        ?assertEqual({ok, connected}, hackney_conn:get_state(Conn)),
        Handler ! respond,
        ?assertMatch({ok, 200, _, <<"ok">>}, result(slow))
    end).

unregistered_conn_releases_slot() ->
    with_server([], fun(URL, Port, Opts) ->
        {ok, 200, _, <<"ok">>} =
            hackney:request(get, <<URL/binary, "/fast">>, [], <<>>, Opts),
        Conn = shared_conn(Opts, Port),
        ?assertEqual(1, hackney_load_regulation:current("localhost", Port)),
        ok = hackney_pool:unregister_h2(Conn, Opts),
        ok = wait_until(fun() -> no_shared_conn(Opts, Port) end),
        hackney_conn:stop(Conn),
        ok = wait_until(fun() ->
            hackney_load_regulation:current("localhost", Port) =:= 0
        end)
    end).

unpooled_conn_not_shared() ->
    with_server([], fun(URL, _Port, Opts) ->
        Parent = self(),
        UnpooledOpts = lists:keystore(pool, 1, Opts, {pool, false}),
        Uploader = spawn(fun() ->
            {ok, UploadConn} = hackney:request(post, <<URL/binary, "/upload">>, [],
                                               stream, UnpooledOpts),
            Parent ! {uploading, UploadConn},
            receive stop -> ok end
        end),
        Conn = receive {uploading, C} -> C after 5000 -> error(no_conn) end,
        {_Handler, _} = started(<<"/upload">>),
        ?assertEqual({error, invalid_state}, hackney_conn:share_h2(Conn)),
        %% Its lifetime stays with its owner, so streams are not monitored.
        ?assertNot(lists:member(Uploader, monitored(Conn))),
        exit(Uploader, kill),
        hackney_conn:stop(Conn)
    end).

%%====================================================================
%% Helpers
%%====================================================================

%% Start an h2 server and a pool of one connection per host. `/fast'
%% answers at once and `/never' never answers; any other path reports
%% {request_started, Path, Handler, ServerConn} and answers on `respond'.
with_server(PoolOpts, Fun) ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Parent = self(),
    Handler = fun(SConn, Sid, _Method, Path, _Headers) ->
        case Path of
            <<"/fast">> -> ok;
            <<"/never">> -> receive after infinity -> ok end;
            _ ->
                Parent ! {request_started, Path, self(), SConn},
                receive respond -> ok end
        end,
        ok = h2:send_response(SConn, Sid, 200,
                              [{<<"content-type">>, <<"text/plain">>}]),
        ok = h2:send_data(SConn, Sid, <<"ok">>, true)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler}),
    Port = h2:server_port(Server),
    Pool = list_to_atom("hackney_h2_shared_" ++ integer_to_list(Port)),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 1} | PoolOpts]),
    URL = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port)]),
    Opts = [{pool, Pool}, {protocols, [http2]}, {recv_timeout, 5000},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    try
        Fun(URL, Port, Opts)
    after
        _ = hackney_pool:stop_pool(Pool),
        _ = h2:stop_server(Server),
        hackney_load_regulation:reset("localhost", Port)
    end.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

sync_request(Tag, URL, Opts) ->
    Parent = self(),
    spawn(fun() ->
        Parent ! {result, Tag, hackney:request(get, URL, [], <<>>, Opts)}
    end).

result(Tag) ->
    receive {result, Tag, Result} -> Result
    after 5000 -> error({no_result, Tag})
    end.

started(Path) ->
    receive {request_started, Path, Handler, SConn} -> {Handler, SConn}
    after 5000 -> error({not_started, Path})
    end.

%% The pool keys shared connections by the TLS options hash, so read the one
%% registered for this port from the pool state instead of rebuilding the key.
shared_conn(Opts, Port) ->
    [Conn] = shared_conns(Opts, Port),
    Conn.

no_shared_conn(Opts, Port) ->
    shared_conns(Opts, Port) =:= [].

shared_conns(Opts, Port) ->
    PoolPid = hackney_pool:find_pool(proplists:get_value(pool, Opts)),
    State = sys:get_state(PoolPid),
    [Pid || Field <- tuple_to_list(State), is_map(Field),
            {{_Host, P, hackney_ssl, _TlsKey}, Pid} <- maps:to_list(Field),
            P =:= Port, is_pid(Pid)].

monitored(Pid) ->
    {monitors, Monitors} = process_info(Pid, monitors),
    lists:sort([P || {process, P} <- Monitors]).

wait_until(Fun) ->
    wait_until(Fun, erlang:monotonic_time(millisecond) + 5000).

wait_until(Fun, Deadline) ->
    case Fun() of
        true -> ok;
        false ->
            case erlang:monotonic_time(millisecond) > Deadline of
                true -> error(wait_until_timeout);
                false ->
                    receive after 5 -> ok end,
                    wait_until(Fun, Deadline)
            end
    end.
