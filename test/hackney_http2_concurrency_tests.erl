%%% Regression tests for issue #836: HTTP/2 pooled shared connection wedges
%%% under concurrent sustained load.
%%%
%%% Before the fix, connected(enter) armed a 2s idle_timeout while the
%%% connection was still classified as HTTP/1.1 (pool checks out TCP, then
%%% upgrades to SSL+ALPN); the timer fired on a perfectly busy H2 conn and
%%% tore it down mid-request, crashing in-flight `gen_statem:call`s with
%%% `exit:{normal, _}`.
-module(hackney_http2_concurrency_tests).

-include_lib("eunit/include/eunit.hrl").

%% Resolve test cert dir from the module's beam location so the paths work
%% regardless of where eunit is run from.
cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    %% _build/test/lib/hackney/test -> project root -> test/certs
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

concurrent_tight_loop_test_() ->
    {timeout, 30, fun run_concurrent_tight_loop/0}.

pooled_sync_requests_share_connection_test_() ->
    {timeout, 30, fun run_pooled_sync_requests_share_connection/0}.

unpooled_response_owner_handoff_test_() ->
    {timeout, 30, fun run_unpooled_response_owner_handoff/0}.

busy_connection_retires_after_drain_test_() ->
    {timeout, 30, fun run_busy_connection_retires_after_drain/0}.

registration_failure_does_not_hang_test_() ->
    {timeout, 30, fun run_registration_failure_does_not_hang/0}.

busy_connection_becomes_reusable_test_() ->
    {timeout, 30, fun run_busy_connection_becomes_reusable/0}.

abandoned_upload_does_not_block_retirement_test_() ->
    {timeout, 30, fun run_abandoned_upload_does_not_block_retirement/0}.

checkout_timeout_honored_while_h2_busy_test_() ->
    {timeout, 30, fun run_checkout_timeout_honored_while_h2_busy/0}.

run_unpooled_response_owner_handoff() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Handler = fun(Conn, Sid, _Method, _Path, _Headers) ->
        send_ok(Conn, Sid)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    try
        URL = iolist_to_binary([<<"https://localhost:">>,
                                integer_to_list(h2:server_port(Server))]),
        Opts = [{pool, false}, {protocols, [http2]},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        Parent = self(),
        {Worker, WorkerRef} = spawn_monitor(fun() ->
            {ok, Conn} = hackney:request(post, URL, [], stream, Opts),
            ok = hackney:finish_send_body(Conn),
            {ok, 200, _, Conn} = hackney:start_response(Conn),
            ok = hackney_conn:set_owner(Conn, Parent),
            Parent ! {response_connection, Conn}
        end),
        Conn = receive {response_connection, Pid} -> Pid end,
        try
            receive {'DOWN', WorkerRef, process, Worker, normal} -> ok end,
            %% Wait for the connection to process the old requester's death.
            ok = wait_until(fun() ->
                {monitors, Monitors} = process_info(Conn, monitors),
                case lists:member({process, Worker}, Monitors) of
                    true -> false;
                    false -> ok
                end
            end, 1000),
            ?assertEqual({ok, <<"ok">>}, hackney:body(Conn))
        after
            catch hackney_conn:stop(Conn)
        end
    after
        catch h2:stop_server(Server)
    end.

run_checkout_timeout_honored_while_h2_busy() ->
    _ = application:ensure_all_started(hackney),
    PreviousPoolHandler = application:get_env(hackney, pool_handler),
    Pool = hackney_h2_checkout_deadline_pool,
    Port = 49199,
    Parent = self(),
    Busy = spawn(fun() -> busy_then_slow_h2(Parent, 0) end),
    _ = hackney_pool:start_pool(Pool, [{max_connections, 1}]),
    application:set_env(hackney, race_dead_pid, Busy),
    application:set_env(hackney, pool_handler, hackney_race_pool),
    ok = hackney_load_regulation:acquire("localhost", Port, 1, 0),
    try
        URL = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port)]),
        Opts = [{pool, Pool},
                {protocols, [http2]},
                {max_per_host, 1},
                {checkout_timeout, 25},
                {connect_timeout, 1000},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        ?assertEqual({error, checkout_timeout},
                     hackney:request(get, URL, [], <<>>, Opts)),
        receive
            {unexpected_h2_probe, Busy} -> ?assert(false)
        after 0 ->
            ok
        end
    after
        hackney_load_regulation:release("localhost", Port),
        restore_pool_handler(PreviousPoolHandler),
        application:unset_env(hackney, race_dead_pid),
        exit(Busy, kill),
        catch hackney_pool:stop_pool(Pool)
    end.

run_busy_connection_becomes_reusable() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    PreviousPoolHandler = application:get_env(hackney, pool_handler),
    Parent = self(),
    Handler = fun(Conn, Sid, _Method, Path, _Headers) ->
        Parent ! {request_started, Path, Conn},
        case Path of
            <<"/upload">> ->
                ok = h2:set_stream_handler(Conn, Sid, self()),
                _ = recv_request_body(Conn, Sid);
            _ ->
                ok
        end,
        send_ok(Conn, Sid)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    Port = h2:server_port(Server),
    Pool = hackney_h2_busy_reuse_pool,
    _ = hackney_pool:start_pool(Pool, [{max_connections, 1}]),
    try
        URL = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port)]),
        Opts = [{pool, Pool},
                {protocols, [http2]},
                {max_per_host, 1},
                {checkout_timeout, 500},
                {recv_timeout, 5000},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        {ok, First} = hackney:request(post, <<URL/binary, "/upload">>,
                                     [], stream, Opts),
        {request_started, <<"/upload">>, Conn} = receive
            UploadStarted = {request_started, <<"/upload">>, _} -> UploadStarted
        end,
        Second = spawn(fun() ->
            receive start -> ok end,
            Parent ! {second_result,
                      hackney:request(get, <<URL/binary, "/second">>,
                                      [], <<>>, Opts)}
        end),
        application:set_env(hackney, race_h2_checkout_observer,
                            {Parent, Second}),
        application:set_env(hackney, pool_handler, hackney_race_pool),
        Second ! start,
        receive
            {h2_checkout, Second, CheckoutResult} ->
                ?assertEqual(none, CheckoutResult)
        after 5000 ->
            ?assert(false)
        end,
        ok = hackney:finish_send_body(First),
        {ok, 200, _, First} = hackney:start_response(First),
        {ok, <<"ok">>} = hackney:body(First),
        receive
            {second_result, SecondResult} ->
                ?assertMatch({ok, 200, _, <<"ok">>}, SecondResult)
        after 1000 ->
            ?assert(false)
        end,
        receive
            {request_started, <<"/second">>, SecondConn} ->
                ?assertEqual(Conn, SecondConn)
        after 1000 ->
            ?assert(false)
        end
    after
        restore_pool_handler(PreviousPoolHandler),
        application:unset_env(hackney, race_h2_checkout_observer),
        catch hackney_pool:stop_pool(Pool),
        catch h2:stop_server(Server)
    end.

run_abandoned_upload_does_not_block_retirement() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Parent = self(),
    Handler = fun(Conn, Sid, _Method, _Path, _Headers) ->
        ok = h2:set_stream_handler(Conn, Sid, self()),
        Parent ! upload_handler_ready,
        _ = recv_request_body(Conn, Sid),
        send_ok(Conn, Sid)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    Port = h2:server_port(Server),
    Pool = hackney_h2_abandoned_upload_pool,
    _ = hackney_pool:start_pool(Pool, [{max_connections, 1}]),
    try
        URL = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port)]),
        Opts = [{pool, Pool},
                {protocols, [http2]},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        {Worker, WorkerRef} = spawn_monitor(fun() ->
            {ok, ConnPid} = hackney:request(post, URL, [], stream, Opts),
            Parent ! {upload_connection, ConnPid},
            receive abandon_upload -> ok end
        end),
        Conn = receive {upload_connection, ConnPid} -> ConnPid end,
        receive upload_handler_ready -> ok end,
        Worker ! abandon_upload,
        receive {'DOWN', WorkerRef, process, Worker, normal} -> ok end,
        ConnRef = monitor(process, Conn),
        ok = hackney_conn:retire_h2(Conn),
        receive
            {'DOWN', ConnRef, process, Conn, normal} -> ok
        after 1000 ->
            ?assert(false)
        end
    after
        catch hackney_pool:stop_pool(Pool),
        catch h2:stop_server(Server)
    end.

run_registration_failure_does_not_hang() ->
    _ = application:ensure_all_started(hackney),
    PreviousPoolHandler = application:get_env(hackney, pool_handler),
    Pool = hackney_h2_registration_failure_pool,
    Host = "h2-registration-failure.invalid",
    Port = 443,
    ok = hackney_load_regulation:reset(Host, Port),
    _ = hackney_pool:start_pool(Pool, [{max_connections, 1}]),
    try
        Opts = [{pool, Pool},
                {protocols, [http2]},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        application:set_env(hackney, race_register_h2_error, self()),
        application:set_env(hackney, pool_handler, hackney_race_pool),
        Parent = self(),
        spawn(fun() ->
            Parent ! {request_result,
                      hackney:request(get,
                                      <<"https://h2-registration-failure.invalid/">>,
                                      [], <<>>, Opts)}
        end),
        Conn = receive
            {h2_registration_candidate, Pid} -> Pid
        after 1000 ->
            ?assert(false)
        end,
        ConnRef = monitor(process, Conn),
        receive
            {request_result, Result} ->
                ?assertEqual({error, set_owner_failed}, Result)
        after 1000 ->
            ?assert(false)
        end,
        receive
            {'DOWN', ConnRef, process, Conn, _Reason} -> ok
        after 1000 ->
            ?assert(false)
        end
    after
        restore_pool_handler(PreviousPoolHandler),
        application:unset_env(hackney, race_register_h2_error),
        catch hackney_pool:stop_pool(Pool),
        hackney_load_regulation:reset(Host, Port)
    end.

run_busy_connection_retires_after_drain() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    PreviousPoolHandler = application:get_env(hackney, pool_handler),
    Handler = fun(Conn, Sid, _Method, Path, _Headers) ->
        case Path of
            <<"/upload">> ->
                ok = h2:set_stream_handler(Conn, Sid, self()),
                _ = recv_request_body(Conn, Sid),
                send_ok(Conn, Sid);
            _ ->
                send_ok(Conn, Sid)
        end
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    Port = h2:server_port(Server),
    Pool = hackney_h2_retire_pool,
    _ = hackney_pool:start_pool(Pool, [{max_connections, 2}]),
    try
        URL = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port)]),
        Opts = [{pool, Pool},
                {protocols, [http2]},
                {recv_timeout, 5000},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        {ok, First} = hackney:request(post, <<URL/binary, "/upload">>,
                                     [], stream, Opts),
        FirstRef = monitor(process, First),
        application:set_env(hackney, race_dead_pid, First),
        application:set_env(hackney, pool_handler, hackney_race_pool),
        ?assertMatch({ok, 200, _, <<"ok">>},
                     hackney:request(get, <<URL/binary, "/second">>,
                                     [], <<>>, Opts)),
        ok = hackney:finish_send_body(First),
        {ok, 200, _, First} = hackney:start_response(First),
        {ok, <<"ok">>} = hackney:body(First),
        receive
            {'DOWN', FirstRef, process, First, normal} -> ok
        after 1000 ->
            ?assert(false)
        end,
        ok = wait_until(fun() ->
            HostStats = hackney_pool:host_stats(Pool, "localhost", Port),
            case {proplists:get_value(in_use, HostStats),
                  proplists:get_value(free, HostStats)} of
                {1, 0} -> ok;
                _ -> false
            end
        end, 1000)
    after
        restore_pool_handler(PreviousPoolHandler),
        application:unset_env(hackney, race_dead_pid),
        catch hackney_pool:stop_pool(Pool),
        catch h2:stop_server(Server)
    end.

run_pooled_sync_requests_share_connection() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Parent = self(),
    Handler = fun(Conn, Sid, _Method, Path, _Headers) ->
        Parent ! {request_started, Path, self(), Conn, Sid},
        receive respond -> ok end,
        ok = h2:send_response(Conn, Sid, 200,
                              [{<<"content-type">>, <<"text/plain">>}]),
        ok = h2:send_data(Conn, Sid, <<"ok">>, true)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    Port = h2:server_port(Server),
    Pool = hackney_h2_sync_pool,
    _ = hackney_pool:start_pool(Pool, [{max_connections, 1}]),
    try
        URL = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port)]),
        Opts = [{pool, Pool},
                {protocols, [http2]},
                {recv_timeout, 5000},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        {First, FirstRef} = spawn_monitor(fun() ->
            Parent ! {request_result, first,
                      hackney:request(get, <<URL/binary, "/first">>, [], <<>>, Opts)}
        end),
        {request_started, <<"/first">>, FirstHandler, Conn, FirstSid} =
            receive FirstStarted = {request_started, <<"/first">>, _, _, _} ->
                FirstStarted
            end,
        spawn(fun() ->
            Parent ! {request_result, second,
                      hackney:request(get, <<URL/binary, "/second">>, [], <<>>, Opts)}
        end),
        {request_started, <<"/second">>, SecondHandler, Conn, SecondSid} =
            receive SecondStarted = {request_started, <<"/second">>, _, _, _} ->
                SecondStarted
            end,
        ?assertNotEqual(FirstSid, SecondSid),

        FirstHandler ! respond,
        receive
            {request_result, first, FirstResult} ->
                ?assertMatch({ok, 200, _, <<"ok">>}, FirstResult)
        end,
        receive {'DOWN', FirstRef, process, First, normal} -> ok end,

        SecondHandler ! respond,
        receive
            {request_result, second, SecondResult} ->
                ?assertMatch({ok, 200, _, <<"ok">>}, SecondResult)
        end
    after
        catch hackney_pool:stop_pool(Pool),
        catch h2:stop_server(Server)
    end.

run_concurrent_tight_loop() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Handler = fun(Conn, Sid, _M, _P, _H) ->
        ok = h2:send_response(Conn, Sid, 200,
                              [{<<"content-type">>, <<"text/plain">>}]),
        ok = h2:send_data(Conn, Sid, <<"ok">>, true)
    end,
    Certs = cert_dir(),
    %% h2 0.6.0 defaults SETTINGS_MAX_CONCURRENT_STREAMS to 100 (RFC 9113
    %% §5.1.2 floor); this tight-loop test multiplexes more than that and
    %% would otherwise hit {error, max_streams_exceeded}.
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => Handler,
        settings => #{max_concurrent_streams => unlimited}
    }),
    Port = h2:server_port(Server),
    Pool = hackney_h2_concurrency_pool,
    _ = hackney_pool:start_pool(Pool, [{max_connections, 10}]),
    try
        URL = iolist_to_binary([<<"https://localhost:">>,
                                integer_to_list(Port), <<"/">>]),
        Opts = [{pool, Pool},
                {protocols, [http2]},
                {recv_timeout, 5000},
                {ssl_options, [{insecure, true}, {verify, verify_none}]}],
        {ok, 200, _, _} = hackney:request(get, URL, [], <<>>, Opts),
        Parent = self(),
        Deadline = erlang:monotonic_time(millisecond) + 3000,
        Worker = fun(Name) ->
            (fun Self(Count) ->
                case erlang:monotonic_time(millisecond) < Deadline of
                    true ->
                        case hackney:request(get, URL, [], <<>>, Opts) of
                            {ok, 200, _, _} -> Self(Count + 1);
                            Other ->
                                Parent ! {worker, Name, {error, Count, Other}}
                        end;
                    false -> Parent ! {worker, Name, {done, Count}}
                end
             end)(0)
        end,
        spawn_link(fun() -> Worker(p1) end),
        spawn_link(fun() -> Worker(p2) end),
        R1 = receive {worker, p1, V1} -> V1 after 8000 -> stall end,
        R2 = receive {worker, p2, V2} -> V2 after 2000 -> stall end,
        ?assertMatch({done, _}, R1),
        ?assertMatch({done, _}, R2),
        {done, N1} = R1,
        {done, N2} = R2,
        ?assert(N1 > 0),
        ?assert(N2 > 0)
    after
        catch hackney_pool:stop_pool(Pool),
        catch h2:stop_server(Server)
    end.

recv_request_body(Conn, Sid) ->
    receive
        {h2, Conn, {data, Sid, _Data, true}} -> ok;
        {h2, Conn, {data, Sid, _Data, false}} -> recv_request_body(Conn, Sid)
    end.

send_ok(Conn, Sid) ->
    ok = h2:send_response(Conn, Sid, 200,
                          [{<<"content-type">>, <<"text/plain">>}]),
    h2:send_data(Conn, Sid, <<"ok">>, true).

restore_pool_handler({ok, Handler}) ->
    application:set_env(hackney, pool_handler, Handler);
restore_pool_handler(undefined) ->
    application:unset_env(hackney, pool_handler).

wait_until(Fun, Timeout) ->
    wait_until(Fun, Timeout, erlang:monotonic_time(millisecond)).

wait_until(Fun, Timeout, Start) ->
    case Fun() of
        false ->
            case erlang:monotonic_time(millisecond) - Start > Timeout of
                true -> erlang:error({timeout_waiting_for, Fun});
                false ->
                    timer:sleep(20),
                    wait_until(Fun, Timeout, Start)
            end;
        Value ->
            Value
    end.

busy_then_slow_h2(Parent, StateChecks) ->
    receive
        {'$gen_call', From, get_state} when StateChecks =:= 0 ->
            gen_statem:reply(From, {ok, streaming_body}),
            busy_then_slow_h2(Parent, 1);
        {'$gen_call', _From, get_state} ->
            Parent ! {unexpected_h2_probe, self()},
            busy_then_slow_h2(Parent, StateChecks)
    end.
