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

run_concurrent_tight_loop() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Handler = fun(Conn, Sid, _M, _P, _H) ->
        ok = h2:send_response(Conn, Sid, 200,
                              [{<<"content-type">>, <<"text/plain">>}]),
        ok = h2:send_data(Conn, Sid, <<"ok">>, true)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => Handler
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
