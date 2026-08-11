%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc Fault injection around the pool: every way a connection attempt can
%%% go wrong must come back as a checkout error, never as a dead pool.
%%%
%%% The pool dials connections from inside its own gen_server, so any call into
%%% a connection process that raises takes the pool down with it and every
%%% caller of that pool with it (issue #927). Ordinary integration tests never
%%% see this: they only exercise servers that answer. Each scenario here arms
%%% one fault through {@link hackney_fault_transport}, drives one checkout
%%% entry point, and asserts the same four invariants:
%%%
%%% <ol>
%%%   <li>the caller gets `{error, _}', not an exit</li>
%%%   <li>the pool process is the same pid it was before (it never died and
%%%       got restarted under the same name)</li>
%%%   <li>nothing is left checked out or queued</li>
%%%   <li>no connection process leaked, and no crash report was logged</li>
%%% </ol>
-module(hackney_pool_fault_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, fault_test_pool).
-define(HOST, "127.0.0.1").
-define(PORT, 8141).

%%====================================================================
%% Fixtures
%%====================================================================

pool_fault_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      scenario("a connect that outlives its timeout is a checkout error (#927)",
               fun t_connect_outlives_timeout/1),
      scenario("a connect that never answers is a checkout error",
               fun t_connect_hangs/1),
      scenario("a connection crashing while dialing is a checkout error",
               fun t_connect_crashes/1),
      scenario("a refused connect is a checkout error",
               fun t_connect_refused/1),
      scenario("checkout_ssl turns a failed dial into a checkout error",
               fun t_checkout_ssl_dial_failure/1),
      scenario("a failed checkout leaves the next one working",
               fun t_pool_still_serves_after_fault/1),
      scenario("a connection crashing mid-request does not crash the pool",
               fun t_crash_mid_request/1),
      scenario("a connection killed while checked out does not crash the pool",
               fun t_kill_checked_out_connection/1),
      scenario("a fault storm never takes the pool down", 60,
               fun t_fault_storm/1)
     ]}.

%% Each scenario runs against the connection count it started with: other
%% suites in the same VM leave connections running, so "no connection leaked"
%% can only be judged relative to a baseline.
scenario(Description, Test) ->
    fun(Baseline) -> {Description, fun() -> Test(Baseline) end} end.

scenario(Description, Timeout, Test) ->
    fun(Baseline) ->
            {Description, {timeout, Timeout, fun() -> Test(Baseline) end}}
    end.

setup() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(fault_test_server, [{port, ?PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    ok = hackney_fault_transport:clear(),
    ok = hackney_crash_sentinel:start(),
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 4}, {prewarm_count, 0}]),
    conn_count().

teardown(_) ->
    ok = hackney_fault_transport:clear(),
    _ = hackney_crash_sentinel:stop(),
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    _ = (try cowboy:stop_listener(fault_test_server) catch _:_ -> ok end),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

%%====================================================================
%% Connect-time faults
%%====================================================================

%% The reported regression: the transport is still dialling when the deadline
%% passes. Before the fix the pool exited with `{timeout, {gen_statem, call, _}}'.
t_connect_outlives_timeout(Baseline) ->
    hackney_fault_transport:set(connect, {slow_error, 300}),
    ?assertEqual({error, connect_timeout}, checkout([{connect_timeout, 30}])),
    assert_pool_healthy(Baseline).

%% Same shape, except the transport never answers at all. The pool must not be
%% held hostage by it either: the checkout has to come back on its own deadline.
t_connect_hangs(Baseline) ->
    hackney_fault_transport:set(connect, {hang, 1500}),
    {Elapsed, Result} = timer:tc(fun() -> checkout([{connect_timeout, 50}]) end),
    ?assertEqual({error, connect_timeout}, Result),
    %% Back well before the transport finishes hanging: the checkout deadline
    %% plus the bounded stop, not the 1.5s the transport sits there for.
    ?assert(Elapsed div 1000 < 800),
    assert_pool_healthy(Baseline).

t_connect_crashes(Baseline) ->
    hackney_fault_transport:set(connect, crash),
    ?assertMatch({error, {{simulated_crash, connect}, _}},
                 checkout([{connect_timeout, 1000}])),
    assert_pool_healthy(Baseline).

t_connect_refused(Baseline) ->
    hackney_fault_transport:set(connect, {error, econnrefused}),
    ?assertEqual({error, econnrefused}, checkout([{connect_timeout, 1000}])),
    assert_pool_healthy(Baseline).

%% checkout_ssl reaches start_connection through its own path
%% (checkout_ssl_fallback), which always dials hackney_tcp, so the fault
%% transport cannot be injected there. Point it at a closed port instead: what
%% matters is that the entry point turns a failed dial into a checkout error.
t_checkout_ssl_dial_failure(Baseline) ->
    Opts = [{pool, ?POOL}, {connect_timeout, 500}, {checkout_timeout, 2000}],
    ?assertMatch({error, _},
                 hackney_pool:checkout_ssl(?HOST, closed_port(), hackney_ssl, Opts)),
    assert_pool_healthy(Baseline).

%% A pool that survives a fault is only useful if it still serves: the failed
%% attempt must not have leaked a slot, a monitor, or a queued caller.
t_pool_still_serves_after_fault(Baseline) ->
    hackney_fault_transport:set(connect, crash),
    ?assertMatch({error, _}, checkout([{connect_timeout, 1000}])),
    ok = hackney_fault_transport:clear(),
    {ok, PoolInfo, Pid} = checkout_ok(),
    ?assert(is_process_alive(Pid)),
    ok = hackney_pool:checkin(PoolInfo, Pid),
    assert_pool_healthy(Baseline).

%%====================================================================
%% Faults on a checked out connection
%%====================================================================

%% The connection dies with the caller holding it. The pool learns about it
%% through its monitor, and must drain the checkout rather than crash.
t_crash_mid_request(Baseline) ->
    {ok, _PoolInfo, Pid} = checkout_ok(),
    hackney_fault_transport:set(send, crash),
    %% The caller sees the failure either way; what matters is the pool.
    try hackney_conn:request(Pid, <<"GET">>, <<"/get">>, [], <<>>) of
        {error, _} -> ok;
        Other -> erlang:error({unexpected_request_result, Other})
    catch
        _:_ -> ok
    end,
    assert_pool_healthy(Baseline).

t_kill_checked_out_connection(Baseline) ->
    {ok, _PoolInfo, Pid} = checkout_ok(),
    exit(Pid, kill),
    assert_pool_healthy(Baseline).

%%====================================================================
%% Storm
%%====================================================================

%% Every fault, from many callers at once, against a pool small enough that
%% they contend for it. Nothing here should reach the pool as an exit.
t_fault_storm(Baseline) ->
    Faults = [{slow_error, 50}, crash, {error, econnrefused}, {sleep, 20}, ok],
    Parent = self(),
    Workers = [spawn_link(fun() -> storm_worker(Parent, N, Faults) end)
               || N <- lists:seq(1, 20)],
    %% One deadline for the whole storm, not one per worker: a pool held
    %% hostage by a slow connection shows up here as workers that never finish.
    await_workers(Workers, erlang:monotonic_time(millisecond) + 30000),
    ok = hackney_fault_transport:clear(),
    assert_pool_healthy(Baseline).

await_workers([], _Deadline) ->
    ok;
await_workers(Workers, Deadline) ->
    Left = Deadline - erlang:monotonic_time(millisecond),
    ?assert(Left > 0),
    receive
        {done, W} -> await_workers(Workers -- [W], Deadline)
    after Left ->
        erlang:error({storm_workers_stuck, length(Workers)})
    end.

storm_worker(Parent, Seed, Faults) ->
    rand:seed(exsss, {Seed, Seed * 7, Seed * 13}),
    lists:foreach(
      fun(_) ->
              Fault = lists:nth(rand:uniform(length(Faults)), Faults),
              hackney_fault_transport:set(connect, Fault),
              case checkout([{connect_timeout, 100}, {checkout_timeout, 5000}]) of
                  {ok, PoolInfo, Pid} ->
                      _ = (try hackney_pool:checkin(PoolInfo, Pid) catch _:_ -> ok end);
                  {error, _} ->
                      ok
              end
      end,
      lists:seq(1, 10)),
    Parent ! {done, self()}.

%%====================================================================
%% Helpers
%%====================================================================

checkout(Extra) ->
    Opts = [{pool, ?POOL}, {checkout_timeout, 2000} | Extra],
    hackney_pool:checkout(?HOST, ?PORT, hackney_fault_transport, Opts).

checkout_ok() ->
    ok = hackney_fault_transport:clear(),
    case checkout([{connect_timeout, 2000}]) of
        {ok, _, _} = Ok -> Ok;
        Other -> erlang:error({checkout_failed, Other})
    end.

%% The four invariants every scenario shares.
assert_pool_healthy(Baseline) ->
    Pool = hackney_pool:find_pool(?POOL),
    ?assert(is_pid(Pool) andalso is_process_alive(Pool)),
    hackney_crash_sentinel:assert_no_crash_from(Pool),
    Stats = wait_until(fun() ->
                               S = hackney_pool:get_stats(?POOL),
                               settled(S, Baseline) andalso S
                       end, 5000),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(0, proplists:get_value(queue_count, Stats)),
    ok.

%% Nothing checked out, nobody queued, and no connection process alive beyond
%% the ones the pool is keeping warm: a failed attempt that leaves a connection
%% behind is a leak even when the pool itself survived.
settled(Stats, Baseline) ->
    proplists:get_value(in_use_count, Stats) =:= 0 andalso
        proplists:get_value(queue_count, Stats) =:= 0 andalso
        conn_count() =< Baseline + proplists:get_value(free_count, Stats).

%% A port nothing listens on: bind one, note it, hand it back.
closed_port() ->
    {ok, L} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(L),
    ok = gen_tcp:close(L),
    Port.

conn_count() ->
    case whereis(hackney_conn_sup) of
        undefined -> 0;
        _ -> proplists:get_value(active, supervisor:count_children(hackney_conn_sup))
    end.

%% Poll `Fun' until it returns something other than `false'.
wait_until(Fun, Timeout) ->
    wait_until(Fun, Timeout, erlang:monotonic_time(millisecond)).

wait_until(Fun, Timeout, Start) ->
    case Fun() of
        false ->
            Now = erlang:monotonic_time(millisecond),
            case Now - Start > Timeout of
                true -> erlang:error({timeout_waiting_for, Fun});
                false ->
                    timer:sleep(20),
                    wait_until(Fun, Timeout, Start)
            end;
        Value ->
            Value
    end.
