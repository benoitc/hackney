%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc Fault injection on the multiplexed checkout paths.
%%%
%%% HTTP/2 and HTTP/3 connections are not checked out exclusively: the pool
%%% keeps one per host and hands the same pid to every caller. That makes a
%%% single bad connection worse than on the HTTP/1 path, because every request
%%% to that host goes through the same probe. The pool must answer `none' for a
%%% connection that is dead, wedged, or dies while being probed, and it must
%%% answer quickly: a probe that blocks holds every caller of the pool, not
%%% just the one that asked.
-module(hackney_pool_h2h3_fault_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, h2h3_fault_test_pool).
-define(HOST, "127.0.0.1").
-define(PORT, 8143).

%% A probe of a wedged connection has to come back in well under the time it
%% would take if it waited for the connection itself.
-define(PROBE_BUDGET_MS, 1000).

%%====================================================================
%% Fixtures
%%====================================================================

h2h3_fault_test_() ->
    {setup,
     fun start_server/0,
     fun stop_server/1,
     {foreach,
      fun setup/0,
      fun teardown/1,
      [
      {"a healthy h2 connection is handed out", fun t_h2_healthy/0},
      {"a wedged h2 connection is dropped without stalling the pool",
       fun t_h2_wedged/0},
      {"an h2 connection that dies when probed does not crash the pool",
       fun t_h2_dies_when_probed/0},
      {"a dead h2 connection is dropped", fun t_h2_dead/0},
      {"a dead h3 connection is dropped", fun t_h3_dead/0},
      {"a wedged h3 connection does not stall the pool", fun t_h3_wedged/0}
      ]}}.

%% The listener lives for the whole module: rebinding the same port between
%% scenarios races with the previous listener shutting down.
start_server() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(h2h3_fault_test_server, [{port, ?PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

stop_server(_) ->
    _ = (try cowboy:stop_listener(h2h3_fault_test_server) catch _:_ -> ok end),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

setup() ->
    ok = hackney_crash_sentinel:start(),
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 4}, {prewarm_count, 0}]),
    ok.

teardown(_) ->
    _ = hackney_crash_sentinel:stop(),
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    ok.

%%====================================================================
%% HTTP/2
%%====================================================================

t_h2_healthy() ->
    Pid = live_conn(),
    ok = hackney_pool:register_h2(?HOST, ?PORT, hackney_tcp, Pid, opts()),
    ?assertEqual({ok, Pid}, checkout_h2()),
    assert_pool_healthy().

%% The connection is alive but answers nothing. Before `get_state' took a
%% timeout the pool sat on the default 5s call for every caller of that host.
t_h2_wedged() ->
    Pid = live_conn(),
    ok = hackney_pool:register_h2(?HOST, ?PORT, hackney_tcp, Pid, opts()),
    ok = sys:suspend(Pid),
    {Elapsed, Result} = timer:tc(fun checkout_h2/0),
    exit(Pid, kill),
    ?assertEqual(none, Result),
    ?assert(Elapsed div 1000 < ?PROBE_BUDGET_MS),
    assert_pool_healthy().

%% Alive when registered, gone by the time the pool asks it anything.
t_h2_dies_when_probed() ->
    Pid = spawn(fun() -> receive _ -> exit(probed) end end),
    ok = hackney_pool:register_h2(?HOST, ?PORT, hackney_tcp, Pid, opts()),
    ?assertEqual(none, checkout_h2()),
    assert_pool_healthy().

t_h2_dead() ->
    Pid = live_conn(),
    ok = hackney_pool:register_h2(?HOST, ?PORT, hackney_tcp, Pid, opts()),
    ?assertEqual({ok, Pid}, checkout_h2()),
    exit(Pid, kill),
    ?assertEqual(none, checkout_h2()),
    ?assertEqual(none, checkout_h2()),
    assert_pool_healthy().

%%====================================================================
%% HTTP/3
%%====================================================================

t_h3_dead() ->
    Pid = live_conn(),
    ok = hackney_pool:register_h3(?HOST, ?PORT, hackney_tcp, Pid, opts()),
    ?assertEqual({ok, Pid}, checkout_h3()),
    exit(Pid, kill),
    ?assertEqual(none, checkout_h3()),
    assert_pool_healthy().

t_h3_wedged() ->
    Pid = live_conn(),
    ok = hackney_pool:register_h3(?HOST, ?PORT, hackney_tcp, Pid, opts()),
    ok = sys:suspend(Pid),
    {Elapsed, _Result} = timer:tc(fun checkout_h3/0),
    exit(Pid, kill),
    ?assert(Elapsed div 1000 < ?PROBE_BUDGET_MS),
    assert_pool_healthy().

%%====================================================================
%% Helpers
%%====================================================================

opts() -> [{pool, ?POOL}].

checkout_h2() ->
    hackney_pool:checkout_h2(?HOST, ?PORT, hackney_tcp, opts()).

checkout_h3() ->
    hackney_pool:checkout_h3(?HOST, ?PORT, hackney_tcp, opts()).

%% A real connection process against the test server, outside the pool's
%% checkout bookkeeping: these tests are about the shared-connection map.
live_conn() ->
    {ok, Pid} = hackney_conn_sup:start_conn(#{
        host => ?HOST,
        port => ?PORT,
        transport => hackney_tcp,
        connect_timeout => 5000,
        recv_timeout => 5000,
        idle_timeout => infinity,
        ssl_options => [],
        connect_options => [],
        pool_pid => self(),
        owner => self()
    }),
    ok = hackney_conn:connect(Pid),
    Pid.

assert_pool_healthy() ->
    Pool = hackney_pool:find_pool(?POOL),
    ?assert(is_pid(Pool) andalso is_process_alive(Pool)),
    hackney_crash_sentinel:assert_no_crash_from(Pool),
    %% Still answering, and promptly: the probe failures above must not have
    %% left the pool blocked on anything.
    {Elapsed, Stats} = timer:tc(fun() -> hackney_pool:get_stats(?POOL) end),
    ?assert(Elapsed div 1000 < ?PROBE_BUDGET_MS),
    ?assertEqual(0, proplists:get_value(queue_count, Stats)),
    ok.
