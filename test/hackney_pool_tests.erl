%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024 Benoit Chesneau
%%%
%%% @doc Tests for hackney_pool implementation.

-module(hackney_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("hackney.hrl").

-define(PORT, 8123).

%%====================================================================
%% Test fixtures
%%====================================================================

%% Unit tests - no server required
hackney_pool_unit_test_() ->
    {setup,
     fun setup_unit/0,
     fun teardown_unit/1,
     [
      {"pool starts with default", fun test_default_pool/0},
      {"start custom pool", fun test_custom_pool/0},
      {"pool stats", fun test_pool_stats/0},
      {"max connections setting", fun test_max_connections/0},
      {"timeout setting", fun test_timeout_setting/0}
     ]}.

%% Integration tests - require server
hackney_pool_integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun teardown_integration/1,
     [
      {"checkout creates connection", fun test_checkout_creates/0},
      {"checkin returns connection", fun test_checkin_returns/0},
      {"connection reuse", fun test_connection_reuse/0},
      {"connection death cleanup", fun test_connection_death/0},
      {"owner crash kills connection", fun test_owner_crash/0},
      {"checkin resets owner to pool", fun test_checkin_resets_owner/0},
      {"prewarm creates connections", fun test_prewarm/0},
      {"queue timeout", {timeout, 120, fun test_queue_timeout/0}},
      {"checkout timeout", {timeout, 120, fun test_checkout_timeout/0}}
     ]}.

setup_unit() ->
    application:ensure_all_started(hackney),
    ok.

teardown_unit(_) ->
    ok.

setup_integration() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Host = '_',
    Routes = [
        {"/pool", pool_resource, []},
        {"/[...]", test_http_resource, []}
    ],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    {ok, _} = cowboy:start_clear(pool_test_server, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}),
    ok.

teardown_integration(_) ->
    cowboy:stop_listener(pool_test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

test_default_pool() ->
    ?assertEqual(undefined, hackney_pool:find_pool(nonexistent_pool)),
    ok = hackney_pool:start_pool(test_pool_1, []),
    ?assert(is_pid(hackney_pool:find_pool(test_pool_1))),
    ok = hackney_pool:stop_pool(test_pool_1).

test_custom_pool() ->
    %% Timeout is capped at 2000ms for keepalive
    Options = [{pool_size, 10}, {timeout, 60000}],
    ok = hackney_pool:start_pool(test_pool_2, Options),
    Pool = hackney_pool:find_pool(test_pool_2),
    ?assert(is_pid(Pool)),
    ?assertEqual(10, hackney_pool:max_connections(test_pool_2)),
    ?assertEqual(2000, hackney_pool:timeout(test_pool_2)),  % Capped at 2s
    ok = hackney_pool:stop_pool(test_pool_2).

test_pool_stats() ->
    ok = hackney_pool:start_pool(test_pool_3, []),
    Stats = hackney_pool:get_stats(test_pool_3),
    ?assertEqual(test_pool_3, proplists:get_value(name, Stats)),
    ?assert(is_integer(proplists:get_value(max, Stats))),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(0, proplists:get_value(free_count, Stats)),
    ?assertEqual(0, proplists:get_value(queue_count, Stats)),
    ok = hackney_pool:stop_pool(test_pool_3).

test_max_connections() ->
    ok = hackney_pool:start_pool(test_pool_4, [{pool_size, 5}]),
    ?assertEqual(5, hackney_pool:max_connections(test_pool_4)),
    hackney_pool:set_max_connections(test_pool_4, 10),
    timer:sleep(10),
    ?assertEqual(10, hackney_pool:max_connections(test_pool_4)),
    ok = hackney_pool:stop_pool(test_pool_4).

test_timeout_setting() ->
    %% Keepalive timeout is capped at 2000ms
    ok = hackney_pool:start_pool(test_pool_5, [{timeout, 5000}]),
    ?assertEqual(2000, hackney_pool:timeout(test_pool_5)),  % Capped at 2s
    hackney_pool:set_timeout(test_pool_5, 1000),
    timer:sleep(10),
    ?assertEqual(1000, hackney_pool:timeout(test_pool_5)),
    hackney_pool:set_timeout(test_pool_5, 10000),
    timer:sleep(10),
    ?assertEqual(2000, hackney_pool:timeout(test_pool_5)),  % Capped at 2s
    ok = hackney_pool:stop_pool(test_pool_5).

%%====================================================================
%% Connection Tests
%%====================================================================

test_checkout_creates() ->
    ok = hackney_pool:start_pool(test_pool_conn_1, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_1}],

    %% Checkout should create a new connection
    {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ?assertMatch({test_pool_conn_1, _, _, _}, PoolInfo),

    %% Stats should show 1 in use
    Stats = hackney_pool:get_stats(test_pool_conn_1),
    ?assertEqual(1, proplists:get_value(in_use_count, Stats)),

    %% Clean up
    hackney_conn:stop(Pid),
    ok = hackney_pool:stop_pool(test_pool_conn_1).

test_checkin_returns() ->
    ok = hackney_pool:start_pool(test_pool_conn_2, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_2}],

    %% Checkout a connection
    {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),

    %% Checkin the connection
    ok = hackney_pool:checkin(PoolInfo, Pid),
    timer:sleep(10),

    %% Stats should show 1 free, 0 in use
    Stats = hackney_pool:get_stats(test_pool_conn_2),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(1, proplists:get_value(free_count, Stats)),

    ok = hackney_pool:stop_pool(test_pool_conn_2).

test_connection_reuse() ->
    ok = hackney_pool:start_pool(test_pool_conn_3, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_3}],

    %% Checkout and checkin
    {ok, PoolInfo, Pid1} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ok = hackney_pool:checkin(PoolInfo, Pid1),
    timer:sleep(10),

    %% Second checkout should reuse the same connection
    {ok, _, Pid2} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ?assertEqual(Pid1, Pid2),

    hackney_conn:stop(Pid2),
    ok = hackney_pool:stop_pool(test_pool_conn_3).

test_connection_death() ->
    ok = hackney_pool:start_pool(test_pool_conn_4, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_4}],

    %% Checkout and checkin
    {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ok = hackney_pool:checkin(PoolInfo, Pid),
    timer:sleep(10),

    %% Verify it's in the pool
    Stats1 = hackney_pool:get_stats(test_pool_conn_4),
    ?assertEqual(1, proplists:get_value(free_count, Stats1)),

    %% Kill the connection
    exit(Pid, kill),
    timer:sleep(50),

    %% Should be removed from pool
    Stats2 = hackney_pool:get_stats(test_pool_conn_4),
    ?assertEqual(0, proplists:get_value(free_count, Stats2)),

    ok = hackney_pool:stop_pool(test_pool_conn_4).

test_owner_crash() ->
    %% Test that when the owner process crashes, the connection dies
    ok = hackney_pool:start_pool(test_pool_owner_crash, [{pool_size, 5}]),
    Opts = [{pool, test_pool_owner_crash}],
    Self = self(),

    %% Spawn a process that checks out a connection
    Owner = spawn(fun() ->
        {ok, _PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
        Self ! {conn_pid, Pid},
        %% Wait to be killed
        receive stop -> ok end
    end),

    %% Get the connection pid
    ConnPid = receive {conn_pid, P} -> P after 5000 -> error(timeout) end,
    ?assert(is_process_alive(ConnPid)),

    %% Verify it's in use
    Stats1 = hackney_pool:get_stats(test_pool_owner_crash),
    ?assertEqual(1, proplists:get_value(in_use_count, Stats1)),

    %% Kill the owner process
    exit(Owner, kill),
    timer:sleep(100),

    %% Connection should have died
    ?assertNot(is_process_alive(ConnPid)),

    %% Should be removed from in_use
    Stats2 = hackney_pool:get_stats(test_pool_owner_crash),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats2)),

    ok = hackney_pool:stop_pool(test_pool_owner_crash).

test_checkin_resets_owner() ->
    %% Test that after checkin, the connection survives if the previous owner crashes
    ok = hackney_pool:start_pool(test_pool_checkin_owner, [{pool_size, 5}]),
    Opts = [{pool, test_pool_checkin_owner}],
    Self = self(),

    %% Spawn a process that checks out and checks in a connection
    Owner = spawn(fun() ->
        {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
        Self ! {conn_pid, Pid},
        %% Check the connection back in
        ok = hackney_pool:checkin(PoolInfo, Pid),
        Self ! checked_in,
        %% Wait to be killed
        receive stop -> ok end
    end),

    %% Get the connection pid
    ConnPid = receive {conn_pid, P} -> P after 5000 -> error(timeout) end,

    %% Wait for checkin
    receive checked_in -> ok after 5000 -> error(timeout) end,
    timer:sleep(50),

    %% Verify connection is in the pool (free)
    Stats1 = hackney_pool:get_stats(test_pool_checkin_owner),
    ?assertEqual(1, proplists:get_value(free_count, Stats1)),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats1)),

    %% Kill the previous owner
    exit(Owner, kill),
    timer:sleep(100),

    %% Connection should still be alive (owner is now the pool)
    ?assert(is_process_alive(ConnPid)),

    %% Should still be in the pool
    Stats2 = hackney_pool:get_stats(test_pool_checkin_owner),
    ?assertEqual(1, proplists:get_value(free_count, Stats2)),

    ok = hackney_pool:stop_pool(test_pool_checkin_owner).

%%====================================================================
%% Prewarm Tests
%%====================================================================

test_prewarm() ->
    %% Test that prewarm creates connections
    ok = hackney_pool:start_pool(test_pool_prewarm, [{pool_size, 10}, {prewarm_count, 3}]),

    %% Initially no connections
    Stats1 = hackney_pool:get_stats(test_pool_prewarm),
    ?assertEqual(0, proplists:get_value(free_count, Stats1)),

    %% Prewarm 3 connections to localhost
    hackney_pool:prewarm(test_pool_prewarm, "127.0.0.1", ?PORT, 3),
    timer:sleep(500),  %% Wait for prewarm connections to be created

    %% Should have 3 free connections
    Stats2 = hackney_pool:get_stats(test_pool_prewarm),
    ?assertEqual(3, proplists:get_value(free_count, Stats2)),

    %% Prewarm with different count
    hackney_pool:prewarm(test_pool_prewarm, "127.0.0.1", ?PORT, 5),
    timer:sleep(500),

    %% Should have 5 free connections now (2 more added)
    Stats3 = hackney_pool:get_stats(test_pool_prewarm),
    ?assertEqual(5, proplists:get_value(free_count, Stats3)),

    ok = hackney_pool:stop_pool(test_pool_prewarm).

%%====================================================================
%% Timeout Tests
%%====================================================================

test_queue_timeout() ->
    %% Test that when pool is full, second checkout returns error immediately
    %% (no more queuing - load regulation handles waiting)
    URL = <<"http://localhost:8123/pool">>,
    Headers = [],
    hackney_pool:start_pool(pool_test, [{pool_size, 1}]),
    Opts = [{pool, pool_test}, {connect_timeout, 100}, {checkout_timeout, 5000}],
    case hackney:request(post, URL, Headers, stream, Opts) of
        {ok, Ref} ->
            %% Second request should fail immediately (pool full, no queue)
            {error, checkout_timeout} = hackney:request(post, URL, Headers, stream, Opts),

            %% Finish first request
            ok = hackney:finish_send_body(Ref),
            {ok, _Status, _Headers, Ref} = hackney:start_response(Ref),
            ok = hackney:skip_body(Ref),

            %% Now pool is free, next request should succeed
            {ok, Ref2} = hackney:request(post, URL, Headers, stream, Opts),
            hackney:close(Ref2)
    end,
    hackney_pool:stop_pool(pool_test).

test_checkout_timeout() ->
    URL = <<"http://localhost:8123/pool">>,
    Headers = [],
    hackney_pool:start_pool(pool_test_timeout, [{pool_size, 1}]),
    Opts = [{max_body, 2048}, {pool, pool_test_timeout}, {connect_timeout, 1000}, {checkout_timeout, 100}],
    case hackney:request(post, URL, Headers, stream, Opts) of
        {ok, Ref} ->
            {error, Error} = hackney:request(post, URL, Headers, stream, Opts),
            hackney:close(Ref),
            ?assertEqual(Error, checkout_timeout)
    end,
    hackney_pool:stop_pool(pool_test_timeout).
