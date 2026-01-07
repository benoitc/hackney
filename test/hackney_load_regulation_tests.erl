%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for hackney_load_regulation.

-module(hackney_load_regulation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

load_regulation_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"acquire and release single", fun test_acquire_release_single/0},
      {"current count tracking", fun test_current_count/0},
      {"acquire respects limit", fun test_acquire_limit/0},
      {"concurrent acquire", fun test_concurrent_acquire/0},
      {"timeout on acquire", fun test_acquire_timeout/0},
      {"infinity timeout", fun test_infinity_timeout/0},
      {"different hosts independent", fun test_different_hosts/0},
      {"host normalization", fun test_host_normalization/0},
      {"reset counter", fun test_reset/0}
     ]}.

setup() ->
    ok = hackney_load_regulation:init(),
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_acquire_release_single() ->
    Host = <<"test.example.com">>,
    Port = 80,
    hackney_load_regulation:reset(Host, Port),

    ?assertEqual(0, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:acquire(Host, Port, 10, 1000),
    ?assertEqual(1, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:release(Host, Port),
    ?assertEqual(0, hackney_load_regulation:current(Host, Port)).

test_current_count() ->
    Host = <<"count.example.com">>,
    Port = 443,
    hackney_load_regulation:reset(Host, Port),

    ?assertEqual(0, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:acquire(Host, Port, 10, 1000),
    ?assertEqual(1, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:acquire(Host, Port, 10, 1000),
    ?assertEqual(2, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:acquire(Host, Port, 10, 1000),
    ?assertEqual(3, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:release(Host, Port),
    ?assertEqual(2, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:release(Host, Port),
    ok = hackney_load_regulation:release(Host, Port),
    ?assertEqual(0, hackney_load_regulation:current(Host, Port)).

test_acquire_limit() ->
    Host = <<"limit.example.com">>,
    Port = 80,
    MaxPerHost = 3,
    hackney_load_regulation:reset(Host, Port),

    %% Acquire up to limit
    ok = hackney_load_regulation:acquire(Host, Port, MaxPerHost, 1000),
    ok = hackney_load_regulation:acquire(Host, Port, MaxPerHost, 1000),
    ok = hackney_load_regulation:acquire(Host, Port, MaxPerHost, 1000),
    ?assertEqual(3, hackney_load_regulation:current(Host, Port)),

    %% Next acquire should timeout (limit reached)
    ?assertEqual({error, timeout},
                 hackney_load_regulation:acquire(Host, Port, MaxPerHost, 50)),
    ?assertEqual(3, hackney_load_regulation:current(Host, Port)),

    %% Release one, then acquire should succeed
    ok = hackney_load_regulation:release(Host, Port),
    ?assertEqual(2, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:acquire(Host, Port, MaxPerHost, 1000),
    ?assertEqual(3, hackney_load_regulation:current(Host, Port)),

    %% Cleanup
    ok = hackney_load_regulation:release(Host, Port),
    ok = hackney_load_regulation:release(Host, Port),
    ok = hackney_load_regulation:release(Host, Port).

test_concurrent_acquire() ->
    Host = <<"concurrent.example.com">>,
    Port = 80,
    MaxPerHost = 3,
    hackney_load_regulation:reset(Host, Port),

    Self = self(),

    %% Spawn 5 processes trying to acquire (limit 3)
    Pids = [spawn_link(fun() ->
        Result = hackney_load_regulation:acquire(Host, Port, MaxPerHost, 500),
        Self ! {self(), Result},
        case Result of
            ok ->
                %% Hold the slot briefly
                timer:sleep(100),
                hackney_load_regulation:release(Host, Port);
            _ ->
                ok
        end
    end) || _ <- lists:seq(1, 5)],

    %% Collect results
    Results = [receive {Pid, R} -> R after 2000 -> timeout end || Pid <- Pids],

    %% At least 3 should succeed (the limit)
    SuccessCount = length([R || R <- Results, R =:= ok]),
    ?assert(SuccessCount >= 3),

    %% Wait for all to complete
    timer:sleep(200),
    ?assertEqual(0, hackney_load_regulation:current(Host, Port)).

test_acquire_timeout() ->
    Host = <<"timeout.example.com">>,
    Port = 80,
    hackney_load_regulation:reset(Host, Port),

    %% Fill up to limit
    ok = hackney_load_regulation:acquire(Host, Port, 1, 1000),

    %% Try to acquire with short timeout
    Start = erlang:monotonic_time(millisecond),
    Result = hackney_load_regulation:acquire(Host, Port, 1, 100),
    Elapsed = erlang:monotonic_time(millisecond) - Start,

    ?assertEqual({error, timeout}, Result),
    %% Should have taken approximately 100ms (with generous tolerance for CI)
    ?assert(Elapsed >= 90),
    ?assert(Elapsed < 500),

    ok = hackney_load_regulation:release(Host, Port).

test_infinity_timeout() ->
    Host = <<"infinity.example.com">>,
    Port = 80,
    hackney_load_regulation:reset(Host, Port),

    Self = self(),

    %% Fill up to limit
    ok = hackney_load_regulation:acquire(Host, Port, 1, infinity),

    %% Spawn process to acquire with infinity timeout
    Pid = spawn_link(fun() ->
        Result = hackney_load_regulation:acquire(Host, Port, 1, infinity),
        Self ! {acquired, Result}
    end),

    %% Wait a bit, should not have acquired yet
    timer:sleep(50),
    receive
        {acquired, _} -> ?assert(false)
    after 0 -> ok
    end,

    %% Release, spawned process should acquire
    ok = hackney_load_regulation:release(Host, Port),

    receive
        {acquired, ok} -> ok
    after 1000 ->
        exit(Pid, kill),
        ?assert(false)
    end,

    ok = hackney_load_regulation:release(Host, Port).

test_different_hosts() ->
    Host1 = <<"host1.example.com">>,
    Host2 = <<"host2.example.com">>,
    Port = 80,
    hackney_load_regulation:reset(Host1, Port),
    hackney_load_regulation:reset(Host2, Port),

    %% Acquire on host1
    ok = hackney_load_regulation:acquire(Host1, Port, 1, 1000),
    ?assertEqual(1, hackney_load_regulation:current(Host1, Port)),
    ?assertEqual(0, hackney_load_regulation:current(Host2, Port)),

    %% Should be able to acquire on host2 (independent limit)
    ok = hackney_load_regulation:acquire(Host2, Port, 1, 1000),
    ?assertEqual(1, hackney_load_regulation:current(Host1, Port)),
    ?assertEqual(1, hackney_load_regulation:current(Host2, Port)),

    %% Host1 is at limit
    ?assertEqual({error, timeout},
                 hackney_load_regulation:acquire(Host1, Port, 1, 50)),

    %% Cleanup
    ok = hackney_load_regulation:release(Host1, Port),
    ok = hackney_load_regulation:release(Host2, Port).

test_host_normalization() ->
    %% Different representations of same host should share limit
    hackney_load_regulation:reset(<<"example.com">>, 80),

    ok = hackney_load_regulation:acquire(<<"EXAMPLE.COM">>, 80, 2, 1000),
    ?assertEqual(1, hackney_load_regulation:current(<<"example.com">>, 80)),

    ok = hackney_load_regulation:acquire("Example.Com", 80, 2, 1000),
    ?assertEqual(2, hackney_load_regulation:current(<<"example.com">>, 80)),

    %% At limit
    ?assertEqual({error, timeout},
                 hackney_load_regulation:acquire(<<"example.com">>, 80, 2, 50)),

    ok = hackney_load_regulation:release(<<"EXAMPLE.COM">>, 80),
    ok = hackney_load_regulation:release("example.com", 80).

test_reset() ->
    Host = <<"reset.example.com">>,
    Port = 80,

    ok = hackney_load_regulation:acquire(Host, Port, 10, 1000),
    ok = hackney_load_regulation:acquire(Host, Port, 10, 1000),
    ?assertEqual(2, hackney_load_regulation:current(Host, Port)),

    ok = hackney_load_regulation:reset(Host, Port),
    ?assertEqual(0, hackney_load_regulation:current(Host, Port)).
