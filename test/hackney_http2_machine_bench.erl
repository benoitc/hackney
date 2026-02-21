%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Benchmarks for HTTP/2 state machine optimizations.
%%%
%%% Measures performance of:
%%% - Stream operations (init, get, store)
%%% - Header preparation
%%% - Flow control window updates
%%% - Lingering stream lookups (gb_sets vs lists)
%%%
%%% Run: rebar3 as test compile && erl -pa _build/test/lib/*/ebin \
%%%      -noshell -eval 'hackney_http2_machine_bench:run().' -s init stop

-module(hackney_http2_machine_bench).

-export([run/0, run/1]).
-export([bench_stream_init/0, bench_stream_lookup/0, bench_headers/0]).
-export([bench_window_update/0, bench_lingering_streams/0]).

-define(DEFAULT_ITERATIONS, 10000).
-define(TEST_OPTS, #{preface_timeout => infinity, settings_timeout => infinity}).

%%====================================================================
%% Public API
%%====================================================================

run() ->
    run(?DEFAULT_ITERATIONS).

run(N) ->
    io:format("~n=== HTTP/2 Machine Benchmark Suite ===~n"),
    io:format("Iterations: ~p~n~n", [N]),

    %% Warm up JIT
    io:format("Warming up...~n"),
    {ok, _, S0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    _ = [begin
        {ok, _, S1} = hackney_http2_machine:init_stream(<<"GET">>, S0),
        S1
    end || _ <- lists:seq(1, 1000)],

    io:format("~n--- Stream Operations ---~n~n"),
    bench_stream_init_n(N),
    bench_stream_lookup_n(N),

    io:format("~n--- Header Preparation ---~n~n"),
    bench_headers_n(N),

    io:format("~n--- Flow Control ---~n~n"),
    bench_window_update_n(N),

    io:format("~n--- Lingering Streams (gb_sets) ---~n~n"),
    bench_lingering_streams_n(N),

    io:format("~n=== Benchmark Complete ===~n"),
    ok.

%%====================================================================
%% Individual Benchmarks
%%====================================================================

bench_stream_init() ->
    bench_stream_init_n(?DEFAULT_ITERATIONS).

bench_stream_init_n(N) ->
    {ok, _, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),

    {Time, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {ok, _, S1} = hackney_http2_machine:init_stream(<<"GET">>, S),
            S1
        end, State0, lists:seq(1, N))
    end),

    io:format("Stream init:~n"),
    io:format("  ~.2f us/op (~p ops in ~.2f ms)~n~n",
              [Time / N, N, Time / 1000]).

bench_stream_lookup() ->
    bench_stream_lookup_n(?DEFAULT_ITERATIONS).

bench_stream_lookup_n(N) ->
    %% Create state with multiple streams
    {ok, _, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    State1 = lists:foldl(fun(_, S) ->
        {ok, _, S1} = hackney_http2_machine:init_stream(<<"GET">>, S),
        S1
    end, State0, lists:seq(1, 100)),

    %% Benchmark lookups on stream 51 (middle)
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_http2_machine:get_stream_local_state(51, State1)
        end, lists:seq(1, N))
    end),

    io:format("Stream lookup (100 streams, middle):~n"),
    io:format("  ~.2f us/op (~p ops in ~.2f ms)~n~n",
              [Time / N, N, Time / 1000]).

bench_headers() ->
    bench_headers_n(?DEFAULT_ITERATIONS).

bench_headers_n(N) ->
    {ok, _, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, StreamId, State1} = hackney_http2_machine:init_stream(<<"GET">>, State0),

    PseudoHeaders = #{
        method => <<"GET">>,
        scheme => <<"https">>,
        authority => <<"example.com">>,
        path => <<"/">>
    },
    Headers = [
        {<<"user-agent">>, <<"hackney/benchmark">>},
        {<<"accept">>, <<"*/*">>}
    ],

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_http2_machine:prepare_headers(
                StreamId, State1, fin, PseudoHeaders, Headers)
        end, lists:seq(1, N))
    end),

    io:format("Header preparation:~n"),
    io:format("  ~.2f us/op (~p ops in ~.2f ms)~n~n",
              [Time / N, N, Time / 1000]).

bench_window_update() ->
    bench_window_update_n(?DEFAULT_ITERATIONS).

bench_window_update_n(N) ->
    {ok, _, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, StreamId, State1} = hackney_http2_machine:init_stream(<<"GET">>, State0),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_http2_machine:update_window(StreamId, 1024, State1)
        end, lists:seq(1, N))
    end),

    io:format("Window update (stream):~n"),
    io:format("  ~.2f us/op (~p ops in ~.2f ms)~n~n",
              [Time / N, N, Time / 1000]).

bench_lingering_streams() ->
    bench_lingering_streams_n(?DEFAULT_ITERATIONS).

bench_lingering_streams_n(N) ->
    {ok, _, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),

    %% Create 50 streams and reset them to build up lingering list
    State1 = lists:foldl(fun(_, S) ->
        {ok, StreamId, S1} = hackney_http2_machine:init_stream(<<"GET">>, S),
        {ok, S2} = hackney_http2_machine:reset_stream(StreamId, S1),
        S2
    end, State0, lists:seq(1, 50)),

    %% Benchmark is_lingering_stream lookups
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            %% Check a stream that is lingering
            hackney_http2_machine:is_lingering_stream(25, State1),
            %% Check a stream that is not lingering
            hackney_http2_machine:is_lingering_stream(999, State1)
        end, lists:seq(1, N))
    end),

    io:format("Lingering stream lookup (50 streams, gb_sets):~n"),
    io:format("  ~.2f us/op (~p ops in ~.2f ms)~n~n",
              [Time / (N * 2), N * 2, Time / 1000]).

%%====================================================================
%% Comparison Utilities
%%====================================================================

%% @doc Compare gb_sets vs list performance for lingering streams.
%% This demonstrates the O(log N) vs O(N) difference.
compare_lingering_implementation() ->
    io:format("~n=== Lingering Stream Implementation Comparison ===~n~n"),

    Sizes = [10, 50, 100],
    N = 100000,

    lists:foreach(fun(Size) ->
        %% Build list
        List = lists:seq(1, Size * 2, 2),  %% Odd numbers

        %% Build gb_set
        Set = gb_sets:from_list(List),

        %% Benchmark list lookup
        {TimeList, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                lists:member(Size, List)
            end, lists:seq(1, N))
        end),

        %% Benchmark gb_sets lookup
        {TimeSet, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                gb_sets:is_member(Size, Set)
            end, lists:seq(1, N))
        end),

        io:format("Size ~p:~n", [Size]),
        io:format("  lists:member   ~.3f us/op~n", [TimeList / N]),
        io:format("  gb_sets:is_member  ~.3f us/op~n", [TimeSet / N]),
        io:format("  Speedup: ~.2fx~n~n", [TimeList / max(TimeSet, 1)])
    end, Sizes).
