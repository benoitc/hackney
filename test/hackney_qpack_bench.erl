%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc QPACK benchmark suite.
%%%
%%% Tests performance of optimized hackney_qpack static table lookups.
%%% @end

-module(hackney_qpack_bench).

-export([run/0, run/1]).
-export([bench_encode/0, bench_decode/0, bench_static_lookup/0]).

-define(DEFAULT_ITERATIONS, 10000).

%% Sample headers for benchmarking
-define(SIMPLE_HEADERS, [
    {<<":method">>, <<"GET">>},
    {<<":path">>, <<"/">>},
    {<<":scheme">>, <<"https">>},
    {<<":authority">>, <<"example.com">>}
]).

-define(TYPICAL_REQUEST, [
    {<<":method">>, <<"GET">>},
    {<<":path">>, <<"/api/v1/users">>},
    {<<":scheme">>, <<"https">>},
    {<<":authority">>, <<"api.example.com">>},
    {<<"user-agent">>, <<"Mozilla/5.0">>},
    {<<"accept">>, <<"*/*">>},
    {<<"accept-encoding">>, <<"gzip, deflate, br">>},
    {<<"authorization">>, <<"Bearer token123">>}
]).

-define(TYPICAL_RESPONSE, [
    {<<":status">>, <<"200">>},
    {<<"content-type">>, <<"application/json">>},
    {<<"content-length">>, <<"1234">>},
    {<<"cache-control">>, <<"no-cache">>},
    {<<"vary">>, <<"accept-encoding">>},
    {<<"server">>, <<"hackney">>}
]).

%%====================================================================
%% Public API
%%====================================================================

run() ->
    run(?DEFAULT_ITERATIONS).

run(N) ->
    io:format("~n=== QPACK Benchmark Suite ===~n"),
    io:format("Iterations: ~p~n~n", [N]),

    %% Warm up JIT
    io:format("Warming up...~n"),
    _ = [hackney_qpack:encode(?SIMPLE_HEADERS) || _ <- lists:seq(1, 1000)],

    io:format("~n--- Encode Benchmarks ---~n~n"),
    bench_encode_test(N, "Simple headers (4)", ?SIMPLE_HEADERS),
    bench_encode_test(N, "Typical request (8)", ?TYPICAL_REQUEST),
    bench_encode_test(N, "Typical response (6)", ?TYPICAL_RESPONSE),

    io:format("~n--- Decode Benchmarks ---~n~n"),
    bench_decode_test(N, "Simple headers (4)", ?SIMPLE_HEADERS),
    bench_decode_test(N, "Typical request (8)", ?TYPICAL_REQUEST),
    bench_decode_test(N, "Typical response (6)", ?TYPICAL_RESPONSE),

    io:format("~n--- Static Table Lookup ---~n~n"),
    bench_static_lookup_test(N),

    io:format("~n=== Benchmark Complete ===~n"),
    ok.

%%====================================================================
%% Individual Benchmarks
%%====================================================================

bench_encode() ->
    io:format("~n--- Encode Performance ---~n"),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:encode(?TYPICAL_REQUEST)
        end, lists:seq(1, 10000))
    end),

    io:format("hackney_qpack: ~.2f ms for 10k iterations~n", [Time / 1000]),
    io:format("  ~.2f us/op~n", [Time / 10000]),
    ok.

bench_decode() ->
    io:format("~n--- Decode Performance ---~n"),

    Encoded = hackney_qpack:encode(?TYPICAL_REQUEST),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:decode(Encoded)
        end, lists:seq(1, 10000))
    end),

    io:format("hackney_qpack: ~.2f ms for 10k iterations~n", [Time / 1000]),
    io:format("  ~.2f us/op~n", [Time / 10000]),
    ok.

bench_static_lookup() ->
    io:format("~n--- Static Table Lookup Performance ---~n"),

    %% Test headers that have exact matches in static table
    ExactMatchHeaders = [
        {<<":method">>, <<"GET">>},      %% Index 17
        {<<":status">>, <<"200">>},      %% Index 25
        {<<"accept">>, <<"*/*">>}        %% Index 29
    ],

    %% Test headers that only have name matches
    NameMatchHeaders = [
        {<<":method">>, <<"PATCH">>},    %% Name at 15-21
        {<<":status">>, <<"201">>},      %% Name at 24-28, 63-71
        {<<"content-type">>, <<"text/xml">>}  %% Name at 44-54
    ],

    %% Test headers with no matches
    NoMatchHeaders = [
        {<<"x-custom-header">>, <<"custom-value">>},
        {<<"x-request-id">>, <<"12345">>}
    ],

    io:format("~nExact match (indexed field):~n"),
    {Time1, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:encode(ExactMatchHeaders)
        end, lists:seq(1, 100000))
    end),
    io:format("  ~.2f us/op~n", [Time1 / 100000]),

    io:format("~nName match (literal with name ref):~n"),
    {Time2, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:encode(NameMatchHeaders)
        end, lists:seq(1, 100000))
    end),
    io:format("  ~.2f us/op~n", [Time2 / 100000]),

    io:format("~nNo match (literal):~n"),
    {Time3, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:encode(NoMatchHeaders)
        end, lists:seq(1, 100000))
    end),
    io:format("  ~.2f us/op~n", [Time3 / 100000]),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

bench_encode_test(N, Label, Headers) ->
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:encode(Headers)
        end, lists:seq(1, N))
    end),

    io:format("~s:~n", [Label]),
    io:format("  hackney_qpack: ~.2f us/op~n~n", [Time / N]).

bench_decode_test(N, Label, Headers) ->
    Encoded = hackney_qpack:encode(Headers),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:decode(Encoded)
        end, lists:seq(1, N))
    end),

    io:format("~s:~n", [Label]),
    io:format("  hackney_qpack: ~.2f us/op~n~n", [Time / N]).

bench_static_lookup_test(N) ->
    %% All 99 static table entries
    AllStaticHeaders = [
        %% Exact matches
        {<<":method">>, <<"GET">>},
        {<<":method">>, <<"POST">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"https">>},
        {<<":status">>, <<"200">>},
        {<<":status">>, <<"404">>},
        {<<"accept">>, <<"*/*">>},
        {<<"accept-encoding">>, <<"gzip, deflate, br">>},
        {<<"content-type">>, <<"application/json">>},
        {<<"cache-control">>, <<"no-cache">>}
    ],

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_qpack:encode(AllStaticHeaders)
        end, lists:seq(1, N))
    end),

    io:format("Static table hits (10 headers, all indexed):~n"),
    io:format("  ~.2f us/op~n", [Time / N]),
    io:format("  ~.2f us/header~n~n", [Time / N / 10]).
