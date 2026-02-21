%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc HPACK benchmark suite.
%%%
%%% Compares performance of hackney_hpack (optimized) vs hackney_cow_hpack (original).
%%% @end

-module(hackney_hpack_bench).

-export([run/0, run/1]).
-export([bench_encode/0, bench_decode/0, bench_dynamic_table/0]).

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
    {<<"accept">>, <<"application/json">>},
    {<<"accept-encoding">>, <<"gzip, deflate">>},
    {<<"authorization">>, <<"Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9">>},
    {<<"x-request-id">>, <<"550e8400-e29b-41d4-a716-446655440000">>}
]).

-define(TYPICAL_RESPONSE, [
    {<<":status">>, <<"200">>},
    {<<"content-type">>, <<"application/json">>},
    {<<"content-length">>, <<"1234">>},
    {<<"cache-control">>, <<"max-age=3600">>},
    {<<"etag">>, <<"\"abc123\"">>},
    {<<"date">>, <<"Fri, 21 Feb 2025 12:00:00 GMT">>},
    {<<"server">>, <<"hackney">>},
    {<<"x-request-id">>, <<"550e8400-e29b-41d4-a716-446655440000">>}
]).

%%====================================================================
%% Public API
%%====================================================================

run() ->
    run(?DEFAULT_ITERATIONS).

run(N) ->
    io:format("~n=== HPACK Benchmark Suite ===~n"),
    io:format("Iterations: ~p~n~n", [N]),

    %% Warm up JIT
    io:format("Warming up...~n"),
    _ = [bench_iteration(encode, ?SIMPLE_HEADERS, hackney_hpack, hackney_hpack:init()) || _ <- lists:seq(1, 1000)],
    _ = [bench_iteration(encode, ?SIMPLE_HEADERS, hackney_cow_hpack, hackney_cow_hpack:init()) || _ <- lists:seq(1, 1000)],

    io:format("~n--- Encode Benchmarks ---~n~n"),
    bench_encode_comparison(N, "Simple headers (4)", ?SIMPLE_HEADERS),
    bench_encode_comparison(N, "Typical request (9)", ?TYPICAL_REQUEST),
    bench_encode_comparison(N, "Typical response (8)", ?TYPICAL_RESPONSE),

    io:format("~n--- Decode Benchmarks ---~n~n"),
    bench_decode_comparison(N, "Simple headers (4)", ?SIMPLE_HEADERS),
    bench_decode_comparison(N, "Typical request (9)", ?TYPICAL_REQUEST),
    bench_decode_comparison(N, "Typical response (8)", ?TYPICAL_RESPONSE),

    io:format("~n--- Dynamic Table Lookup ---~n~n"),
    bench_dynamic_table_comparison(N),

    io:format("~n=== Benchmark Complete ===~n"),
    ok.

%%====================================================================
%% Individual Benchmarks
%%====================================================================

bench_encode() ->
    io:format("~n--- Encode Performance ---~n"),

    State1 = hackney_hpack:init(),
    State2 = hackney_cow_hpack:init(),

    {Time1, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_hpack:encode(?TYPICAL_REQUEST, S),
            S2
        end, State1, lists:seq(1, 10000))
    end),

    {Time2, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_cow_hpack:encode(?TYPICAL_REQUEST, S),
            S2
        end, State2, lists:seq(1, 10000))
    end),

    io:format("hackney_hpack:     ~.2f ms~n", [Time1 / 1000]),
    io:format("hackney_cow_hpack: ~.2f ms~n", [Time2 / 1000]),
    io:format("Speedup: ~.2fx~n", [Time2 / max(Time1, 1)]),
    ok.

bench_decode() ->
    io:format("~n--- Decode Performance ---~n"),

    {Encoded, _} = hackney_hpack:encode(?TYPICAL_REQUEST),
    EncodedBin = iolist_to_binary(Encoded),

    State1 = hackney_hpack:init(),
    State2 = hackney_cow_hpack:init(),

    {Time1, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_hpack:decode(EncodedBin, S),
            S2
        end, State1, lists:seq(1, 10000))
    end),

    {Time2, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_cow_hpack:decode(EncodedBin, S),
            S2
        end, State2, lists:seq(1, 10000))
    end),

    io:format("hackney_hpack:     ~.2f ms~n", [Time1 / 1000]),
    io:format("hackney_cow_hpack: ~.2f ms~n", [Time2 / 1000]),
    io:format("Speedup: ~.2fx~n", [Time2 / max(Time1, 1)]),
    ok.

bench_dynamic_table() ->
    io:format("~n--- Dynamic Table Lookup Performance ---~n"),
    io:format("(After inserting 50 unique headers)~n"),

    %% Generate unique headers to fill dynamic table
    Headers = [{<<"x-custom-", (integer_to_binary(I))/binary>>,
                <<"value-", (integer_to_binary(I))/binary>>}
               || I <- lists:seq(1, 50)],

    %% Build up state with entries
    State1 = lists:foldl(fun(H, S) ->
        {_, S2} = hackney_hpack:encode([H], S),
        S2
    end, hackney_hpack:init(), Headers),

    State2 = lists:foldl(fun(H, S) ->
        {_, S2} = hackney_cow_hpack:encode([H], S),
        S2
    end, hackney_cow_hpack:init(), Headers),

    %% Now benchmark lookups
    TestHeader = [{<<"x-custom-25">>, <<"value-25">>}],

    {Time1, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_hpack:encode(TestHeader, State1)
        end, lists:seq(1, 100000))
    end),

    {Time2, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_cow_hpack:encode(TestHeader, State2)
        end, lists:seq(1, 100000))
    end),

    io:format("hackney_hpack (O(1) map):    ~.2f ms~n", [Time1 / 1000]),
    io:format("hackney_cow_hpack (O(n) list): ~.2f ms~n", [Time2 / 1000]),
    io:format("Speedup: ~.2fx~n", [Time2 / max(Time1, 1)]),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

bench_encode_comparison(N, Label, Headers) ->
    State1 = hackney_hpack:init(),
    State2 = hackney_cow_hpack:init(),

    {Time1, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_hpack:encode(Headers, S),
            S2
        end, State1, lists:seq(1, N))
    end),

    {Time2, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_cow_hpack:encode(Headers, S),
            S2
        end, State2, lists:seq(1, N))
    end),

    io:format("~s:~n", [Label]),
    io:format("  hackney_hpack:     ~.2f us/op~n", [Time1 / N]),
    io:format("  hackney_cow_hpack: ~.2f us/op~n", [Time2 / N]),
    io:format("  Speedup: ~.2fx~n~n", [Time2 / max(Time1, 1)]).

bench_decode_comparison(N, Label, Headers) ->
    {Encoded, _} = hackney_hpack:encode(Headers),
    EncodedBin = iolist_to_binary(Encoded),

    State1 = hackney_hpack:init(),
    State2 = hackney_cow_hpack:init(),

    {Time1, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_hpack:decode(EncodedBin, S),
            S2
        end, State1, lists:seq(1, N))
    end),

    {Time2, _} = timer:tc(fun() ->
        lists:foldl(fun(_, S) ->
            {_, S2} = hackney_cow_hpack:decode(EncodedBin, S),
            S2
        end, State2, lists:seq(1, N))
    end),

    io:format("~s:~n", [Label]),
    io:format("  hackney_hpack:     ~.2f us/op~n", [Time1 / N]),
    io:format("  hackney_cow_hpack: ~.2f us/op~n", [Time2 / N]),
    io:format("  Speedup: ~.2fx~n~n", [Time2 / max(Time1, 1)]).

bench_dynamic_table_comparison(N) ->
    %% Generate headers to fill dynamic table
    Headers = [{<<"x-custom-", (integer_to_binary(I))/binary>>,
                <<"value-", (integer_to_binary(I))/binary>>}
               || I <- lists:seq(1, 50)],

    State1 = lists:foldl(fun(H, S) ->
        {_, S2} = hackney_hpack:encode([H], S),
        S2
    end, hackney_hpack:init(), Headers),

    State2 = lists:foldl(fun(H, S) ->
        {_, S2} = hackney_cow_hpack:encode([H], S),
        S2
    end, hackney_cow_hpack:init(), Headers),

    %% Benchmark lookups with different positions
    bench_dyn_lookup(N, "Newest entry (position 1)",
        [{<<"x-custom-50">>, <<"value-50">>}], State1, State2),
    bench_dyn_lookup(N, "Middle entry (position 25)",
        [{<<"x-custom-25">>, <<"value-25">>}], State1, State2),
    bench_dyn_lookup(N, "Oldest entry (position 50)",
        [{<<"x-custom-1">>, <<"value-1">>}], State1, State2).

bench_dyn_lookup(N, Label, Headers, State1, State2) ->
    {Time1, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_hpack:encode(Headers, State1)
        end, lists:seq(1, N))
    end),

    {Time2, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            hackney_cow_hpack:encode(Headers, State2)
        end, lists:seq(1, N))
    end),

    io:format("~s:~n", [Label]),
    io:format("  hackney_hpack (O(1)):    ~.2f us/op~n", [Time1 / N]),
    io:format("  hackney_cow_hpack (O(n)): ~.2f us/op~n", [Time2 / N]),
    io:format("  Speedup: ~.2fx~n~n", [Time2 / max(Time1, 1)]).

bench_iteration(encode, Headers, Module, State) ->
    {_, S2} = Module:encode(Headers, State),
    S2.
