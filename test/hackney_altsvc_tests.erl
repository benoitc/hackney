%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for Alt-Svc header parsing and caching.

-module(hackney_altsvc_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    hackney_altsvc:init(),
    hackney_altsvc:clear_all(),
    ok.

cleanup(_) ->
    hackney_conn_sup:stop_all(),
    hackney_altsvc:clear_all(),
    ok.

%%====================================================================
%% Parse Tests
%%====================================================================

parse_test_() ->
    {
        "Alt-Svc parsing tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"parse h3 with port", fun test_parse_h3_port/0},
                {"parse h3 with ma parameter", fun test_parse_h3_ma/0},
                {"parse multiple entries", fun test_parse_multiple/0},
                {"parse clear", fun test_parse_clear/0},
                {"parse h3-29 variant", fun test_parse_h3_variant/0}
            ]
        }
    }.

test_parse_h3_port() ->
    Result = hackney_altsvc:parse(<<"h3=\":443\"">>),
    ?assertEqual([{h3, same, 443, 86400}], Result).

test_parse_h3_ma() ->
    Result = hackney_altsvc:parse(<<"h3=\":443\"; ma=3600">>),
    ?assertEqual([{h3, same, 443, 3600}], Result).

test_parse_multiple() ->
    Result = hackney_altsvc:parse(<<"h3=\":443\"; ma=86400, h2=\":443\"">>),
    ?assertEqual([{h3, same, 443, 86400}, {<<"h2">>, same, 443, 86400}], Result).

test_parse_clear() ->
    Result = hackney_altsvc:parse(<<"clear">>),
    ?assertEqual([], Result).

test_parse_h3_variant() ->
    Result = hackney_altsvc:parse(<<"h3-29=\":443\"">>),
    ?assertEqual([{h3, same, 443, 86400}], Result).

%%====================================================================
%% Cache Tests
%%====================================================================

cache_test_() ->
    {
        "Alt-Svc cache tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"cache and lookup", fun test_cache_lookup/0},
                {"lookup returns none when not cached", fun test_lookup_none/0},
                {"clear removes entry", fun test_clear/0},
                {"parse_and_cache from headers", fun test_parse_and_cache/0}
            ]
        }
    }.

test_cache_lookup() ->
    hackney_altsvc:cache(<<"example.com">>, 443, 443, 3600),
    Result = hackney_altsvc:lookup(<<"example.com">>, 443),
    ?assertEqual({ok, h3, 443}, Result).

test_lookup_none() ->
    Result = hackney_altsvc:lookup(<<"nonexistent.example.com">>, 443),
    ?assertEqual(none, Result).

test_clear() ->
    hackney_altsvc:cache(<<"clear-test.example.com">>, 443, 443, 3600),
    ?assertEqual({ok, h3, 443}, hackney_altsvc:lookup(<<"clear-test.example.com">>, 443)),
    hackney_altsvc:clear(<<"clear-test.example.com">>, 443),
    ?assertEqual(none, hackney_altsvc:lookup(<<"clear-test.example.com">>, 443)).

test_parse_and_cache() ->
    Headers = [{<<"alt-svc">>, <<"h3=\":443\"; ma=3600">>}],
    Result = hackney_altsvc:parse_and_cache(<<"cached.example.com">>, 443, Headers),
    ?assertEqual({ok, h3, 443}, Result),
    %% Verify it's cached
    ?assertEqual({ok, h3, 443}, hackney_altsvc:lookup(<<"cached.example.com">>, 443)).

%%====================================================================
%% Blocked Cache Tests
%%====================================================================

blocked_test_() ->
    {
        "HTTP/3 blocked cache tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"mark and check blocked", fun test_mark_blocked/0},
                {"not blocked by default", fun test_not_blocked/0}
            ]
        }
    }.

test_mark_blocked() ->
    hackney_altsvc:mark_h3_blocked(<<"blocked.example.com">>, 443),
    ?assert(hackney_altsvc:is_h3_blocked(<<"blocked.example.com">>, 443)).

test_not_blocked() ->
    ?assertNot(hackney_altsvc:is_h3_blocked(<<"notblocked.example.com">>, 443)).
