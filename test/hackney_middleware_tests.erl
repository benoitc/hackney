%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_middleware_tests).
-include_lib("eunit/include/eunit.hrl").

-define(REQ, #{method  => get,
               url     => placeholder,
               headers => [],
               body    => <<>>,
               options => []}).

%% ============================================================================
%% apply_chain
%% ============================================================================

empty_chain_is_identity_test() ->
    Terminal = fun(_) -> {ok, 200, [], <<"terminal">>} end,
    ?assertEqual({ok, 200, [], <<"terminal">>},
                 hackney_middleware:apply_chain([], ?REQ, Terminal)).

outermost_first_order_test() ->
    %% Record order of entry and exit. `[A, B, C]' must run
    %% A-pre, B-pre, C-pre, terminal, C-post, B-post, A-post.
    Parent = self(),
    Mk = fun(Tag) ->
        fun(R, Next) ->
            Parent ! {pre, Tag},
            Resp = Next(R),
            Parent ! {post, Tag},
            Resp
        end
    end,
    Terminal = fun(_) -> Parent ! terminal, {ok, 200, [], <<>>} end,
    _ = hackney_middleware:apply_chain([Mk(a), Mk(b), Mk(c)], ?REQ, Terminal),
    ?assertEqual([{pre, a}, {pre, b}, {pre, c},
                  terminal,
                  {post, c}, {post, b}, {post, a}],
                 drain()).

request_rewrite_is_seen_downstream_test() ->
    AddHeader = fun(R, Next) ->
        H = maps:get(headers, R),
        Next(R#{headers := [{<<"x-test">>, <<"1">>} | H]})
    end,
    Terminal = fun(R) -> {ok, 200, maps:get(headers, R), <<>>} end,
    {ok, 200, Headers, _} =
        hackney_middleware:apply_chain([AddHeader], ?REQ, Terminal),
    ?assertEqual([{<<"x-test">>, <<"1">>}], Headers).

response_rewrite_is_seen_upstream_test() ->
    Inner = fun(R, Next) ->
        {ok, S, H, _B} = Next(R),
        {ok, S, H, <<"rewritten">>}
    end,
    Terminal = fun(_) -> {ok, 200, [], <<"original">>} end,
    ?assertMatch({ok, 200, [], <<"rewritten">>},
                 hackney_middleware:apply_chain([Inner], ?REQ, Terminal)).

short_circuit_does_not_call_terminal_test() ->
    Parent = self(),
    Cache = fun(_R, _Next) -> {ok, 200, [], <<"cached">>} end,
    Terminal = fun(_) -> Parent ! terminal_fired, {ok, 500, [], <<>>} end,
    ?assertEqual({ok, 200, [], <<"cached">>},
                 hackney_middleware:apply_chain([Cache], ?REQ, Terminal)),
    receive
        terminal_fired -> ?assert(false, "terminal fired despite short-circuit")
    after 10 -> ok
    end.

middleware_crash_propagates_test() ->
    Bad = fun(_R, _Next) -> erlang:error(boom) end,
    Terminal = fun(_) -> {ok, 200, [], <<>>} end,
    ?assertError(boom,
                 hackney_middleware:apply_chain([Bad], ?REQ, Terminal)).

error_tuple_flows_back_unchanged_test() ->
    Pass = fun(R, Next) -> Next(R) end,
    Terminal = fun(_) -> {error, closed} end,
    ?assertEqual({error, closed},
                 hackney_middleware:apply_chain([Pass], ?REQ, Terminal)).

%% ============================================================================
%% resolve_chain
%% ============================================================================

resolve_chain_prefers_per_request_option_test() ->
    A = fun(R, N) -> N(R) end,
    B = fun(R, N) -> N(R) end,
    application:set_env(hackney, middleware, [A]),
    try
        ?assertEqual([B],
                     hackney_middleware:resolve_chain([{middleware, [B]}]))
    after
        application:unset_env(hackney, middleware)
    end.

resolve_chain_falls_back_to_app_env_test() ->
    A = fun(R, N) -> N(R) end,
    application:set_env(hackney, middleware, [A]),
    try
        ?assertEqual([A], hackney_middleware:resolve_chain([]))
    after
        application:unset_env(hackney, middleware)
    end.

resolve_chain_defaults_to_empty_test() ->
    application:unset_env(hackney, middleware),
    ?assertEqual([], hackney_middleware:resolve_chain([])).

%% ============================================================================
%% Helpers
%% ============================================================================

drain() ->
    drain([]).

drain(Acc) ->
    receive
        M -> drain([M | Acc])
    after 0 -> lists:reverse(Acc)
    end.
