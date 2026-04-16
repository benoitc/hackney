%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% End-to-end test: drive a real cowboy server through the hackney
%%% request funnel with a middleware that counts + rewrites headers.
%%% Proves the chain runs at the public `hackney:request/5' entry point.
-module(hackney_middleware_integration_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 9981).
-define(PT_COUNT, {?MODULE, count}).

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    persistent_term:put(?PT_COUNT, 0),
    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", test_http_resource, []}]}
    ]),
    {ok, _} = cowboy:start_clear(middleware_test_http,
                                 [{port, ?PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

cleanup(_) ->
    cowboy:stop_listener(middleware_test_http),
    persistent_term:erase(?PT_COUNT),
    application:unset_env(hackney, middleware),
    ok.

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?PORT))/binary, Path/binary>>.

middleware_integration_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
        fun per_request_chain_fires_on_every_call/0,
        fun header_rewrite_reaches_the_server/0,
        fun global_env_chain_is_used_when_option_absent/0,
        fun short_circuit_skips_the_network/0
     ]}.

%% ============================================================================

per_request_chain_fires_on_every_call() ->
    persistent_term:put(?PT_COUNT, 0),
    Counter = fun(Req, Next) ->
        persistent_term:put(?PT_COUNT, persistent_term:get(?PT_COUNT) + 1),
        Next(Req)
    end,
    Opts = [{middleware, [Counter]}, {pool, false}],
    {ok, 200, _, _} = hackney:request(get, url(<<"/get">>), [], <<>>, Opts),
    {ok, 200, _, _} = hackney:request(get, url(<<"/get">>), [], <<>>, Opts),
    {ok, 200, _, _} = hackney:request(get, url(<<"/get">>), [], <<>>, Opts),
    ?assertEqual(3, persistent_term:get(?PT_COUNT)).

header_rewrite_reaches_the_server() ->
    %% Middleware appends a header; test_http_resource echoes the
    %% request headers back in the JSON body at /get.
    AddHeader = fun(Req, Next) ->
        H = maps:get(headers, Req),
        Next(Req#{headers := [{<<"x-mw">>, <<"present">>} | H]})
    end,
    Opts = [{middleware, [AddHeader]}, {pool, false}],
    {ok, 200, _, Body} =
        hackney:request(get, url(<<"/get">>), [], <<>>, Opts),
    #{<<"headers">> := Headers} = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"present">>, maps:get(<<"x-mw">>, Headers)).

global_env_chain_is_used_when_option_absent() ->
    persistent_term:put(?PT_COUNT, 0),
    Counter = fun(Req, Next) ->
        persistent_term:put(?PT_COUNT, persistent_term:get(?PT_COUNT) + 1),
        Next(Req)
    end,
    application:set_env(hackney, middleware, [Counter]),
    try
        {ok, 200, _, _} =
            hackney:request(get, url(<<"/get">>), [], <<>>, [{pool, false}])
    after
        application:unset_env(hackney, middleware)
    end,
    ?assertEqual(1, persistent_term:get(?PT_COUNT)).

short_circuit_skips_the_network() ->
    Cache = fun(_Req, _Next) -> {ok, 418, [], <<"from-cache">>} end,
    Opts = [{middleware, [Cache]}, {pool, false}],
    ?assertEqual({ok, 418, [], <<"from-cache">>},
                 hackney:request(get, url(<<"/get">>), [], <<>>, Opts)),
    %% Prove the cowboy listener is still alive — no network call was
    %% actually attempted.
    {ok, 200, _, _} =
        hackney:request(get, url(<<"/get">>), [], <<>>, [{pool, false}]).
