%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CONFIG, hackney_config).

%% hackney_metrics requires the hackney_config ETS table to exist
%% These tests set up a minimal environment

metrics_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"init inserts mod_metrics into ETS",
         fun() ->
             hackney_metrics:init(),
             %% Verify that mod_metrics key exists in the ETS table
             Result = ets:lookup(?CONFIG, mod_metrics),
             ?assertMatch([{mod_metrics, _}], Result)
         end},
        {"get_engine returns the metrics engine",
         fun() ->
             hackney_metrics:init(),
             Engine = hackney_metrics:get_engine(),
             %% Should return a metrics module reference
             ?assert(is_tuple(Engine) orelse is_atom(Engine))
         end},
        {"init with dummy metrics",
         fun() ->
             %% Ensure we're using dummy metrics (default)
             application:set_env(hackney, mod_metrics, dummy),
             hackney_metrics:init(),
             Engine = hackney_metrics:get_engine(),
             ?assert(is_tuple(Engine) orelse is_atom(Engine)),
             application:unset_env(hackney, mod_metrics)
         end}
     ]}.

setup() ->
    %% Create the ETS table if it doesn't exist
    case ets:info(?CONFIG) of
        undefined ->
            ets:new(?CONFIG, [named_table, public, set]);
        _ ->
            ok
    end,
    ok.

teardown(_) ->
    %% Clean up the ETS table entry
    catch ets:delete(?CONFIG, mod_metrics),
    ok.
