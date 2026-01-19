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
        {"init inserts metrics_backend into ETS",
         fun() ->
             hackney_metrics:init(),
             %% Verify that metrics_backend key exists in the ETS table
             Result = ets:lookup(?CONFIG, metrics_backend),
             ?assertMatch([{metrics_backend, _}], Result)
         end},
        {"get_backend returns the metrics backend module",
         fun() ->
             hackney_metrics:init(),
             Backend = hackney_metrics:get_backend(),
             %% Should return a metrics module (atom)
             ?assert(is_atom(Backend))
         end},
        {"init uses dummy backend by default",
         fun() ->
             %% Ensure we're using dummy metrics (default)
             application:unset_env(hackney, metrics_backend),
             hackney_metrics:init(),
             Backend = hackney_metrics:get_backend(),
             ?assertEqual(hackney_metrics_dummy, Backend)
         end},
        {"dummy backend counter_inc works",
         fun() ->
             %% counter_inc should not crash
             ?assertEqual(ok, hackney_metrics:counter_inc(test_counter, #{host => <<"test">>}))
         end},
        {"dummy backend gauge_set works",
         fun() ->
             %% gauge_set should not crash
             ?assertEqual(ok, hackney_metrics:gauge_set(test_gauge, #{pool => default}, 42))
         end},
        {"dummy backend histogram_observe works",
         fun() ->
             %% histogram_observe should not crash
             ?assertEqual(ok, hackney_metrics:histogram_observe(test_histogram, #{host => <<"test">>}, 0.5))
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
    catch ets:delete(?CONFIG, metrics_backend),
    ok.
