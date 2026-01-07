%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_app_tests).
-include_lib("eunit/include/eunit.hrl").

%% get_app_env/1 tests
get_app_env_1_test_() ->
    {setup,
     fun() ->
         %% Set a test environment variable
         application:set_env(hackney, test_key, test_value),
         ok
     end,
     fun(_) ->
         %% Clean up
         application:unset_env(hackney, test_key),
         ok
     end,
     [
        {"returns value when key exists",
         fun() ->
             ?assertEqual(test_value, hackney_app:get_app_env(test_key))
         end},
        {"returns undefined when key does not exist",
         fun() ->
             ?assertEqual(undefined, hackney_app:get_app_env(nonexistent_key))
         end}
     ]}.

%% get_app_env/2 tests
get_app_env_2_test_() ->
    {setup,
     fun() ->
         %% Set a test environment variable
         application:set_env(hackney, test_key2, test_value2),
         ok
     end,
     fun(_) ->
         %% Clean up
         application:unset_env(hackney, test_key2),
         ok
     end,
     [
        {"returns value when key exists (ignores default)",
         fun() ->
             ?assertEqual(test_value2, hackney_app:get_app_env(test_key2, default_val))
         end},
        {"returns default when key does not exist",
         fun() ->
             ?assertEqual(my_default, hackney_app:get_app_env(nonexistent_key2, my_default))
         end},
        {"returns atom default",
         fun() ->
             ?assertEqual(some_atom, hackney_app:get_app_env(missing_key, some_atom))
         end},
        {"returns integer default",
         fun() ->
             ?assertEqual(42, hackney_app:get_app_env(missing_key, 42))
         end},
        {"returns list default",
         fun() ->
             ?assertEqual([a, b, c], hackney_app:get_app_env(missing_key, [a, b, c]))
         end}
     ]}.

%% stop/1 test
stop_test_() ->
    [
        {"stop returns ok",
         fun() ->
             ?assertEqual(ok, hackney_app:stop(any_state))
         end}
    ].
