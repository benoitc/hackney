%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025 Benoît Chesneau <bchesneau@gmail.com>
%%%

-module(hackney_proxy_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test empty proxy environment variables handling
empty_proxy_env_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
         [
          {"Empty http_proxy", fun() -> test_empty_proxy_env("http_proxy", State) end},
          {"Empty HTTP_PROXY", fun() -> test_empty_proxy_env("HTTP_PROXY", State) end},
          {"Empty https_proxy", fun() -> test_empty_proxy_env("https_proxy", State) end},
          {"Empty HTTPS_PROXY", fun() -> test_empty_proxy_env("HTTPS_PROXY", State) end},
          {"Whitespace http_proxy", fun() -> test_whitespace_proxy_env("http_proxy", State) end},
          {"Whitespace HTTPS_PROXY", fun() -> test_whitespace_proxy_env("HTTPS_PROXY", State) end},
          {"Multiple spaces proxy", fun() -> test_multiple_spaces_proxy_env("http_proxy", State) end},
          {"Valid proxy URL", fun() -> test_valid_proxy_env("http_proxy", State) end},
          {"Valid proxy with spaces", fun() -> test_valid_proxy_with_spaces("HTTP_PROXY", State) end}
         ]
     end}.

setup() ->
    %% Save current environment variables
    SavedEnv = [{Var, os:getenv(Var)} || Var <- ["http_proxy", "HTTP_PROXY", 
                                                  "https_proxy", "HTTPS_PROXY",
                                                  "all_proxy", "ALL_PROXY"]],
    %% Clear all proxy environment variables
    [os:unsetenv(Var) || {Var, _} <- SavedEnv],
    %% Clear hackney's cached proxy settings
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    SavedEnv.

teardown(SavedEnv) ->
    %% Restore original environment variables
    lists:foreach(fun
        ({Var, false}) -> os:unsetenv(Var);
        ({Var, Value}) -> os:putenv(Var, Value)
    end, SavedEnv),
    %% Clear hackney's cached proxy settings
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy).

test_empty_proxy_env(Var, _State) ->
    os:putenv(Var, ""),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should not crash and should return false (no proxy)
    Result = hackney:get_proxy_env(http),
    ?assertEqual(false, Result),
    os:unsetenv(Var).

test_whitespace_proxy_env(Var, _State) ->
    os:putenv(Var, "   "),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should not crash and should return false (no proxy)
    Result = hackney:get_proxy_env(http),
    ?assertEqual(false, Result),
    os:unsetenv(Var).

test_multiple_spaces_proxy_env(Var, _State) ->
    os:putenv(Var, "     \t   \n   "),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should not crash and should return false (no proxy)
    Result = hackney:get_proxy_env(http),
    ?assertEqual(false, Result),
    os:unsetenv(Var).

test_valid_proxy_env(Var, _State) ->
    ProxyUrl = "http://proxy.example.com:8080",
    os:putenv(Var, ProxyUrl),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should return the proxy URL
    Result = hackney:get_proxy_env(http),
    ?assertEqual({ok, ProxyUrl}, Result),
    os:unsetenv(Var).

test_valid_proxy_with_spaces(Var, _State) ->
    ProxyUrl = "  http://proxy.example.com:8080  ",
    ExpectedUrl = "http://proxy.example.com:8080",
    os:putenv(Var, ProxyUrl),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should return the proxy URL with stripped spaces
    Result = hackney:get_proxy_env(http),
    ?assertEqual({ok, ExpectedUrl}, Result),
    os:unsetenv(Var).

%% Direct tests for do_get_proxy_env function
do_get_proxy_env_test_() ->
    [
     {"Empty list returns false", 
      fun() -> ?assertEqual(false, hackney:do_get_proxy_env([])) end},
     {"All empty values returns false",
      fun() ->
          os:putenv("TEST_PROXY1", ""),
          os:putenv("TEST_PROXY2", "   "),
          os:putenv("TEST_PROXY3", "\t\n"),
          Result = hackney:do_get_proxy_env(["TEST_PROXY1", "TEST_PROXY2", "TEST_PROXY3"]),
          ?assertEqual(false, Result),
          os:unsetenv("TEST_PROXY1"),
          os:unsetenv("TEST_PROXY2"),
          os:unsetenv("TEST_PROXY3")
      end},
     {"First valid proxy is returned",
      fun() ->
          os:putenv("TEST_PROXY1", ""),
          os:putenv("TEST_PROXY2", "  http://proxy.example.com  "),
          os:putenv("TEST_PROXY3", "http://proxy2.example.com"),
          Result = hackney:do_get_proxy_env(["TEST_PROXY1", "TEST_PROXY2", "TEST_PROXY3"]),
          ?assertEqual({ok, "http://proxy.example.com"}, Result),
          os:unsetenv("TEST_PROXY1"),
          os:unsetenv("TEST_PROXY2"),
          os:unsetenv("TEST_PROXY3")
      end}
    ].

