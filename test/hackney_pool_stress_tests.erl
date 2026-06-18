%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Concurrent same-URI pool stress: many workers hit one URI through a small
%%% warm pool; every request must return a 2xx. Covers keep-alive, Connection:
%%% close, and a random mix on the same URI.
-module(hackney_pool_stress_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8138).
-define(POOL, stress_test_pool).
-define(WORKERS, 50).
-define(REQS, 20).

stress_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"concurrent same-URI keep-alive requests all return ok",
       {timeout, 120, fun all_keepalive/0}},
      {"concurrent same-URI Connection: close requests all return ok",
       {timeout, 120, fun all_close/0}},
      {"concurrent same-URI random keep-alive/close requests all return ok",
       {timeout, 120, fun random_close/0}}
     ]}.

setup() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(stress_test_server, [{port, ?PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    %% Small warm pool on purpose: the burst is served by overflow connections.
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 4}, {prewarm_count, 0}]),
    ok.

teardown(_) ->
    try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end,
    try cowboy:stop_listener(stress_test_server) catch _:_ -> ok end,
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

all_keepalive() -> assert_all_ok(<<"/get">>).

all_close() -> assert_all_ok(<<"/connection-close">>).

random_close() -> assert_all_ok(<<"/maybe-close">>).

assert_all_ok(Path) ->
    Url = iolist_to_binary([<<"http://127.0.0.1:">>, integer_to_list(?PORT), Path]),
    #{ok := Ok, errors := Errors} =
        stress_pool_concurrency:hammer(?POOL, Url, ?WORKERS, ?REQS),
    ?assertEqual([], Errors),
    ?assertEqual(?WORKERS * ?REQS, Ok),
    %% Pool is still healthy after the burst.
    ?assert(is_list(hackney_pool:get_stats(?POOL))).
