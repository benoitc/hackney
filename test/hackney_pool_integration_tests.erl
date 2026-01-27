%% @doc Integration tests for pool connection release behavior
-module(hackney_pool_integration_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 9877).
-define(POOL, test_pool_integration).

%% Setup/teardown for integration tests
setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(test_pool_int_http, [{port, ?PORT}], #{
        env => #{dispatch => Dispatch}
    }),
    %% Create a test pool with small limits for easy testing
    hackney_pool:start_pool(?POOL, [{max_connections, 10}]),
    ok.

cleanup(_) ->
    hackney_pool:stop_pool(?POOL),
    cowboy:stop_listener(test_pool_int_http),
    ok.

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?PORT))/binary, Path/binary>>.

%% =============================================================================
%% Pool Integration Tests
%% =============================================================================

pool_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"connection released after with_body request", fun test_with_body_release/0},
      {"connection released after manual body read", fun test_manual_body_release/0},
      {"connection released after close", fun test_close_release/0},
      {"multiple requests reuse connections", fun test_connection_reuse_integration/0},
      {"concurrent requests respect pool limits", fun test_concurrent_requests/0},
      {"connection released on error response", fun test_error_response_release/0},
      {"pool stats accurate during requests", fun test_pool_stats_accuracy/0}
     ]}.

%% Test that connection is released after with_body request
test_with_body_release() ->
    %% Get initial stats
    InitStats = hackney_pool:get_stats(?POOL),
    InitFree = proplists:get_value(free_count, InitStats),
    InitInUse = proplists:get_value(in_use_count, InitStats),

    %% Make request with with_body
    {ok, 200, _Headers, _Body} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                                  [{pool, ?POOL}, {with_body, true}]),

    %% Allow time for async checkin
    timer:sleep(50),

    %% Check stats after - should have one more free connection
    AfterStats = hackney_pool:get_stats(?POOL),
    AfterFree = proplists:get_value(free_count, AfterStats),
    AfterInUse = proplists:get_value(in_use_count, AfterStats),

    %% Connection should be returned to pool (free increased or same)
    ?assert(AfterFree >= InitFree orelse AfterInUse =< InitInUse),
    %% No connections should be in use
    ?assertEqual(0, AfterInUse).

%% Test that connection is released after body read
%% In 3.x, body is always returned directly and connection is auto-released
test_manual_body_release() ->
    %% Make request - body is returned directly
    {ok, 200, _Headers, Body} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                                [{pool, ?POOL}]),
    ?assert(is_binary(Body)),

    %% Allow time for async checkin
    timer:sleep(50),

    %% Check stats after - no connections in use (connection auto-released)
    AfterStats = hackney_pool:get_stats(?POOL),
    AfterInUse = proplists:get_value(in_use_count, AfterStats),
    ?assertEqual(0, AfterInUse).

%% Test that connection is released after request completes
%% In 3.x, body is returned directly so connection is auto-released
test_close_release() ->
    %% Make request - body is returned directly
    {ok, 200, _Headers, Body} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                                [{pool, ?POOL}]),
    ?assert(is_binary(Body)),

    %% Allow time for process cleanup
    timer:sleep(50),

    %% Check stats after - no connections in use (connection auto-released)
    AfterStats = hackney_pool:get_stats(?POOL),
    AfterInUse = proplists:get_value(in_use_count, AfterStats),
    ?assertEqual(0, AfterInUse).

%% Test that connections are reused
test_connection_reuse_integration() ->
    %% Make first request
    {ok, 200, _, _} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                       [{pool, ?POOL}, {with_body, true}]),
    timer:sleep(50),

    Stats1 = hackney_pool:get_stats(?POOL),
    Free1 = proplists:get_value(free_count, Stats1),

    %% Make second request - should reuse connection
    {ok, 200, _, _} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                       [{pool, ?POOL}, {with_body, true}]),
    timer:sleep(50),

    Stats2 = hackney_pool:get_stats(?POOL),
    Free2 = proplists:get_value(free_count, Stats2),

    %% Should have same number of free connections (connection reused)
    ?assertEqual(Free1, Free2).

%% Test concurrent requests respect pool limits
test_concurrent_requests() ->
    %% Start 5 concurrent requests
    Self = self(),
    NumRequests = 5,

    Pids = [spawn(fun() ->
        Result = hackney:request(get, url(<<"/get">>), [], <<>>,
                                  [{pool, ?POOL}, {with_body, true}]),
        Self ! {done, self(), Result}
    end) || _ <- lists:seq(1, NumRequests)],

    %% Collect results
    Results = [receive {done, Pid, R} -> R end || Pid <- Pids],

    %% All should succeed
    lists:foreach(fun(R) ->
        ?assertMatch({ok, 200, _, _}, R)
    end, Results),

    %% Allow time for cleanup
    timer:sleep(100),

    %% No connections in use after all complete
    Stats = hackney_pool:get_stats(?POOL),
    InUse = proplists:get_value(in_use_count, Stats),
    ?assertEqual(0, InUse).

%% Test connection released after error response (4xx/5xx)
test_error_response_release() ->
    %% Make request to 404 endpoint
    {ok, 404, _Headers, Body} = hackney:request(get, url(<<"/not-found">>), [], <<>>,
                                                 [{pool, ?POOL}, {with_body, true}]),
    ?assertMatch(<<"{\"error\":", _/binary>>, Body),

    timer:sleep(50),

    %% Connection should still be returned to pool
    Stats = hackney_pool:get_stats(?POOL),
    InUse = proplists:get_value(in_use_count, Stats),
    ?assertEqual(0, InUse).

%% Test pool stats are accurate after request lifecycle
%% In 3.x, body is returned directly so connection is released immediately
test_pool_stats_accuracy() ->
    %% Initial state
    Stats0 = hackney_pool:get_stats(?POOL),
    InUse0 = proplists:get_value(in_use_count, Stats0),

    %% Make request - body is returned directly
    {ok, 200, _Headers, Body} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                                [{pool, ?POOL}]),
    ?assert(is_binary(Body)),

    %% Allow time for async checkin
    timer:sleep(50),

    %% After request - connection returned (should be same as initial)
    Stats2 = hackney_pool:get_stats(?POOL),
    InUse2 = proplists:get_value(in_use_count, Stats2),
    ?assertEqual(InUse0, InUse2).
