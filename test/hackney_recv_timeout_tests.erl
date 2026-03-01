%%% -*- erlang -*-
%%%
%%% Test for recv_timeout option with connection pooling
%%%
%%% Bug: When using connection pooling, the recv_timeout option passed in
%%% hackney:request/5 is ignored. The connection uses its original timeout
%%% from when it was created.
%%%
%%% Expected: Each request should respect its own recv_timeout option
%%% Actual: Pooled connections use the timeout they were created with

-module(hackney_recv_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8125).
-define(URL(Path), "http://127.0.0.1:" ++ integer_to_list(?PORT) ++ Path).

%%====================================================================
%% Test Setup
%%====================================================================

recv_timeout_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"recv_timeout works without pooling", fun test_timeout_no_pool/0},
      {"recv_timeout should work with pooling", fun test_timeout_with_pool/0}
     ]}.

setup() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),

    %% Start test server with delay endpoint
    Host = '_',
    Routes = [
        {"/delay/:seconds", delay_handler, []},
        {"/[...]", test_http_resource, []}
    ],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    {ok, _} = cowboy:start_clear(recv_timeout_test_server,
                                  [{port, ?PORT}],
                                  #{env => #{dispatch => Dispatch}}),
    ok.

teardown(_) ->
    cowboy:stop_listener(recv_timeout_test_server),
    error_logger:tty(true),
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_timeout_no_pool() ->
    %% Without pooling, recv_timeout should work correctly
    %% Request to /delay/2 with 100ms timeout should timeout
    Url = ?URL("/delay/2"),
    Opts = [
        {pool, false},           % Disable pooling
        {recv_timeout, 100}      % 100ms timeout
    ],

    Result = hackney:request(get, Url, [], <<>>, Opts),

    %% Should timeout
    ?assertEqual({error, timeout}, Result).

test_timeout_with_pool() ->
    %% With pooling, recv_timeout should still work for each request
    PoolName = recv_timeout_test_pool,

    %% Create a dedicated pool for this test
    ok = hackney_pool:start_pool(PoolName, [{pool_size, 1}]),

    try
        Url = ?URL("/delay/2"),

        %% First request: create connection with long timeout (10000ms)
        %% This should succeed (delay is only 2 seconds)
        {ok, 200, _, _} = hackney:request(get, ?URL("/get"), [], <<>>,
                                           [{pool, PoolName}, {recv_timeout, 10000}]),

        %% Second request: try to use same pooled connection with 100ms timeout
        %% This SHOULD timeout because the delay is 2 seconds
        ShortTimeoutOpts = [{pool, PoolName}, {recv_timeout, 100}],

        Result = hackney:request(get, Url, [], <<>>, ShortTimeoutOpts),

        %% Expected: {error, timeout}
        %% Bug: {ok, 200, _, _} - request succeeds because timeout is ignored
        ?assertEqual({error, timeout}, Result)
    after
        hackney_pool:stop_pool(PoolName)
    end.
