%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/3 connection pooling in hackney_pool.

-module(hackney_pool_h3_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

cleanup(_) ->
    hackney_conn_sup:stop_all(),
    ok.

%%====================================================================
%% HTTP/3 Pool Tests
%%====================================================================

h3_pool_test_() ->
    {
        "HTTP/3 pool tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"checkout_h3 returns none when no connection", fun test_h3_checkout_none/0},
                {"register_h3 and checkout_h3", fun test_h3_register_checkout/0},
                {"unregister_h3 removes connection", fun test_h3_unregister/0},
                {"connection death cleans h3_connections", fun test_h3_connection_death/0}
            ]
        }
    }.

test_h3_checkout_none() ->
    %% Without any registered H3 connection, checkout should return none
    Result = hackney_pool:checkout_h3("test.example.com", 443, hackney_ssl, []),
    ?assertEqual(none, Result).

test_h3_register_checkout() ->
    %% Start a dummy process to act as a connection
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it as an H3 connection
    ok = hackney_pool:register_h3("h3test.example.com", 443, hackney_ssl, DummyConn, []),

    %% Wait a bit for the async cast to process
    timer:sleep(50),

    %% Checkout should now return the connection
    Result = hackney_pool:checkout_h3("h3test.example.com", 443, hackney_ssl, []),
    ?assertEqual({ok, DummyConn}, Result),

    %% Cleanup
    DummyConn ! stop.

test_h3_unregister() ->
    %% Start a dummy process
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it
    ok = hackney_pool:register_h3("h3unreg.example.com", 443, hackney_ssl, DummyConn, []),
    timer:sleep(50),

    %% Verify it's there
    ?assertEqual({ok, DummyConn}, hackney_pool:checkout_h3("h3unreg.example.com", 443, hackney_ssl, [])),

    %% Unregister
    ok = hackney_pool:unregister_h3(DummyConn, []),
    timer:sleep(50),

    %% Should be gone now
    ?assertEqual(none, hackney_pool:checkout_h3("h3unreg.example.com", 443, hackney_ssl, [])),

    %% Cleanup
    DummyConn ! stop.

test_h3_connection_death() ->
    %% Start a dummy process
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it
    ok = hackney_pool:register_h3("h3death.example.com", 443, hackney_ssl, DummyConn, []),
    timer:sleep(50),

    %% Verify it's there
    ?assertEqual({ok, DummyConn}, hackney_pool:checkout_h3("h3death.example.com", 443, hackney_ssl, [])),

    %% Kill the process
    DummyConn ! stop,
    timer:sleep(100),

    %% Checkout should detect the dead connection and return none
    ?assertEqual(none, hackney_pool:checkout_h3("h3death.example.com", 443, hackney_ssl, [])).

%%====================================================================
%% Multiplexing Tests
%%====================================================================

h3_multiplexing_test_() ->
    {
        "HTTP/3 multiplexing tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"multiple callers get same connection", fun test_h3_multiplexing/0},
                {"different hosts get different connections", fun test_h3_different_hosts/0}
            ]
        }
    }.

test_h3_multiplexing() ->
    %% Start a dummy connection process
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it
    ok = hackney_pool:register_h3("h3mux.example.com", 443, hackney_ssl, DummyConn, []),
    timer:sleep(50),

    %% Multiple checkouts should return the same connection (multiplexing)
    {ok, Conn1} = hackney_pool:checkout_h3("h3mux.example.com", 443, hackney_ssl, []),
    {ok, Conn2} = hackney_pool:checkout_h3("h3mux.example.com", 443, hackney_ssl, []),
    {ok, Conn3} = hackney_pool:checkout_h3("h3mux.example.com", 443, hackney_ssl, []),

    %% All should be the same connection (multiplexed)
    ?assertEqual(DummyConn, Conn1),
    ?assertEqual(DummyConn, Conn2),
    ?assertEqual(DummyConn, Conn3),
    ?assertEqual(Conn1, Conn2),
    ?assertEqual(Conn2, Conn3),

    %% Cleanup
    DummyConn ! stop.

test_h3_different_hosts() ->
    %% Start two dummy connections
    Conn1 = spawn(fun() -> receive stop -> ok end end),
    Conn2 = spawn(fun() -> receive stop -> ok end end),

    %% Register them for different hosts
    ok = hackney_pool:register_h3("host1.example.com", 443, hackney_ssl, Conn1, []),
    ok = hackney_pool:register_h3("host2.example.com", 443, hackney_ssl, Conn2, []),
    timer:sleep(50),

    %% Checkout for host1 should return Conn1
    ?assertEqual({ok, Conn1}, hackney_pool:checkout_h3("host1.example.com", 443, hackney_ssl, [])),

    %% Checkout for host2 should return Conn2
    ?assertEqual({ok, Conn2}, hackney_pool:checkout_h3("host2.example.com", 443, hackney_ssl, [])),

    %% They should be different
    ?assertNotEqual(Conn1, Conn2),

    %% Cleanup
    Conn1 ! stop,
    Conn2 ! stop.
