%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/2 connection pooling in hackney_pool.

-module(hackney_pool_h2_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

cleanup(_) ->
    hackney_pool:unregister_h2_all(),
    hackney_conn_sup:stop_all(),
    ok.

%%====================================================================
%% HTTP/2 Pool Tests
%%====================================================================

h2_pool_test_() ->
    {
        "HTTP/2 pool tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"checkout_h2 returns none when no connection", fun test_h2_checkout_none/0},
                {"register_h2 and checkout_h2", fun test_h2_register_checkout/0},
                {"unregister_h2 removes connection", fun test_h2_unregister/0},
                {"connection death cleans h2_connections", fun test_h2_connection_death/0}
            ]
        }
    }.

test_h2_checkout_none() ->
    %% Without any registered H2 connection, checkout should return none
    Result = hackney_pool:checkout_h2("test.example.com", 443, hackney_ssl, []),
    ?assertEqual(none, Result).

test_h2_register_checkout() ->
    %% Start a dummy process to act as a connection
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it as an H2 connection
    ok = hackney_pool:register_h2("h2test.example.com", 443, hackney_ssl, DummyConn, []),

    %% Wait a bit for the async cast to process
    timer:sleep(50),

    %% Checkout should now return the connection
    Result = hackney_pool:checkout_h2("h2test.example.com", 443, hackney_ssl, []),
    ?assertEqual({ok, DummyConn}, Result),

    %% Cleanup
    DummyConn ! stop.

test_h2_unregister() ->
    %% Start a dummy process
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it
    ok = hackney_pool:register_h2("h2unreg.example.com", 443, hackney_ssl, DummyConn, []),
    timer:sleep(50),

    %% Verify it's there
    ?assertEqual({ok, DummyConn}, hackney_pool:checkout_h2("h2unreg.example.com", 443, hackney_ssl, [])),

    %% Unregister
    ok = hackney_pool:unregister_h2(DummyConn, []),
    timer:sleep(50),

    %% Should be gone now
    ?assertEqual(none, hackney_pool:checkout_h2("h2unreg.example.com", 443, hackney_ssl, [])),

    %% Cleanup
    DummyConn ! stop.

test_h2_connection_death() ->
    %% Start a dummy process
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it
    ok = hackney_pool:register_h2("h2death.example.com", 443, hackney_ssl, DummyConn, []),
    timer:sleep(50),

    %% Verify it's there
    ?assertEqual({ok, DummyConn}, hackney_pool:checkout_h2("h2death.example.com", 443, hackney_ssl, [])),

    %% Kill the process
    DummyConn ! stop,
    timer:sleep(100),

    %% Pool should receive DOWN message and clean up - checkout returns none
    ?assertEqual(none, hackney_pool:checkout_h2("h2death.example.com", 443, hackney_ssl, [])).

%%====================================================================
%% Multiplexing Tests
%%====================================================================

h2_multiplexing_test_() ->
    {
        "HTTP/2 multiplexing tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"multiple callers get same connection", fun test_h2_multiplexing/0},
                {"different hosts get different connections", fun test_h2_different_hosts/0}
            ]
        }
    }.

test_h2_multiplexing() ->
    %% Start a dummy connection process
    DummyConn = spawn(fun() -> receive stop -> ok end end),

    %% Register it
    ok = hackney_pool:register_h2("h2mux.example.com", 443, hackney_ssl, DummyConn, []),
    timer:sleep(50),

    %% Multiple checkouts should return the same connection (multiplexing)
    {ok, Conn1} = hackney_pool:checkout_h2("h2mux.example.com", 443, hackney_ssl, []),
    {ok, Conn2} = hackney_pool:checkout_h2("h2mux.example.com", 443, hackney_ssl, []),
    {ok, Conn3} = hackney_pool:checkout_h2("h2mux.example.com", 443, hackney_ssl, []),

    %% All should be the same connection (multiplexed)
    ?assertEqual(DummyConn, Conn1),
    ?assertEqual(DummyConn, Conn2),
    ?assertEqual(DummyConn, Conn3),
    ?assertEqual(Conn1, Conn2),
    ?assertEqual(Conn2, Conn3),

    %% Cleanup
    DummyConn ! stop.

test_h2_different_hosts() ->
    %% Start two dummy connections
    Conn1 = spawn(fun() -> receive stop -> ok end end),
    Conn2 = spawn(fun() -> receive stop -> ok end end),

    %% Register them for different hosts
    ok = hackney_pool:register_h2("host1.example.com", 443, hackney_ssl, Conn1, []),
    ok = hackney_pool:register_h2("host2.example.com", 443, hackney_ssl, Conn2, []),
    timer:sleep(50),

    %% Checkout for host1 should return Conn1
    ?assertEqual({ok, Conn1}, hackney_pool:checkout_h2("host1.example.com", 443, hackney_ssl, [])),

    %% Checkout for host2 should return Conn2
    ?assertEqual({ok, Conn2}, hackney_pool:checkout_h2("host2.example.com", 443, hackney_ssl, [])),

    %% They should be different
    ?assertNotEqual(Conn1, Conn2),

    %% Cleanup
    Conn1 ! stop,
    Conn2 ! stop.
