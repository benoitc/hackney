%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_happy_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for connect/3 and connect/4 using the Happy Eyeballs algorithm
%% These tests verify connection behavior with various address formats

%% connect/3 with IPv4 tuple address
connect_ipv4_tuple_test_() ->
    {setup,
     fun() -> start_test_server(inet) end,
     fun(Port) -> stop_test_server(Port) end,
     fun(Port) ->
         [
            {"connect to IPv4 tuple address",
             fun() ->
                 Result = hackney_happy:connect({127,0,0,1}, Port, []),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end}
         ]
     end}.

%% connect/3 with IPv6 tuple address (loopback)
connect_ipv6_tuple_test_() ->
    {setup,
     fun() ->
         case start_test_server(inet6) of
             Port when is_integer(Port) -> {ok, Port};
             {error, _} -> skip  %% IPv6 may not be available
         end
     end,
     fun(skip) -> ok;
        ({ok, Port}) -> stop_test_server(Port)
     end,
     fun(skip) ->
         [{"skip - IPv6 not available", fun() -> ok end}];
        ({ok, Port}) ->
         [
            {"connect to IPv6 tuple address",
             fun() ->
                 Result = hackney_happy:connect({0,0,0,0,0,0,0,1}, Port, []),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end}
         ]
     end}.

%% connect/3 with hostname string - localhost
%% Note: "localhost" may resolve to IPv6 (::1) or IPv4 (127.0.0.1) depending on
%% the system configuration. We start servers on both to handle either case.
connect_localhost_test_() ->
    {setup,
     fun() -> start_test_server_dual_stack() end,
     fun({Port4, Port6Opt}) ->
         stop_test_server(Port4),
         case Port6Opt of
             {ok, Port6} -> stop_test_server(Port6);
             _ -> ok
         end
     end,
     fun({Port, _Port6Opt}) ->
         [
            {"connect to localhost string",
             fun() ->
                 Result = hackney_happy:connect("localhost", Port, []),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end},
            {"connect to 127.0.0.1 string",
             fun() ->
                 Result = hackney_happy:connect("127.0.0.1", Port, []),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end}
         ]
     end}.

%% connect/4 with timeout
connect_with_timeout_test_() ->
    {setup,
     fun() -> start_test_server(inet) end,
     fun(Port) -> stop_test_server(Port) end,
     fun(Port) ->
         [
            {"connect with explicit timeout",
             fun() ->
                 Result = hackney_happy:connect("127.0.0.1", Port, [], 5000),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end}
         ]
     end}.

%% connect to non-existent host
connect_failure_test_() ->
    [
        {"connect to invalid port fails",
         fun() ->
             %% Port 0 is not valid, should fail
             Result = hackney_happy:connect("127.0.0.1", 1, [], 100),
             ?assertMatch({error, _}, Result)
         end},
        {"connect to non-existent domain fails",
         fun() ->
             %% Non-existent domain should return nxdomain
             Result = hackney_happy:connect("this.domain.does.not.exist.invalid", 80, [], 100),
             ?assertMatch({error, _}, Result)
         end}
    ].

%% connect with IPv4 string address
connect_ipv4_string_test_() ->
    {setup,
     fun() -> start_test_server(inet) end,
     fun(Port) -> stop_test_server(Port) end,
     fun(Port) ->
         [
            {"connect to IPv4 string address",
             fun() ->
                 Result = hackney_happy:connect("127.0.0.1", Port, []),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end}
         ]
     end}.

%% connect with binary address
connect_binary_address_test_() ->
    {setup,
     fun() -> start_test_server(inet) end,
     fun(Port) -> stop_test_server(Port) end,
     fun(Port) ->
         [
            {"connect with binary IPv4 address",
             fun() ->
                 Result = hackney_happy:connect(<<"127.0.0.1">>, Port, []),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end}
         ]
     end}.

%% connect with bracketed IPv6 address
connect_bracketed_ipv6_test_() ->
    {setup,
     fun() ->
         case start_test_server(inet6) of
             Port when is_integer(Port) -> {ok, Port};
             {error, _} -> skip
         end
     end,
     fun(skip) -> ok;
        ({ok, Port}) -> stop_test_server(Port)
     end,
     fun(skip) ->
         [{"skip - IPv6 not available", fun() -> ok end}];
        ({ok, Port}) ->
         [
            {"connect with bracketed IPv6 address",
             fun() ->
                 Result = hackney_happy:connect("[::1]", Port, []),
                 ?assertMatch({ok, _Socket}, Result),
                 {ok, Socket} = Result,
                 gen_tcp:close(Socket)
             end}
         ]
     end}.

%% Helper functions

start_test_server(Family) ->
    Opts = [Family, binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(0, Opts) of
        {ok, LSock} ->
            {ok, Port} = inet:port(LSock),
            %% Start an acceptor process
            spawn(fun() -> accept_loop(LSock) end),
            Port;
        {error, Reason} ->
            {error, Reason}
    end.

%% Start test servers on both IPv4 and IPv6 (same port) for localhost tests
%% Returns {Port, IPv6Result} where IPv6Result is {ok, Port} or {error, Reason}
start_test_server_dual_stack() ->
    %% Start IPv4 server first to get a port
    Port = start_test_server(inet),
    %% Try to start IPv6 server on the same port
    Opts6 = [inet6, binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    Port6Result = case gen_tcp:listen(Port, Opts6) of
        {ok, LSock6} ->
            spawn(fun() -> accept_loop(LSock6) end),
            {ok, Port};
        {error, Reason} ->
            {error, Reason}
    end,
    {Port, Port6Result}.

stop_test_server(_Port) ->
    %% The server will stop when the socket is closed
    ok.

accept_loop(LSock) ->
    case gen_tcp:accept(LSock, 1000) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            accept_loop(LSock);
        {error, timeout} ->
            accept_loop(LSock);
        {error, closed} ->
            ok;
        {error, _} ->
            ok
    end.
