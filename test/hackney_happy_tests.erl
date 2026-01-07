%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_happy_tests).
-include_lib("eunit/include/eunit.hrl").

-behaviour(ranch_protocol).
-export([start_link/4]).

%% Ranch protocol - just accept and hold connection open
start_link(Ref, _Socket, Transport, _Opts) ->
    Pid = spawn_link(fun() -> init(Ref, Transport) end),
    {ok, Pid}.

init(Ref, Transport) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Transport, Socket).

loop(Transport, Socket) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, _Data} -> loop(Transport, Socket);
        {error, closed} -> ok;
        {error, _} -> ok
    end.

%% Tests for connect/3 and connect/4 using the Happy Eyeballs algorithm

connect_ipv4_tuple_test_() ->
    {setup,
     fun() -> start_ranch(inet) end,
     fun(Ref) -> stop_ranch(Ref) end,
     fun(Ref) ->
         Port = ranch:get_port(Ref),
         [{"connect to IPv4 tuple address",
           fun() ->
               Result = hackney_happy:connect({127,0,0,1}, Port, []),
               ?assertMatch({ok, _Socket}, Result),
               {ok, Socket} = Result,
               gen_tcp:close(Socket)
           end}]
     end}.

connect_ipv6_tuple_test_() ->
    {setup,
     fun() ->
         case start_ranch(inet6) of
             {error, _} -> skip;
             Ref -> {ok, Ref}
         end
     end,
     fun(skip) -> ok;
        ({ok, Ref}) -> stop_ranch(Ref)
     end,
     fun(skip) ->
         [{"skip - IPv6 not available", fun() -> ok end}];
        ({ok, Ref}) ->
         Port = ranch:get_port(Ref),
         [{"connect to IPv6 tuple address",
           fun() ->
               Result = hackney_happy:connect({0,0,0,0,0,0,0,1}, Port, []),
               ?assertMatch({ok, _Socket}, Result),
               {ok, Socket} = Result,
               gen_tcp:close(Socket)
           end}]
     end}.

connect_localhost_test_() ->
    {setup,
     fun() -> start_ranch_dual_stack() end,
     fun({Ref4, Ref6Opt}) ->
         stop_ranch(Ref4),
         case Ref6Opt of
             {ok, Ref6} -> stop_ranch(Ref6);
             _ -> ok
         end
     end,
     fun({Ref4, _Ref6Opt}) ->
         Port = ranch:get_port(Ref4),
         [{"connect to localhost string",
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
           end}]
     end}.

connect_with_timeout_test_() ->
    {setup,
     fun() -> start_ranch(inet) end,
     fun(Ref) -> stop_ranch(Ref) end,
     fun(Ref) ->
         Port = ranch:get_port(Ref),
         [{"connect with explicit timeout",
           fun() ->
               Result = hackney_happy:connect("127.0.0.1", Port, [], 5000),
               ?assertMatch({ok, _Socket}, Result),
               {ok, Socket} = Result,
               gen_tcp:close(Socket)
           end}]
     end}.

connect_failure_test_() ->
    [{"connect to invalid port fails",
      fun() ->
          Result = hackney_happy:connect("127.0.0.1", 1, [], 100),
          ?assertMatch({error, _}, Result)
      end},
     {"connect to non-existent domain fails",
      fun() ->
          Result = hackney_happy:connect("this.domain.does.not.exist.invalid", 80, [], 100),
          ?assertMatch({error, _}, Result)
      end}].

connect_ipv4_string_test_() ->
    {setup,
     fun() -> start_ranch(inet) end,
     fun(Ref) -> stop_ranch(Ref) end,
     fun(Ref) ->
         Port = ranch:get_port(Ref),
         [{"connect to IPv4 string address",
           fun() ->
               Result = hackney_happy:connect("127.0.0.1", Port, []),
               ?assertMatch({ok, _Socket}, Result),
               {ok, Socket} = Result,
               gen_tcp:close(Socket)
           end}]
     end}.

connect_binary_address_test_() ->
    {setup,
     fun() -> start_ranch(inet) end,
     fun(Ref) -> stop_ranch(Ref) end,
     fun(Ref) ->
         Port = ranch:get_port(Ref),
         [{"connect with binary IPv4 address",
           fun() ->
               Result = hackney_happy:connect(<<"127.0.0.1">>, Port, []),
               ?assertMatch({ok, _Socket}, Result),
               {ok, Socket} = Result,
               gen_tcp:close(Socket)
           end}]
     end}.

connect_bracketed_ipv6_test_() ->
    {setup,
     fun() ->
         case start_ranch(inet6) of
             {error, _} -> skip;
             Ref -> {ok, Ref}
         end
     end,
     fun(skip) -> ok;
        ({ok, Ref}) -> stop_ranch(Ref)
     end,
     fun(skip) ->
         [{"skip - IPv6 not available", fun() -> ok end}];
        ({ok, Ref}) ->
         Port = ranch:get_port(Ref),
         [{"connect with bracketed IPv6 address",
           fun() ->
               Result = hackney_happy:connect("[::1]", Port, []),
               ?assertMatch({ok, _Socket}, Result),
               {ok, Socket} = Result,
               gen_tcp:close(Socket)
           end}]
     end}.

%% Helper functions

ensure_ranch_started() ->
    case application:ensure_all_started(ranch) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

start_ranch(inet) ->
    ensure_ranch_started(),
    Ref = make_ref(),
    case ranch:start_listener(Ref, ranch_tcp, #{socket_opts => [{ip, {127,0,0,1}}, {port, 0}]},
                              ?MODULE, []) of
        {ok, _} -> Ref;
        {error, _} = Err -> Err
    end;
start_ranch(inet6) ->
    ensure_ranch_started(),
    Ref = make_ref(),
    case ranch:start_listener(Ref, ranch_tcp, #{socket_opts => [inet6, {ip, {0,0,0,0,0,0,0,1}}, {port, 0}]},
                              ?MODULE, []) of
        {ok, _} -> Ref;
        {error, _} = Err -> Err
    end.

start_ranch_dual_stack() ->
    Ref4 = start_ranch(inet),
    Port = ranch:get_port(Ref4),
    %% Try to start IPv6 on same port
    Ref6 = make_ref(),
    Ref6Result = case ranch:start_listener(Ref6, ranch_tcp,
                                           #{socket_opts => [inet6, {ip, {0,0,0,0,0,0,0,1}}, {port, Port}]},
                                           ?MODULE, []) of
        {ok, _} -> {ok, Ref6};
        {error, Reason} -> {error, Reason}
    end,
    {Ref4, Ref6Result}.

stop_ranch(Ref) ->
    ranch:stop_listener(Ref).
