%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% A SOCKS5 proxy that accepts the connection and then stops answering
%%% must not block the caller past connect_timeout.

-module(hackney_socks5_tests).

-include_lib("eunit/include/eunit.hrl").

socks5_stall_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"stall before the method reply", fun t_stall_greeting/0},
      {"stall before the connect reply", fun t_stall_connect/0},
      {"stall inside the bound address", fun t_stall_bound_addr/0},
      {"stall before the auth reply", fun t_stall_auth/0},
      {"request through a stalled proxy", fun t_stall_request/0},
      {"a full reply still connects", fun t_full_reply/0}]}.

t_stall_greeting() ->
    Port = stall_proxy(fun(_S) -> ok end),
    assert_timeout(fun() -> connect(Port, []) end).

t_stall_connect() ->
    Port = stall_proxy(fun(S) ->
        {ok, _} = gen_tcp:recv(S, 0, 5000),
        ok = gen_tcp:send(S, <<5, 0>>)
    end),
    assert_timeout(fun() -> connect(Port, []) end).

t_stall_bound_addr() ->
    Port = stall_proxy(fun(S) ->
        {ok, _} = gen_tcp:recv(S, 0, 5000),
        ok = gen_tcp:send(S, <<5, 0>>),
        {ok, _} = gen_tcp:recv(S, 0, 5000),
        %% reply header says IPv4, then only part of the address
        ok = gen_tcp:send(S, <<5, 0, 0, 1, 127, 0>>)
    end),
    assert_timeout(fun() -> connect(Port, []) end).

t_stall_auth() ->
    Port = stall_proxy(fun(S) ->
        {ok, _} = gen_tcp:recv(S, 0, 5000),
        ok = gen_tcp:send(S, <<5, 2>>),
        {ok, _} = gen_tcp:recv(S, 0, 5000)
    end),
    assert_timeout(fun() ->
        connect(Port, [{socks5_user, <<"u">>}, {socks5_pass, <<"p">>}])
    end).

t_stall_request() ->
    Port = stall_proxy(fun(_S) -> ok end),
    assert_timeout(fun() ->
        hackney:request(get, <<"http://example.invalid/">>, [], <<>>,
                        [{proxy, {socks5, "127.0.0.1", Port}},
                         {connect_timeout, 300}, {recv_timeout, 300}])
    end).

t_full_reply() ->
    Port = stall_proxy(fun(S) ->
        {ok, _} = gen_tcp:recv(S, 0, 5000),
        ok = gen_tcp:send(S, <<5, 0>>),
        {ok, _} = gen_tcp:recv(S, 0, 5000),
        ok = gen_tcp:send(S, <<5, 0, 0, 1, 127, 0, 0, 1, 0, 80>>)
    end),
    {ok, Sock} = connect(Port, []),
    hackney_socks5:close(Sock).

%% helpers

connect(Port, Extra) ->
    hackney_socks5:connect("example.invalid", 80,
                           [{socks5_host, "127.0.0.1"}, {socks5_port, Port},
                            {socks5_transport, hackney_tcp} | Extra],
                           300).

assert_timeout(Fun) ->
    T0 = erlang:monotonic_time(millisecond),
    ?assertEqual({error, timeout}, Fun()),
    ?assert(erlang:monotonic_time(millisecond) - T0 < 3000).

%% One-shot proxy: accepts a connection, runs Script on it, then holds
%% the socket open without sending anything more.
stall_proxy(Script) ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(L),
    Parent = self(),
    spawn(fun() ->
        {ok, S} = gen_tcp:accept(L, 5000),
        Script(S),
        MRef = erlang:monitor(process, Parent),
        receive {'DOWN', MRef, _, _, _} -> ok
        after 10000 -> ok
        end,
        gen_tcp:close(S),
        gen_tcp:close(L)
    end),
    Port.
