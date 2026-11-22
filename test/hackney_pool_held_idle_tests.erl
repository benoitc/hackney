%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Keepalive timer of a pooled HTTP/1.1 conn taken with hackney:connect/2
%%% (#938). The timer is for conns idle in the pool. A fresh conn the caller
%%% just dialed used to start it at once, so one left unused for longer than
%%% keepalive_timeout was closed under its caller, while a reused one (its
%%% timer cancelled at checkout, #934) was not. A conn given back unused, in
%%% turn, sat in the pool with no timer at all.
-module(hackney_pool_held_idle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEEPALIVE, 200).
-define(WAIT, 5000).

held_idle_test_() ->
    {setup,
     fun() -> _ = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{timeout, 30, {Title, fun() -> with_pool(Fun) end}} || {Title, Fun} <- [
        {"a fresh conn waits for its first request", fun fresh_waits_for_request/2},
        {"a fresh conn given back unused ages out of the pool", fun fresh_given_back/2},
        {"a reused conn given back unused ages out of the pool", fun reused_given_back/2}]]}.

%%====================================================================
%% Tests
%%====================================================================

fresh_waits_for_request(Srv, Opts) ->
    {ok, Conn} = hackney:connect(url(Srv), Opts),
    receive after ?KEEPALIVE * 3 -> ok end,
    ?assertMatch({ok, 200, _, Conn},
                 hackney:send_request(Conn, {get, <<"/">>, [], <<>>})),
    ?assertEqual({ok, <<"ok">>}, hackney:body(Conn)),
    ok = hackney:close(Conn).

fresh_given_back(Srv, Opts) ->
    {ok, Conn} = hackney:connect(url(Srv), Opts),
    given_back_ages_out(Conn, Opts).

reused_given_back(Srv, Opts) ->
    {ok, 200, _, <<"ok">>} = hackney:get(url(Srv), [], <<>>, Opts),
    ?assertEqual(1, free_count(Opts)),
    {ok, Conn} = hackney:connect(url(Srv), Opts),
    %% The pooled conn was handed out, no new socket was dialed.
    ?assertEqual(0, free_count(Opts)),
    ?assertEqual(1, accepted(Srv)),
    given_back_ages_out(Conn, Opts).

%%====================================================================
%% Helpers
%%====================================================================

%% close/1 gives the unused conn back to the pool, which then closes it once
%% it has been idle for keepalive_timeout.
given_back_ages_out(Conn, Opts) ->
    MRef = erlang:monitor(process, Conn),
    ok = hackney:close(Conn),
    ?assertEqual(1, free_count(Opts)),
    receive {'DOWN', MRef, process, Conn, _} -> ok
    after ?WAIT -> error(conn_never_aged_out)
    end,
    ok = wait_until(fun() -> free_count(Opts) =:= 0 end).

free_count(Opts) ->
    Stats = hackney_pool:get_stats(proplists:get_value(pool, Opts)),
    proplists:get_value(free_count, Stats).

wait_until(Fun) ->
    wait_until(Fun, 100).

wait_until(_Fun, 0) ->
    timeout;
wait_until(Fun, N) ->
    case Fun() of
        true -> ok;
        false -> receive after 20 -> wait_until(Fun, N - 1) end
    end.

with_pool(Fun) ->
    Pool = list_to_atom("hackney_held_idle_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = hackney_pool:start_pool(Pool, [{keepalive_timeout, ?KEEPALIVE}]),
    Srv = start_server(),
    try
        Fun(Srv, [{pool, Pool}])
    after
        _ = hackney_pool:stop_pool(Pool),
        stop_server(Srv)
    end.

url(#{port := Port}) ->
    iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/"]).

%%====================================================================
%% Keep-alive HTTP/1.1 server: answers every request with "ok" and counts
%% the sockets it accepted.
%%====================================================================

start_server() ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(L),
    Counter = counters:new(1, []),
    Acceptor = spawn(fun() -> receive go -> accept_loop(L, Counter) end end),
    ok = gen_tcp:controlling_process(L, Acceptor),
    Acceptor ! go,
    #{port => Port, acceptor => Acceptor, listen => L, counter => Counter}.

stop_server(#{acceptor := Acceptor, listen := L}) ->
    exit(Acceptor, kill),
    _ = gen_tcp:close(L),
    ok.

accepted(#{counter := Counter}) ->
    counters:get(Counter, 1).

accept_loop(L, Counter) ->
    case gen_tcp:accept(L) of
        {ok, S} ->
            counters:add(Counter, 1, 1),
            Pid = spawn(fun() -> receive go -> serve(S, <<>>) end end),
            ok = gen_tcp:controlling_process(S, Pid),
            Pid ! go,
            accept_loop(L, Counter);
        {error, _} ->
            ok
    end.

serve(S, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        nomatch ->
            case gen_tcp:recv(S, 0) of
                {ok, Data} -> serve(S, <<Acc/binary, Data/binary>>);
                {error, _} -> ok
            end;
        {Pos, 4} ->
            ok = gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok">>),
            serve(S, binary:part(Acc, Pos + 4, byte_size(Acc) - Pos - 4))
    end.
