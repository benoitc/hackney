#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin
%%
%%
%%
%%
-define(MAX, 100).

loop(Ref, WaitPid) ->
    receive
        {hackney_response, Ref, {status, _StatusInt, _Reason}} ->
            loop(Ref, WaitPid);
        {hackney_response, Ref, {headers, _Headers}} ->
            loop(Ref, WaitPid);
        {hackney_response, Ref, done} ->
            WaitPid ! ack;
        {hackney_response, Ref, _Bin} ->
            loop(Ref, WaitPid);
        _Else ->
            WaitPid ! ack
    end.

wait_request() ->
    receive
        start -> ok
    end.

request(WaitPid) ->
    wait_request(),
    Url = <<"https://friendpaste.com/_all_languages">>,
    Opts = [async],
    {ok, Ref} = hackney:get(Url, [], <<>>, Opts),
    loop(Ref, WaitPid).

wait(100) ->
    io:format("~n", []),
    ok;
wait(N) ->
    receive
        ack ->
            io:format(".", []),
            wait(N+1)
    after 15000 ->
            PoolSize = hackney_pool:count(default),
            io:format("~ntimeout ~p/~p done. ~p in the pool.~n",
                      [N, ?MAX, PoolSize])
    end.


main(_) ->
    hackney:start(),
    hackney_pool:start_pool(default, []),
    io:format("processing ", []),
    Self = self(),
    Pids = lists:foldr(fun(_, Acc) ->
                    Pid = spawn(fun() -> timer:sleep(100), request(Self) end),
                    [Pid | Acc]
            end, [], lists:seq(1, ?MAX)),

    [Pid ! start || Pid <- Pids],
    wait(0),
    io:format("manager info ~p~n", [ets:info(hackney_manager)]).
