#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin
%%
%%
%%
pool_size() ->
    hackney_pool:count(default).

log(Started, Connected, Closed, PoolSize) ->
    io:format("started: ~p - connected: ~p - closed: ~p - pool:  ~p~n",
              [Started, Connected, Closed, PoolSize]).


request(Collector, I) ->
    Method = get,
    Url = <<"https://github.com/benoitc">>,
    Options = [{pool, default}],
    Collector ! {I, pool_size(), started},
    {ok, _, _Headers, Ref} = hackney:request(Method, Url, [], <<>>, Options),
    io:format("~p connected~n", [I]),
    Collector ! {I, pool_size(), connected},
    case hackney:skip_body(Ref) of
        ok -> ok;
        Error ->
                io:format("got an error ~p~n", [Error])
    end,
    io:format("~p: body read~n", [I]),
    Collector ! {I, pool_size(), closed}.

collect(Parent, _, _, Closed, _) when Closed >= 100 ->
    Parent ! done;
collect(Parent, Started, Connected, Closed, Hist) ->
    receive
        {Request, PoolSize, connected} ->
            log(Started, Connected, Closed, PoolSize),
            io:format("~p connected~n", [Request]),
            collect(Parent, Started, Connected+1, Closed, Hist);
        {Request, PoolSize, started} ->
            log(Started, Connected, Closed, PoolSize),
            io:format("request ~p started~n", [Request]),
            collect(Parent, Started + 1, Connected, Closed, dict:store(Request, PoolSize, Hist));
        {Request, PoolSize, closed} ->
            log(Started, Connected, Closed, PoolSize),
            io:format("request ~p closed~n", [Request]),
            collect(Parent, Started, Connected,  Closed + 1, dict:erase(Request, Hist))
    after 15000 ->
            Parent ! {timeout, {Started, Connected, Closed, dict:to_list(Hist)}}
    end.

main(_) ->
    hackney:start(),
    hackney_pool:start_pool(default, []),
    io:format("Type ctrl-C to quit.", []),
    Self = self(),
    Collector = spawn(fun() -> collect(Self, 0, 0, 0, dict:new()) end),
    lists:foreach(fun(I) ->
                spawn(fun() -> request(Collector, I) end)
        end, lists:seq(1, 100)),
    %% just wait
    receive
        done ->
            ok;
        {timeout, {Started, Connected, Closed, Hist}} ->
            io:format("Timeout: started: ~p - connected: ~p " ++
                      "-closed: ~p pool: ~p~n",
                      [Started, Connected, Closed, pool_size()]),
            io:format("waiting requests:~n", []),
            lists:foreach(fun({Request, PoolSize}) ->
                        io:format("~p: ~p~n", [Request, PoolSize])
                end, Hist)
    end.
