%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Concurrent same-URI pool stress harness.
%%%
%%% Hammers a single URI from many workers through a small warm pool and checks
%%% that every request returns a 2xx. The warm pool is deliberately small; the
%%% burst is served by overflow connections, so this exercises the checkin
%%% keepalive/readiness gate and the checkout path under contention.
%%%
%%% Standalone (starts its own cowboy server + pool):
%%%   rebar3 as test shell
%%%   stress_pool_concurrency:run().
%%%   stress_pool_concurrency:run(#{workers => 200, reqs => 50, pool_size => 4}).
%%%
%%% Against an already-running server/pool (used by the eunit stress suite):
%%%   stress_pool_concurrency:hammer(Pool, Url, Workers, Reqs).
-module(stress_pool_concurrency).

-export([run/0, run/1, hammer/4]).

-define(DEFAULT_PORT, 8138).
-define(SERVER_REF, stress_pool_concurrency_server).
-define(POOL, stress_pool_concurrency_pool).

%% @doc Standalone stress run with default settings.
run() -> run(#{}).

%% @doc Standalone stress run. Starts a local cowboy server and a small pool,
%% hammers each scenario URI concurrently, then tears everything down. Returns
%% `ok' only if every request returned a 2xx, else `{error, Errors}'.
%% Opts: workers (100), reqs (20), pool_size (4), port (8138),
%%       scenarios ([<<"/get">>, <<"/connection-close">>, <<"/maybe-close">>]).
run(Opts) when is_map(Opts) ->
    Workers = maps:get(workers, Opts, 100),
    Reqs = maps:get(reqs, Opts, 20),
    PoolSize = maps:get(pool_size, Opts, 4),
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    Scenarios = maps:get(scenarios, Opts,
                         [<<"/get">>, <<"/connection-close">>, <<"/maybe-close">>]),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(?SERVER_REF, [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    ok = hackney_pool:start_pool(?POOL, [{pool_size, PoolSize}, {prewarm_count, 0}]),
    try
        Results = [{Path, hammer(?POOL, url(Port, Path), Workers, Reqs)}
                   || Path <- Scenarios],
        report(Results)
    after
        try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end,
        try cowboy:stop_listener(?SERVER_REF) catch _:_ -> ok end
    end.

%% @doc Spawn `Workers' processes, each making `Reqs' sequential requests to
%% `Url' through `Pool'. Returns a summary `#{ok => N, errors => [term()]}'.
%% A worker never crashes the run: any exception is captured as an error entry.
hammer(Pool, Url, Workers, Reqs) ->
    Parent = self(),
    _ = [spawn(fun() -> Parent ! {worker_done, worker(Pool, Url, Reqs)} end)
         || _ <- lists:seq(1, Workers)],
    gather(Workers, #{ok => 0, errors => []}).

%%====================================================================
%% Internal
%%====================================================================

worker(Pool, Url, Reqs) ->
    %% Generous concurrency cap and timeouts: the warm pool is small on purpose,
    %% but the per-host concurrency cap and checkout/recv timeouts must not be
    %% what makes a request fail - a healthy run returns ok for every request.
    ReqOpts = [{pool, Pool},
               {max_per_host, 256},
               {checkout_timeout, 30000},
               {connect_timeout, 30000},
               {recv_timeout, 30000}],
    lists:foldl(
      fun(_, Acc) ->
          Res = try hackney:request(get, Url, [], <<>>, ReqOpts)
                catch Class:Reason -> {caught, Class, Reason} end,
          classify(Res, Acc)
      end, #{ok => 0, errors => []}, lists:seq(1, Reqs)).

classify({ok, Status, _H, _B}, Acc) when Status >= 200, Status < 300 ->
    bump_ok(Acc);
classify(Other, Acc) ->
    add_err(Acc, Other).

gather(0, Acc) ->
    Acc;
gather(N, Acc) ->
    receive
        {worker_done, R} -> gather(N - 1, merge_summary(Acc, R))
    after 120000 ->
        add_err(Acc, {timeout_waiting_for_workers, N})
    end.

bump_ok(#{ok := N} = A) -> A#{ok := N + 1}.

add_err(#{errors := Es} = A, E) -> A#{errors := [E | Es]}.

merge_summary(#{ok := O1, errors := E1}, #{ok := O2, errors := E2}) ->
    #{ok => O1 + O2, errors => E1 ++ E2}.

url(Port, Path) ->
    iolist_to_binary([<<"http://127.0.0.1:">>, integer_to_list(Port), Path]).

report(Results) ->
    Total = lists:sum([summary_total(S) || {_, S} <- Results]),
    Errors = lists:append([[{Path, E} || E <- maps:get(errors, S)]
                           || {Path, S} <- Results]),
    io:format("stress: ~p requests across ~p scenarios, ~p errors~n",
              [Total, length(Results), length(Errors)]),
    case Errors of
        [] -> ok;
        _ -> {error, lists:sublist(Errors, 20)}
    end.

summary_total(#{ok := Ok, errors := Es}) -> Ok + length(Es).
