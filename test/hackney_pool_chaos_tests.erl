%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc Randomized pool chaos: real requests against a real server while the
%%% transport misbehaves underneath and callers die at the wrong moment.
%%%
%%% {@link hackney_pool_fault_tests} pins one fault at a time. This one runs
%%% them concurrently and in unpredictable order, which is where the
%%% interesting failures live: a fault arriving between two steps of a checkout,
%%% a connection dying while another caller is queued behind it, a caller
%%% vanishing mid-request. The assertions are not about any single request
%%% succeeding - under injected faults many will not - but about what must hold
%%% once the storm passes:
%%%
%%% <ul>
%%%   <li>the pool is the same process it was when the run started</li>
%%%   <li>nothing is checked out, queued, or left running</li>
%%%   <li>the pool serves normal traffic again</li>
%%% </ul>
%%%
%%% Scale it up for a soak run:
%%%
%%% ```
%%% HACKNEY_CHAOS_WORKERS=64 HACKNEY_CHAOS_ROUNDS=200 rebar3 eunit \
%%%     --module=hackney_pool_chaos_tests
%%% '''
%%%
%%% Every worker seeds its own generator from `HACKNEY_CHAOS_SEED' (default 42)
%%% so a failing run can be replayed with the seed printed in the output.
-module(hackney_pool_chaos_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, chaos_test_pool).
-define(HOST, "127.0.0.1").
-define(PORT, 8142).

%%====================================================================
%% Fixture
%%====================================================================

chaos_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"the pool survives a randomized fault storm and serves again",
       {timeout, 300, fun t_chaos/0}}]}.

setup() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(chaos_test_server, [{port, ?PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    ok = hackney_fault_transport:clear(),
    ok = hackney_crash_sentinel:start(),
    %% Deliberately smaller than the worker count: most rounds dial a fresh
    %% overflow connection, which is where connect faults land.
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 2}, {prewarm_count, 0}]),
    ok.

teardown(_) ->
    ok = hackney_fault_transport:clear(),
    _ = hackney_crash_sentinel:stop(),
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    _ = (try cowboy:stop_listener(chaos_test_server) catch _:_ -> ok end),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

%%====================================================================
%% The run
%%====================================================================

t_chaos() ->
    Pool = hackney_pool:find_pool(?POOL),
    %% Other suites in the same VM leave connections running: "nothing leaked"
    %% is judged against what was already there.
    Baseline = conn_count(),
    Workers = env(<<"HACKNEY_CHAOS_WORKERS">>, 12),
    Rounds = env(<<"HACKNEY_CHAOS_ROUNDS">>, 400),
    Seed = env(<<"HACKNEY_CHAOS_SEED">>, 42),
    ?debugFmt("chaos: ~b workers x ~b rounds, seed ~b", [Workers, Rounds, Seed]),

    Monkey = spawn_link(fun() -> monkey() end),
    Parent = self(),
    Pids = [spawn_link(fun() -> worker(Parent, Seed + N, Rounds) end)
            || N <- lists:seq(1, Workers)],
    Tally = await(Pids, #{}, deadline(120000)),
    Monkey ! stop,
    ok = hackney_fault_transport:clear(),
    ?debugFmt("chaos: ~b dials, ~b sends, outcomes ~p",
              [hackney_fault_transport:calls(connect),
               hackney_fault_transport:calls(send),
               maps:to_list(Tally)]),

    %% The pool never died and nothing is left behind.
    ?assertEqual(Pool, hackney_pool:find_pool(?POOL)),
    ?assert(is_process_alive(Pool)),
    hackney_crash_sentinel:assert_no_crash_from(Pool),
    Stats = wait_until(fun() ->
                              S = hackney_pool:get_stats(?POOL),
                              settled(S, Baseline) andalso S
                      end, 10000),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(0, proplists:get_value(queue_count, Stats)),

    %% The run actually exercised both sides: requests got through between
    %% fault windows, and dials failed while faults were armed. Without these
    %% the suite would still pass if every checkout started failing, or if the
    %% faults stopped reaching the code under test.
    ?assert(maps:get({status, 200}, Tally, 0) > 0),
    ?assert(checkout_errors(Tally) > 0),

    %% And it still works. A pool that survives by wedging is no better than a
    %% pool that died.
    [?assertEqual(ok, one_request()) || _ <- lists:seq(1, 10)],
    ok.

%%====================================================================
%% Workers
%%====================================================================

%% One round: take a connection, maybe use it, then give it back the polite
%% way, the rude way, or not at all.
worker(Parent, Seed, Rounds) ->
    rand:seed(exsss, {Seed, Seed * 7, Seed * 13}),
    Tally = lists:foldl(fun(_, Acc) -> count(round_trip(), Acc) end,
                        #{}, lists:seq(1, Rounds)),
    Parent ! {done, self(), Tally}.

round_trip() ->
    Opts = [{pool, ?POOL}, {connect_timeout, 200}, {checkout_timeout, 5000}],
    case hackney_pool:checkout(?HOST, ?PORT, hackney_fault_transport, Opts) of
        {ok, PoolInfo, Pid} ->
            Outcome = use(Pid),
            release(PoolInfo, Pid),
            Outcome;
        {error, Reason} ->
            {checkout_error, Reason}
    end.

use(Pid) ->
    case rand:uniform(10) of
        1 ->
            %% Checked out and abandoned without a request.
            idle;
        _ ->
            try hackney_conn:request(Pid, <<"GET">>, <<"/get">>, [], <<>>) of
                {ok, Status, _Headers} -> {status, Status};
                {ok, Status, _Headers, _Body} -> {status, Status};
                {error, Reason} -> {request_error, Reason}
            catch
                _:_ -> request_exit
            end
    end.

release(PoolInfo, Pid) ->
    case rand:uniform(10) of
        1 ->
            %% The connection dies with the caller holding it.
            exit(Pid, kill);
        2 ->
            %% The caller never checks in: the pool learns through its monitor.
            ok;
        _ ->
            _ = (try hackney_pool:checkin(PoolInfo, Pid) catch _:_ -> ok end),
            ok
    end.

%%====================================================================
%% Chaos monkey
%%====================================================================

%% Arms one fault at a time, holds it briefly, clears it, moves to the next.
%% It rotates through the whole matrix instead of picking at random: with a
%% fixed seed a random monkey reproducibly skips whole callbacks, and a run
%% that never arms a connect fault never tests the path issue #927 lived on.
%% The chaos comes from where the faults land relative to concurrent callers,
%% not from the order they are armed in.
monkey() ->
    monkey_loop(matrix()).

matrix() ->
    [{Callback, Fault}
     || Callback <- [connect, send, recv, close, setopts],
        Fault <- [{slow_error, 50}, {sleep, 30}, crash,
                  {error, econnrefused}, {error, closed}, {hang, 300}]].

monkey_loop([]) ->
    monkey_loop(matrix());
monkey_loop([{Callback, Fault} | Rest]) ->
    hackney_fault_transport:set(Callback, Fault),
    receive stop -> ok
    after 15 ->
        hackney_fault_transport:clear(Callback),
        %% The quiet gaps are half the test: they are when the pool has to
        %% recover and serve normally again.
        receive stop -> ok
        after 25 -> monkey_loop(Rest)
        end
    end.

%%====================================================================
%% Helpers
%%====================================================================

one_request() ->
    Opts = [{pool, ?POOL}, {connect_timeout, 5000}, {checkout_timeout, 5000}],
    case hackney_pool:checkout(?HOST, ?PORT, hackney_fault_transport, Opts) of
        {ok, PoolInfo, Pid} ->
            Result = hackney_conn:request(Pid, <<"GET">>, <<"/get">>, [], <<>>),
            ok = hackney_pool:checkin(PoolInfo, Pid),
            case Result of
                {ok, 200, _} -> ok;
                {ok, 200, _, _} -> ok;
                Other -> {unexpected, Other}
            end;
        Error ->
            {checkout_failed, Error}
    end.

await([], Tally, _Deadline) ->
    Tally;
await(Pids, Tally, Deadline) ->
    Left = Deadline - erlang:monotonic_time(millisecond),
    ?assert(Left > 0),
    receive
        {done, Pid, WorkerTally} ->
            await(Pids -- [Pid], merge(Tally, WorkerTally), Deadline)
    after Left ->
        erlang:error({chaos_workers_stuck, length(Pids)})
    end.

settled(Stats, Baseline) ->
    proplists:get_value(in_use_count, Stats) =:= 0 andalso
        proplists:get_value(queue_count, Stats) =:= 0 andalso
        conn_count() =< Baseline + proplists:get_value(free_count, Stats).

conn_count() ->
    case whereis(hackney_conn_sup) of
        undefined -> 0;
        _ -> proplists:get_value(active, supervisor:count_children(hackney_conn_sup))
    end.

checkout_errors(Tally) ->
    maps:fold(fun({checkout_error, _}, N, Acc) -> Acc + N;
                 (_, _, Acc) -> Acc
              end, 0, Tally).

count(Outcome, Tally) ->
    Key = case Outcome of
              {status, Status} -> {status, Status};
              {checkout_error, Reason} -> {checkout_error, class(Reason)};
              {request_error, Reason} -> {request_error, class(Reason)};
              Other -> Other
          end,
    maps:update_with(Key, fun(N) -> N + 1 end, 1, Tally).

%% Keep the tally readable: crash reasons carry stacktraces.
class(Reason) when is_atom(Reason) -> Reason;
class({Reason, _}) when is_atom(Reason) -> Reason;
class(_) -> other.

merge(A, B) ->
    maps:fold(fun(K, V, Acc) -> maps:update_with(K, fun(N) -> N + V end, V, Acc) end,
              A, B).

deadline(Ms) -> erlang:monotonic_time(millisecond) + Ms.

env(Name, Default) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        Value -> list_to_integer(Value)
    end.

wait_until(Fun, Timeout) ->
    poll(Fun, deadline(Timeout)).

poll(Fun, Deadline) ->
    case Fun() of
        false ->
            ?assert(erlang:monotonic_time(millisecond) < Deadline),
            timer:sleep(50),
            poll(Fun, Deadline);
        Value ->
            Value
    end.
