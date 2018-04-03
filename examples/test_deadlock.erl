#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/hackney/ebin  -pa ../_build/default/lib/*/ebin  -pa ../_build/default/lib/certifi/ebin  -pa ../_build/default/lib/idna/ebin  -pa ../_build/default/lib/metrics/ebin -pa ../_build/default/lib/mimerl/ebin   -pa ../_build/default/lib/ssl_verify_fun/ebin -pa ../_build/default/lib/unicode_util_compat/ebin

-module(test_async).

-export([main/1]).

deadlock(Url, N) ->
  Fun = fun() ->
          {ok, _, _Headers, Ref} = hackney:get(Url, [], <<>>, [{connect_timeout, 1000}, {recv_timeout, 1000}]),
          {ok, _Body} = hackney:body(Ref)
        end,
  Pids = [spawn(
    fun() ->
      spawn_link(Fun),
      timer:sleep(500),
      exit(timeout)
    end) || _I <- lists:seq(1, N)],
  MRefs = [erlang:monitor(process, Pid) || Pid <- Pids],
  wait_pids(MRefs),
  ok.

wait_pids([]) -> ok;
wait_pids(MRefs) ->
  receive
    {'DOWN', MRef, process, _, _} ->
      wait_pids(MRefs -- [MRef])
  end.


main(_) ->
  {ok, _}= application:ensure_all_started(hackney),
  io:format("start test~n", []),
  deadlock("https://httparrot.herokuapp.com/delay/5", 550),
  io:format("pools are ~p~n", [ets:tab2list(hackney_pool)]),
  io:format("state of the pool:~n ~p~n", [hackney_pool:get_stats(default)]),
  ok.
