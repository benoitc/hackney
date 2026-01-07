%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024 Benoit Chesneau
%%%
%%% @doc Tests for hackney_conn_sup supervisor.

-module(hackney_conn_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

hackney_conn_sup_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"supervisor starts", fun test_supervisor_starts/0},
      {"start child connection", fun test_start_child/0},
      {"start multiple connections", fun test_start_multiple/0},
      {"stop child connection", fun test_stop_child/0},
      {"child crash is handled", fun test_child_crash/0}
     ]}.

setup() ->
    application:ensure_all_started(hackney),
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_supervisor_starts() ->
    %% hackney_conn_sup is started by hackney application
    ?assert(whereis(hackney_conn_sup) =/= undefined),
    ?assert(is_process_alive(whereis(hackney_conn_sup))).

test_start_child() ->
    Opts = #{
        host => "localhost",
        port => 8000,
        transport => hackney_tcp
    },
    {ok, Pid} = hackney_conn_sup:start_conn(Opts),
    ?assert(is_process_alive(Pid)),

    %% Verify it's a child of the supervisor
    Children = supervisor:which_children(hackney_conn_sup),
    ?assert(lists:any(fun({_, P, _, _}) -> P =:= Pid end, Children)),

    hackney_conn_sup:stop_conn(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

test_start_multiple() ->
    Opts1 = #{host => "localhost", port => 8000, transport => hackney_tcp},
    Opts2 = #{host => "localhost", port => 8001, transport => hackney_tcp},
    Opts3 = #{host => "localhost", port => 8002, transport => hackney_tcp},

    {ok, Pid1} = hackney_conn_sup:start_conn(Opts1),
    {ok, Pid2} = hackney_conn_sup:start_conn(Opts2),
    {ok, Pid3} = hackney_conn_sup:start_conn(Opts3),

    %% All should be alive
    ?assert(is_process_alive(Pid1)),
    ?assert(is_process_alive(Pid2)),
    ?assert(is_process_alive(Pid3)),

    %% All should be different
    ?assertNotEqual(Pid1, Pid2),
    ?assertNotEqual(Pid2, Pid3),
    ?assertNotEqual(Pid1, Pid3),

    %% Clean up
    hackney_conn_sup:stop_conn(Pid1),
    hackney_conn_sup:stop_conn(Pid2),
    hackney_conn_sup:stop_conn(Pid3).

test_stop_child() ->
    Opts = #{host => "localhost", port => 8000, transport => hackney_tcp},
    {ok, Pid} = hackney_conn_sup:start_conn(Opts),
    ?assert(is_process_alive(Pid)),

    ok = hackney_conn_sup:stop_conn(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)),

    %% Should no longer be in children list
    Children = supervisor:which_children(hackney_conn_sup),
    ?assertNot(lists:any(fun({_, P, _, _}) -> P =:= Pid end, Children)).

test_child_crash() ->
    Opts = #{host => "localhost", port => 8000, transport => hackney_tcp},
    {ok, Pid} = hackney_conn_sup:start_conn(Opts),

    %% Count children before
    ChildrenBefore = supervisor:count_children(hackney_conn_sup),
    ActiveBefore = proplists:get_value(active, ChildrenBefore),

    %% Kill the child process
    exit(Pid, kill),
    timer:sleep(10),

    %% Child should be gone (temporary restart strategy)
    ?assertNot(is_process_alive(Pid)),

    %% Active count should decrease
    ChildrenAfter = supervisor:count_children(hackney_conn_sup),
    ActiveAfter = proplists:get_value(active, ChildrenAfter),
    ?assertEqual(ActiveBefore - 1, ActiveAfter).
