%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024 Benoit Chesneau
%%%
%%% @doc Tests for hackney_conn gen_statem.

-module(hackney_conn_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

hackney_conn_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"start and stop", fun test_start_stop/0},
      {"initial state is idle", fun test_initial_state/0},
      {"connect to server", fun test_connect/0},
      {"connect timeout", fun test_connect_timeout/0},
      {"connect to invalid host", fun test_connect_invalid/0},
      {"reconnect after close", fun test_reconnect/0},
      {"owner death stops connection", fun test_owner_death/0}
     ]}.

setup() ->
    application:ensure_all_started(hackney),
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_start_stop() ->
    Opts = #{
        host => "localhost",
        port => 8000,
        transport => hackney_tcp
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    ok = hackney_conn:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

test_initial_state() ->
    Opts = #{
        host => "localhost",
        port => 8000,
        transport => hackney_tcp
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    {ok, State} = hackney_conn:get_state(Pid),
    ?assertEqual(idle, State),
    hackney_conn:stop(Pid).

test_connect() ->
    %% This test requires a server running on localhost:8000
    %% Skip if not available
    case gen_tcp:connect("127.0.0.1", 8000, [], 100) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            Opts = #{
                host => "127.0.0.1",
                port => 8000,
                transport => hackney_tcp,
                connect_timeout => 5000
            },
            {ok, Pid} = hackney_conn:start_link(Opts),
            ?assertEqual({ok, idle}, hackney_conn:get_state(Pid)),
            Result = hackney_conn:connect(Pid),
            ?assertEqual(ok, Result),
            ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),
            hackney_conn:stop(Pid);
        {error, _} ->
            %% No server available, skip test
            ?debugMsg("Skipping test_connect - no server on localhost:8000"),
            ok
    end.

test_connect_timeout() ->
    %% Use a non-routable IP to trigger timeout
    Opts = #{
        host => "10.255.255.1",
        port => 12345,
        transport => hackney_tcp,
        connect_timeout => 100
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    Result = hackney_conn:connect(Pid, 200),
    ?assertMatch({error, _}, Result),
    %% Process should have stopped
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

test_connect_invalid() ->
    %% Connect to a port that refuses connections
    Opts = #{
        host => "127.0.0.1",
        port => 1,  %% Usually not available
        transport => hackney_tcp,
        connect_timeout => 1000
    },
    {ok, Pid} = hackney_conn:start_link(Opts),
    Result = hackney_conn:connect(Pid),
    ?assertMatch({error, _}, Result),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

test_reconnect() ->
    %% This test requires a server running on localhost:8000
    case gen_tcp:connect("127.0.0.1", 8000, [], 100) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            Opts = #{
                host => "127.0.0.1",
                port => 8000,
                transport => hackney_tcp,
                idle_timeout => 50  %% Short timeout for testing
            },
            {ok, Pid} = hackney_conn:start_link(Opts),

            %% First connect
            ok = hackney_conn:connect(Pid),
            ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),

            %% Wait for idle timeout
            timer:sleep(100),
            ?assertEqual({ok, closed}, hackney_conn:get_state(Pid)),

            %% Reconnect
            ok = hackney_conn:connect(Pid),
            ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),

            hackney_conn:stop(Pid);
        {error, _} ->
            ?debugMsg("Skipping test_reconnect - no server on localhost:8000"),
            ok
    end.

test_owner_death() ->
    Opts = #{
        host => "localhost",
        port => 8000,
        transport => hackney_tcp
    },

    %% Start connection from a temporary process
    Self = self(),
    Spawner = spawn(fun() ->
        {ok, Pid} = hackney_conn:start_link(Opts),
        Self ! {conn_pid, Pid},
        receive
            die -> ok
        end
    end),

    Pid = receive
        {conn_pid, P} -> P
    after 1000 ->
        error(timeout)
    end,

    ?assert(is_process_alive(Pid)),

    %% Kill the owner
    Spawner ! die,
    timer:sleep(50),

    %% Connection should have stopped
    ?assertNot(is_process_alive(Pid)).
