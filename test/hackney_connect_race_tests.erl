%%% Regression tests for issue #914.
%%%
%%% When a pooled HTTP/2 or HTTP/3 connection terminates during the checkout
%%% get_state liveness probe, the probe's gen_statem:call exits. That exit must
%%% be caught so checkout falls through to a fresh connection instead of
%%% crashing the caller of hackney:connect/4.
-module(hackney_connect_race_tests).

-include_lib("eunit/include/eunit.hrl").

race_test_() ->
    {setup, fun setup/0, fun teardown/1, fun tests/1}.

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    Prev = application:get_env(hackney, pool_handler),
    DeadPid = dead_pid(),
    application:set_env(hackney, race_dead_pid, DeadPid),
    application:set_env(hackney, pool_handler, hackney_race_pool),
    {Prev, DeadPid}.

teardown({Prev, _DeadPid}) ->
    case Prev of
        {ok, Handler} -> application:set_env(hackney, pool_handler, Handler);
        undefined -> application:unset_env(hackney, pool_handler)
    end,
    application:unset_env(hackney, race_dead_pid),
    ok.

tests({_Prev, DeadPid}) ->
    [{"h2 checkout survives a terminating pooled connection",
      fun() -> h2_no_crash(DeadPid) end},
     {"h3 checkout get_state probe exit does not escape",
      fun() -> h3_probe_does_not_escape(DeadPid) end}].

%% A guaranteed-dead pid: spawn, wait for the DOWN, then it is gone. No timing.
dead_pid() ->
    {Pid, Ref} = spawn_monitor(fun() -> ok end),
    receive {'DOWN', Ref, process, Pid, _} -> ok end,
    Pid.

%% H2 fallback is a plain TCP connect, so a closed local port lets the whole
%% call return cleanly: the checkout probe exits, is caught, and the fresh
%% connection to the closed port fails with {error, _} instead of crashing.
h2_no_crash(DeadPid) ->
    ?assertNot(is_process_alive(DeadPid)),
    Port = closed_port(),
    Result = connect([http2], Port),
    %% Buggy: EXIT {noproc,{gen_statem,call,[_,get_state|_]}}.
    %% Fixed:  {error, _} from the fresh-connection fallback.
    ?assertMatch({error, _}, Result).

%% H3's fresh-connection fallback drives a real QUIC handshake (a separate
%% path), so here we assert only the #914 property: the get_state probe exit
%% must not escape hackney:connect/4.
h3_probe_does_not_escape(DeadPid) ->
    ?assertNot(is_process_alive(DeadPid)),
    Port = closed_port(),
    Result = connect([http3], Port),
    ?assertNotMatch({exit, {_, {gen_statem, call, [_, get_state | _]}}}, Result).

closed_port() ->
    {ok, L} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(L),
    ok = gen_tcp:close(L),
    Port.

connect(Protocols, Port) ->
    try
        hackney:connect(hackney_ssl, "127.0.0.1", Port,
                        [{protocols, Protocols},
                         {pool, default},
                         {connect_timeout, 500}])
    catch
        Class:Reason -> {Class, Reason}
    end.
