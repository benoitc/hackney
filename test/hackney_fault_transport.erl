%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc Test transport that behaves exactly like {@link hackney_tcp} until it
%%% is told to misbehave.
%%%
%%% Pass it as the `Transport' of a pool checkout (or as `transport' in
%%% connection options) and arm a fault per callback:
%%%
%%% ```
%%% hackney_fault_transport:set(connect, {slow_error, 200}),  %% issue #927
%%% hackney_fault_transport:set(send, crash),
%%% hackney_fault_transport:clear().
%%% '''
%%%
%%% Unarmed callbacks delegate to `hackney_tcp', so a connection dialled
%%% through this module talks to a real server and can carry real requests.
%%% That is what makes the faults interesting: they land in the middle of the
%%% ordinary code path rather than in a stub.
%%%
%%% Faults:
%%% <ul>
%%%   <li>`ok' - delegate (the default for an unarmed callback)</li>
%%%   <li>`{sleep, Ms}' - pause, then delegate</li>
%%%   <li>`{slow_error, Ms}' - pause longer than the caller's timeout, then
%%%       fail: a DNS/TCP/TLS attempt that outlives its deadline</li>
%%%   <li>`{hang, Ms}' - pause without ever answering; the process stays wedged
%%%       for `Ms' and answers nothing, not even a stop request</li>
%%%   <li>`{error, Reason}' - fail immediately</li>
%%%   <li>`crash' - raise, taking the calling connection process down</li>
%%% </ul>
-module(hackney_fault_transport).

%% Fault control
-export([set/1, set/2, clear/0, clear/1, calls/1]).

%% hackney transport callbacks
-export([messages/1,
         connect/3, connect/4,
         recv/2, recv/3,
         send/2,
         setopts/2,
         controlling_process/2,
         peername/1,
         close/1,
         shutdown/2,
         sockname/1]).

-define(TABLE, hackney_fault_transport_faults).
-define(OWNER, hackney_fault_transport_owner).

-type fault() :: ok
               | {sleep, non_neg_integer()}
               | {slow_error, non_neg_integer()}
               | {hang, non_neg_integer()}
               | {error, term()}
               | crash.

-export_type([fault/0]).

%%====================================================================
%% Fault control
%%====================================================================

%% @doc Arm `Fault' on connect/4, the callback most scenarios care about.
-spec set(fault()) -> ok.
set(Fault) -> set(connect, Fault).

%% @doc Arm `Fault' on one callback (`connect', `send', `recv', `setopts',
%% `close', `controlling_process').
-spec set(atom(), fault()) -> ok.
set(Callback, Fault) ->
    ensure_table(),
    true = ets:insert(?TABLE, {{fault, Callback}, Fault}),
    ok.

%% @doc Disarm every callback. Call counters survive: a test that clears
%% faults before reporting still wants to know what ran.
-spec clear() -> ok.
clear() ->
    ensure_table(),
    true = ets:match_delete(?TABLE, {{fault, '_'}, '_'}),
    ok.

%% @doc Disarm one callback.
-spec clear(atom()) -> ok.
clear(Callback) ->
    ensure_table(),
    true = ets:delete(?TABLE, {fault, Callback}),
    ok.

%% @doc How many times a callback ran since the table was created. Lets a test
%% prove it exercised the path it thinks it did: a chaos run that never dials
%% is not testing connect faults, however many faults it armed.
-spec calls(atom()) -> non_neg_integer().
calls(Callback) ->
    try ets:lookup(?TABLE, {calls, Callback}) of
        [{_, N}] -> N;
        [] -> 0
    catch
        error:badarg -> 0
    end.

%%====================================================================
%% Transport callbacks
%%====================================================================

messages(Socket) -> hackney_tcp:messages(Socket).

connect(Host, Port, Opts) -> connect(Host, Port, Opts, infinity).

connect(Host, Port, Opts, Timeout) ->
    with_fault(connect, fun() -> hackney_tcp:connect(Host, Port, Opts, Timeout) end).

recv(Socket, Length) -> recv(Socket, Length, infinity).

recv(Socket, Length, Timeout) ->
    with_fault(recv, fun() -> hackney_tcp:recv(Socket, Length, Timeout) end).

send(Socket, Packet) ->
    with_fault(send, fun() -> hackney_tcp:send(Socket, Packet) end).

setopts(Socket, Opts) ->
    with_fault(setopts, fun() -> hackney_tcp:setopts(Socket, Opts) end).

controlling_process(Socket, Pid) ->
    with_fault(controlling_process,
               fun() -> hackney_tcp:controlling_process(Socket, Pid) end).

close(Socket) ->
    with_fault(close, fun() -> hackney_tcp:close(Socket) end).

peername(Socket) -> hackney_tcp:peername(Socket).

sockname(Socket) -> hackney_tcp:sockname(Socket).

shutdown(Socket, How) -> hackney_tcp:shutdown(Socket, How).

%%====================================================================
%% Internal
%%====================================================================

with_fault(Callback, Delegate) ->
    count(Callback),
    case fault(Callback) of
        ok ->
            Delegate();
        {sleep, Ms} ->
            timer:sleep(Ms),
            Delegate();
        {slow_error, Ms} ->
            timer:sleep(Ms),
            {error, simulated_timeout};
        {hang, Ms} ->
            timer:sleep(Ms),
            {error, simulated_hang};
        {error, Reason} ->
            {error, Reason};
        crash ->
            erlang:error({simulated_crash, Callback})
    end.

count(Callback) ->
    try ets:update_counter(?TABLE, {calls, Callback}, 1, {{calls, Callback}, 0}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

fault(Callback) ->
    try ets:lookup(?TABLE, {fault, Callback}) of
        [{_, Fault}] -> Fault;
        [] -> ok
    catch
        error:badarg -> ok
    end.

%% The table outlives the eunit process that armed it: each test runs in a
%% fresh process, and connection processes read faults after it has exited.
ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined -> start_owner();
        _Tid -> ok
    end.

start_owner() ->
    Self = self(),
    _ = spawn(fun() ->
        _ = (try ets:new(?TABLE, [named_table, public, set]) catch _:_ -> ok end),
        _ = (try register(?OWNER, self()) catch _:_ -> ok end),
        Self ! {?MODULE, ready},
        receive stop -> ok end
    end),
    receive {?MODULE, ready} -> ok after 5000 -> ok end,
    ok.
