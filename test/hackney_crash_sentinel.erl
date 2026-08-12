%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc Test helper: capture process crash reports so a test can assert none
%%% happened.
%%%
%%% The integration suites call `error_logger:tty(false)' to keep expected
%%% failures out of the test output, which also hides unexpected ones: a pool
%%% gen_server dying mid-test leaves no trace beyond a confusing follow-on
%%% failure (issue #927 was exactly that). Install the sentinel around a
%%% scenario and assert on what it collected:
%%%
%%% ```
%%% ok = hackney_crash_sentinel:start(),
%%% ... run the scenario ...
%%% hackney_crash_sentinel:assert_no_crash_from(PoolPid),
%%% ok = hackney_crash_sentinel:stop().
%%% '''
-module(hackney_crash_sentinel).

-export([start/0, stop/0, clear/0, reports/0, reports_from/1,
         assert_no_crash/0, assert_no_crash_from/1]).

%% logger handler callback
-export([log/2]).

-define(NAME, ?MODULE).
-define(CALL_TIMEOUT, 5000).

%%====================================================================
%% API
%%====================================================================

%% @doc Start collecting crash reports. Idempotent.
start() ->
    _ = stop(),
    Self = self(),
    Pid = spawn(fun() -> Self ! {?NAME, started}, collect([]) end),
    %% Safe because stop/0 above waited for any previous collector to be gone.
    true = register(?NAME, Pid),
    receive {?NAME, started} -> ok after ?CALL_TIMEOUT -> ok end,
    ok = logger:add_handler(?NAME, ?MODULE, #{level => error}),
    ok.

%% @doc Stop collecting and drop everything collected. Safe to call twice.
%% Waits for the collector to be gone: `start/0' registers the same name, and
%% an asynchronous stop leaves it taken for a moment.
stop() ->
    _ = logger:remove_handler(?NAME),
    case whereis(?NAME) of
        undefined ->
            ok;
        Pid ->
            Ref = erlang:monitor(process, Pid),
            Pid ! stop,
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after ?CALL_TIMEOUT ->
                erlang:demonitor(Ref, [flush]),
                exit(Pid, kill),
                ok
            end
    end.

%% @doc Drop everything collected so far, keep collecting.
clear() ->
    call(clear).

%% @doc Every crash report collected so far, oldest first.
reports() ->
    call(reports).

%% @doc Crash reports emitted by `Pid'.
reports_from(Pid) ->
    [R || R <- reports(), maps:get(pid, R, undefined) =:= Pid].

%% @doc Fail unless no process crashed since `start/0' or the last `clear/0'.
assert_no_crash() ->
    assert_empty(reports()).

%% @doc Fail unless `Pid' survived. Use this when the scenario crashes other
%% processes on purpose (a connection, a request owner) but one process is
%% expected to stay up.
assert_no_crash_from(Pid) ->
    assert_empty(reports_from(Pid)).

%%====================================================================
%% logger handler
%%====================================================================

%% Runs in the caller's process: never raise, never log.
log(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    case crash(Msg) of
        {true, Label, Reason} ->
            Report = #{label => Label,
                       reason => Reason,
                       level => Level,
                       pid => maps:get(pid, Meta, undefined)},
            _ = (try ?NAME ! {report, Report} catch _:_ -> ok end),
            ok;
        false ->
            ok
    end;
log(_Event, _Config) ->
    ok.

%% Only abnormal terminations count. A `normal' or `shutdown' stop is how the
%% suites tear pools and connections down.
crash({report, #{label := {_, terminate} = Label, reason := Reason}}) ->
    abnormal(Label, Reason);
crash({report, #{label := {proc_lib, crash} = Label} = R}) ->
    abnormal(Label, maps:get(report, R, unknown));
crash({report, #{label := {supervisor, child_terminated} = Label} = R}) ->
    abnormal(Label, maps:get(report, R, unknown));
crash({report, #{label := {supervisor, start_error} = Label} = R}) ->
    abnormal(Label, maps:get(report, R, unknown));
crash(_) ->
    false.

abnormal(_Label, normal) -> false;
abnormal(_Label, shutdown) -> false;
abnormal(_Label, {shutdown, _}) -> false;
abnormal(Label, Reason) -> {true, Label, Reason}.

%%====================================================================
%% Internal
%%====================================================================

collect(Acc) ->
    receive
        {report, Report} ->
            collect([Report | Acc]);
        {reports, From} ->
            From ! {?NAME, lists:reverse(Acc)},
            collect(Acc);
        {clear, From} ->
            From ! {?NAME, []},
            collect([]);
        stop ->
            ok
    end.

call(Op) ->
    case whereis(?NAME) of
        undefined ->
            [];
        Pid ->
            Pid ! {Op, self()},
            receive {?NAME, Reply} -> Reply after ?CALL_TIMEOUT -> [] end
    end.

assert_empty([]) ->
    ok;
assert_empty(Reports) ->
    erlang:error({unexpected_crash, Reports}).
