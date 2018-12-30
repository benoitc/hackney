-module(oc_reporter_pid).

-behaviour(oc_reporter).

-export([init/1,
         report/2]).

init(_) ->
    application:get_env(opencensus, pid_reporter, #{}).

report(Spans, Opts) ->
    Pid = maps:get(pid, Opts),
    [Pid ! {span, Span} || Span <- Spans],
    ok.
