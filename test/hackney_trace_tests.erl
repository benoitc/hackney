%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_trace_tests).
-include_lib("eunit/include/eunit.hrl").

%% report_event/4 tests
%% Tests for the report_event function which validates its inputs
report_event_test_() ->
    [
        {"report_event accepts valid parameters",
         fun() ->
             %% Valid call should return 'hopefully_traced'
             Result = hackney_trace:report_event(50, "test label", test_service, [content]),
             ?assertEqual(hopefully_traced, Result)
         end},
        {"report_event accepts severity 0",
         fun() ->
             Result = hackney_trace:report_event(0, "min severity", test_service, []),
             ?assertEqual(hopefully_traced, Result)
         end},
        {"report_event accepts severity 100",
         fun() ->
             Result = hackney_trace:report_event(100, "max severity", test_service, []),
             ?assertEqual(hopefully_traced, Result)
         end},
        {"report_event accepts empty content list",
         fun() ->
             Result = hackney_trace:report_event(50, "empty content", test_service, []),
             ?assertEqual(hopefully_traced, Result)
         end},
        {"report_event accepts complex content",
         fun() ->
             Content = [{key, value}, {nested, [{a, 1}]}],
             Result = hackney_trace:report_event(50, "complex", test_service, Content),
             ?assertEqual(hopefully_traced, Result)
         end}
    ].

%% Tests for parameter validation via guard clauses
report_event_validation_test_() ->
    [
        {"report_event rejects negative severity",
         fun() ->
             ?assertError(function_clause,
                 hackney_trace:report_event(-1, "label", service, []))
         end},
        {"report_event rejects severity over 100",
         fun() ->
             ?assertError(function_clause,
                 hackney_trace:report_event(101, "label", service, []))
         end},
        {"report_event rejects non-list label",
         fun() ->
             ?assertError(function_clause,
                 hackney_trace:report_event(50, not_a_list, service, []))
         end},
        {"report_event rejects non-atom service",
         fun() ->
             ?assertError(function_clause,
                 hackney_trace:report_event(50, "label", "not_atom", []))
         end},
        {"report_event rejects non-list content",
         fun() ->
             ?assertError(function_clause,
                 hackney_trace:report_event(50, "label", service, not_a_list))
         end}
    ].

%% Tests for enable/disable flow (these test state changes)
enable_disable_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) ->
         %% Ensure tracing is disabled after tests
         catch hackney_trace:disable(),
         ok
     end,
     [
        {"enable with io returns ok",
         fun() ->
             Result = hackney_trace:enable(max, io),
             ?assertEqual(ok, Result),
             hackney_trace:disable()
         end},
        {"enable with min level returns ok",
         fun() ->
             Result = hackney_trace:enable(min, io),
             ?assertEqual(ok, Result),
             hackney_trace:disable()
         end},
        {"enable with integer level returns ok",
         fun() ->
             Result = hackney_trace:enable(50, io),
             ?assertEqual(ok, Result),
             hackney_trace:disable()
         end},
        {"enable with custom handler returns ok",
         fun() ->
             Handler = fun(_Event, State) -> State end,
             Result = hackney_trace:enable(max, {Handler, initial_state}),
             ?assertEqual(ok, Result),
             hackney_trace:disable()
         end},
        {"disable returns ok",
         fun() ->
             hackney_trace:enable(max, io),
             Result = hackney_trace:disable(),
             ?assertEqual(ok, Result)
         end}
     ]}.

%% set_level tests
set_level_test_() ->
    {setup,
     fun() ->
         hackney_trace:enable(max, io)
     end,
     fun(_) ->
         catch hackney_trace:disable()
     end,
     [
        {"set_level to max returns ok",
         fun() ->
             Result = hackney_trace:set_level(max),
             ?assertEqual(ok, Result)
         end},
        {"set_level to min returns ok",
         fun() ->
             Result = hackney_trace:set_level(min),
             ?assertEqual(ok, Result)
         end},
        {"set_level to integer returns ok",
         fun() ->
             Result = hackney_trace:set_level(75),
             ?assertEqual(ok, Result)
         end}
     ]}.
