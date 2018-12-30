-module(hackney_trace_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

-include_lib("opencensus/include/opencensus.hrl").

trace_test_() ->
  {setup,
   fun start/0,
   fun stop/1,
   [{timeout, 1, get_request()}]}.

start() ->
  {ok, _} = application:ensure_all_started(cowboy),
  Host = '_',
  Resource = {"/get", empty_clen_resource, []},
  Dispatch = cowboy_router:compile([{Host, [Resource]}]),
  cowboy:start_http(test_server, 10, [{port, 8123}], [{env, [{dispatch, Dispatch}]}]).

stop({ok, _Pid}) ->
  cowboy:stop_listener(test_server),
  application:stop(cowboy),
  ok.

get_request() ->
    fun() ->
        %% stop and reload everything to make sure the new env vars are used when starting
        application:stop(hackney),
        application:stop(opencensus),
        application:load(opencensus),
        application:set_env(opencensus, pid_reporter, #{pid => self()}),
        application:set_env(opencensus, sampler, {oc_sampler_always, []}),
        application:set_env(opencensus, reporter, {oc_reporter_pid, []}),

        {ok, _} = application:ensure_all_started(hackney),

        URL = <<"http://localhost:8123/get">>,
        {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
        ?assertEqual(200, StatusCode),

        receive
          {span, Span} ->
            ?assertEqual(<<"localhost:8123/get">>, Span#span.name)
        end
    end.
