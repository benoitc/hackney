-module(hackney_metrics_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

%% Metrics callbacks
-export([new/2, increment_counter/1, increment_counter/2, decrement_counter/1, decrement_counter/2, update_histogram/2, update_meter/2, update_gauge/2]).

all_tests() ->
    [fun test_host_status_metrics/0].

http_requests_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(ok) ->
             {inorder, all_tests()}
     end}.

start() ->
    application:set_env(hackney, mod_metrics, ?MODULE),
    {ok, _} = application:ensure_all_started(hackney),
    ok.

stop(ok) ->
    application:unset_env(hackney, mod_metrics),
    ok.

test_host_status_metrics() ->
    URL = <<"http://localhost:8000/status/401">>,
    Headers = [{<<"Host">>, <<"myhost.com">>}],
    Options = [with_body],
    {ok, 401, _H, _JsonBody} = hackney:get(URL, Headers, <<>>, Options),
    receive
        {update_histogram, [hackney, "localhost", status_code], 401} ->
            ok
    after
        0 -> throw(metric_not_received)
    end.


new(_Type, _) -> ok.

increment_counter(Metric) -> increment_counter(Metric, 1).
increment_counter(Metric, Value) -> self() ! {increment_counter, Metric, Value}.

decrement_counter(Metric) -> decrement_counter(Metric, 1).
decrement_counter(Metric, Value) -> self() ! {decrement_counter, Metric, Value}.

update_meter(Metric, Value) -> self() ! {update_meter, Metric, Value}.
update_gauge(Metric, Value) -> self() ! {update_gauge, Metric, Value}.

update_histogram(Metric, Value) -> self() ! {update_histogram, Metric, Value}.
