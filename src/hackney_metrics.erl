%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2026 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_metrics).
-author("benoitc").

%% API
-export([
  init/0,
  get_backend/0
]).

%% Counter operations
-export([
  counter_inc/2,
  counter_inc/3
]).

%% Gauge operations
-export([
  gauge_set/3,
  gauge_inc/2,
  gauge_dec/2
]).

%% Histogram operations
-export([
  histogram_observe/3
]).

%% Metric declarations
-export([
  declare_counter/3,
  declare_gauge/3,
  declare_histogram/3,
  declare_histogram/4,
  declare_pool_metrics/1
]).

-include("hackney.hrl").

%% Default duration histogram buckets (in seconds)
-define(DEFAULT_DURATION_BUCKETS, [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]).

%% @doc Initialize the metrics system.
%% Determines the backend to use and declares all hackney metrics.
init() ->
  Backend = hackney_util:mod_metrics(),
  ets:insert(?CONFIG, {metrics_backend, Backend}),
  declare_metrics(Backend).

%% @doc Get the current metrics backend module.
get_backend() ->
  try
    ets:lookup_element(?CONFIG, metrics_backend, 2)
  catch
    error:badarg ->
      %% ETS table not ready yet, return dummy backend
      hackney_metrics_dummy
  end.

%% @doc Increment a counter by 1.
counter_inc(Name, Labels) ->
  (get_backend()):counter_inc(Name, Labels).

%% @doc Increment a counter by a value.
counter_inc(Name, Labels, Value) ->
  (get_backend()):counter_inc(Name, Labels, Value).

%% @doc Set a gauge to a value.
gauge_set(Name, Labels, Value) ->
  (get_backend()):gauge_set(Name, Labels, Value).

%% @doc Increment a gauge by 1.
gauge_inc(Name, Labels) ->
  (get_backend()):gauge_inc(Name, Labels).

%% @doc Decrement a gauge by 1.
gauge_dec(Name, Labels) ->
  (get_backend()):gauge_dec(Name, Labels).

%% @doc Observe a value for a histogram.
histogram_observe(Name, Labels, Value) ->
  (get_backend()):histogram_observe(Name, Labels, Value).

%% @doc Declare a counter metric.
declare_counter(Name, Help, LabelKeys) ->
  (get_backend()):declare_counter(Name, Help, LabelKeys).

%% @doc Declare a gauge metric.
declare_gauge(Name, Help, LabelKeys) ->
  (get_backend()):declare_gauge(Name, Help, LabelKeys).

%% @doc Declare a histogram metric with default buckets.
declare_histogram(Name, Help, LabelKeys) ->
  (get_backend()):declare_histogram(Name, Help, LabelKeys).

%% @doc Declare a histogram metric with custom buckets.
declare_histogram(Name, Help, LabelKeys, Buckets) ->
  (get_backend()):declare_histogram(Name, Help, LabelKeys, Buckets).

%% @doc Declare pool-specific metrics.
%% Called when a new pool is created.
declare_pool_metrics(_PoolName) ->
  Backend = get_backend(),
  %% Only declare once (idempotent for prometheus)
  Backend:declare_gauge(hackney_pool_free_count,
    <<"Number of free/available connections in the pool">>, [pool]),
  Backend:declare_gauge(hackney_pool_in_use_count,
    <<"Number of connections currently in use">>, [pool]),
  Backend:declare_counter(hackney_pool_checkouts_total,
    <<"Total number of connection checkouts">>, [pool]),
  ok.

%% @private
%% Declare all hackney metrics at startup.
declare_metrics(Backend) ->
  %% Request metrics
  Backend:declare_counter(hackney_requests_total,
    <<"Total number of HTTP requests started">>, [host]),
  Backend:declare_gauge(hackney_requests_active,
    <<"Number of currently active HTTP requests">>, [host]),
  Backend:declare_counter(hackney_requests_finished_total,
    <<"Total number of HTTP requests finished">>, [host]),
  Backend:declare_histogram(hackney_request_duration_seconds,
    <<"HTTP request duration in seconds">>, [host], ?DEFAULT_DURATION_BUCKETS),
  %% Pool metrics are declared when pools are created
  ok.
