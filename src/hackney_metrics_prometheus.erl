%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2026 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_metrics_prometheus).
-author("benoitc").

-behaviour(hackney_metrics_backend).

%% hackney_metrics_backend callbacks
-export([
  counter_inc/2,
  counter_inc/3,
  gauge_set/3,
  gauge_inc/2,
  gauge_dec/2,
  histogram_observe/3,
  declare_counter/3,
  declare_gauge/3,
  declare_histogram/3,
  declare_histogram/4
]).

%% Default histogram buckets for duration metrics (in seconds)
-define(DEFAULT_BUCKETS, [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]).

%% Counter operations
counter_inc(Name, Labels) ->
    counter_inc(Name, Labels, 1).

counter_inc(Name, Labels, Value) ->
    try
        prometheus_counter:inc(Name, labels_to_list(Labels), Value)
    catch
        _:_ -> ok
    end.

%% Gauge operations
gauge_set(Name, Labels, Value) ->
    try
        prometheus_gauge:set(Name, labels_to_list(Labels), Value)
    catch
        _:_ -> ok
    end.

gauge_inc(Name, Labels) ->
    try
        prometheus_gauge:inc(Name, labels_to_list(Labels))
    catch
        _:_ -> ok
    end.

gauge_dec(Name, Labels) ->
    try
        prometheus_gauge:dec(Name, labels_to_list(Labels))
    catch
        _:_ -> ok
    end.

%% Histogram operations
histogram_observe(Name, Labels, Value) ->
    try
        prometheus_histogram:observe(Name, labels_to_list(Labels), Value)
    catch
        _:_ -> ok
    end.

%% Metric lifecycle - declarations are idempotent in prometheus
declare_counter(Name, Help, LabelKeys) ->
    try
        prometheus_counter:declare([
            {name, Name},
            {help, Help},
            {labels, LabelKeys}
        ])
    catch
        _:_ -> ok
    end.

declare_gauge(Name, Help, LabelKeys) ->
    try
        prometheus_gauge:declare([
            {name, Name},
            {help, Help},
            {labels, LabelKeys}
        ])
    catch
        _:_ -> ok
    end.

declare_histogram(Name, Help, LabelKeys) ->
    declare_histogram(Name, Help, LabelKeys, ?DEFAULT_BUCKETS).

declare_histogram(Name, Help, LabelKeys, Buckets) ->
    try
        prometheus_histogram:declare([
            {name, Name},
            {help, Help},
            {labels, LabelKeys},
            {buckets, Buckets}
        ])
    catch
        _:_ -> ok
    end.

%% Internal helpers

%% Convert a map of labels to a list of values in the order expected by prometheus
%% The order is determined by the keys when sorted alphabetically
labels_to_list(Labels) when is_map(Labels) ->
    lists:map(fun({_K, V}) -> V end, lists:keysort(1, maps:to_list(Labels))).
