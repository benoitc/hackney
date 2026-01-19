%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2026 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_metrics_dummy).
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

%% Counter operations - no-op
counter_inc(_Name, _Labels) -> ok.
counter_inc(_Name, _Labels, _Value) -> ok.

%% Gauge operations - no-op
gauge_set(_Name, _Labels, _Value) -> ok.
gauge_inc(_Name, _Labels) -> ok.
gauge_dec(_Name, _Labels) -> ok.

%% Histogram operations - no-op
histogram_observe(_Name, _Labels, _Value) -> ok.

%% Metric lifecycle - no-op
declare_counter(_Name, _Help, _LabelKeys) -> ok.
declare_gauge(_Name, _Help, _LabelKeys) -> ok.
declare_histogram(_Name, _Help, _LabelKeys) -> ok.
declare_histogram(_Name, _Help, _LabelKeys, _Buckets) -> ok.
