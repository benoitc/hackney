%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2026 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_metrics_backend).
-author("benoitc").

%% Behaviour callbacks for hackney metrics backends
%%
%% Implementations must export all callback functions.
%% See hackney_metrics_dummy for a reference implementation.

%% Counter operations (monotonically increasing)
-callback counter_inc(Name :: atom(), Labels :: map()) -> ok.
-callback counter_inc(Name :: atom(), Labels :: map(), Value :: number()) -> ok.

%% Gauge operations (can go up or down)
-callback gauge_set(Name :: atom(), Labels :: map(), Value :: number()) -> ok.
-callback gauge_inc(Name :: atom(), Labels :: map()) -> ok.
-callback gauge_dec(Name :: atom(), Labels :: map()) -> ok.

%% Histogram operations (for timing/distribution measurements)
-callback histogram_observe(Name :: atom(), Labels :: map(), Value :: number()) -> ok.

%% Metric lifecycle
-callback declare_counter(Name :: atom(), Help :: binary(), LabelKeys :: [atom()]) -> ok.
-callback declare_gauge(Name :: atom(), Help :: binary(), LabelKeys :: [atom()]) -> ok.
-callback declare_histogram(Name :: atom(), Help :: binary(), LabelKeys :: [atom()]) -> ok.
-callback declare_histogram(Name :: atom(), Help :: binary(), LabelKeys :: [atom()], Buckets :: [number()]) -> ok.
