%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc dummy metric module
%%%
-module(hackney_dummy_metrics).


-export([
    new/2,
    delete/1,
    increment_counter/1,
    increment_counter/2,
    decrement_counter/1,
    decrement_counter/2,
    update_histogram/2,
    update_gauge/2,
    update_meter/2]).


new(_, _) ->
    ok.

delete(_) ->
    ok.

increment_counter(_) ->
    ok.

increment_counter(_, _) ->
    ok.

decrement_counter(_) ->
    ok.

decrement_counter(_, _) ->
    ok.

update_histogram(_, Fun) when is_function(Fun, 0) ->
    Fun();
update_histogram(_, _) ->
    ok.

update_gauge(_, _) ->
    ok.

update_meter(_, _) ->
    ok.
