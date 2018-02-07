%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2014 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1,
         get_app_env/1, get_app_env/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Metrics = metrics:init(hackney_util:mod_metrics()),
  application:set_env(hackney, metrics, Metrics),
  hackney_sup:start_link().

stop(_State) ->
  ok.

%% @doc return a config value
get_app_env(Key) ->
  get_app_env(Key, undefined).

%% @doc return a config value
get_app_env(Key, Default) ->
  case application:get_env(hackney, Key) of
    {ok, Val} -> Val;
    undefined -> Default
  end.
