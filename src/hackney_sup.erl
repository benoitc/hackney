%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%

-module(hackney_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("hackney.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  %% start the pool handler
  PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),
  ok = PoolHandler:start(),
  %% finish to start the application
  {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  %% initialize the config table
  _ = ets:new(?CONFIG, [set, named_table, public]),
  %% initialize the metric engine
  hackney_metrics:init(),
  Specs = [% manager
           #{ id => hackney_manager,
              start => {hackney_manager, start_link, []} }
          ],

  SupFlags = #{ strategy => one_for_one,
                intensity => 10000,
                period => 1 },
  {ok, { SupFlags, Specs}}.

