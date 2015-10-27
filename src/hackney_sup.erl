%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2014 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-include("hackney.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ok = init_pools(),
    {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Manager = ?CHILD(hackney_manager, worker),

    %% server maintaing some meta data related to connections
    Server = {hackney_server,
              {hackney_server, start_link, []},
              permanent, 5000, worker, [hackney_server]},

    %% cache used to store host lookup prefereneces
    CacheSize = application:get_env(hackney, lookup_cache_size, ?DEFAULT_CACHE_SIZE),
    Cache = {?LOOKUP_CACHE,
             {lru, start_link, [{local, ?LOOKUP_CACHE}, CacheSize, []]},
             permanent, 5000, worker, [lru]},


    {ok, { {one_for_one, 10, 1}, [Manager, Server, Cache]}}.


default_pool() ->
      IdleTimeout = hackney_util:get_env(idle_timeout, ?DEFAULT_IDLE_TIMEOUT),
      GroupLimit = hackney_util:get_env(group_limit, ?DEFAULT_GROUP_LIMIT),
      MaxConns =  hackney_util:get_env(max_connections,  ?DEFAULT_MAX_CONNS),

      [{idle_timeout, IdleTimeout},
       {group_limit, GroupLimit},
       {max_conns, MaxConns}].

init_pools() ->
    Pools0 = hackney_util:get_env(pools, []),
    DefaultPool = default_pool(),
    Pools = case lists:member(default, Pools0) of
              false -> [{default, DefaultPool} | Pools0];
              true -> Pools0
            end,
    lists:foreach(fun({Name, Opts}) ->
                    {ok, _} = hackney:start_pool(Name, Opts)
                  end, Pools),
    ok.
