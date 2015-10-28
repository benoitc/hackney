-module(hackney_pool_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

-include("hackney.hrl").

start_link(Ref, PoolOpts) ->
    supervisor:start_link(?MODULE, {Ref, PoolOpts}).


init({Ref, PoolOpts}) ->
    NbConnectors = hackney_util:get_opt(nb_connectors, PoolOpts, ?DEFAULT_NB_CONNECTORS),
    FallbackTime = hackney_util:get_opt(fallback_time, PoolOpts, ?DEFAULT_FALLBACK_TIME),

    %% pool, caching sockets
    Pool = {hackney_pool,
            {hackney_pool, start_link, [Ref, PoolOpts]},
            permanent, brutal_kill, worker, [hackney_pool]},

    %% connection supervisor
    ConnectorSup = {hackney_connector_sup,
                    {hackney_connector_sup, start_link, [Ref, NbConnectors, FallbackTime]},
                    permanent, infinity, supervisor, [hackney_connector_sup]},

    {ok, {{rest_for_one, 10, 10}, [Pool, ConnectorSup]}}.
