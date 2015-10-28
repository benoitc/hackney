-module(hackney_connector_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(Pool, NbConnectors, FallbackTime) ->
    supervisor:start_link(?MODULE, [Pool, NbConnectors, FallbackTime]).


init([Ref, NbConnectors, FallbackTime]) ->
    Pool = hackney_server:get_pool(Ref),
    
    Procs = [{{hackney_connector, self(), N},
              {hackney_connector, start_link, [Pool, FallbackTime]},
              permanent, brutal_kill, worker, []} || N <- lists:seq(1, NbConnectors)],
    {ok, {{one_for_one, 10, 10}, Procs}}.