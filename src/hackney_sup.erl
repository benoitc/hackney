
-module(hackney_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    PoolOptions = [{name, hackney_pool}],
    DefaultPool = {hackney_pool,
                   {hackney_pool, start_link, [PoolOptions]},
                   permanent, 10000, worker, [hackney_pool]},
    {ok, { {one_for_one, 10, 1}, [DefaultPool]}}.

