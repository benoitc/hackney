%% @private
-module(hackney_stream_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    %% define a stream spec
    Stream = {hackney_stream, {hackney_stream, start_link, []},
              transient, 5000, worker, [hackney_stream]},

    %% start table to keep async streams ref
    ets:new(hackney_streams, [set, public, named_table]),

    {ok, {{simple_one_for_one, 10, 10}, [Stream]}}.
