%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Simplified manager that only handles metrics.
%%% Request tracking is no longer needed - hackney_conn processes
%%% manage their own lifecycle and monitor their owners.

-module(hackney_manager).
-behaviour(gen_server).

%% Metrics API
-export([start_request/1,
         finish_request/2,
         get_metrics_engine/0]).

%% Backward compatibility API
-export([get_state/1, async_response_pid/1]).

%% gen_server API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    metrics_engine
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Called when a new request starts. Updates request counters.
-spec start_request(Host :: string() | binary()) -> ok.
start_request(Host) ->
    gen_server:cast(?MODULE, {start_request, Host}).

%% @doc Called when a request finishes. Updates metrics.
-spec finish_request(Host :: string() | binary(), StartTime :: erlang:timestamp()) -> ok.
finish_request(Host, StartTime) ->
    gen_server:cast(?MODULE, {finish_request, Host, StartTime}).

%% @doc Get the current metrics engine.
-spec get_metrics_engine() -> module().
get_metrics_engine() ->
    hackney_metrics:get_engine().

%% @doc Check the state of a connection (backward compatibility).
%% In the old architecture, this tracked request state.
%% In the new architecture, we simply check if the connection process is alive.
%% Returns `req_not_found' if the process is dead, or the connection state.
-spec get_state(pid() | term()) -> req_not_found | term().
get_state(ConnPid) when is_pid(ConnPid) ->
    case is_process_alive(ConnPid) of
        false -> req_not_found;
        true ->
            case hackney_conn:get_state(ConnPid) of
                {ok, State} -> State;
                _ -> req_not_found
            end
    end;
get_state(_) ->
    req_not_found.

%% @doc Check if a connection is in async mode (backward compatibility).
%% In the old architecture, this returned the async response process PID.
%% In the new architecture, we check if the connection process is in async mode.
%% Returns `{error, req_not_async}' if not in async mode.
-spec async_response_pid(pid() | term()) -> {ok, pid()} | {error, req_not_async}.
async_response_pid(ConnPid) when is_pid(ConnPid) ->
    case is_process_alive(ConnPid) of
        false -> {error, req_not_async};
        true ->
            case hackney_conn:get_state(ConnPid) of
                {ok, State} when State =:= receiving; State =:= streaming ->
                    {ok, ConnPid};
                _ ->
                    {error, req_not_async}
            end
    end;
async_response_pid(_) ->
    {error, req_not_async}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Initialize metrics
    Engine = hackney_metrics:get_engine(),
    _ = metrics:new(Engine, counter, [hackney, nb_requests]),
    _ = metrics:new(Engine, counter, [hackney, total_requests]),
    _ = metrics:new(Engine, counter, [hackney, finished_requests]),
    {ok, #state{metrics_engine = Engine}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start_request, Host}, #state{metrics_engine = Engine} = State) ->
    _ = metrics:increment_counter(Engine, [hackney, Host, nb_requests]),
    _ = metrics:increment_counter(Engine, [hackney, nb_requests]),
    _ = metrics:increment_counter(Engine, [hackney, total_requests]),
    {noreply, State};

handle_cast({finish_request, Host, StartTime}, #state{metrics_engine = Engine} = State) ->
    RequestTime = timer:now_diff(os:timestamp(), StartTime) / 1000,
    _ = metrics:update_histogram(Engine, [hackney, Host, request_time], RequestTime),
    _ = metrics:decrement_counter(Engine, [hackney, Host, nb_requests]),
    _ = metrics:decrement_counter(Engine, [hackney, nb_requests]),
    _ = metrics:increment_counter(Engine, [hackney, finished_requests]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
