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
         finish_request/2]).

%% Backward compatibility API
-export([get_state/1, async_response_pid/1]).

%% gen_server API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

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

%% @doc Check the state of a connection (backward compatibility).
%% In the old architecture, this tracked request state.
%% In the new architecture, we simply check if the connection process is alive.
%% Returns `req_not_found' if the process is dead, or the connection state name.
-spec get_state(pid() | term()) -> req_not_found | atom().
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

%% @doc Get the async response pid (backward compatibility).
%% In the new architecture, all streaming connections are considered "async".
-spec async_response_pid(pid()) -> {ok, pid()} | {error, req_not_found | req_not_async}.
async_response_pid(Ref) when is_pid(Ref) ->
    case get_state(Ref) of
        req_not_found -> {error, req_not_found};
        streaming -> {ok, Ref};
        streaming_once -> {ok, Ref};
        _ -> {error, req_not_async}
    end;
async_response_pid(_) ->
    {error, req_not_async}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start_request, Host}, State) ->
    HostBin = to_binary(Host),
    Labels = #{host => HostBin},
    _ = hackney_metrics:counter_inc(hackney_requests_total, Labels),
    _ = hackney_metrics:gauge_inc(hackney_requests_active, Labels),
    {noreply, State};

handle_cast({finish_request, Host, StartTime}, State) ->
    HostBin = to_binary(Host),
    Labels = #{host => HostBin},
    %% Calculate duration in seconds (Prometheus convention)
    DurationMicros = timer:now_diff(os:timestamp(), StartTime),
    DurationSeconds = DurationMicros / 1000000,
    _ = hackney_metrics:histogram_observe(hackney_request_duration_seconds, Labels, DurationSeconds),
    _ = hackney_metrics:gauge_dec(hackney_requests_active, Labels),
    _ = hackney_metrics:counter_inc(hackney_requests_finished_total, Labels),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

to_binary(Host) when is_binary(Host) -> Host;
to_binary(Host) when is_list(Host) -> list_to_binary(Host);
to_binary(Host) when is_atom(Host) -> atom_to_binary(Host, utf8).
