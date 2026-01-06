%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% Copyright (c) 2012-2024, Benoît Chesneau <benoitc@e-engura.org>

%% @doc Pool of connection processes.
%%
%% This module manages hackney_conn processes in a pool. Instead of storing
%% raw sockets, it tracks connection process pids. Idle timeout is handled
%% by the connection processes themselves (gen_statem state_timeout).
%%
-module(hackney_pool).
-behaviour(gen_server).

%% PUBLIC API
-export([start/0,
         checkout/4,
         checkin/2]).

-export([
         get_stats/1,
         start_pool/2,
         stop_pool/1,
         find_pool/1,
         notify/2
        ]).

-export([count/1, count/2,
         max_connections/1,
         set_max_connections/2,
         timeout/1,
         set_timeout/2,
         child_spec/2]).

-export([start_link/2]).

%% For internal use
-export([to_pool_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("hackney.hrl").
-include_lib("hackney_internal.hrl").

-record(state, {
    name,
    metrics,
    max_connections,
    keepalive_timeout,
    %% Available connection processes: #{Key => [Pid]}
    available = #{},
    %% In-use connections: #{Pid => Key}
    in_use = #{},
    %% Pid to monitor ref mapping: #{Pid => MonitorRef}
    pid_monitors = #{}
}).

-define(DEFAULT_MAX_CONNECTIONS, 50).
-define(DEFAULT_KEEPALIVE_TIMEOUT, 2000).  % 2 seconds max idle

start() ->
    %% Create ETS table to store pool pid by name
    _ = ets:new(?MODULE, [set, public, named_table, {read_concurrency, true}]),
    ok.

%% @doc Checkout a connection process from the pool.
%% Returns {ok, PoolInfo, Pid} where Pid is a hackney_conn process.
-spec checkout(Host :: string(), Port :: non_neg_integer(),
               Transport :: module(), Options :: list()) ->
    {ok, term(), pid()} | {error, term()}.
checkout(Host, Port, Transport, Options) ->
    Requester = self(),
    try
        do_checkout(Requester, Host, Port, Transport, Options)
    catch
        exit:{timeout, _} ->
            ?report_trace("pool: checkout timeout", []),
            {error, checkout_timeout};
        _:Error ->
            ?report_trace("pool: checkout failure", [{error, Error}]),
            {error, checkout_failure}
    end.

do_checkout(Requester, Host, Port, Transport, Opts) ->
    ConnectTimeout = proplists:get_value(connect_timeout, Opts, 8000),
    CheckoutTimeout = proplists:get_value(checkout_timeout, Opts, ConnectTimeout),
    PoolName = proplists:get_value(pool, Opts, default),
    Pool = find_pool(PoolName, Opts),
    Key = connection_key(Host, Port, Transport),

    case gen_server:call(Pool, {checkout, Key, Requester, Opts}, CheckoutTimeout) of
        {ok, Pid} ->
            %% Return pool info for later checkin
            PoolInfo = {PoolName, Key, Pool, Transport},
            {ok, PoolInfo, Pid};
        {error, _} = Error ->
            Error;
        {'EXIT', {timeout, _}} ->
            {error, checkout_timeout}
    end.

%% @doc Return a connection process to the pool.
-spec checkin(PoolInfo :: term(), Pid :: pid()) -> ok.
checkin({_PoolName, _Key, Pool, _Transport}, Pid) ->
    gen_server:cast(Pool, {checkin, nil, Pid}),
    ok.

get_stats(Pool) ->
    gen_server:call(find_pool(Pool), stats).

%% @doc start a pool
start_pool(Name, Options) ->
    case find_pool(Name, Options) of
        Pid when is_pid(Pid) ->
            ok;
        Error ->
            Error
    end.

%% @doc stop a pool
stop_pool(Name) ->
    case find_pool(Name) of
        undefined ->
            ok;
        _Pid ->
            case supervisor:terminate_child(hackney_sup, Name) of
                ok ->
                    _ = supervisor:delete_child(hackney_sup, Name),
                    ets:delete(hackney_pool, Name),
                    ok;
                Error ->
                    Error
            end
    end.

notify(Pool, Msg) ->
    case find_pool(Pool) of
        undefined -> ok;
        Pid -> Pid ! Msg
    end.

%% @doc return a child spec suitable for embedding your pool in the supervisor
child_spec(Name, Options0) ->
    Options = [{name, Name} | Options0],
    {Name, {hackney_pool, start_link, [Name, Options]},
     permanent, 10000, worker, [hackney_pool]}.

%% @doc get the number of connections in the pool
count(Name) ->
    case find_pool(Name) of
        undefined -> 0;
        Pid -> gen_server:call(Pid, count)
    end.

%% @doc get the number of connections in the pool for `{Host, Port, Transport}'
count(Name, Key) ->
    case find_pool(Name) of
        undefined -> 0;
        Pid -> gen_server:call(Pid, {count, Key})
    end.

%% @doc get max pool size
max_connections(Name) ->
    case find_pool(Name) of
        undefined -> 0;
        Pid -> gen_server:call(Pid, max_connections)
    end.

%% @doc change the pool size
set_max_connections(Name, NewSize) ->
    gen_server:cast(find_pool(Name), {set_maxconn, NewSize}).

%% @doc get timeout
timeout(Name) ->
    case find_pool(Name) of
        undefined -> 0;
        Pid -> gen_server:call(Pid, timeout)
    end.

%% @doc change the connection timeout
set_timeout(Name, NewTimeout) ->
    gen_server:cast(find_pool(Name), {set_timeout, NewTimeout}).

to_pool_name(Name) when is_atom(Name) ->
    list_to_atom("hackney_pool_" ++ atom_to_list(Name));
to_pool_name(Name) when is_list(Name) ->
    list_to_atom("hackney_pool_" ++ Name);
to_pool_name(Name) when is_binary(Name) ->
    to_pool_name(binary_to_list(Name)).

%% @private
do_start_pool(Name, Options) ->
    Spec = child_spec(Name, Options),
    case supervisor:start_child(hackney_sup, Spec) of
        {ok, Pid} ->
            Pid;
        {error, {already_started, _}} ->
            find_pool(Name, Options)
    end.

find_pool(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            undefined;
        [{_, Pid}] ->
            Pid
    end.

find_pool(Name, Options) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            do_start_pool(Name, Options);
        [{_, Pid}] ->
            Pid
    end.

start_link(Name, Options0) ->
    Options = hackney_util:maybe_apply_defaults([max_connections, timeout],
                                                Options0),
    gen_server:start_link(?MODULE, [Name, Options], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, Options]) ->
    MaxConn = case proplists:get_value(pool_size, Options) of
                  undefined ->
                      proplists:get_value(max_connections, Options, ?DEFAULT_MAX_CONNECTIONS);
                  Size ->
                      Size
              end,
    %% keepalive_timeout: max idle time for pooled connections (capped at 2s)
    %% Also accept 'timeout' for backward compatibility
    RawTimeout = case proplists:get_value(keepalive_timeout, Options) of
                     undefined ->
                         proplists:get_value(timeout, Options, ?DEFAULT_KEEPALIVE_TIMEOUT);
                     KT ->
                         KT
                 end,
    KeepaliveTimeout = min(RawTimeout, ?DEFAULT_KEEPALIVE_TIMEOUT),

    %% register the module
    ets:insert(?MODULE, {Name, self()}),

    %% initialize metrics
    Engine = init_metrics(Name),

    {ok, #state{name=Name, metrics=Engine, max_connections=MaxConn, keepalive_timeout=KeepaliveTimeout}}.

handle_call(stats, _From, State) ->
    {reply, handle_stats(State), State};

handle_call(count, _From, #state{available=Available, in_use=InUse}=State) ->
    AvailCount = maps:fold(fun(_, Pids, Acc) -> Acc + length(Pids) end, 0, Available),
    {reply, AvailCount + maps:size(InUse), State};

handle_call(timeout, _From, #state{keepalive_timeout=Timeout}=State) ->
    {reply, Timeout, State};

handle_call(max_connections, _From, #state{max_connections=MaxConn}=State) ->
    {reply, MaxConn, State};

handle_call({count, Key}, _From, #state{available=Available}=State) ->
    Count = case maps:find(Key, Available) of
                {ok, Pids} -> length(Pids);
                error -> 0
            end,
    {reply, Count, State};

handle_call({checkout, Key, Requester, Opts}, _From, State) ->
    #state{name=PoolName, metrics=Engine, max_connections=MaxConn,
           available=Available, in_use=InUse} = State,

    TotalInUse = maps:size(InUse),

    ?report_trace("pool: checkout request", [{pool, PoolName}, {key, Key},
        {total_in_use, TotalInUse}, {max_conn, MaxConn}]),

    case find_available(Key, Available) of
        {ok, Pid, Available2} ->
            %% Found an available connection - update owner to new requester
            ?report_debug("pool: reusing connection", [{pool, PoolName}, {pid, Pid}]),
            ok = hackney_conn:set_owner(Pid, Requester),
            _ = metrics:update_meter(Engine, [hackney_pool, PoolName, take_rate], 1),
            InUse2 = maps:put(Pid, Key, InUse),
            {reply, {ok, Pid}, State#state{available=Available2, in_use=InUse2}};
        none when TotalInUse >= MaxConn ->
            %% At max connections - return error immediately (no queue)
            %% Note: In the new architecture, load_regulation handles waiting
            ?report_trace("pool: at max connections", [{pool, PoolName}, {in_use, TotalInUse}]),
            {reply, {error, checkout_timeout}, State};
        none ->
            %% No available connection, start a new one
            ?report_trace("pool: starting new connection", [{pool, PoolName}]),
            case start_connection(Key, Requester, Opts, State) of
                {ok, Pid, State2} ->
                    InUse2 = maps:put(Pid, Key, State2#state.in_use),
                    {reply, {ok, Pid}, State2#state{in_use=InUse2}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({checkin_sync, Pid}, _From, State) ->
    %% Synchronous checkin - caller waits for acknowledgement
    State2 = do_checkin(Pid, State),
    {reply, ok, State2}.

handle_cast({checkin, _PoolInfo, Pid}, State) ->
    State2 = do_checkin(Pid, State),
    {noreply, State2};

handle_cast({set_maxconn, MaxConn}, State) ->
    {noreply, State#state{max_connections=MaxConn}};

handle_cast({set_timeout, NewTimeout}, State) ->
    %% Cap at 2 seconds
    Capped = min(NewTimeout, ?DEFAULT_KEEPALIVE_TIMEOUT),
    {noreply, State#state{keepalive_timeout=Capped}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonRef, process, Pid, Reason}, State) ->
    %% Connection process died, remove from available and in_use
    #state{name=PoolName, available=Available, in_use=InUse, pid_monitors=PidMonitors} = State,
    ?report_trace("pool: connection DOWN", [{pool, PoolName}, {pid, Pid}, {reason, Reason},
        {was_in_use, maps:is_key(Pid, InUse)}]),

    %% Remove from available pools
    Available2 = maps:fold(
        fun(Key, Pids, Acc) ->
            case lists:delete(Pid, Pids) of
                [] -> maps:remove(Key, Acc);
                Pids2 -> maps:put(Key, Pids2, Acc)
            end
        end,
        Available,
        Available
    ),

    %% Remove from in_use and release load_regulation slot if it was checked out
    InUse2 = case maps:take(Pid, InUse) of
        {Key, NewInUse} ->
            %% Connection died while in use - release the load_regulation slot
            {Host, Port, _Transport} = Key,
            hackney_load_regulation:release(Host, Port),
            NewInUse;
        error ->
            InUse
    end,

    PidMonitors2 = maps:remove(Pid, PidMonitors),
    {noreply, State#state{available=Available2, in_use=InUse2, pid_monitors=PidMonitors2}};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{name=PoolName, metrics=Engine, available=Available, pid_monitors=PidMonitors}) ->
    %% Stop all available connections
    maps:foreach(
        fun(_Key, Pids) ->
            lists:foreach(fun(Pid) ->
                catch hackney_conn:stop(Pid)
            end, Pids)
        end,
        Available
    ),

    %% Demonitor all
    maps:foreach(fun(_Pid, MonRef) -> erlang:demonitor(MonRef, [flush]) end, PidMonitors),

    %% delete pool metrics
    ok = delete_metrics(Engine, PoolName),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

connection_key(Host0, Port, Transport) ->
    Host = string_compat:to_lower(Host0),
    {Host, Port, Transport}.

find_available(Key, Available) ->
    case maps:find(Key, Available) of
        {ok, [Pid | Rest]} ->
            Available2 = case Rest of
                [] -> maps:remove(Key, Available);
                _ -> maps:put(Key, Rest, Available)
            end,
            %% Verify connection is still alive and usable
            case is_process_alive(Pid) of
                true ->
                    %% is_ready checks both state and socket health in one call
                    case hackney_conn:is_ready(Pid) of
                        {ok, connected} -> {ok, Pid, Available2};
                        {ok, closed} ->
                            %% Connection closed, try reconnect
                            case hackney_conn:connect(Pid) of
                                ok -> {ok, Pid, Available2};
                                _ -> find_available(Key, Available2)
                            end;
                        _ -> find_available(Key, Available2)
                    end;
                false ->
                    find_available(Key, Available2)
            end;
        {ok, []} ->
            none;
        error ->
            none
    end.

start_connection(Key, Owner, Opts, State) ->
    {Host, Port, Transport} = Key,
    ConnectTimeout = proplists:get_value(connect_timeout, Opts, 8000),
    RecvTimeout = proplists:get_value(recv_timeout, Opts, infinity),
    IdleTimeout = State#state.keepalive_timeout,
    SslOpts = proplists:get_value(ssl_options, Opts, []),
    ConnectOpts = proplists:get_value(connect_options, Opts, []),

    ConnOpts = #{
        host => Host,
        port => Port,
        transport => Transport,
        connect_timeout => ConnectTimeout,
        recv_timeout => RecvTimeout,
        idle_timeout => IdleTimeout,
        ssl_options => SslOpts,
        connect_options => ConnectOpts,
        pool_pid => self(),
        owner => Owner
    },

    case hackney_conn_sup:start_conn(ConnOpts) of
        {ok, Pid} ->
            %% Connect the connection
            case hackney_conn:connect(Pid) of
                ok ->
                    %% Monitor the process
                    MonRef = erlang:monitor(process, Pid),
                    PidMonitors = maps:put(Pid, MonRef, State#state.pid_monitors),
                    {ok, Pid, State#state{pid_monitors=PidMonitors}};
                {error, Reason} ->
                    catch hackney_conn:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Process a checkin - return connection to pool
%% Only TCP connections are stored. SSL upgraded connections are closed.
%% Always releases the load_regulation slot since connection is no longer in use.
do_checkin(Pid, State) ->
    #state{in_use=InUse, available=Available, pid_monitors=PidMonitors} = State,

    %% Get the key from in_use and remove
    case maps:take(Pid, InUse) of
        {Key, InUse2} ->
            %% Release load_regulation slot - connection is no longer in active use
            {Host, Port, _Transport} = Key,
            hackney_load_regulation:release(Host, Port),

            %% Check if connection is still alive
            case is_process_alive(Pid) of
                true ->
                    %% Check if this is an SSL upgraded connection
                    %% SSL connections should NOT be pooled (security requirement)
                    case catch hackney_conn:is_upgraded_ssl(Pid) of
                        true ->
                            %% SSL upgraded - close instead of storing
                            catch hackney_conn:stop(Pid),
                            %% Remove monitor if exists
                            PidMonitors2 = case maps:take(Pid, PidMonitors) of
                                {MonRef, PM} ->
                                    erlang:demonitor(MonRef, [flush]),
                                    PM;
                                error -> PidMonitors
                            end,
                            State#state{in_use=InUse2, pid_monitors=PidMonitors2};
                        _ ->
                            %% TCP connection - store in pool
                            %% Set owner to pool so connection doesn't die if previous requester crashes.
                            %% Use async version to avoid deadlock when called from checkin_sync
                            hackney_conn:set_owner_async(Pid, self()),
                            Available2 = maps:update_with(Key, fun(Pids) -> [Pid | Pids] end, [Pid], Available),

                            %% Ensure we're monitoring this pid
                            PidMonitors2 = case maps:is_key(Pid, PidMonitors) of
                                true -> PidMonitors;
                                false ->
                                    MonRef = erlang:monitor(process, Pid),
                                    maps:put(Pid, MonRef, PidMonitors)
                            end,

                            State#state{in_use=InUse2, available=Available2, pid_monitors=PidMonitors2}
                    end;
                false ->
                    State#state{in_use=InUse2}
            end;
        error ->
            State
    end.

init_metrics(PoolName) ->
    Engine = hackney_metrics:get_engine(),
    _ = metrics:new(Engine, histogram, [hackney_pool, PoolName, take_rate]),
    _ = metrics:new(Engine, counter, [hackney_pool, PoolName, no_socket]),
    _ = metrics:new(Engine, histogram, [hackney_pool, PoolName, in_use_count]),
    _ = metrics:new(Engine, histogram, [hackney_pool, PoolName, free_count]),
    _ = metrics:new(Engine, histogram, [hackney_pool, PoolName, queue_count]),
    Engine.

delete_metrics(Engine, PoolName) ->
    _ = metrics:delete(Engine, [hackney_pool, PoolName, take_rate]),
    _ = metrics:delete(Engine, [hackney_pool, PoolName, no_socket]),
    _ = metrics:delete(Engine, [hackney_pool, PoolName, in_use_count]),
    _ = metrics:delete(Engine, [hackney_pool, PoolName, free_count]),
    _ = metrics:delete(Engine, [hackney_pool, PoolName, queue_count]),
    ok.

handle_stats(State) ->
    #state{name=PoolName, max_connections=Max, available=Available,
           in_use=InUse} = State,
    AvailCount = maps:fold(fun(_, Pids, Acc) -> Acc + length(Pids) end, 0, Available),
    [{name, PoolName},
     {max, Max},
     {in_use_count, maps:size(InUse)},
     {free_count, AvailCount},
     {queue_count, 0}].  % No more queue
