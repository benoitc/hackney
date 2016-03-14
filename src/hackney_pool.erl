%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% Copyright (c) 2012-2015, Benoît Chesneau <benoitc@e-engura.org>

%% @doc pool of sockets connections
%%
-module(hackney_pool).
-behaviour(gen_server).

%% PUBLIC API
-export([start/0,
         checkout/4,
         checkin/2]).

-export([start_pool/2,
         stop_pool/1,
         find_pool/1,
         notify/2]).


-export([count/1, count/2,
         max_connections/1,
         set_max_connections/2,
         timeout/1,
         set_timeout/2,
         child_spec/2]).

-export([start_link/2]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("hackney.hrl").
-include_lib("hackney_internal.hrl").

-record(state, {
        name,
        metrics,
        max_connections,
        timeout,
        clients = dict:new(),
        queues = dict:new(),  % Dest => queue of Froms
        connections = dict:new(),
        sockets = dict:new(),
        nb_waiters=0}).


start() ->
    %% NB this is first called from hackney_sup:start_link
    %%    BEFORE the hackney_pool ETS table exists
    ok.

%% @doc fetch a socket from the pool
checkout(Host0, Port, Transport, #client{options=Opts}=Client) ->
    Host = string:to_lower(Host0),
    Pid = self(),
    RequestRef = Client#client.request_ref,
    Name = proplists:get_value(pool, Opts, default),
    Pool = find_pool(Name, Opts),
    case gen_server:call(Pool, {checkout, {Host, Port, Transport},
                                           Pid, RequestRef}, infinity) of
        {ok, Socket, Owner} ->
            CheckinReference = {Host, Port, Transport},
            {ok, {Name, RequestRef, CheckinReference, Owner, Transport}, Socket};
        {error, no_socket, Owner} ->
            CheckinReference = {Host, Port, Transport},
            {error, no_socket, {Name, RequestRef, CheckinReference, Owner,
                                Transport}};

        {error, Reason} ->
            {error, Reason}
    end.

%% @doc release a socket in the pool
checkin({_Name, Ref, Dest, Owner, Transport}, Socket) ->
    Transport:setopts(Socket, [{active, false}]),
    case sync_socket(Transport, Socket) of
        true ->
            case Transport:controlling_process(Socket, Owner) of
                ok ->
                    gen_server:call(Owner, {checkin, Ref, Dest, Socket, Transport},
                                    infinity);
                _Error ->
                    catch Transport:close(Socket),
                    ok
            end;
        false ->
            catch Transport:close(Socket),
            ok
    end.


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
                    supervisor:delete_child(hackney_sup, Name),
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



%%
%%  util functions for this pool
%%

%% @doc return a child spec suitable for embeding your pool in the
%% supervisor
child_spec(Name, Options0) ->
    Options = [{name, Name} | Options0],
    {Name, {hackney_pool, start_link, [Name, Options]},
      permanent, 10000, worker, [hackney_pool]}.


%% @doc get the number of connections in the pool
count(Name) ->
    gen_server:call(find_pool(Name), count).

%% @doc get the number of connections in the pool for `{Host0, Port, Transport}'
count(Name, {Host0, Port, Transport}) ->
    Host = string:to_lower(Host0),
    gen_server:call(find_pool(Name), {count, {Host, Port, Transport}}).

%% @doc get max pool size
max_connections(Name) ->
    gen_server:call(find_pool(Name), max_connections).

%% @doc change the pool size
set_max_connections(Name, NewSize) ->
    gen_server:cast(find_pool(Name), {set_maxconn, NewSize}).

%% @doc get timeout
timeout(Name) ->
    gen_server:call(find_pool(Name), timeout).

%% @doc change the connection timeout
%%
set_timeout(Name, NewTimeout) ->
    gen_server:cast(find_pool(Name), {set_timeout, NewTimeout}).

%% @private
%%
%%
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

init([Name, Options]) ->
    process_flag(priority, high),
    case lists:member({seed,1}, ssl:module_info(exports)) of
        true ->
            % Make sure that the ssl random number generator is seeded
            % This was new in R13 (ssl-3.10.1 in R13B vs. ssl-3.10.0 in R12B-5)
            apply(ssl, seed, [crypto:rand_bytes(255)]);
        false ->
            ok
    end,

    MaxConn = case proplists:get_value(pool_size, Options) of
        undefined ->
            proplists:get_value(max_connections, Options);
        Size ->
            Size
    end,
    Timeout = proplists:get_value(timeout, Options),

    %% register the module
    ets:insert(?MODULE, {Name, self()}),

    %% initialize metrics
    Engine = init_metrics(Name),

    {ok, #state{name=Name, metrics=Engine, max_connections=MaxConn,
                timeout=Timeout}}.

handle_call(count, _From, #state{sockets=Sockets}=State) ->
    {reply, dict:size(Sockets), State};
handle_call(timeout, _From, #state{timeout=Timeout}=State) ->
    {reply, Timeout, State};
handle_call(max_connections, _From, #state{max_connections=MaxConn}=State) ->
    {reply, MaxConn, State};
handle_call({checkout, Dest, Pid, RequestRef}, From, State) ->
    #state{name=PoolName,
           metrics = Engine,
           max_connections=MaxConn,
           clients=Clients,
           queues = Queues,
           nb_waiters = NbWaiters} = State,

    {Reply, State2} = find_connection(Dest, Pid, State),
    case Reply of
        {ok, _Socket, _Owner} ->
            State3 = monitor_client(Dest, RequestRef, State2),
            update_usage(State3),
            {reply, Reply, State3};
        no_socket ->
            case dict:size(Clients) >= MaxConn of
                true ->
                    Queues2 = add_to_queue(Dest, From, RequestRef, Queues),
                    NbWaiters2 = NbWaiters + 1,
                    metrics:update_histogram(Engine,
                                             [hackney_pool, PoolName, queue_count],
                                             NbWaiters2),
                    {noreply, State2#state{queues = Queues2,
                                           nb_waiters=NbWaiters2}};
                false ->
                    State3 = monitor_client(Dest, RequestRef, State2),
                    update_usage(State3),
                    {reply, {error, no_socket, self()}, State3}
            end
    end;
handle_call({checkin, Ref, Dest, Socket, Transport}, From, State) ->
    gen_server:reply(From, ok),
    Clients2 = case dict:find(Ref, State#state.clients) of
                   {ok, Dest} ->
                       dict:erase(Ref, State#state.clients);
                   error ->
                        State#state.clients
               end,
    State2 = case Transport:peername(Socket) of
                 {ok, {_Adress, _Port}} ->
                     %% socket is not closed, try to deliver it or store it
                     deliver_socket(Socket, Dest, State#state{clients=Clients2});
                 Error ->
                     %% socket may be half-closed, close it and return
                     catch Transport:close(Socket),
                     ?report_trace("checkin: socket is not ok~n", [{socket, Socket}, {peername, Error}]),
                     State#state{clients=Clients2}
             end,
    update_usage(State2),
    {noreply, State2};

handle_call({count, Key}, _From, #state{connections=Conns}=State) ->
    Size = case dict:find(Key, Conns) of
        {ok, Sockets} ->
            length(Sockets);
        error ->
            0
    end,
    {reply, Size, State}.

handle_cast({set_maxconn, MaxConn}, State) ->
    {noreply, State#state{max_connections=MaxConn}};
handle_cast({set_timeout, NewTimeout}, State) ->
    {noreply, State#state{timeout=NewTimeout}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Socket}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({tcp, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({tcp_closed, Socket}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({ssl, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({ssl_closed, Socket}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({tcp_error, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({ssl_error, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({'DOWN', Ref, request, _Pid, _Reason}, State) ->
    case dict:find(Ref, State#state.clients) of
        {ok, Dest} ->
            Clients2 = dict:erase(Ref, State#state.clients),
            case queue_out(Dest, State#state.queues) of
                empty ->
                    {noreply, State#state{clients = Clients2}};
                {ok, {From, Ref2}, Queues2} ->
                    NbWaiters = State#state.nb_waiters - 1,
                    metrics:update_histogram(State#state.metrics,
                                             [hackney_pool, State#state.name, queue_count], NbWaiters),
                    gen_server:reply(From, {error, no_socket, self()}),
                    State2 = State#state{queues = Queues2, clients = Clients2,
                                        nb_waiters=NbWaiters},
                    {noreply, monitor_client(Dest, Ref2, State2)}
            end;
        error ->
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

terminate(_Reason, #state{name=PoolName, metrics=Engine, sockets=Sockets}) ->
    %% close any sockets in the pool
    lists:foreach(fun({Socket, {{_, _, Transport}, Timer}}) ->
                cancel_timer(Socket, Timer),
                Transport:close(Socket)
        end, dict:to_list(Sockets)),

    %% delete pool metrics
    delete_metrics(Engine, PoolName),
    ok.

%% internals

find_connection({_Host, _Port, Transport}=Dest, Pid,
                #state{connections=Conns, sockets=Sockets}=State) ->
    case dict:find(Dest, Conns) of
        {ok, [S | Rest]} ->
            Transport:setopts(S, [{active, false}]),
            case sync_socket(Transport, S) of
                true ->
                    case Transport:controlling_process(S, Pid) of
                        ok ->
                            {_, Timer} = dict:fetch(S, Sockets),
                            cancel_timer(S, Timer),
                            NewConns = update_connections(Rest, Dest, Conns),
                            NewSockets = dict:erase(S, Sockets),
                            NewState = State#state{connections=NewConns,
                                                   sockets=NewSockets},
                            {{ok, S, self()}, NewState};
                        {error, badarg} ->
                            %% something happened here normally the PID died,
                            %% but make sure we still have the control of the
                            %% process
                            catch Transport:controlling_process(S, self()),
                            %% and then close it
                            find_connection(Dest, Pid,
                                            remove_socket(S,  State));
                        _Else ->
                            find_connection(Dest, Pid, remove_socket(S, State))
                    end;
                false ->
                    ?report_trace("checkout: socket unsynced~n", []),
                    find_connection(Dest, Pid, remove_socket(S, State))
            end;
        _Else ->
            {no_socket, State}
    end.

remove_socket(Socket, #state{connections=Conns, sockets=Sockets}=State) ->
    metrics:update_histogram(State#state.metrics,
                             [hackney, State#state.name, free_count],
                             dict:size(Sockets)),
    case dict:find(Socket, Sockets) of
        {ok, {{_Host, _Port, Transport}=Key, Timer}} ->
            cancel_timer(Socket, Timer),
            catch Transport:close(Socket),
            ConnSockets = lists:delete(Socket, dict:fetch(Key, Conns)),
            NewConns = update_connections(ConnSockets, Key, Conns),
            NewSockets = dict:erase(Socket, Sockets),
            State#state{connections=NewConns, sockets=NewSockets};
        error ->
            State
    end.


store_socket({_Host, _Port, Transport} = Dest, Socket,
             #state{timeout=Timeout, connections=Conns,
                    sockets=Sockets}=State) ->
    Timer = erlang:send_after(Timeout, self(), {timeout, Socket}),
    %% make sure to close the socket if anything is received while we are in
    %% the pool.
    Transport:setopts(Socket, [{active, once}, {packet, 0}]),
    ConnSockets = case dict:find(Dest, Conns) of
        {ok, OldSockets} ->
            [Socket | OldSockets];
        error -> [Socket]
    end,
    State#state{connections = dict:store(Dest, ConnSockets, Conns),
                sockets = dict:store(Socket, {Dest, Timer}, Sockets)}.

update_connections([], Key, Connections) ->
    dict:erase(Key, Connections);
update_connections(Sockets, Key, Connections) ->
    dict:store(Key, Sockets, Connections).

cancel_timer(Socket, Timer) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Socket} -> ok
            after
                0 -> ok
            end;
        _ -> ok
    end.

%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_to_queue({_Host, _Port, _Transport} = Dest, From, Ref, Queues) ->
    case dict:find(Dest, Queues) of
        error ->
            dict:store(Dest, queue:in({From, Ref}, queue:new()), Queues);
        {ok, Q} ->
            dict:store(Dest, queue:in({From, Ref}, Q), Queues)
    end.

%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
queue_out({_Host, _Port, _Transport} = Dest, Queues) ->
    case dict:find(Dest, Queues) of
        error ->
            empty;
        {ok, Q} ->
            {{value, {From, Ref}}, Q2} = queue:out(Q),
            Queues2 = case queue:is_empty(Q2) of
                true ->
                    dict:erase(Dest, Queues);
                false ->
                    dict:store(Dest, Q2, Queues)
            end,
            {ok, {From, Ref}, Queues2}
    end.

%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
deliver_socket(Socket, {_, _, Transport} = Dest, State) ->
    case queue_out(Dest, State#state.queues) of
        empty ->
            store_socket(Dest, Socket, State);
        {ok, {{PidWaiter, _} = FromWaiter, Ref}, Queues2} ->
            NbWaiters = State#state.nb_waiters - 1,
            metrics:update_histogram(State#state.metrics,
                                     [hackney_pool, State#state.name, queue_count],
                                     NbWaiters),
            case Transport:controlling_process(Socket, PidWaiter) of
                ok ->
                    gen_server:reply(FromWaiter, {ok, Socket, self()}),
                    monitor_client(Dest, Ref,
                                   State#state{queues = Queues2,
                                               nb_waiters=NbWaiters});
                _Error ->
                    % Something wrong, close the socket
                    catch Transport:close(Socket),
                    %% and let the waiter connect to a new one
                    gen_server:reply(FromWaiter, {error, no_socket, self()}),
                    State#state{queues = Queues2, nb_waiters = NbWaiters}
            end
    end.

%% check that no events from the sockets is received after setting it to
%% passive.
sync_socket(Transport, Socket) ->
    {Msg, MsgClosed, MsgError} = Transport:messages(Socket),
    receive
        {Msg, Socket, _} -> false;
        {MsgClosed, Socket} -> false;
        {MsgError, Socket, _} -> false
    after 0 ->
              true
    end.

%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
monitor_client(Dest, Ref, State) ->
    Clients2 = dict:store(Ref, Dest, State#state.clients),
    State#state{clients = Clients2}.


init_metrics(PoolName) ->
    %% get metrics module
    Engine = metrics:init(hackney_util:mod_metrics()),

    %% initialise metrics
    metrics:new(Engine, histogram, [hackney_pool, PoolName, take_rate]),
    metrics:new(Engine, counter, [hackney_pool, PoolName, no_socket]),
    metrics:new(Engine, histogram, [hackney_pool, PoolName, in_use_count]),
    metrics:new(Engine, histogram, [hackney_pool, PoolName, free_count]),
    metrics:new(Engine, histogram, [hackney_pool, PoolName, queue_counter]),
    Engine.

delete_metrics(Engine, PoolName) ->
    metrics:delete(Engine, [hackney_pool, PoolName, take_rate]),
    metrics:delete(Engine, [hackney_pool, PoolName, no_socket]),
    metrics:delete(Engine, [hackney_pool, PoolName, in_use_count]),
    metrics:delete(Engine, [hackney_pool, PoolName, free_count]),
    metrics:delete(Engine, [hackney_pool, PoolName, queue_counter]).


update_usage(#state{name=PoolName, metrics=Engine, sockets=Sockets,
                    clients=Clients}) ->
    metrics:update_histogram(Engine, [hackney_pool, PoolName,in_use_count],
                         dict:size(Clients) - 1),
    metrics:update_histogram(Engine, [hackney_pool, PoolName, free_count],
                         dict:size(Sockets) - 1).
