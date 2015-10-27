-module(hackney_pool).

%% public API
-export([request/6,
         release/1]).

%% hackney internal api
-export([register_connector/2]).
-export([start_link/2]).

%% pool internals
-export([init/3]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-include("hackney.hrl").
-include("hackney_socket.hrl").

-record(state, {
        name :: atom(),
        parent :: pid(),
        idle_timeout :: non_neg_integer(),
        group_limit = 8,
        max_conns = 200,
        refs,
        sockets,
        connectors=[],
        next=0}).

request(Pool, Group, Host, Port, Options, Timeout) when is_pid(Pool) ->
    Expires = case Timeout of
                  infinity -> infinity;
                  Timeout -> now_in_ms() + (Timeout * 1000)
              end,
    Tag = erlang:monitor(process, Pool),
    From = {self(), Tag},
    Req = {Host, Port, Options, Timeout},
    catch Pool ! {request, From, Group, Req, Expires},
    receive
        {Tag, {ok, HS}} ->
            {ok, HS};
        {'DOWN', Tag, _, _, Reason} ->
            {error, Reason};
        {Tag, Error} -> 
            Error
    after Timeout ->
        {error, timeout}
    end;
request(Pool, Group, Host, Port, Options, Timeout) ->
    request(hackney_server:get_pool(Pool), Group, Host, Port, Options, Timeout).


release(HS) ->
    hackney_socket:setopts(HS, [{active, false}]),
    case sync_socket(HS) of
        true -> release1(HS);
        false ->
            catch hackney_socket:close(HS),
            {error, sync_error}
    end.


release1(#hackney_socket{pool=Pool}=HS) ->
    hackney_socket:controlling_process(HS, Pool),
    Tag = erlang:monitor(process, Pool),
    Pool ! {release, {self(), Tag}, HS},
    %% we directly pass the socket control to a pending connection if any. if
    %% the pool can't accept the socket we kill it.
    receive
        {Tag, ok} ->
            ok;
        {'DOWN', Tag, _, _, Reason} ->
            hackney_socket:close(HS),
            {error, Reason};
        {Tag, Error} ->
            hackney_socket:close(HS),
            Error
    end.

register_connector(Pool, Pid) ->
    Pool ! {register_connector, Pid},
    receive Pool -> ok end.



start_link(Ref, Opts) ->
    proc_lib:start_link(?MODULE, init, [self(), Ref, Opts]).


init(Parent, Ref, Opts) ->
    true = hackney_server:register_pool(Ref, self()),

    IdleTimeout = hackney_util:get_opt(idle_timeout, Opts, ?DEFAULT_IDLE_TIMEOUT),
    GroupLimit = hackney_util:get_opt(group_limit, Opts, ?DEFAULT_GROUP_LIMIT),
    MaxConns =  hackney_util:get_opt(max_connections, Opts, ?DEFAULT_MAX_CONNS),

    %% init tables
    Refs = ets:new(hackney_pool_refs, [bag]),
    Sockets = ets:new(hackney_pool_sockets, [set]),

    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{name = Ref,
                parent = Parent,
                idle_timeout = IdleTimeout,
                group_limit = GroupLimit,
                max_conns = MaxConns,
                refs = Refs,
                sockets = Sockets}).


loop(State=#state{parent=Parent}) ->
    receive
        {request, {Pid, Tag} = From, Group, CReq, Expires} ->
            case reuse_connection(Group, State) of
                {ok, HS} ->
                    hackney_socket:controlling_process(HS, Pid),
                    Pid ! {Tag, {ok, HS}},
                    loop(State);
                no_socket ->
                    %% insert pending request
                    ets:insert(State#state.refs, {{pending, Group}, {From, Expires}}),
                    State2 = request_socket(Group, CReq, State),
                    loop(State2)
            end;
        {preconnect, {Pid, Tag}, Group, CReq} ->
            State2 = request_socket(Group, CReq, State),
            Pid ! {Tag, ok},
            loop(State2);
        {release, {Pid, Tag}, HS} ->
            Reply = release_socket(HS, State),
            Pid ! {Tag, Reply},
            loop(State);
        {register_connector, Pid} ->
            _ = erlang:monitor(process, Pid),
            Pid ! self(),
            loop(State#state{connectors=[Pid | State#state.connectors]});
        {timeout, Sock} ->
            delete_socket(Sock, State),
            loop(State);
        {tcp, Sock, _} -> 
            delete_socket(Sock, State),
            loop(State);
        {tcp_closed, Sock} -> 
            delete_socket(Sock, State),
            loop(State);
        {tcp_error, Sock, _} -> 
            delete_socket(Sock, State),
            loop(State);
        {ssl, Sock, _} -> 
            delete_socket(Sock, State),
            loop(State);
        {ssl_closed, Sock} -> 
            delete_socket(Sock, State),
            loop(State);
        {ssl_error, Sock, _} -> 
            delete_socket(Sock, State),
            loop(State);
        {'DOWN', _, process, Pid, Reason} ->
            error_logger:error_msg(
                    "hackney connector failure failure; "
                    "~p crashed with reason: ~p~n", [Pid, Reason]),
            Connectors2 = State#state.connectors -- [Pid],
            loop(State#state{connectors=Connectors2});
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {system, From, Request} ->
            system:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                     State);
        Msg ->
            error_logger:error_msg(
              "Hackney pool ~p received an unexped message ~p",
              [State#state.name, Msg])
    end.

system_continue(_, _, State) ->
    loop(State).

system_terminate(Reason, _, _, _State) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.


reuse_connection(Group, State) ->
    Refs = ets:lookup(State#state.refs, {conns, Group}),
    reuse_connection1(Refs, State).


reuse_connection1([], _State) ->
    no_socket;
reuse_connection1([{Group, #hackney_socket{sock=S}=HS} | Rest], State) ->
    [{_, _, HS, T}] = ets:lookup(State#state.sockets, S),

    ets:delete_object(State#state.refs, {Group, HS}),
    ets:delete(State#state.sockets, S),
    cancel_timer(T, S),
    hackney_socket:setopts(HS, [{active, false}]),
    case sync_socket(HS) of
        true ->
            {ok, HS};
        false ->
            reuse_connection1(Rest, State)
    end.

request_socket(Group, Req, State) ->
    %% we simply balance connections tasks between connectors using an RR algorithm
    {Connector, State2} = pick_connector(State),
    Connector ! {connect, Group, Req},
    State2.


pick_connector(State=#state{connectors=Connectors}) ->
    [Connector | Rest] = Connectors,
    {Connector, State#state{connectors = Rest ++ [Connector]}}.


release_socket(#hackney_socket{group=Group}=HS, State) ->
    Pending = ets:lookup(State#state.refs, {pending, Group}),
    case Pending of
        [] -> cache_socket(HS, State);
        _ -> dispatch_socket(Pending, HS, State)
    end.


cache_socket(HS=#hackney_socket{sock=Sock, group=Group}, State) ->
    Conns = ets:lookup(State#state.refs, {conns, Group}),
    TotalGroup = length(Conns),
    TotalConns = ets:info(State#state.sockets, size),

    case {TotalConns < State#state.max_conns, TotalGroup < State#state.group_limit} of
        {true, true} ->
            T = erlang:send_after(State#state.idle_timeout, self(), {timeout, Sock}),
            ets:insert(State#state.sockets, {Sock, Group, HS, T}),
            ets:insert(State#state.refs, {{conns, Group}, HS}),
            ok;
        {_, _} ->
            {error, max_conns}
    end.


dispatch_socket([{_Key, {{Pid, Tag}, Expires}}=Obj | Rest], HS, State) ->
    Now = now_in_ms(),
    %% delete the pending connection from the list
    ets:delete_object(State#state.refs, Obj),
    %% if the pending connection expired, we ignore it, else 
    %% we try to give the socket to it.
    if
        Expires > Now ->
            case (catch hackney_socket:controlling_process(HS, Pid)) of
                ok ->
                    catch Pid ! {Tag, {ok, HS}},
                    ok;
                _Else ->
                    catch hackney_socket:close(HS),
                    dispatch_socket(Rest, HS, State)
            end;
        true ->
            dispatch_socket(Rest, HS, State)
    end.


delete_socket(Sock, State) ->
    case ets:lookup(State#state.sockets, Sock) of
        [] -> ok;
        [{Sock, Group, HS, T}] ->
            catch hackney_socket:close(HS),
            cancel_timer(T, Sock),
            ets:delete(State#state.sockets, Sock),
            ets:delete_obj(State#state.refs, {{conns, Group}, HS}),
            ok
    end.


cancel_timer(Timer, Socket) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Socket} -> ok
            after
                0 -> ok
            end;
        _ -> ok
    end.

%% check that no events from the sockets is received after setting it to
%% passive.
sync_socket(#hackney_socket{transport=Transport, sock=Sock} ) ->
    {Msg, MsgClosed, MsgError} = Transport:messages(),
    receive
        {Msg, Sock, _} -> false;
        {MsgClosed, Sock} -> false;
        {MsgError, Sock, _} -> false
    after 0 ->
              true
    end.

now_in_ms() -> timer:now_diff(os:timestamp(), {0, 0, 0}).