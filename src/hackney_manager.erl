%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.


-module(hackney_manager).
-behaviour(gen_server).

-export([new_request/1,
         start_async_response/1,
         stop_async_response/1,
         cancel_request/1,
         close_request/1,
         controlling_process/2]).

-export([get_state/1, get_state/2,
         update_state/1, update_state/2,
         handle_error/1]).

-export([async_response_pid/1,
         with_async_response_pid/2]).

-export([start_link/0]).


%% private gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("hackney.hrl").

-record(request, {ref,
                  pid,
                  async_pid=nil,
                  state}).


new_request(InitialState) ->
    gen_server:call(?MODULE, {new_request, self(), InitialState}).

cancel_request(#client{request_ref=Ref}) ->
    cancel_request(Ref);
cancel_request(Ref) when is_reference(Ref) ->
    gen_server:call(?MODULE, {cancel_request, Ref, self()}).

close_request(Ref) ->
    gen_server:call(?MODULE, {close_request, Ref}).

controlling_process(Ref, Pid) ->
    Reply = gen_server:call(?MODULE, {controlling_process, Ref, Pid}),
    case Reply of
        {ok, {Transport, Socket}} ->
            Transport:controlling_process(Socket, Pid);
        Error ->
            Error
    end.

start_async_response(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            req_not_found;
        [{Ref, #request{state=Client}=Req}] ->
            #client{transport=Transport,
                    socket=Socket} = Client,

            case gen_server:call(?MODULE, {start_async_response, Req}) of
                {ok, Pid} ->
                    Transport:controlling_process(Socket, Pid);
                Error ->
                    Error
            end
    end.

stop_async_response(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {error, req_not_found};
        [{_Ref, #request{async_pid=nil}}] ->
            {error, req_not_async};
        [{_Ref, #request{async_pid=Pid}=Req}] ->
            Pid ! {Ref, stop_async, self()},
            receive
                {Ref, ok} ->
                    ets:insert(?MODULE, {Ref, Req#request{async_pid=nil}}),
                    {ok, Ref};
                Error ->
                    Error
            after 5000 ->
                    {error, timeout}
            end
    end.

async_response_pid(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {error, req_not_found};
        [{_Ref, #request{async_pid=nil}}] ->
            {error, req_not_async};
        [{_Ref, #request{async_pid=Pid}}] ->
            {ok, Pid}
    end.

with_async_response_pid(Ref, Fun) ->
    case async_response_pid(Ref) of
        {ok, Pid} ->
            Fun(Pid);
        Error ->
            Error
    end.

get_state(#client{request_ref=Ref}) ->
    get_state(Ref);

get_state(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            req_not_found;
        [{Ref, #request{state=State}}] ->
            State
    end.

get_state(Ref, Fun) ->
    case get_state(Ref) of
        req_not_found -> {error, req_not_found};
        State -> Fun(State)
    end.

update_state(#client{request_ref=Ref}=NState) ->
    update_state(Ref, NState).

update_state(Ref, NState) ->
    case ets:lookup(?MODULE, Ref) of
        [] -> ok;
        [{Ref, Req}] ->
            true = ets:insert(?MODULE, {Ref, Req#request{state=NState}}),
            ok
    end.

handle_error(#client{request_ref=Ref, dynamic=true}) ->
    close_request(Ref);

handle_error(#client{request_ref=Ref, transport=Transport,
                    socket=Socket}) ->
    case ets:lookup(?MODULE, Ref) of
        [] -> ok;
        [{Ref, #request{state=State}}] ->

            Transport:controlling_process(Socket, self()),
            Transport:close(Socket),
            NState = State#client{socket=nil,
                                  state=closed},
            true = ets:insert(?MODULE, {Ref, NState}),
            ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(hackney_pool, [
                    named_table,
                    set,
                    public
            ]),
    ets:new(?MODULE, [
                    set,
                    {keypos, 1},
                    public,
                    named_table,
                    {write_concurrency, true}
            ]),
    process_flag(trap_exit, true),
    {ok, dict:new()}.


handle_call({new_request, Pid, InitialState}, _From, Children) ->
    {Ref, NChildren} = make_request(Pid, InitialState, Children),
    {reply, Ref, NChildren};
handle_call({start_async_response, #request{ref=Ref}=Req}, _From, Children) ->
    case do_start_async_response(Req, Children) of
        {ok, #request{async_pid=Pid}=NReq, Children2} ->
            true = ets:insert(?MODULE, {Ref, NReq}),
            NChildren = dict:store(Pid, Ref, Children2),
            {reply, {ok, Pid}, NChildren};
        Error ->
            {reply, Error, Children}
    end;
handle_call({controlling_process, Ref, Pid}, _From, Children) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {reply, req_not_found, Children};
        [{Ref, #request{pid=Owner, state=St}=Req}] ->
            Children1 = case dict:is_key(Owner, Children) of
                true ->
                    unlink(Owner),
                    dict:erase(Owner, Children);
                false ->
                    Children
            end,
            ets:insert(?MODULE, Req#request{pid=Pid}),
            NChildren = dict:store(Pid, Ref, Children1),
            link(Pid),

            #client{transport=Transport, socket=Socket} = St,

            {reply, {ok, {Transport, Socket}}, NChildren}
    end;

handle_call({cancel_request, Ref, Owner}, _From, Children) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {reply, req_not_found, Children};
        [{Ref, #request{pid=Pid, state=St}=Req}] ->
            ets:delete(?MODULE, Ref),
            Children1 = case dict:is_key(Pid, Children) of
                true ->
                    unlink(Pid),
                    dict:erase(Pid, Children);
                false ->
                    Children
            end,

            case maybe_close_async_response(Req, Children1) of
                {ok, NChildren} ->
                    #client{transport=Transport,
                            socket=Socket,
                            buffer=Buffer,
                            response_state=RespState}=St,
                    case Transport:controlling_process(Socket, Owner) of
                        ok ->
                            {reply, {Transport, Socket, Buffer,
                                     RespState}, NChildren};
                        Error ->
                            {reply, Error, NChildren}
                    end;
                {Error, NChildren} ->
                    {reply, Error, NChildren}
            end
    end;

handle_call({close_request, Ref}, _From, Children) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {reply, req_not_found, Children};
        [{Ref, #request{pid=Pid, state=St}=Req}] ->
            ets:delete(?MODULE, Ref),
            Children1 = case dict:is_key(Pid, Children) of
                true ->
                    unlink(Pid),
                    dict:erase(Pid, Children);
                false ->
                    Children
            end,

            %% close the async request if any
            {Reply, NChildren} = maybe_close_async_response(Req, Children1),

            %% make sure we close the socket
            #client{transport=Transport, socket=Socket,
                    state=Status}=St,
            case Status of
                done ->
                    ok;
                _ when Socket /= nil ->
                    catch Transport:controlling_process(Socket, self()),
                    catch Transport:close(Socket);
                _ ->
                    ok
            end,

            {reply, Reply, NChildren}
    end.

handle_cast(_Msg, Children) ->
    {noreply, Children}.

handle_info({'EXIT', Pid, Reason}, Children) ->
    NChildren = case dict:find(Pid, Children) of
        {value, Ref} ->
            case ets:lookup(?MODULE, Ref) of
                [] ->
                    dict:erase(Pid, Children);
                [{Ref, Req}] ->
                    handle_exit(Pid, Req, Reason, Children)
            end,
            dict:erase(Pid, Children);
        _ ->
            Children
    end,
    {noreply, NChildren};

handle_info(_Info, Children) ->
    {noreply, Children}.


code_change(_OldVsn, Ring, _Extra) ->
   {ok, Ring}.

terminate(_Reason, _Ring) ->
    ok.

make_request(Pid, InitialState, Children) ->
    Ref = make_ref(),
    Req = #request{ref=Ref,
                   pid=Pid,
                   state=InitialState#client{request_ref=Ref}},

    link(Pid),
    NChildren = dict:store(Pid, Ref, Children),
    ets:insert(?MODULE, {Ref, Req}),
    {Ref, NChildren}.


do_start_async_response(#request{ref=Ref, pid=Owner, state=Client}=Req,
                       Children) ->
    {StreamTo, NChildren} = case Client#client.stream_to of
        false ->
            {Owner, Children};
        To ->
            link(To),
            Children1 = case dict:is_key(Owner, Children) of
                true ->
                    unlink(Owner),
                    dict:erase(Owner, Children);
                false ->
                    Children
            end,
            {To, dict:store(To, Ref, Children1)}

    end,
    case catch hackney_stream:start_link(StreamTo, Ref, Client) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Req#request{pid=StreamTo, async_pid=Pid}, NChildren};
        {error, What} ->
            {error, What};
        What ->
            {error, What}
    end.


%% handle ext.
handle_exit(AsyncPid, #request{ref=Ref, pid=Pid, async_pid=AsyncPid}=Req,
            Reason, Children) ->
    %% the async request exited, tell it to the parent.
    case Reason of
        normal ->
            ok;
        _ ->
            Pid ! {'DOWN', Ref, Reason}
    end,
    %% store the new request reference
    ets:insert(?MODULE, {Ref, Req#request{async_pid=nil}}),

    %% delete the pid from the children
    dict:erase(AsyncPid, Children);
handle_exit(Pid, #request{ref=Ref, pid=Pid, async_pid=nil}, _Reason,
            Children) ->
    %% delete the reference, the parent exited
    ets:delete(?MODULE, Ref),
    %% delete the pid from the children
    dict:erase(Pid, Children);
handle_exit(Pid, #request{pid=Pid, async_pid=AsyncPId}=Req,
            Reason, Children) ->

    %% delete the async response process from the children
    NChildren = dict:erase(AsyncPId, Children),
    %% terminate the async request
    terminate_async_response(Req),

    %% go deleting the reference
    handle_exit(Pid, Req#request{async_pid=nil}, Reason, NChildren).

monitor_child(Pid) ->
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
        {'EXIT', Pid, Reason} ->
            receive
                {'DOWN', _, process, Pid, _} ->
                    {error, Reason}
            end
    after 0 ->
            ok
    end.

terminate_async_response(Req) ->
    terminate_async_response(Req, shutdown).

terminate_async_response(#request{async_pid=Pid}=Req, Reason) ->
    case monitor_child(Pid) of
        ok ->
            exit(Pid, Reason),
            wait_async_response(Req, Pid);
        Error ->
            Error
    end.

wait_async_response(_Req, Pid) ->
    receive
        {'DOWN', _MRef, process, Pid, _Reason} ->
            ok
    end.

maybe_close_async_response(#request{async_pid=nil}, Children) ->
    {ok, Children};
maybe_close_async_response(#request{async_pid=Pid}=Req, Children)
        when is_pid(Pid) ->
    case dict:is_key(Pid, Children) of
        true ->
            NChildren = dict:erase(Pid, Children),
            case terminate_async_response(Req) of
                ok ->
                    {ok, NChildren};
                Error ->
                    {Error, NChildren}
            end;
        false ->
            {ok, Children}
    end.
