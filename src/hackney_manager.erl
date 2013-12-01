%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.


-module(hackney_manager).
-behaviour(gen_server).

-export([new_request/1,
         cancel_request/1,
         close_request/1,
         controlling_process/2]).

-export([get_state/1, get_state/2,
         update_state/1, update_state/2,
         handle_error/1]).


-export([start_link/0]).


%% private gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("hackney.hrl").

-record(request, {ref,
                  pid,
                  state}).


new_request(InitialState) ->
    gen_server:call(?MODULE, {new_request, self(), InitialState}).

cancel_request(#client{request_ref=Ref}) ->
    cancel_request(Ref);
cancel_request(Ref) when is_reference(Ref) ->
    gen_server:call(?MODULE, {cancel_request, Ref}).


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
    ets:new(?MODULE, [set, {keypos, 1}, public, named_table,
                      {write_concurrency, true}]),

    {ok, dict:new()}.


handle_call({new_request, Pid, InitialState}, _From, Children) ->
    {Ref, NChildren} = make_request(Pid, InitialState, Children),
    {reply, Ref, NChildren};

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

handle_call({cancel_request, Ref}, _From, Children) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {reply, req_not_found, Children};
        [{Ref, #request{pid=Pid, state=St}}] ->
            ets:delete(?MODULE, Ref),
            NChildren = case dict:is_key(Pid, Children) of
                true ->
                    unlink(Pid),
                    dict:erase(Pid, Children);
                false ->
                    Children
            end,

            #client{transport=Transport, socket=Socket,
                    buffer=Buffer}=St,
            {reply, {Transport, Socket, Buffer}, NChildren}
    end;

handle_call({close_request, Ref}, _From, Children) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {reply, req_not_found, Children};
        [{Ref, #request{pid=Pid, state=St}}] ->
            ets:delete(?MODULE, Ref),
            NChildren = case dict:is_key(Pid, Children) of
                true ->
                    unlink(Pid),
                    dict:erase(Pid, Children);
                false ->
                    Children
            end,

            #client{transport=Transport, socket=Socket,
                    state=Status}=St,
            case Status of
                done ->
                    ok;
                _ ->
                    Transport:controlling_process(Socket, self()),
                    Transport:close(Socket)
            end,

            {reply, ok, NChildren}
    end.

handle_cast(_Msg, Children) ->
    {noreply, Children}.

handle_info({'EXIT', Pid, _Reason}, Children) ->
    NChildren = case dict:find(Pid, Children) of
        {value, Ref} ->
            case ets:lookup(?MODULE, Ref) of
                [] ->
                    ok;
                [{Ref, #request{}}] ->
                    ets:delete(?MODULE, Ref)
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
