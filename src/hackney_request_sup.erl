%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.


-module(hackney_request_sup).
-behaviour(gen_server).

-export([new_request/1,
         cancel_request/1,
         close_request/1,
         controlling_process/2]).

-export([get_state/1]).
-export([increment_bytes_sent/2,
         increment_bytes_received/2]).

-export([start_link/0]).


%% private gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("hackney.hrl").

-record(request, {ref,
                  pid,
                  state,
                  bytes_sent=0,
                  bytes_received=0}).


new_request(InitialState) ->
    gen_server:call(?MODULE, {new_request, self(), InitialState}).

cancel_request(Ref) ->
    gen_server:call(?MODULE, {cancel_request, Ref}).

close_request(Ref) ->
    gen_server:call(?MODULE, {close_request, Ref}).

nb_requests(Ref) ->


controlling_process(Ref, Pid) ->
    Reply = gen_server:call(?MODULE, {controlling_process, Ref, Pid}),
    case Reply of
        {ok, {Transport, Socket}} ->
            Transport:controlling_process(Socket, Pid);
        Error ->
            Error
    end.


get_state(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            not_found;
        [#request{state=State}] ->
            {ok, State}
    end.

increment_bytes_sent(Ref, Bytes) ->
    ets:update_counter(?MODULE, {Ref, #request.bytes_sent}, Bytes).

increment_bytes_received(Ref, Bytes) ->
    ets:update_counter(?MODULE, {Ref, #request.bytes_received}, Bytes).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(?MODULE, [set, {keypos, #request.ref}, public, named_table,
                      {read_concurrency, true}]),

    {ok, dict:new()}.


handle_call({new_request, Pid, InitialState}, _From, Children) ->
    {Ref, NChildren} = make_request(Pid, InitialState, Children),
    {reply, Ref, NChildren};

handle_call({controlling_process, Ref, Pid}, _From, Children) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            {reply, req_not_found, Children};
        [#request{pid=Owner, state=St}=Req] ->
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
        [#request{pid=Pid, state=St}] ->
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
        [#request{pid=Pid, state=St}] ->
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
                [#request{}] ->
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
                   state=InitialState},

    link(Children),
    NChildren = dict:store(Pid, Ref, Children),
    ets:insert(Req, ?MODULE),
    {Ref, NChildren}.
