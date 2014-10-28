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
         take_control/2,
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

-define(REFS, hackney_manager_refs).

new_request(InitialState) ->
    %% initialize the request
    Ref = make_ref(),
    Req = #request{ref=Ref,
                   state=InitialState#client{request_ref=Ref}},
    ets:insert(?MODULE, {Ref, Req}),
    %% supervise the process owner
    ok = gen_server:call(?MODULE, {new_request, self(), Ref}),
    Ref.



cancel_request(#client{request_ref=Ref}) ->
    cancel_request(Ref);
cancel_request(Ref) when is_reference(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            req_not_found;
        [{Ref, #request{state=Client}}] ->
            #client{transport=Transport, socket=Socket,
                    buffer=Buffer, response_state=RespState} = Client,

            %% only the owner can cancel the request
            case Transport:controlling_process(Socket, self()) of
                ok ->
                    %% remove the request
                    ets:delete(?MODULE, Ref),

                    %% stop to monitor the request
                    case gen_server:call(?MODULE, {cancel_request, Ref}) of
                        ok ->
                            %% return the latest state
                            {ok, {Transport, Socket, Buffer, RespState}};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

close_request(#client{request_ref=Ref}) ->
    close_request(Ref);
close_request(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            req_not_found;
        [{Ref, #request{state=Client}}] ->
            #client{transport=Transport, socket=Socket,
                    state=Status} = Client,

            %% remove the request
            ets:delete(?MODULE, Ref),

            %% stop to monitor the request
            Reply = gen_server:call(?MODULE, {cancel_request, Ref}),

            case Status of
                done -> ok;
                _ when Socket /= nil ->
                    catch Transport:controlling_process(Socket, self()),
                    catch Transport:close(Socket),
                    ok;
                _ -> ok
            end,
            Reply
    end.

controlling_process(Ref, Pid) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            req_not_found;
        [{Ref, #request{state=Client}}] ->
            Reply = gen_server:call(?MODULE, {controlling_process, Ref, Pid}),
            case Reply of
                ok ->
                    #client{transport=Transport, socket=Socket} = Client,
                    Transport:controlling_process(Socket, Pid);
                Error ->
                    Error
            end
    end.

start_async_response(Ref) ->
    case ets:lookup(?MODULE, Ref) of
        [] ->
            req_not_found;
        [{Ref, #request{state=Client}}] ->
            #client{transport=Transport, socket=Socket,
                    stream_to=StreamTo} = Client,
            case gen_server:call(?MODULE, {start_async_response, Ref,
                                           StreamTo, Client}) of
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
        [{Ref, _Req}] ->
            gen_server:call(?MODULE, {stop_async_response, Ref, self()})
    end.

async_response_pid(Ref) ->
    case ets:lookup(?REFS, Ref) of
        [] ->
            {error, req_not_found};
        [{Ref, {_, nil}}] ->
            {error, req_not_async};
        [{Ref, {_, Pid}}] ->
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

take_control(Ref, NState) ->
    case ets:lookup(?MODULE, Ref) of
        [] -> {error, req_not_found};
        [{Ref, Req}] ->
            true = ets:insert(?MODULE, {Ref, Req#request{pid=self(),
                                                         state=NState}}),
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
                    {read_concurrency, true},
                    {write_concurrency, true}
            ]),

    ets:new(?REFS, [named_table, set, protected]),


    process_flag(trap_exit, true),
    %% return {ok, {Pids, Refs}}
    %% Pids are the managed pids
    %% Refs are the managed requests
    {ok, dict:new()}.


handle_call({new_request, Pid, Ref}, _From, Pids) ->
    %% link the request owner
    link(Pid),
    %% store the pid
    Pids2 = dict:store(Pid, {Ref, owner}, Pids),
    ets:insert(?REFS, {Ref, {Pid, nil}}),
    {reply, ok, Pids2};

handle_call({start_async_response, Ref, StreamTo, Client}, _From, State) ->
    %% start the stream and eventually update the owner of the request
    case do_start_async_response(Ref, StreamTo, Client, State) of
        {ok, Pid, NState} ->
            {reply, {ok, Pid}, NState};
        Error ->
            {reply, Error, State}
    end;

handle_call({stop_async_response, Ref, To}, _From, Pids) ->
    case ets:lookup(?REFS, Ref) of
        [] -> {reply, {ok, Ref}, Pids};
        [{Ref, {_Owner, nil}}] ->
            %% there is no async request to handle, just return
            {ok, Ref};
        [{Ref, {Owner, Stream}}] ->
            %% tell to the stream to stop
            Stream ! {Ref, stop_async, self()},
            receive
                {Ref, ok} ->
                    %% if the stream return, we unlink it and update the
                    %% state. if we stop the async request and want to use it
                    %% in another process, make sure to unlink the old owner
                    %% and link the new one.
                    unlink(Stream),
                    ets:insert(?REFS, {Ref, {To, nil}}),
                    Pids1 = dict:erase(Stream, Pids),

                    Pids2 = case To of
                                Owner ->
                                    %% same owner do nothing
                                    Pids1;
                                _ ->
                                    %% new owner, link it and un link the old
                                    %% one
                                    unlink(Owner),
                                    link(To),
                                    dict:store(To, {Ref, owner},
                                               dict:erase(Owner, Pids1))
                            end,

                    {reply, {ok, Ref}, Pids2};
                Error ->
                    {reply, Error, Pids}
            after 5000 ->
                      {reply, {error, timeout}, Pids}
            end
    end;

handle_call({controlling_process, Ref, Pid}, _From, Pids) ->
    case ets:lookup(?REFS, Ref) of
        [] -> {reply, badarg, Pids};
        [{Ref, {Pid, _}}] ->
            %% the request is already controlled by this process just return
            {reply, ok, Pids};
        [{Ref, {Owner, Stream}}] ->
            %% new owner, link it.
            unlink(Owner),
            link(Pid),
            Pids2 = dict:store(Pid, {Ref, owner}, dict:erase(Owner, Pids)),
            ets:insert(?REFS, {Ref, {Pid, Stream}}),
            {reply, ok, Pids2}
    end;

handle_call({cancel_request, Ref}, _From, Pids) ->
    case ets:lookup(?REFS, Ref) of
        [] -> {reply, badarg, Pids};
        [{Ref, {Owner, nil}}] ->
            %% no stream just cancel the request and unlink the owner.
            unlink(Owner),
            ets:delete(?REFS, Ref),
            Pids2 = dict:erase(Owner, Pids),
            {reply, ok, Pids2};
        [{Ref, {Owner, Stream}}] when is_pid(Stream) ->
            unlink(Owner),
            unlink(Stream),
            Pids2 = dict:erase(Stream, dict:erase(Owner, Pids)),
            ets:delete(?REFS, Ref),
            case terminate_async_response(Stream) of
                ok ->
                    {reply, ok, Pids2};
                Error ->
                    {reply, Error, Pids2}
            end
    end.

handle_cast(_Msg, Children) ->
    {noreply, Children}.

handle_info({'EXIT', Pid, Reason}, Pids) ->
    case dict:find(Pid, Pids) of
        {ok, PidInfo} ->
            handle_exit(Pid, PidInfo, Reason, Pids);
        _ ->
            {noreply, Pids}
    end;

handle_info(_Info, Children) ->
    {noreply, Children}.


code_change(_OldVsn, Ring, _Extra) ->
   {ok, Ring}.

terminate(_Reason, _Ring) ->
    ok.


do_start_async_response(Ref, StreamTo, Client, Pids) ->
    %% get current owner
    [{Ref, {Owner, _}}] = ets:lookup(?REFS, Ref),

    %% if not stream target we use the owner
    StreamTo2 = case StreamTo of
                    false -> Owner;
                    _ -> StreamTo
                end,

    %% start the stream process
    case catch hackney_stream:start_link(StreamTo2, Ref, Client) of
        {ok, Pid} when is_pid(Pid) ->
            ets:insert(?REFS, {Ref, {StreamTo2, Pid}}),
            Pids2 = case StreamTo2 of
                        Owner ->
                            dict:store(Pid, {Ref, stream}, Pids);
                        _ ->
                            %% unlink and replace the old owner by the new
                            %% target of the request
                            unlink(Owner),
                            Pids1 = dict:store(StreamTo2, {Ref, stream},
                                               dict:erase(Owner, Pids)),
                            %% store stthe stream
                            dict:store(Pid, {Ref, stream}, Pids1)
                    end,
            {ok, Pid, Pids2};
        {error, What} ->
            {error, What};
        What ->
            {error, What}
    end.


%% a stream exited
handle_exit(Pid, {Ref, stream}, Reason, Pids) ->
    %% delete the pid from our list
    Pids1 = dict:erase(Pid, Pids),

    case ets:lookup(?REFS, Ref) of
        [] ->
            %% ref already removed just return
            {noreply, Pids1};
        [{Ref, {Owner, Pid}}] ->
            %% unlink the owner
            unlink(Owner),
            Pids2 = dict:erase(Pid, Pids1),
            %% if anormal reason let the owner knows
            case Reason of
                normal ->
                    ok;
                _ ->
                    Owner ! {'DOWN', Ref, Reason}
            end,
            %% remove the reference
            ets:delete(?REFS, Ref),
            ets:delete(?MODULE, Ref),
            %% reply
            {noreply, Pids2}
    end;
%% owner exited
handle_exit(Pid, {Ref, owner}, _Reason, Pids) ->
    %% delete the pid from our list
    Pids1 = dict:erase(Pid, Pids),
    case ets:lookup(?REFS, Ref) of
        [] ->
            %% ref already removed just return
            {noreply, Pids1};
        [{Ref, {Pid, nil}}] ->
            %% no stream
            %% remove the reference
            ets:delete(?REFS, Ref),
            ets:delete(?MODULE, Ref),
            %% reply
            {noreply, Pids1};
        [{Ref, {Pid, Stream}}] ->
            unlink(Stream),
            Pids2 = dict:erase(Stream, Pids1),
            %% terminate the async stream
            terminate_async_response(Stream),
            %% remove the reference
            ets:delete(?REFS, Ref),
            ets:delete(?MODULE, Ref),
            {noreply, Pids2}
    end.

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

terminate_async_response(Stream) ->
    terminate_async_response(Stream, shutdown).

terminate_async_response(Stream, Reason) ->
    case monitor_child(Stream) of
        ok ->
            exit(Stream, Reason),
            wait_async_response(Stream);
        Error ->
            Error
    end.

wait_async_response(Stream) ->
    receive
        {'DOWN', _MRef, process, Stream, _Reason} ->
            ok
    end.
