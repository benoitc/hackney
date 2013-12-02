%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%

-module(hackney_stream).

-export([start_link/3]).

%% internal
-export([async_recv/4, maybe_continue/4]).
-export([init/4,
         system_continue/3,
         system_terminate/4,
         system_code_change/4]).

-include("hackney.hrl").

start_link(Owner, Ref, Client) ->
    proc_lib:start_link(?MODULE, init, [self(), Owner, Ref, Client]).

init(Parent, Owner, Ref, #client{transport=Transport,
                                 socket=Socket}=Client) ->
    %% pass the control to the process
    Transport:controlling_process(Socket, self()),
    %% register the stream
    ok = proc_lib:init_ack(Parent, {ok, self()}),

    try
        stream_loop(Parent, Owner, Ref, Client)
    catch Class:Reason ->
        Owner ! {Ref, {error, {unknown_error,
                               {{Class, Reason, erlang:get_stacktrace()},
                                "An unexpected error occurred."}}}}
    end.


stream_loop(Parent, Owner, Ref, #client{response_state=St}=Client) ->
    hackney_manager:update_state(Client),

    Resp = case St of
        waiting -> hackney_response:stream_status(Client);
        on_status -> hackney_response:stream_status(Client);
        on_header -> hackney_response:stream_headers(Client);
        on_body -> hackney_response:stream_body(Client)
    end,

    case Resp of
        {more, Client2} ->
            hackney_manager:update_state(Client2),
            async_recv(Parent, Owner, Ref, Client2);
        {ok, StatusInt, Reason, Client2} ->
            hackney_manager:update_state(Client2),
            Owner ! {Ref, {status, StatusInt, Reason}},
            maybe_continue(Parent, Owner, Ref, Client2);
        {ok, {headers, Headers}, Client2} ->
            hackney_manager:update_state(Client2),
            Owner ! {Ref, {headers, Headers}},
            maybe_continue(Parent, Owner, Ref, Client2);
        {ok, Data, Client2} ->
            hackney_manager:update_state(Client2),
            Owner ! {Ref, Data},
            maybe_continue(Parent, Owner, Ref, Client2);
        {done, Client2} ->
            hackney_manager:update_state(Client2),
            Owner ! {Ref, done};
        {error, _Reason} = Error ->
            hackney_manager:handle_error(Client),
            Owner ! {Ref, Error}
    end.

maybe_continue(Parent, Owner, Ref, #client{transport=Transport,
                                           socket=Socket,
                                           async=true}=Client) ->
    receive
        {Ref, resume} ->
            stream_loop(Parent, Owner, Ref, Client);
        {Ref, pause} ->
            erlang:hibernate(?MODULE, maybe_continue, [Parent, Owner, Ref,
                                                       Client]);
        {Ref, stop_async, From} ->
            hackney_manager:update_state(Client),
            Transport:setopts(Socket, [{active, false}]),
            Transport:controlling_process(Socket, From),
            From ! {Ref, ok};
        {Ref, close} ->
            hackney_manager:update_state(Client),
            hackney_response:close(Client);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {stream_loop, Parent, Owner, Ref, Client});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else])

    after 0 ->
            stream_loop(Parent, Owner, Ref, Client)
    end;
maybe_continue(Parent, Owner, Ref, #client{transport=Transport,
                                    socket=Socket,
                                    async=once}=Client) ->
    receive
        {Ref, stream_next} ->
            stream_loop(Parent, Owner, Ref, Client);
        {Ref, stop_async, From} ->
            hackney_manager:update_state(Client),
            Transport:setopts(Socket, [{active, false}]),
            Transport:controlling_process(Socket, From),
            From ! {Ref, ok};
        {Ref, close} ->
            hackney_manager:update_state(Client),
            hackney_reponse:close(Client);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {maybe_continue, Parent, Owner, Ref,
                                   Client});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else])
    after 5000 ->

        erlang:hibernate(?MODULE, maybe_continue, [Parent, Owner, Ref,
                                                   Client])

    end.

async_recv(Parent, Owner, Ref, #client{transport=Transport,
                                socket=Sock,
                                buffer=Buffer,
                                recv_timeout=Timeout}=Client) ->

    {OK, Closed, Error} = Transport:messages(),
    Transport:setopts(Sock, [{active, once}]),
    %% some useful info
    #client{version=Version, clen=CLen, te=TE} = Client,
    receive
        {Ref, resume} ->
            async_recv(Parent, Owner, Ref, Client);
        {Ref, stream_next} ->
            async_recv(Parent, Owner, Ref, Client);
        {Ref, pause} ->
            %% make sure that the proces won't be awoken by a tcp msg
            Transport:setopts(Sock, [{active, false}]),
            %% hibernate
            erlang:hibernate(?MODULE, async_recv, [Parent, Owner, Ref, Client]);
        {Ref, close} ->
            Transport:close(Sock);
        {Ref, stop_async, From} ->
            ets:delete(hackney_streams, Ref),
            Transport:setopts(Sock, [{active, false}]),
            Transport:controlling_process(Sock, From),
            From ! {Ref, Client};
        {OK, Sock, Data} ->
            NewBuffer =  << Buffer/binary, Data/binary >>,
            stream_loop(Parent, Owner, Ref, Client#client{buffer=NewBuffer});
        {Closed, Sock} ->
            case Client#client.response_state of
                on_body when Version =:= {1, 0}, CLen =:= nil ->
                    Owner ! {Ref, Buffer};
                on_body when TE =:= <<"identity">> ->
                    Owner ! {Ref, Buffer};
                on_body ->
                    Owner ! {Ref, {error, {closed, Buffer}}};
                _ ->
                    Owner ! {error, closed}
            end,
            Transport:close(Sock);
        {Error, Sock, Reason} ->
            Owner ! {error, Reason},
            Transport:close(Sock);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {async_recv, Parent, Owner, Ref, Client});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else])
    after Timeout ->
        Owner ! {error, {closed, timeout}},
        Transport:close(Sock)
    end.

system_continue(_, _, {maybe_continue, Parent, Owner, Ref, Client}) ->
    maybe_continue(Parent, Owner, Ref, Client);
system_continue(_, _, {stream_loop, Parent, Owner, Ref, Client}) ->
    stream_loop(Parent, Owner, Ref, Client);
system_continue(_, _, {async_recv, Parent, Owner, Ref, Client}) ->
    async_recv(Parent, Owner, Ref, Client).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, {_, _, _, _Ref, _}) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.
