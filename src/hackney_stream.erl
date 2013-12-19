%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%

-module(hackney_stream).

-export([start_link/3]).

%% internal
-export([async_recv/5, maybe_continue/4]).
-export([init/4,
         system_continue/3,
         system_terminate/4,
         system_code_change/4]).

-include("hackney.hrl").

start_link(Owner, Ref, Client) ->
    proc_lib:start_link(?MODULE, init, [self(), Owner, Ref, Client]).

init(Parent, Owner, Ref, Client) ->
    %% register the stream
    ok = proc_lib:init_ack(Parent, {ok, self()}),

    Parser = hackney_http:parser([response]),
    try
        stream_loop(Parent, Owner, Ref, Client#client{parser=Parser})
    catch Class:Reason ->
        Owner ! {Ref, {error, {unknown_error,
                               {{Class, Reason, erlang:get_stacktrace()},
                                "An unexpected error occurred."}}}}
    end.

stream_loop(Parent, Owner, Ref, Client) ->
    case parse(Client) of
        {loop, Client2} ->
            stream_loop(Parent, Owner, Ref, Client2);
        {more, Client2, Rest} ->
            async_recv(Parent, Owner, Ref, Client2, Rest);
        {ok, StatusInt, Reason, Client2} ->
            maybe_redirect(Parent, Owner, Ref, StatusInt, Reason,
                           Client2);
        {ok, {headers, Headers}, Client2} ->
            Owner ! {Ref, {headers, Headers}},
            maybe_continue(Parent, Owner, Ref, Client2);
        {ok, Data, Client2} ->
            Owner ! {Ref, Data},
            maybe_continue(Parent, Owner, Ref, Client2);
        done ->
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
            hackney_manager:update_state(Client#client{async=false}),
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


%% if follow_redirect is true, we are parsing the headers to fetch the
%% location. If we can still redirect, send a message with the location
%% to the receiver so he can eventually start a new request.
%%
%% redirect messages:
%% - {redirect, To, Headers}
%% - {see_other, To, Headers} for status 303 and POST requests.
maybe_redirect(Parent, Owner, Ref, StatusInt, Reason,
               #client{transport=Transport,
                       socket=Socket,
                       method=Method,
                       follow_redirect=true}=Client) ->
    case lists:member(StatusInt, [301, 302, 307]) of
        true ->
            Transport:setopts(Socket, [{active, false}]),
            case parse(Client) of
                {loop, Client2} ->
                    maybe_redirect(Parent, Owner, Ref, StatusInt,
                                   Reason, Client2);
                {ok, {headers, Headers}, Client2} ->
                    Location = hackney:redirect_location(Headers),
                    case {Location, lists:member(Method, [get, head])} of
                        {undefined, _} ->
                            Owner ! {error, invalid_redirection},
                            hackney_manager:handle_error(Client2);
                        {_, _} ->
                            case hackney_response:skip_body(Client2) of
                                {ok, Client3} ->
                                    hackney_manager:update_state(Client3),
                                    Owner ! {Ref, {redirect, Location,
                                                   Headers}};
                                Error ->
                                    Owner ! {Ref, Error},
                                    hackney_manager:handle_error(Client2)
                            end
                    end;
                {error, Error} ->
                    hackney_manager:handle_error(Client),
                    Owner ! {Ref, {error, Error}}
            end;
        false when StatusInt =:= 303, Method =:= post ->
            Transport:setopts(Socket, [{active, false}]),
            case parse(Client) of
                {loop, Client2} ->
                    maybe_redirect(Parent, Owner, Ref, StatusInt,
                                   Reason, Client2);
                {ok, {headers, Headers}, Client2} ->
                    case hackney:redirect_location(Headers) of
                        undefined ->
                            Owner ! {error, invalid_redirection},
                            hackney_manager:handle_error(Client2);
                        Location ->
                            case hackney_response:skip_body(Client2) of
                                {ok, Client3} ->
                                    hackney_manager:update_state(Client3),
                                    Owner ! {Ref, {see_other, Location,
                                                   Headers}};
                                Error ->
                                    Owner ! {Ref, Error},
                                    hackney_manager:handle_error(Client2)
                            end
                    end;
                {error, Error} ->
                    hackney_manager:handle_error(Client),
                    Owner ! {Ref, {error, Error}}
            end;
        _ ->
            Owner ! {Ref, {status, StatusInt, Reason}},
            maybe_continue(Parent, Owner, Ref, Client)
    end;
maybe_redirect(Parent, Owner, Ref, StatusInt, Reason, Client) ->
    Owner ! {Ref, {status, StatusInt, Reason}},
    maybe_continue(Parent, Owner, Ref, Client).


async_recv(Parent, Owner, Ref,
           #client{transport=Transport,
                   socket=Sock,
                   recv_timeout=Timeout}=Client, Buffer) ->

    {OK, Closed, Error} = Transport:messages(Sock),
    Transport:setopts(Sock, [{active, once}]),
    %% some useful info
    #client{version=Version, clen=CLen, te=TE} = Client,
    receive
        {Ref, resume} ->
            async_recv(Parent, Owner, Ref, Client, Buffer);
        {Ref, stream_next} ->
            async_recv(Parent, Owner, Ref, Client, Buffer);
        {Ref, pause} ->
            %% make sure that the proces won't be awoken by a tcp msg
            Transport:setopts(Sock, [{active, false}]),
            %% hibernate
            erlang:hibernate(?MODULE, async_recv, [Parent, Owner, Ref,
                                                   Client, Buffer]);
        {Ref, close} ->
            Transport:close(Sock);
        {Ref, stop_async, From} ->
            hackney_manager:update_state(Client#client{async=false}),
            Transport:setopts(Sock, [{active, false}]),
            Transport:controlling_process(Sock, From),
            From ! {Ref, ok};
        {OK, Sock, Data} ->
            stream_loop(Parent, Owner, Ref, Client#client{buffer=Data});
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
system_continue(_, _, {async_recv, Parent, Owner, Ref, Client, Buffer}) ->
    async_recv(Parent, Owner, Ref, Client, Buffer).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, {_, _, _, _Ref, _}) ->
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.


parse(#client{parser=Parser, buffer=Buffer}=Client) ->
    Res = hackney_http:execute(Parser, Buffer),
    process(Res, Client#client{buffer= <<>>}).

process({more, NParser}, Client) ->
    NClient = update_client(NParser, Client),
    {more, NClient, <<>>};
process({more, NParser, Buffer}, Client) ->
    NClient = update_client(NParser, Client),
    {more, NClient, Buffer};
process({response, Version, Status, Reason, NParser}, Client) ->
    NClient = update_client(NParser, Client#client{version=Version}),
    {ok, Status, Reason, NClient};
process({header, {Key, Value}=KV, NParser},
        #client{partial_headers=Headers}=Client) ->
    %% store useful headers
    Client1 = case hackney_util:to_lower(Key) of
        <<"content-length">> ->
            CLen = list_to_integer(binary_to_list(Value)),
            Client#client{clen=CLen};
        <<"transfer-encoding">> ->
            Client#client{te=hackney_util:to_lower(Value)};
        <<"connection">> ->
            Client#client{connection=hackney_util:to_lower(Value)};
        <<"content-type">> ->
            Client#client{ctype=hackney_util:to_lower(Value)};
        <<"location">> ->
            Client#client{location=Value};
        _ ->
            Client
    end,
    NHeaders = [KV | Headers],
    NClient = update_client(NParser, Client1#client{partial_headers=NHeaders}),
    {loop, NClient};
process({headers_complete, NParser},
        #client{partial_headers=Headers}=Client) ->
    NClient = update_client(NParser, Client#client{partial_headers=[]}),
    {ok, {headers, lists:reverse(Headers)}, NClient};
process({ok, Data, NParser}, Client) ->
    NClient = update_client(NParser, Client),
    {ok, Data, NClient};
process({done, Rest}, Client) ->
    update_client(nil, Client#client{buffer=Rest}),
    done;
process(done, Client) ->
    update_client(nil, Client),
    done;
process({error, Reason}, _Client) ->
    {error, Reason};
process(Error, _Client) ->
    {error, Error}.

update_client(Parser, Client) ->
    NClient = Client#client{parser=Parser},
    hackney_manager:update_state(NClient),
    NClient.
