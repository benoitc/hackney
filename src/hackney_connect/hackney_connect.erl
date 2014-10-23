%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2014 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_connect).

-export([connect/3, connect/4, connect/5,
         create_connection/4, create_connection/5,
         maybe_connect/1,
         reconnect/4,
         set_sockopts/2,
         close/1,
         is_pool/1]).

-include("hackney.hrl").


connect(Transport, Host, Port) ->
    connect(Transport, Host, Port, []).

connect(Transport, Host, Port, Options) ->
    connect(Transport, Host, Port, Options, false).

connect(Transport, Host, Port, Options, Dynamic) when is_binary(Host) ->
    connect(Transport, binary_to_list(Host), Port, Options, Dynamic);
connect(Transport, Host, Port, Options, Dynamic) ->
    case create_connection(Transport, hackney_idna:to_ascii(Host), Port,
                           Options, Dynamic) of
        {ok, #client{request_ref=Ref}} ->
            {ok, Ref};
        Error ->
            Error
    end.


%% @doc create a connection and return a client state
create_connection(Transport, Host, Port, Options) ->
    create_connection(Transport, Host, Port, Options, true).

create_connection(Transport, Host, Port, Options, Dynamic)
        when is_list(Options) ->
    Netloc = case {Transport, Port} of
                 {hackney_tcp_transport, 80}  -> list_to_binary(Host);
                 {hackney_ssl_transport, 443} -> list_to_binary(Host);
                 _ ->
                     iolist_to_binary([Host, ":", integer_to_list(Port)])
             end,
    %% default timeout
    Timeout = proplists:get_value(recv_timeout, Options, infinity),
    FollowRedirect = proplists:get_value(follow_redirect, Options, false),
    MaxRedirect = proplists:get_value(max_redirect, Options, 5),
    ForceRedirect = proplists:get_value(force_redirect, Options, false),
    Async =  proplists:get_value(async, Options, false),
    StreamTo = proplists:get_value(stream_to, Options, false),

    %% initial state
    InitialState = #client{transport=Transport,
                           host=Host,
                           port=Port,
                           netloc=Netloc,
                           options=Options,
                           dynamic=Dynamic,
                           recv_timeout=Timeout,
                           follow_redirect=FollowRedirect,
                           max_redirect=MaxRedirect,
                           force_redirect=ForceRedirect,
                           async=Async,
                           stream_to=StreamTo,
                           buffer = <<>>},
    %% if we use a pool then checkout the connection from the pool, else
    %% connect the socket to the remote
    case is_pool(InitialState) of
        false ->
            %% the client won't use any pool
            do_connect(Host, Port, Transport, InitialState);
        true ->
            socket_from_pool(Host, Port, Transport, InitialState)
    end.


%% @doc connect a socket and create a client state.
%%
maybe_connect(#client{state=closed, redirect=nil}=Client) ->
    %% the socket has been closed, reconnect it.
    #client{transport=Transport,
            host=Host,
            port=Port} = Client,
    reconnect(Host, Port, Transport, Client);
maybe_connect(#client{state=closed, redirect=Redirect}=Client) ->
    %% connection closed after a redirection, reinit the options and
    %% reconnect it.
    {Transport, Host, Port, Options} = Redirect,
    Client1 = Client#client{options=Options,
                            redirect=nil},
    reconnect(Host, Port, Transport, Client1);
maybe_connect(#client{redirect=nil}=Client) ->
    {ok, Client};
maybe_connect(#client{redirect=Redirect}=Client) ->
    #client{socket=Socket, socket_ref=Ref, pool_handler=Handler}=Client,
    %% the connection was redirected. If we are using a pool, checkin
    %% the socket and create the newone, else close the current socket
    %% and create a new one.
    case is_pool(Client) of
        false ->
            close(Client);
        true ->
            Handler:checkin(Ref, Socket)
    end,
    %% reinit the options and reconnect the client
    {Transport, Host, Port, Options} = Redirect,
    Client1 = Client#client{options=Options,
                            redirect=nil},
    reconnect(Host, Port, Transport, Client1).


%% @doc add set sockets options in the client
set_sockopts(#client{transport=Transport, socket=Skt}, Options) ->
    Transport:setopts(Skt, Options).


%% @doc close the client
%%
%%
close(#client{socket=nil}=Client) ->
    Client#client{state = closed};
close(#client{transport=Transport, socket=Skt}=Client) ->
    Transport:close(Skt),
    Client#client{state = closed, socket=nil};
close(Ref) when is_reference(Ref) ->
    hackney_manager:close_request(Ref).


%% @doc get current pool pid or name used by a client if needed
is_pool(#client{options=Opts}) ->
    UseDefaultPool = use_default_pool(),
    case proplists:get_value(pool, Opts) of
        false ->
            false;
        undefined when UseDefaultPool =:= true ->
            true;
        undefined ->
            false;
        _ ->
            true
    end.


reconnect(Host, Port, Transport, State) ->
    %% if we use a pool then checkout the connection from the pool, else
    %% connect the socket to the remote
    case is_pool(State) of
        false ->
            %% the client won't use any pool
            do_connect(Host, Port, Transport, State);
        true ->
            socket_from_pool(Host, Port, Transport, State)
    end.

%%
%% internal functions
%%

socket_from_pool(Host, Port, Transport, #client{request_ref=ReqRef0}=Client) ->
    PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),

    case PoolHandler:checkout(Host, Port, Transport, Client) of
        {ok, Ref, Skt} ->
            Client1 = Client#client{socket=Skt,
                                    socket_ref=Ref,
                                    pool_handler=PoolHandler,
                                    state = connected},

            FinalClient = case is_reference(ReqRef0) of
                true ->
                    ok = hackney_manager:take_control(ReqRef0, Client1),
                    Client1;
                false ->
                    RequestRef = hackney_manager:new_request(Client1),
                    Client1#client{request_ref=RequestRef}
            end,
            {ok, FinalClient};
        {error, no_socket, Ref} ->
            do_connect(Host, Port, Transport, Client#client{socket_ref=Ref});
        Error ->
            Error
    end.

do_connect(Host, Port, Transport, #client{options=Opts,
                                          request_ref=Ref0}=Client) ->
    ConnectOpts0 = proplists:get_value(connect_options, Opts, []),
    ConnectTimeout = proplists:get_value(connect_timeout, Opts, 8000),

    %% handle ipv6
    ConnectOpts1 = case hackney_util:is_ipv6(Host) of
        true ->
            [inet6 | ConnectOpts0];
        false ->
            ConnectOpts0
    end,

    ConnectOpts = case {Transport, proplists:get_value(ssl_options, Opts)} of
        {hackney_ssl_transport, undefined} ->
            case proplists:get_value(insecure, Opts) of
                true ->
                    ConnectOpts1 ++ [{verify, verify_none},
                             {reuse_sessions, true}];
                _ ->
                    ConnectOpts1
            end;
        {hackney_ssl_transport, SslOpts} ->
            ConnectOpts1 ++ SslOpts;
        {_, _} ->
            ConnectOpts1
    end,

    case Transport:connect(Host, Port, ConnectOpts, ConnectTimeout) of
        {ok, Skt} ->
            Client1 = Client#client{socket=Skt,
                                    state = connected},
            FinalClient = case is_reference(Ref0) of
                true ->
                    ok = hackney_manager:take_control(Ref0, Client1),
                    Client1;
                false ->
                    Ref = hackney_manager:new_request(Client1),
                    Client1#client{request_ref=Ref}
            end,
            {ok, FinalClient};
        {error, timeout} ->
            {error, connect_timeout};
        Error ->
            Error
    end.

use_default_pool() ->
    case application:get_env(hackney, use_default_pool) of
        {ok, Val} ->
            Val;
        _ ->
            true
    end.
