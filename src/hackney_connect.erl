%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%
%%%
%%%
%%%

-module(hackney_connect).

-export([connect/1, connect/3, connect/4,
         create_connection/4,
         set_sockopts/2,
         close/1,
         is_pool/1]).

-include("hackney.hrl").

%% @doc connect a socket and create a client state.
connect(#client{state=connected, redirect=nil}=Client) ->
    {ok, Client};

connect(#client{state=connected, redirect=Redirect}=Client) ->
    #client{socket=Socket, socket_ref=Ref, pool_handler=Handler}=Client,

    case is_pool(Client) of
        false ->
            close(Client);
        true ->
            Handler:checkout(Ref, Socket)
    end,
    connect(Redirect);
connect(#client{state=closed, redirect=nil}=Client) ->
    #client{transport=Transport,
            host=Host,
            port=Port,
            options=Options} = Client,

    connect(Transport, Host, Port, Options);

connect(#client{state=closed, redirect=Redirect}) ->
    connect(Redirect);
connect({Transport, Host, Port, Options}) ->
    connect(Transport, Host, Port, Options).

connect(Transport, Host, Port) ->
    connect(Transport, Host, Port, []).

connect(Transport, Host, Port, Options) ->
    create_connection(Transport, Host, Port, Options).

create_connection(Transport, Host, Port, Options) when is_list(Options) ->
    Timeout = proplists:get_value(recv_timeout, Options, infinity),

    InitialState = #client{recv_timeout=Timeout,  options=Options},
    case is_pool(InitialState) of
        false ->
            %% the client won't use any pool
            do_connect(Host, Port, Transport, InitialState);
        true ->
            socket_from_pool(Host, Port, Transport, InitialState)
    end.

%% @doc add set sockets options in the client
set_sockopts(#client{transport=Transport, socket=Skt}, Options) ->
    Transport:setopts(Skt, Options).


%% @doc close the client
close(Client) ->
    hackney_response:close(Client).

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


%% internal functions
%%

socket_from_pool(Host, Port, Transport, #client{options=Opts}=Client) ->
    PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),

    case PoolHandler:checkout(Host, Port, Transport, Client) of
        {ok, Ref, Skt} ->
            FollowRedirect = proplists:get_value(follow_redirect,
                                                 Opts, false),
            MaxRedirect = proplists:get_value(max_redirect, Opts, 5),
            Async =  proplists:get_value(async, Opts, false),
            {ok, Client#client{transport=Transport,
                               host=Host,
                               port=Port,
                               socket=Skt,
                               socket_ref=Ref,
                               pool_handler=PoolHandler,
                               state = connected,
                               follow_redirect=FollowRedirect,
                               max_redirect=MaxRedirect,
                               async=Async}};
        {error, no_socket, Ref} ->
            do_connect(Host, Port, Transport, Client#client{socket_ref=Ref});
        Error ->
            Error
    end.

do_connect(Host, Port, Transport, #client{options=Opts}=Client) ->
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
            FollowRedirect = proplists:get_value(follow_redirect,
                                                 Opts, false),
            MaxRedirect = proplists:get_value(max_redirect, Opts, 5),
            ForceRedirect = proplists:get_value(force_redirect, Opts,
                                                false),
            Async =  proplists:get_value(async, Opts, false),

            {ok, Client#client{transport=Transport,
                               host=Host,
                               port=Port,
                               socket=Skt,
                               state = connected,
                               follow_redirect=FollowRedirect,
                               max_redirect=MaxRedirect,
                               force_redirect=ForceRedirect,
                               async=Async}};
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
