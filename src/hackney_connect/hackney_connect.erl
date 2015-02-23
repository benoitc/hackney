%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_connect).

-export([connect/3, connect/4, connect/5,
         create_connection/4, create_connection/5,
         maybe_connect/1,
         reconnect/4,
         set_sockopts/2,
         close/1,
         is_pool/1]).

-export([ssl_opts/2]).

-include("hackney.hrl").

-ifdef(no_ssl_name_validation).
-define(VALIDATE_SSL, normal).
-else.
-define(VALIDATE_SSL, host).
-endif.


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

    %% get mod metrics
    Mod = hackney_util:mod_metrics(),

    %% initial state
    InitialState = #client{mod_metrics=Mod,
                           transport=Transport,
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
    %%
    reconnect(Host, Port, Transport, InitialState).


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
    {ok, check_mod_metrics(Client)};
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
            do_connect(Host, Port, Transport, check_mod_metrics(State));
        true ->
            socket_from_pool(Host, Port, Transport, check_mod_metrics(State))
    end.

%%
%% internal functions
%%

socket_from_pool(Host, Port, Transport, Client0) ->
    PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),
    PoolName = proplists:get_value(pool, Client0#client.options, default),
    Mod = Client0#client.mod_metrics,

    %% new request
    {_RequestRef, Client} = hackney_manager:new_request(Client0),

    case PoolHandler:checkout(Host, Port, Transport, Client) of
        {ok, Ref, Skt} ->
            Mod:update_meter([hackney_pool, PoolName, take_rate], 1),

            Client1 = Client#client{socket=Skt,
                                    socket_ref=Ref,
                                    pool_handler=PoolHandler,
                                    state = connected},

            hackney_manager:update_state(Client1),
            {ok, Client1};
        {error, no_socket, Ref} ->
            Mod:increment_counter([hackney_pool, PoolName, no_socket]),
            do_connect(Host, Port, Transport, Client#client{socket_ref=Ref},
                       pool);
        Error ->
            Error
    end.

do_connect(Host, Port, Transport, Client) ->
    do_connect(Host, Port, Transport, Client, direct).



do_connect(Host, Port, Transport, #client{mod_metrics=Mod,
                                          options=Opts}=Client0, Type) ->
    Begin = os:timestamp(),
    {_RequestRef, Client} = case Type of
                               pool ->
                                   {Client0#client.request_ref, Client0};
                               direct ->
                                   hackney_manager:new_request(Client0)
                           end,

    ConnectOpts0 = proplists:get_value(connect_options, Opts, []),
    ConnectTimeout = proplists:get_value(connect_timeout, Opts, 8000),

    %% handle ipv6
    ConnectOpts1 = case lists:member(inet, ConnectOpts0) orelse
                       lists:member(inet6, ConnectOpts0) of
                       true ->
                           ConnectOpts0;
                       false ->
                           case hackney_util:is_ipv6(Host) of
                               true ->
                                   [inet6 | ConnectOpts0];
                               false ->
                                   ConnectOpts0
                           end
                   end,

    ConnectOpts = case Transport of
                      hackney_ssl_transport ->
                          ConnectOpts1 ++ ssl_opts(Host, Opts);
                      _ ->
                          ConnectOpts1
                  end,

    case Transport:connect(Host, Port, ConnectOpts, ConnectTimeout) of
        {ok, Skt} ->
            ConnectTime = timer:now_diff(os:timestamp(), Begin)/1000,
            Mod:update_histogram([hackney, Host, connect_time], ConnectTime),
            Client1 = Client#client{socket=Skt,
                                    state = connected},
            hackney_manager:update_state(Client1),
            {ok, Client1};
        {error, timeout} ->
            Mod:increment_counter([hackney, Host, connect_timeout]),
            hackney_manager:cancel_request(Client),
            {error, connect_timeout};
        Error ->
            Mod:increment_counter([hackney, Host, connect_error]),
            hackney_manager:cancel_request(Client),
            Error
    end.


use_default_pool() ->
    case application:get_env(hackney, use_default_pool) of
        {ok, Val} ->
            Val;
        _ ->
            true
    end.

check_mod_metrics(#client{mod_metrics=Mod}=State)
  when Mod /= nil, Mod /= undefined ->
    State;
check_mod_metrics(State) ->
    State#client{mod_metrics=hackney_util:mod_metrics()}.


ssl_opts(Host, Options) ->
    case proplists:get_value(ssl_options, Options) of
        undefined ->
            Insecure =  proplists:get_value(insecure, Options),
            ShouldValidate = should_validate_ssl(),

            case {Insecure, ShouldValidate} of
                {true, _} ->
                    [{verify, verify_none},
                     {reuse_sessions, true}];
                {_, host} ->
                    CACertFile = filename:join(hackney_util:privdir(),
                                               "ca-bundle.crt"),
                    [{verify_fun, {fun ssl_verify_hostname:verify_fun/3,
                                   [{check_hostname, Host}]}},
                     {cacertfile, CACertFile },
                     {server_name_indication, Host},
                     {verify, verify_peer}, {depth, 99},
                     {reuse_sessions, true}];
                {_, normal} ->
                    CACertFile = filename:join(hackney_util:privdir(),
                                               "ca-bundle.crt"),
                    [{cacertfile, CACertFile },
                     {verify, verify_peer}, {depth, 99},
                     {reuse_sessions, true}]
            end;
        SSLOpts ->
            SSLOpts
    end.

should_validate_ssl() ->
    ?VALIDATE_SSL.
