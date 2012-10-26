%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney).
-export([start/0, stop/0]).
-export([connect/1, connect/3, connect/4,
         close/1,
         set_sockopts/2,
         start_pool/2, stop_pool/1,
         request/1, request/2, request/3, request/4, request/5,
         send_request/2,
         start_response/1,
         stream_request_body/2,
         stream_body/1,
         body/1, body/2, skip_body/1,
         pool/1]).

-include("hackney.hrl").

%% @doc Start the couchbeam process. Useful when testing using the shell.
start() ->
    hackney_deps:ensure(),
    application:load(hackney),
    hackney_app:ensure_deps_started(),
    application:start(hackney).

%% @doc Stop the couchbeam process. Useful when testing using the shell.
stop() ->
    application:stop(hackney).

%% @doc connect a socket and create a client state.
connect(#client{state=connected, redirect=nil}=Client) ->
    {ok, Client};

connect(#client{state=connected, redirect=Redirect}=Client) ->
    #client{host=Host, port=Port, transport=Transport,
                    socket=Socket}=Client,

    case pool(Client) of
        undefined ->
            close(Client);
        Pool ->
            hackney_pool:release(Pool, {Transport, Host, Port}, Socket)
    end,
    connect(Redirect);
connect(#client{state=closed, redirect=nil}=Client) ->
    #client{transport=Transport, host=Host, port=Port} = Client,
    connect(Transport, Host, Port, Client);

connect(#client{state=closed, redirect=Redirect}) ->
    connect(Redirect);

connect({Transport, Host, Port, Options}) ->
    connect(Transport, Host, Port, #client{options=Options}).

connect(Transport, Host, Port) ->
    connect(Transport, Host, Port, #client{options=[]}).


connect(_Transport, _Host, _Port, #client{state=connected}=Client) ->
    {ok, Client};
connect(Transport, Host, Port, #client{socket=Skt, options=Opts}=Client)
        when is_list(Host), is_integer(Port), Skt =:= nil ->

    UseDefaultPool = use_default_pool(),
    case pool(Client) of
        undefined when UseDefaultPool == true ->
            Opts1 = [{pool, default} | Opts],
            socket_from_pool(default, {Transport, Host, Port},
                             Client#client{options=Opts1});
        undefined ->
            do_connect(Transport, Host, Port, Client);
        Pool ->
            socket_from_pool(Pool, {Transport, Host, Port}, Client)
    end;
connect(Transport, Host, Port, Options) when is_list(Options) ->
    connect(Transport, Host, Port, #client{options=Options}).

%% @doc close the client
close(Client) ->
    hackney_response:close(Client).

%% @doc add set sockets options in the client
set_sockopts(#client{transport=Transport, socket=Skt}, Options) ->
    Transport:setopts(Skt, Options).

%% @doc start a pool
start_pool(Name, Options) ->
    hackney_pool:start_pool(Name, Options).

%% @doc stop a pool
stop_pool(Name) ->
    hackney_pool:stop_pool(Name).


%% @doc make a request
-spec request(binary()|list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(URL) ->
    request(get, URL).

%% @doc make a request
-spec request(term(), binary()|list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL) ->
    request(Method, URL, [], <<>>, []).

%% @doc make a request
-spec request(term(), binary()|list(), list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL, Headers) ->
    request(Method, URL, Headers, <<>>, []).

%% @doc make a request
-spec request(term(), binary()|list(), list(), term())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL, Headers, Body) ->
    request(Method, URL, Headers, Body, []).

%% @doc make a request
%%
%% Args:
%% <ul>
%% <li><em>Method</em>: method used for the request (get, post,
%% ...)</li>
%% <li><em>Url</em>: full url of the request</li>
%% <li><em>Headers</em> Proplists </li>
%% <li><em>Body</em>:
%%      <ul>
%%      <li>{form, [{K, V}, ...]}: send a form url encoded</li>
%%      <li>{multipart, [{K, V}, ...]}: send a form using multipart</li>
%%      <li>{file, "/path/to/file"}: to send a file</li>
%%      <li>Bin: binary or iolist</li>
%%      </ul>
%%  </li>
%%  </ul>
-spec request(term(), binary(), list(), term(), list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, #hackney_url{}=URL, Headers, Body, Options0) ->
    #hackney_url{transport=Transport,
                 host = Host,
                 port = Port,
                 user = User,
                 password = Password,
                 raw_path = Path} = URL,

    Options = case User of
        nil ->
            Options0;
        _ ->
            lists:keystore(basic_auth, 1, Options0,
                             {basic_auth, {User, Password}})
    end,

    case maybe_proxy(Transport, Host, Port, Options) of
        {ok, Client} ->
            send_request(Client, {Method, Path, Headers, Body});
        Error ->
            Error
    end;
request(Method, URL, Headers, Body, Options)
        when is_binary(URL) orelse is_list(URL) ->
    request(Method, hackney_url:parse_url(URL), Headers, Body, Options).


%% @doc send a request using the current client state
send_request(#client{response_state=done}=Client0 ,
             {Method, Path, Headers, Body}) ->
    Client = Client0#client{response_state=start, body_state=waiting},
    send_request(Client, {Method, Path, Headers, Body});

send_request(Client0, {Method, Path, Headers, Body}=Req) ->
    case connect(Client0) of
        {ok, Client} ->
            case {Client#client.response_state, Client#client.body_state} of
                {start, waiting} ->
                     Resp = hackney_request:perform(Client, {Method,
                                                             Path,
                                                             Headers,
                                                             Body}),
                     maybe_redirect(Resp, Req, 0);

                _ ->
                    {error, invalide_state}
            end;
        Error ->
            Error
    end.

%% @doc stream the request body. It isued after sending a request using
%% the `request' and `send_request' functions.
-spec stream_request_body(term(), #client{})
    -> {ok, #client{}} | {error, term()}.
stream_request_body(Body, Client) ->
    hackney_request:stream_body(Body, Client).

%% @doc start a response.
%% Useful if you stream the body by yourself. It will fetch the status
%% and headers of the response. and return
-spec start_response(#client{})
    -> {ok, integer(), list(), #client{}} | {error, term()}.
start_response(Client) ->
    hackney_response:start_response(Client).


%% @doc Stream the response body.
stream_body(Client) ->
    hackney_response:stream_body(Client).

%% @doc Return the full body sent with the response.
-spec body(#client{}) -> {ok, binary(), #client{}} | {error, atom()}.
body(Client) ->
    hackney_response:body(Client).

%% @doc Return the full body sent with the response as long as the body
%% length doesn't go over MaxLength.
-spec body(non_neg_integer() | infinity, #client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
body(MaxLength, Client) ->
    hackney_response:body(MaxLength, Client).


%% @doc skip the full body. (read all the body if needed).
-spec skip_body(#client{}) -> {ok, #client{}} | {error, atom()}.
skip_body(Client) ->
    hackney_response:skip_body(Client).


%% @doc get current pool pid or name used by a client if needed
pool(#client{options=Opts}) ->
    case proplists:get_value(pool, Opts) of
        undefined ->
            undefined;
        default ->
            whereis(hackney_pool);
        Pool ->
            Pool
    end.


%% internal functions
%%
maybe_proxy(Transport, Host, Port, Options)
        when is_list(Host), is_integer(Port), is_list(Options) ->

    case proplists:get_value(proxy, Options) of
        Url when is_binary(Url) orelse is_list(Url) ->
            ProxyOpts = [{basic_auth, proplists:get_value(proxy_auth,
                                                          Options)}],
            #hackney_url{transport=PTransport} = hackney_url:parse_url(Url),

            if PTransport =/= Transport ->
                    {error, invalid_proxy_transport};
                true ->
                    connect_proxy(Url, Host, Port, ProxyOpts, Options)
            end;
        {ProxyHost, ProxyPort} ->
            Netloc = iolist_to_binary([ProxyHost, ":",
                                       integer_to_list(ProxyPort)]),
            Scheme = hackney_url:transport_scheme(Transport),
            Url = #hackney_url{scheme=Scheme, netloc=Netloc},
            ProxyOpts = [{basic_auth, proplists:get_value(proxy_auth,
                                                          Options)}],
            connect_proxy(hackney_url:unparse_url(Url), Host, Port, ProxyOpts,
                          Options);

        _ ->
            connect(Transport, Host, Port, Options)
    end.

connect_proxy(ProxyUrl, Host, Port, ProxyOpts, Options) ->
    Host = iolist_to_binary([Host, ":", integer_to_list(Port)]),
    Headers = [{<<"Host">>, Host}],
    case request(connect, ProxyUrl, Headers, <<>>, ProxyOpts) of
        {ok, 200, _, Client0} ->
            Client = skip_body(Client0),
            {ok, Client#client{options=Options}};
        {ok, S, H, Client} ->
            Body = body(Client),
            {error, {proxy_connection, S, H, Body}};
        Error ->
            {error, {proxy_connection, Error}}
    end.

socket_from_pool(Pool, {Transport, Host, Port}=Key,
                 #client{options=Opts}=Client) ->
    case hackney_pool:socket(Pool, Key) of
        {ok, Skt} ->
            FollowRedirect = proplists:get_value(follow_redirect,
                                                 Opts, false),
            MaxRedirect = proplists:get_value(max_redirect, Opts, 5),
            {ok, Client#client{transport=Transport,
                               host=Host,
                               port=Port,
                               socket=Skt,
                               state = connected,
                               follow_redirect=FollowRedirect,
                               max_redirect=MaxRedirect}};
        no_socket ->
            do_connect(Transport, Host, Port, Client)
    end.

do_connect(Transport, Host, Port, #client{options=Opts}=Client) ->
    ConnectOpts0 = proplists:get_value(connect_options, Opts, []),

    %% handle ipv6
    ConnectOpts1 = case hackney_util:is_ipv6(Host) of
        true ->
            [inet6 | ConnectOpts0];
        false ->
            ConnectOpts0
    end,

    ConnectOpts = case {Transport, proplists:get_value(ssl_options, Opts)} of
        {hackney_ssl_transport, undefined} ->
            ConnectOpts1 ++ [{verify, verify_none},
                             {reuse_sessions, true}];
        {hackney_ssl_transport, SslOpts} ->
            ConnectOpts1 ++ SslOpts;
        {_, _} ->
            ConnectOpts1
    end,

    case Transport:connect(Host, Port, ConnectOpts) of
        {ok, Skt} ->
            FollowRedirect = proplists:get_value(follow_redirect,
                                                 Opts, false),
            MaxRedirect = proplists:get_value(max_redirect, Opts, 5),
            ForceRedirect = proplists:get_value(force_redirect, Opts,
                                                false),
            {ok, Client#client{transport=Transport,
                               host=Host,
                               port=Port,
                               socket=Skt,
                               state = connected,
                               follow_redirect=FollowRedirect,
                               max_redirect=MaxRedirect,
                               force_redirect=ForceRedirect}};
        Error ->
            Error
    end.

maybe_redirect({ok, _}=Resp, _Req, _Tries) ->
    Resp;
maybe_redirect({ok, S, H, #client{follow_redirect=true,
                                  max_redirect=Max,
                                  force_redirect=ForceRedirect}=Client}=Resp,
               Req, Tries) when Tries < Max ->

    {Method, _Path, Headers, Body} = Req,
    case lists:member(S, [301, 302, 307]) of
        true ->
            Location = redirect_location(H),
            %% redirect the location if possible. If the method is
            %% different from  get or head it will return
            %% `{ok, {maybe_redirect, Status, Headers, Client}}' to let
            %% the  user make his choice.
            case {Location, lists:member(Method, [get, head])} of
                {undefined, _} ->
                    {error, {invalid_redirection, Resp}};
                {_, true} ->
                        NewReq = {Method, Location, Headers, Body},
                        maybe_redirect(redirect(Client, NewReq), Req,
                                       Tries+1);
                {_, _} when ForceRedirect =:= true ->
                        NewReq = {Method, Location, Headers, Body},
                        maybe_redirect(redirect(Client, NewReq), Req,
                                       Tries+1);
                {_, _} ->
                    {ok, {maybe_redirect, S, H, Client}}
            end;
        false when S =:= 303 ->
            %% see other. If methos is not POST we consider it as an
            %% invalid redirection
            Location = redirect_location(H),
            case {Location, Method} of
                {undefined, _} ->
                    {error, {invalid_redirection, Resp}};
                {_, post} ->
                    NewReq = {get, Location, [], <<>>},
                    maybe_redirect(redirect(Client, NewReq), Req, Tries+1);
                {_, _} ->

                    {error, {invalid_redirection, Resp}}
            end;
        _ ->
            Resp
    end;
maybe_redirect({ok, S, _H, #client{follow_redirect=true}}=Resp,
               _Req, _Tries) ->
    case lists:member(S, [301, 302, 303, 307]) of
        true ->
            {error, {max_redirect_overflow, Resp}};
        false ->
            Resp
    end;
maybe_redirect(Resp, _Req, _Tries) ->
    Resp.


redirect(Client0, {Method, NewLocation, Headers, Body}) ->
    %% skip the body
    {ok, Client} = skip_body(Client0),


    %% close the connection if we don't use a pool
    Client1 = case Client#client.state of
        closed -> Client;
        _ -> close(Client)
    end,

    %% make a request without any redirection
    #client{transport=Transport,
            host=Host,
            port=Port,
            options=Opts0,
            redirect=Redirect} = Client1,
    Opts = lists:keystore(follow_redirect, 1, Opts0,
                          {follow_redirect, false}),


    case request(Method, NewLocation, Headers, Body, Opts) of
        {ok,  S, H, RedirectClient} when Redirect /= nil ->
            NewClient = RedirectClient#client{redirect=Redirect,
                                              options=Opts0},
            {ok, S, H, NewClient};
        {ok, S, H, RedirectClient} ->
            NewRedirect = {Transport, Host, Port, Opts0},
            NewClient = RedirectClient#client{redirect=NewRedirect,
                                              options=Opts0},
            {ok, S, H, NewClient};

        Error ->
            Error
    end.

redirect_location(Headers) ->
    hackney_headers:get_value(<<"location">>, hackney_headers:new(Headers)).

use_default_pool() ->
    case application:get_env(hackney, use_default_pool) of
        {ok, true} ->
            true;
        _ ->
            false
    end.
