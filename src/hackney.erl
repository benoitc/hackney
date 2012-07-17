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
connect(#client{state=connected}=Client) ->
    {ok, Client};

connect(#client{state=closed}=Client) ->
    #client{transport=Transport, host=Host, port=Port} = Client,
    connect(Transport, Host, Port, Client).

connect(Transport, Host, Port) ->
    connect(Transport, Host, Port, #client{options=[]}).


connect(_Transport, _Host, _Port, #client{state=connected}=Client) ->
    {ok, Client};
connect(Transport, Host, Port, #client{socket=Skt}=Client)
        when is_list(Host), is_integer(Port), Skt =:= nil ->
    case pool(Client) of
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
    request(Method, URL, Headers, Body).

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
%%      <li>{form, [{K, V}, ...]}: send a form</li>
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

    case connect(Transport, Host, Port, Options) of
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

send_request(Client0, {Method, Path, Headers, Body}) ->
    case connect(Client0) of
        {ok, Client} ->
            case {Client#client.response_state, Client#client.body_state} of
                {start, waiting} ->
                    hackney_request:perform(Client,
                                            {Method, Path, Headers, Body});

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
socket_from_pool(Pool, {Transport, Host, Port}=Key, Client) ->
    case hackney_pool:socket(Pool, Key) of
        {ok, Skt} ->
            {ok, Client#client{transport=Transport,
                               host=Host,
                               port=Port,
                               socket=Skt,
                               state = connected}};
        no_socket ->
            do_connect(Transport, Host, Port, Client)
    end.

do_connect(Transport, Host, Port, #client{options=Options}=Client) ->
    ConnectOpts0 = proplists:get_value(connect_options, Options, []),

    %% handle ipv6
    ConnectOpts = case hackney_util:is_ipv6(Host) of
        true ->
            [inet6 | ConnectOpts0];
        false ->
            ConnectOpts0
    end,

    case Transport:connect(Host, Port, ConnectOpts) of
        {ok, Skt} ->
            {ok, Client#client{transport=Transport,
                               host=Host,
                               port=Port,
                               socket=Skt,
                               state = connected}};
        Error ->
            Error
    end.
