%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney).

-export([connect/1, connect/3, connect/4,
         close/1,
         set_sockopts/2,
         request/1, request/2, request/3, request/4, request/5,
         send_request/2,
         stream_body/1,
         body/1, body/2, skip_body/1]).

-include("hackney.hrl").


connect(#client{state=connected}=Client) ->
    Client;

connect(#client{state=closed}=Client) ->
    #client{transport=Transport, host=Host, port=Port} = Client,
    connect(Transport, Host, Port, Client).

connect(Transport, Host, Port) ->
    connect(Transport, Host, Port, #client{options=[]}).


connect(_Transport, _Host, _Port, #client{state=connected}=Client) ->
    {ok, Client};
connect(Transport, Host, Port, #client{options=Options,
                                       socket=Skt0}=Client)
        when is_list(Host), is_integer(Port), Skt0 =:= nil ->

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
    end;
connect(Transport, Host, Port, Options) when is_list(Options) ->
    connect(Transport, Host, Port, #client{options=Options}).

close(Client) ->
    hackney_response:close(Client).

set_sockopts(#client{transport=Transport, socket=Skt}, Options) ->
    Transport:setopts(Skt, Options).


%% @doc make a request
-spec request(binary())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(URL) ->
    request(get, URL).

%% @doc make a request
-spec request(term(), binary())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL) ->
    request(Method, URL, [], <<>>, []).

%% @doc make a request
-spec request(term(), binary(), list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL, Headers) ->
    request(Method, URL, Headers, <<>>, []).

%% @doc make a request
-spec request(term(), binary(), list(), term())
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
%% <li><em>Headers</em></li> Proplists </li>
%% <li><em>Body</em>:
%%      <ul>
%%      <li>{form, [{K, V}, ...]}: send a form</li>
%%      <li>{file, <<"/path/to/file">>}: to send a file</li>
%%      <li>Bin: binary or iolist</li>
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


send_request(#client{response_state=done}=Client0 ,
             {Method, Path, Headers, Body}) ->
    Client = Client0#client{response_state=on_status,
                           body_state=waiting},
    send_request(Client, {Method, Path, Headers, Body});

send_request(Client, {Method, Path, Headers, Body}) ->

    case {Client#client.response_state, Client#client.body_state} of
        {on_status, waiting} ->
            hackney_request:perform(connect(Client),
                                    {Method, Path, Headers, Body});
        _ ->
            {error, bad_response_state}
    end.


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
