%%% -*- erlang -*-
%%%
%%% Mock WebSocket handler for hackney tests
%%% Uses cowboy_websocket behavior

-module(mock_ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, <<"echo:", Msg/binary>>}, State) ->
    %% Echo the message back
    {reply, {text, Msg}, State};
websocket_handle({text, <<"ping">>}, State) ->
    %% Send a ping frame
    {reply, ping, State};
websocket_handle({text, <<"close">>}, State) ->
    %% Close with normal code
    {reply, {close, 1000, <<"goodbye">>}, State};
websocket_handle({text, <<"close:", CodeBin/binary>>}, State) ->
    %% Close with specific code
    Code = binary_to_integer(CodeBin),
    {reply, {close, Code, <<"custom close">>}, State};
websocket_handle({text, Msg}, State) ->
    %% Echo text messages
    {reply, {text, Msg}, State};
websocket_handle({binary, Data}, State) ->
    %% Echo binary messages
    {reply, {binary, Data}, State};
websocket_handle(ping, State) ->
    %% Respond to ping (cowboy does this automatically but just in case)
    {reply, pong, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
