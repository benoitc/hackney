%%% -*- erlang -*-
%%%
%%% Mock WebSocket handler for hackney tests
%%% Uses cowboy_websocket behavior

-module(mock_ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _State) ->
    %% Extract path info for special handlers
    Path = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    Qs = cowboy_req:qs(Req),
    NewState = #{path => Path, headers => Headers, qs => Qs},
    {cowboy_websocket, Req, NewState}.

websocket_init(State) ->
    {ok, State}.

%% Echo commands
websocket_handle({text, <<"echo:", Msg/binary>>}, State) ->
    {reply, {text, Msg}, State};

%% Ping command - send a ping frame
websocket_handle({text, <<"ping">>}, State) ->
    {reply, ping, State};

%% Ping with data
websocket_handle({text, <<"ping:", Data/binary>>}, State) ->
    {reply, {ping, Data}, State};

%% Close commands
websocket_handle({text, <<"close">>}, State) ->
    {reply, {close, 1000, <<"goodbye">>}, State};
websocket_handle({text, <<"close:", CodeBin/binary>>}, State) ->
    Code = binary_to_integer(CodeBin),
    {reply, {close, Code, <<"custom close">>}, State};

%% Delayed response - sleep then echo
websocket_handle({text, <<"delay:", Rest/binary>>}, State) ->
    [DelayBin, Msg] = binary:split(Rest, <<":">>),
    Delay = binary_to_integer(DelayBin),
    timer:sleep(Delay),
    {reply, {text, Msg}, State};

%% Get headers - return the headers we received during handshake
websocket_handle({text, <<"get_headers">>}, State) ->
    #{headers := Headers} = State,
    %% Convert headers map to JSON-like string
    HeaderList = maps:to_list(Headers),
    HeaderBin = iolist_to_binary([io_lib:format("~s:~s;", [K, V]) || {K, V} <- HeaderList]),
    {reply, {text, HeaderBin}, State};

%% Get query string
websocket_handle({text, <<"get_qs">>}, State) ->
    #{qs := Qs} = State,
    {reply, {text, Qs}, State};

%% Large message test - generate N bytes of data
websocket_handle({text, <<"large:", SizeBin/binary>>}, State) ->
    Size = binary_to_integer(SizeBin),
    Data = binary:copy(<<"X">>, Size),
    {reply, {binary, Data}, State};

%% Multiple replies
websocket_handle({text, <<"multi:", CountBin/binary>>}, State) ->
    Count = binary_to_integer(CountBin),
    Replies = [{text, integer_to_binary(N)} || N <- lists:seq(1, Count)],
    {reply, Replies, State};

%% Echo text messages
websocket_handle({text, Msg}, State) ->
    {reply, {text, Msg}, State};

%% Echo binary messages
websocket_handle({binary, Data}, State) ->
    {reply, {binary, Data}, State};

%% Handle ping - Cowboy handles this automatically, so we do nothing
%% (returning {ok, State} means no additional reply)
websocket_handle(ping, State) ->
    {ok, State};

%% Handle pong
websocket_handle(pong, State) ->
    {ok, State};
websocket_handle({pong, _Data}, State) ->
    {ok, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
