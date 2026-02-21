%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Minimal HTTP/2 server for h2spec compliance testing.
%%%
%%% This server implements just enough of HTTP/2 to pass h2spec tests.
%%% It handles the connection preface, SETTINGS frames, and responds
%%% to simple GET requests.

-module(h2spec_server).

-export([start/1, start/2, stop/1]).
-export([accept_loop/2]).

-define(ALPN_PROTOCOLS, [<<"h2">>]).
-define(DEFAULT_MAX_FRAME_SIZE, 16384).

%% @doc Start the h2spec test server on the given port.
%% Returns {ok, ListenSocket} on success.
-spec start(inet:port_number()) -> {ok, ssl:sslsocket()} | {error, term()}.
start(Port) ->
    start(Port, #{}).

-spec start(inet:port_number(), map()) -> {ok, pid()} | {error, term()}.
start(Port, _Opts) ->
    CertFile = cert_file(),
    KeyFile = key_file(),
    case {filelib:is_regular(CertFile), filelib:is_regular(KeyFile)} of
        {true, true} ->
            %% Spawn a server process that owns the listen socket
            %% This ensures the socket stays open even if the calling process exits
            Self = self(),
            Pid = spawn(fun() -> server_process(Port, CertFile, KeyFile, Self) end),
            receive
                {Pid, started} ->
                    {ok, Pid};
                {Pid, {error, Reason}} ->
                    {error, Reason}
            after 5000 ->
                {error, timeout}
            end;
        _ ->
            {error, {missing_certs, CertFile, KeyFile}}
    end.

server_process(Port, CertFile, KeyFile, Caller) ->
    ListenOpts = [
        binary,
        {active, false},
        {reuseaddr, true},
        {alpn_preferred_protocols, ?ALPN_PROTOCOLS},
        {certfile, CertFile},
        {keyfile, KeyFile},
        {versions, ['tlsv1.2', 'tlsv1.3']}
    ],
    case ssl:listen(Port, ListenOpts) of
        {ok, LSock} ->
            Caller ! {self(), started},
            %% Enter accept loop directly (this process owns the socket)
            accept_loop(LSock, Caller);
        {error, Reason} ->
            Caller ! {self(), {error, Reason}}
    end.

%% @doc Stop the server by killing the server process.
-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    exit(Pid, shutdown),
    ok;
stop(LSock) ->
    %% Backward compatibility: allow passing socket directly
    ssl:close(LSock).

%% @private Accept loop - spawns a handler for each connection.
accept_loop(LSock, Parent) ->
    case ssl:transport_accept(LSock, 5000) of
        {ok, Socket} ->
            case ssl:handshake(Socket, 5000) of
                {ok, SSLSocket} ->
                    %% Verify ALPN negotiated h2
                    case ssl:negotiated_protocol(SSLSocket) of
                        {ok, <<"h2">>} ->
                            %% Transfer socket ownership to new process
                            Pid = spawn(fun() ->
                                receive {socket, S} -> connection_loop(S) end
                            end),
                            ssl:controlling_process(SSLSocket, Pid),
                            Pid ! {socket, SSLSocket};
                        _ ->
                            ssl:close(SSLSocket)
                    end;
                {error, _} ->
                    ssl:close(Socket)
            end,
            accept_loop(LSock, Parent);
        {error, timeout} ->
            accept_loop(LSock, Parent);
        {error, closed} ->
            ok;
        {error, _Reason} ->
            accept_loop(LSock, Parent)
    end.

%% @private Connection handler - implements HTTP/2 server side.
connection_loop(Socket) ->
    %% Initialize HTTP/2 machine in server mode
    Opts = #{preface_timeout => infinity, settings_timeout => infinity},
    {ok, Preface, H2Machine0} = hackney_http2_machine:init(server, Opts),

    %% Send server preface (SETTINGS frame)
    case ssl:send(Socket, Preface) of
        ok ->
            %% Set socket to active once to receive data
            ok = ssl:setopts(Socket, [{active, once}]),
            %% Enter preface loop first (to consume client magic string)
            preface_loop(Socket, H2Machine0, <<>>);
        {error, closed} ->
            %% Client closed connection before we could send preface
            ok;
        {error, _Reason} ->
            ssl:close(Socket)
    end.

%% @private Wait for client preface magic string before frame processing
preface_loop(Socket, H2Machine, Buffer) ->
    receive
        {ssl, Socket, Data} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case hackney_http2:parse_sequence(NewBuffer) of
                {ok, Rest} ->
                    %% Now process any remaining data as frames
                    ok = ssl:setopts(Socket, [{active, once}]),
                    case process_frames(Socket, H2Machine, Rest) of
                        {ok, H2Machine1, Rest1} ->
                            frame_loop(Socket, H2Machine1, Rest1);
                        {error, _Reason} ->
                            ssl:close(Socket)
                    end;
                more ->
                    ok = ssl:setopts(Socket, [{active, once}]),
                    preface_loop(Socket, H2Machine, NewBuffer);
                {connection_error, ErrorCode, _Msg} ->
                    send_goaway(Socket, 0, ErrorCode),
                    ssl:close(Socket)
            end;
        {ssl_closed, Socket} ->
            ok;
        {ssl_error, Socket, _Reason} ->
            ssl:close(Socket)
    after 30000 ->
        ssl:close(Socket)
    end.

frame_loop(Socket, H2Machine, Buffer) ->
    receive
        {ssl, Socket, Data} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case process_frames(Socket, H2Machine, NewBuffer) of
                {ok, H2Machine1, Rest} ->
                    ok = ssl:setopts(Socket, [{active, once}]),
                    frame_loop(Socket, H2Machine1, Rest);
                {error, _Reason} ->
                    ssl:close(Socket)
            end;
        {ssl_closed, Socket} ->
            ok;
        {ssl_error, Socket, _Reason} ->
            ssl:close(Socket)
    after 30000 ->
        ssl:close(Socket)
    end.

process_frames(Socket, H2Machine, Buffer) ->
    case hackney_http2:parse(Buffer, ?DEFAULT_MAX_FRAME_SIZE) of
        {ok, Frame, Rest} ->
            case handle_frame(Socket, H2Machine, Frame) of
                {ok, H2Machine1} ->
                    %% Handle frames that require immediate response
                    H2Machine2 = case Frame of
                        {settings, _Settings} ->
                            SettingsAck = hackney_http2:settings_ack(),
                            ssl:send(Socket, SettingsAck),
                            H2Machine1;
                        {ping, Opaque} ->
                            PingAck = hackney_http2:ping_ack(Opaque),
                            ssl:send(Socket, PingAck),
                            H2Machine1;
                        _ ->
                            H2Machine1
                    end,
                    process_frames(Socket, H2Machine2, Rest);
                {error, Reason} ->
                    {error, Reason}
            end;
        more ->
            {ok, H2Machine, Buffer};
        {ignore, Rest} ->
            %% Unknown frame types MUST be ignored per RFC 7540 4.1
            process_frames(Socket, H2Machine, Rest);
        {connection_error, ErrorCode, _Msg} ->
            send_goaway(Socket, 0, ErrorCode),
            {error, ErrorCode};
        {stream_error, StreamId, ErrorCode, _, Rest} ->
            send_rst_stream(Socket, StreamId, ErrorCode),
            process_frames(Socket, H2Machine, Rest)
    end.

handle_frame(Socket, H2Machine, Frame) ->
    case hackney_http2_machine:frame(Frame, H2Machine) of
        {ok, H2Machine1} ->
            %% No response needed (e.g., SETTINGS ACK, PING ACK, PRIORITY)
            {ok, H2Machine1};
        {ok, {headers, StreamId, IsFin, Headers, PseudoHeaders, _Len}, H2Machine1} ->
            %% Incoming request - send response
            handle_request(Socket, H2Machine1, StreamId, IsFin, Headers, PseudoHeaders);
        {ok, {data, _StreamId, _IsFin, _Data}, H2Machine1} ->
            %% Request body data - just acknowledge
            {ok, H2Machine1};
        {ok, {rst_stream, _StreamId, _Reason}, H2Machine1} ->
            {ok, H2Machine1};
        {ok, {goaway, _LastStreamId, _ErrorCode, _DebugData}, H2Machine1} ->
            {ok, H2Machine1};
        {ok, {ping, Opaque}, H2Machine1} ->
            %% Send PING ACK
            PingAck = hackney_http2:ping_ack(Opaque),
            ok = ssl:send(Socket, PingAck),
            {ok, H2Machine1};
        {ok, {trailers, _StreamId, _Trailers}, H2Machine1} ->
            {ok, H2Machine1};
        {ok, {window_update, _}, H2Machine1} ->
            %% Window update processed
            {ok, H2Machine1};
        {ok, {settings, _Settings}, H2Machine1} ->
            %% Settings received, ACK should be sent automatically by machine
            {ok, H2Machine1};
        {ok, {settings_ack}, H2Machine1} ->
            %% Settings ACK received
            {ok, H2Machine1};
        {ok, {priority, _StreamId, _Exclusive, _DepStreamId, _Weight}, H2Machine1} ->
            %% Priority frame - just acknowledge
            {ok, H2Machine1};
        {ok, Other, H2Machine1} ->
            %% Log unexpected but handle gracefully
            error_logger:info_msg("h2spec_server: unhandled frame result: ~p~n", [Other]),
            {ok, H2Machine1};
        {send, SendData, H2Machine1} ->
            %% Send queued data (e.g., SETTINGS_ACK, WINDOW_UPDATE)
            lists:foreach(fun({_StreamId, _Fin, DataList}) ->
                lists:foreach(fun
                    ({data, D}) -> ssl:send(Socket, D);
                    ({trailers, _T}) -> ok
                end, DataList)
            end, SendData),
            {ok, H2Machine1};
        {send, Data, _Event, H2Machine1} when is_binary(Data); is_list(Data) ->
            %% Direct send data (SETTINGS_ACK, etc.)
            ok = ssl:send(Socket, Data),
            {ok, H2Machine1};
        {error, {connection_error, ErrorCode, _Msg}, _H2Machine1} ->
            send_goaway(Socket, 0, ErrorCode),
            {error, ErrorCode};
        {error, {stream_error, StreamId, ErrorCode, _Msg}, H2Machine1} ->
            send_rst_stream(Socket, StreamId, ErrorCode),
            {ok, H2Machine1};
        Other ->
            %% Catch-all for any other return values
            error_logger:warning_msg("h2spec_server: unexpected frame/2 return: ~p~n", [Other]),
            {error, {unexpected_return, Other}}
    end.

handle_request(Socket, H2Machine0, StreamId, _IsFin, _Headers, PseudoHeaders) ->
    %% Simple response - 200 OK with body
    Method = maps:get(method, PseudoHeaders, <<"GET">>),
    Path = maps:get(path, PseudoHeaders, <<"/">>),

    %% Prepare response
    ResponseHeaders = [{<<"content-type">>, <<"text/plain">>}],
    ResponseBody = <<"Hello from h2spec test server\r\nMethod: ",
                     Method/binary, "\r\nPath: ", Path/binary>>,

    %% Prepare and send headers
    {ok, _RespIsFin, HeaderBlock, H2Machine1} = hackney_http2_machine:prepare_headers(
        StreamId, H2Machine0, nofin,
        #{status => 200}, ResponseHeaders),

    HeadersFrame = hackney_http2:headers(StreamId, nofin, HeaderBlock),
    ok = ssl:send(Socket, HeadersFrame),

    %% Send body as DATA frame with END_STREAM
    DataFrame = hackney_http2:data(StreamId, fin, ResponseBody),
    ok = ssl:send(Socket, DataFrame),

    {ok, H2Machine1}.

send_goaway(Socket, LastStreamId, ErrorCode) ->
    Frame = hackney_http2:goaway(LastStreamId, ErrorCode, <<>>),
    ssl:send(Socket, Frame).

send_rst_stream(Socket, StreamId, ErrorCode) ->
    Frame = hackney_http2:rst_stream(StreamId, ErrorCode),
    ssl:send(Socket, Frame).

%% @private Path to test certificate file.
cert_file() ->
    filename:join([code:priv_dir(hackney), "test_certs", "server.pem"]).

%% @private Path to test key file.
key_file() ->
    filename:join([code:priv_dir(hackney), "test_certs", "server.key"]).
