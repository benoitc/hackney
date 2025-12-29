%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024 Benoit Chesneau
%%%
%%% @doc gen_statem process for managing a single HTTP connection.
%%%
%%% This module implements a state machine for HTTP connections,
%%% handling connection establishment, request/response cycles,
%%% and connection reuse.
%%%
%%% States:
%%% - idle: Process started, not connected
%%% - connecting: TCP/SSL handshake in progress
%%% - connected: Ready for requests
%%% - sending: Sending request data
%%% - receiving: Awaiting/streaming response
%%% - closed: Connection terminated

-module(hackney_conn).
-behaviour(gen_statem).

%% API
-export([
    start_link/1,
    stop/1,
    connect/1,
    connect/2,
    get_state/1,
    %% Request/Response
    request/5,
    request/6,
    body/1,
    body/2,
    stream_body/1
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4
]).

%% State functions
-export([
    idle/3,
    connecting/3,
    connected/3,
    sending/3,
    receiving/3,
    closed/3
]).

-include("hackney.hrl").
-include("hackney_lib.hrl").

-define(CONNECT_TIMEOUT, 8000).
-define(IDLE_TIMEOUT, infinity).

%% State data record
-record(conn_data, {
    %% Connection owner
    owner :: pid(),
    owner_mon :: reference() | undefined,

    %% Connection identity
    host :: string(),
    port :: inet:port_number(),
    netloc :: binary() | undefined,
    transport :: module(),

    %% Socket state
    socket :: inet:socket() | ssl:sslsocket() | undefined,
    buffer = <<>> :: binary(),

    %% Options
    connect_timeout = ?CONNECT_TIMEOUT :: timeout(),
    recv_timeout = ?RECV_TIMEOUT :: timeout(),
    idle_timeout = ?IDLE_TIMEOUT :: timeout(),
    connect_options = [] :: list(),
    ssl_options = [] :: list(),

    %% Request tracking
    request_ref :: reference() | undefined,
    request_from :: {pid(), reference()} | undefined,
    method :: binary() | undefined,
    path :: binary() | undefined,

    %% Response state
    version :: {integer(), integer()} | undefined,
    status :: integer() | undefined,
    reason :: binary() | undefined,
    response_headers :: term() | undefined,

    %% Parser state
    parser :: #hparser{} | undefined,

    %% Async mode
    async = false :: boolean() | once,
    stream_to :: pid() | undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a connection process.
%% Options:
%%   - host: Target host (string)
%%   - port: Target port (integer)
%%   - transport: hackney_tcp or hackney_ssl
%%   - connect_timeout: Connection timeout (default 8000ms)
%%   - recv_timeout: Receive timeout (default 5000ms)
%%   - idle_timeout: Idle timeout before closing (default infinity)
%%   - connect_options: Options passed to transport connect
%%   - ssl_options: Additional SSL options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_statem:start_link(?MODULE, [self(), Opts], []).

%% @doc Stop the connection process.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%% @doc Connect to the target host. Blocks until connected or timeout.
-spec connect(pid()) -> ok | {error, term()}.
connect(Pid) ->
    connect(Pid, ?CONNECT_TIMEOUT).

-spec connect(pid(), timeout()) -> ok | {error, term()}.
connect(Pid, Timeout) ->
    gen_statem:call(Pid, connect, Timeout).

%% @doc Get current state name for debugging.
-spec get_state(pid()) -> {ok, atom()} | {error, term()}.
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%% @doc Send an HTTP request and wait for the response status and headers.
%% Returns {ok, Status, Headers, Pid} on success.
%% After this, use body/1 or stream_body/1 to get the response body.
-spec request(pid(), binary(), binary(), list(), binary() | iolist()) ->
    {ok, integer(), list()} | {error, term()}.
request(Pid, Method, Path, Headers, Body) ->
    request(Pid, Method, Path, Headers, Body, infinity).

-spec request(pid(), binary(), binary(), list(), binary() | iolist(), timeout()) ->
    {ok, integer(), list()} | {error, term()}.
request(Pid, Method, Path, Headers, Body, Timeout) ->
    gen_statem:call(Pid, {request, Method, Path, Headers, Body}, Timeout).

%% @doc Get the full response body.
-spec body(pid()) -> {ok, binary()} | {error, term()}.
body(Pid) ->
    body(Pid, infinity).

-spec body(pid(), timeout()) -> {ok, binary()} | {error, term()}.
body(Pid, Timeout) ->
    gen_statem:call(Pid, body, Timeout).

%% @doc Stream the response body in chunks.
%% Returns {ok, Data} for each chunk, {done, Pid} when complete.
-spec stream_body(pid()) -> {ok, binary()} | done | {error, term()}.
stream_body(Pid) ->
    gen_statem:call(Pid, stream_body).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() -> [state_functions, state_enter].

init([Owner, Opts]) ->
    process_flag(trap_exit, true),
    OwnerMon = monitor(process, Owner),

    Host = maps:get(host, Opts),
    Port = maps:get(port, Opts, 80),
    Transport = maps:get(transport, Opts, hackney_tcp),

    %% Compute netloc for Host header
    Netloc = compute_netloc(Host, Port, Transport),

    Data = #conn_data{
        owner = Owner,
        owner_mon = OwnerMon,
        host = Host,
        port = Port,
        netloc = Netloc,
        transport = Transport,
        connect_timeout = maps:get(connect_timeout, Opts, ?CONNECT_TIMEOUT),
        recv_timeout = maps:get(recv_timeout, Opts, ?RECV_TIMEOUT),
        idle_timeout = maps:get(idle_timeout, Opts, ?IDLE_TIMEOUT),
        connect_options = maps:get(connect_options, Opts, []),
        ssl_options = maps:get(ssl_options, Opts, [])
    },
    {ok, idle, Data}.

terminate(_Reason, _State, #conn_data{socket = Socket, transport = Transport}) ->
    case Socket of
        undefined -> ok;
        _ -> Transport:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% State: idle - Not connected yet
%%====================================================================

idle(enter, _OldState, _Data) ->
    keep_state_and_data;

idle({call, From}, connect, Data) ->
    %% Perform connection synchronously
    #conn_data{
        host = Host,
        port = Port,
        transport = Transport,
        connect_timeout = Timeout,
        connect_options = ConnectOpts,
        ssl_options = SslOpts
    } = Data,

    %% Build connection options
    Opts = case Transport of
        hackney_ssl -> ConnectOpts ++ SslOpts;
        _ -> ConnectOpts
    end,

    %% Attempt connection
    case Transport:connect(Host, Port, Opts, Timeout) of
        {ok, Socket} ->
            %% Connection successful
            NewData = Data#conn_data{socket = Socket},
            {next_state, connected, NewData, [{reply, From, ok}]};
        {error, Reason} ->
            %% Connection failed
            {stop_and_reply, normal, [{reply, From, {error, Reason}}]}
    end;

idle({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, idle}}]};

idle(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    %% Owner died
    {stop, normal, Data};

idle(EventType, Event, Data) ->
    handle_common(EventType, Event, idle, Data).

%%====================================================================
%% State: connecting - TCP/SSL handshake in progress
%%====================================================================

connecting(enter, _OldState, _Data) ->
    keep_state_and_data;

connecting(state_timeout, connect_timeout, Data) ->
    reply_and_stop(Data#conn_data.request_from, {error, connect_timeout}, Data);

connecting({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, connecting}}]};

connecting(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

connecting(EventType, Event, Data) ->
    handle_common(EventType, Event, connecting, Data).

%%====================================================================
%% State: connected - Ready for requests
%%====================================================================

connected(enter, _OldState, #conn_data{idle_timeout = Timeout} = Data) ->
    case Timeout of
        infinity -> keep_state_and_data;
        _ -> {keep_state, Data, [{state_timeout, Timeout, idle_timeout}]}
    end;

connected(state_timeout, idle_timeout, Data) ->
    %% Idle timeout - close connection
    {next_state, closed, Data};

connected({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, connected}}]};

connected({call, From}, {request, Method, Path, Headers, Body}, Data) ->
    %% Start a new request
    NewData = Data#conn_data{
        request_from = From,
        method = Method,
        path = Path,
        parser = undefined,
        version = undefined,
        status = undefined,
        reason = undefined,
        response_headers = undefined,
        buffer = <<>>
    },
    {next_state, sending, NewData, [{next_event, internal, {send_request, Method, Path, Headers, Body}}]};

connected(info, {tcp_closed, Socket}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

connected(info, {ssl_closed, Socket}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

connected(info, {tcp_error, Socket, _Reason}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

connected(info, {ssl_error, Socket, _Reason}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

connected(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

connected(EventType, Event, Data) ->
    handle_common(EventType, Event, connected, Data).

%%====================================================================
%% State: sending - Sending request data
%%====================================================================

sending(enter, connected, _Data) ->
    keep_state_and_data;

sending(internal, {send_request, Method, Path, Headers, Body}, Data) ->
    #conn_data{
        transport = Transport,
        socket = Socket,
        netloc = Netloc
    } = Data,

    %% Build request headers
    FinalHeaders = build_headers(Method, Headers, Body, Netloc),

    %% Build request line and headers
    Path1 = case Path of
        <<>> -> <<"/">>;
        _ -> Path
    end,
    RequestLine = <<Method/binary, " ", Path1/binary, " HTTP/1.1\r\n">>,
    HeadersBin = headers_to_binary(FinalHeaders),
    RequestData = <<RequestLine/binary, HeadersBin/binary, "\r\n">>,

    %% Send request
    case Transport:send(Socket, RequestData) of
        ok ->
            %% Send body if present
            case send_body(Transport, Socket, Body) of
                ok ->
                    {next_state, receiving, Data, [{next_event, internal, do_recv_response}]};
                {error, Reason} ->
                    {next_state, closed, Data, [{reply, Data#conn_data.request_from, {error, Reason}}]}
            end;
        {error, Reason} ->
            {next_state, closed, Data, [{reply, Data#conn_data.request_from, {error, Reason}}]}
    end;

sending({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, sending}}]};

sending(info, {tcp_closed, Socket}, #conn_data{socket = Socket, request_from = From} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}, [{reply, From, {error, closed}}]};

sending(info, {ssl_closed, Socket}, #conn_data{socket = Socket, request_from = From} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}, [{reply, From, {error, closed}}]};

sending(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

sending(EventType, Event, Data) ->
    handle_common(EventType, Event, sending, Data).

%%====================================================================
%% State: receiving - Awaiting/streaming response
%%====================================================================

receiving(enter, sending, _Data) ->
    %% Just enter state, request handling happens in internal event
    keep_state_and_data;

receiving(enter, receiving, _Data) ->
    %% Re-entering after body streaming
    keep_state_and_data;

receiving(internal, do_recv_response, Data) ->
    %% Receive and parse response status and headers
    Parser = hackney_http:parser([response]),
    DataWithParser = Data#conn_data{parser = Parser},
    case recv_status_and_headers(DataWithParser) of
        {ok, Status, Headers, NewData} ->
            %% Reply with status and headers, keep connection in receiving state
            From = NewData#conn_data.request_from,
            HeadersList = hackney_headers_new:to_list(Headers),
            {keep_state, NewData#conn_data{
                request_from = undefined,
                response_headers = Headers
            }, [{reply, From, {ok, Status, HeadersList}}]};
        {error, Reason} ->
            From = Data#conn_data.request_from,
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
    end;

receiving({call, From}, body, Data) ->
    %% Read full body
    case read_full_body(Data, <<>>) of
        {ok, Body, NewData} ->
            %% Return to connected state
            {next_state, connected, NewData, [{reply, From, {ok, Body}}]};
        {error, Reason} ->
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
    end;

receiving({call, From}, stream_body, Data) ->
    %% Stream body chunk
    case stream_body_chunk(Data) of
        {ok, Chunk, NewData} ->
            {keep_state, NewData, [{reply, From, {ok, Chunk}}]};
        {done, NewData} ->
            {next_state, connected, NewData, [{reply, From, done}]};
        {error, Reason} ->
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
    end;

receiving({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, receiving}}]};

receiving(info, {tcp_closed, Socket}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

receiving(info, {ssl_closed, Socket}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

receiving(info, {tcp_error, Socket, _Reason}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

receiving(info, {ssl_error, Socket, _Reason}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

receiving(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

receiving(EventType, Event, Data) ->
    handle_common(EventType, Event, receiving, Data).

%%====================================================================
%% State: closed - Connection terminated
%%====================================================================

closed(enter, _OldState, #conn_data{socket = Socket, transport = Transport} = Data) ->
    %% Close socket if still open
    case Socket of
        undefined -> ok;
        _ -> Transport:close(Socket)
    end,
    %% Could stop here or allow reconnection
    {keep_state, Data#conn_data{socket = undefined}};

closed({call, From}, connect, Data) ->
    %% Allow reconnection from closed state
    #conn_data{
        host = Host,
        port = Port,
        transport = Transport,
        connect_timeout = Timeout,
        connect_options = ConnectOpts,
        ssl_options = SslOpts
    } = Data,

    %% Build connection options
    Opts = case Transport of
        hackney_ssl -> ConnectOpts ++ SslOpts;
        _ -> ConnectOpts
    end,

    %% Attempt connection
    case Transport:connect(Host, Port, Opts, Timeout) of
        {ok, Socket} ->
            %% Connection successful
            NewData = Data#conn_data{socket = Socket},
            {next_state, connected, NewData, [{reply, From, ok}]};
        {error, Reason} ->
            %% Connection failed, stay in closed state
            {keep_state_and_data, [{reply, From, {error, Reason}}]}
    end;

closed({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, closed}}]};

closed(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

closed(EventType, Event, Data) ->
    handle_common(EventType, Event, closed, Data).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Handle common events across all states
handle_common(cast, _Msg, _State, _Data) ->
    keep_state_and_data;

handle_common({call, From}, _, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

handle_common(info, _Msg, _State, _Data) ->
    keep_state_and_data.

%% @private Reply to caller and stop
reply_and_stop(undefined, _Reply, Data) ->
    {stop, normal, Data};
reply_and_stop(From, Reply, _Data) ->
    {stop_and_reply, normal, [{reply, From, Reply}]}.

%% @private Compute netloc for Host header
compute_netloc(Host, Port, Transport) ->
    HostBin = hackney_bstr:to_binary(Host),
    case {Transport, Port} of
        {hackney_tcp, 80} -> HostBin;
        {hackney_ssl, 443} -> HostBin;
        _ ->
            PortBin = integer_to_binary(Port),
            <<HostBin/binary, ":", PortBin/binary>>
    end.

%% @private Build request headers
build_headers(_Method, Headers0, Body, Netloc) ->
    %% Start with user headers
    Headers1 = hackney_headers_new:new(Headers0),

    %% Add Host header if not present
    {_, Headers2} = hackney_headers_new:store_new(<<"Host">>, Netloc, Headers1),

    %% Add User-Agent if not present
    {_, Headers3} = hackney_headers_new:store_new(<<"User-Agent">>, default_ua(), Headers2),

    %% Add Content-Length for bodies
    case Body of
        <<>> -> Headers3;
        [] -> Headers3;
        _ when is_binary(Body) ->
            Len = byte_size(Body),
            case hackney_headers_new:is_key(<<"content-length">>, Headers3) of
                true -> Headers3;
                false ->
                    hackney_headers_new:store(<<"Content-Length">>, integer_to_binary(Len), Headers3)
            end;
        _ when is_list(Body) ->
            Len = iolist_size(Body),
            case hackney_headers_new:is_key(<<"content-length">>, Headers3) of
                true -> Headers3;
                false ->
                    hackney_headers_new:store(<<"Content-Length">>, integer_to_binary(Len), Headers3)
            end;
        _ ->
            %% Streaming body - expect user to have set Content-Length or Transfer-Encoding
            Headers3
    end.

%% @private Convert headers to binary
headers_to_binary(Headers) ->
    hackney_headers_new:to_binary(Headers).

%% @private Get default User-Agent
default_ua() ->
    Version = case application:get_key(hackney, vsn) of
        {ok, FullVersion} ->
            list_to_binary(hd(string:tokens(FullVersion, "-")));
        _ ->
            <<"0.0.0">>
    end,
    <<"hackney/", Version/binary>>.

%% @private Send request body
send_body(_Transport, _Socket, <<>>) ->
    ok;
send_body(_Transport, _Socket, []) ->
    ok;
send_body(Transport, Socket, Body) when is_binary(Body); is_list(Body) ->
    Transport:send(Socket, Body).

%% @private Receive and parse response status and headers
recv_status_and_headers(Data) ->
    recv_status(Data).

recv_status(#conn_data{parser = Parser, buffer = Buffer} = Data) ->
    case hackney_http:execute(Parser, Buffer) of
        {more, NewParser} ->
            case recv_data(Data) of
                {ok, RecvData} ->
                    recv_status(Data#conn_data{parser = NewParser, buffer = RecvData});
                {error, Reason} ->
                    {error, Reason}
            end;
        {response, Version, Status, Reason, NewParser} ->
            recv_headers(Data#conn_data{
                parser = NewParser,
                buffer = <<>>,
                version = Version,
                status = Status,
                reason = Reason
            }, hackney_headers_new:new());
        {error, Reason} ->
            {error, Reason}
    end.

recv_headers(#conn_data{parser = Parser} = Data, Headers) ->
    case hackney_http:execute(Parser) of
        {more, NewParser} ->
            case recv_data(Data) of
                {ok, RecvData} ->
                    case hackney_http:execute(NewParser, RecvData) of
                        {header, {Key, Value}, NewParser2} ->
                            Headers2 = hackney_headers_new:append(Key, Value, Headers),
                            recv_headers(Data#conn_data{parser = NewParser2}, Headers2);
                        {headers_complete, NewParser2} ->
                            {ok, Data#conn_data.status, Headers, Data#conn_data{parser = NewParser2}};
                        {more, NewParser2} ->
                            recv_headers(Data#conn_data{parser = NewParser2, buffer = <<>>}, Headers);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {header, {Key, Value}, NewParser} ->
            Headers2 = hackney_headers_new:append(Key, Value, Headers),
            recv_headers(Data#conn_data{parser = NewParser}, Headers2);
        {headers_complete, NewParser} ->
            {ok, Data#conn_data.status, Headers, Data#conn_data{parser = NewParser}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Read full response body
read_full_body(#conn_data{method = <<"HEAD">>} = Data, Acc) ->
    %% HEAD requests have no body
    {ok, Acc, Data};
read_full_body(Data, Acc) ->
    case stream_body_chunk(Data) of
        {ok, Chunk, NewData} ->
            read_full_body(NewData, <<Acc/binary, Chunk/binary>>);
        {done, NewData} ->
            {ok, Acc, NewData};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Stream a single body chunk
stream_body_chunk(#conn_data{method = <<"HEAD">>} = Data) ->
    {done, Data};
stream_body_chunk(#conn_data{parser = Parser, transport = Transport, socket = Socket, recv_timeout = Timeout} = Data) ->
    case hackney_http:execute(Parser) of
        {more, NewParser, _Buffer} ->
            %% Need more data
            case Transport:recv(Socket, 0, Timeout) of
                {ok, RecvData} ->
                    stream_body_chunk_result(hackney_http:execute(NewParser, RecvData), Data);
                {error, closed} ->
                    {done, Data};
                {error, Reason} ->
                    {error, Reason}
            end;
        {more, NewParser} ->
            %% Need more data
            case Transport:recv(Socket, 0, Timeout) of
                {ok, RecvData} ->
                    %% Execute with new data and handle result
                    stream_body_chunk_result(hackney_http:execute(NewParser, RecvData), Data);
                {error, closed} ->
                    {done, Data};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Chunk, NewParser} ->
            {ok, Chunk, Data#conn_data{parser = NewParser}};
        {done, Rest} ->
            {done, Data#conn_data{buffer = Rest, parser = undefined}};
        done ->
            {done, Data#conn_data{buffer = <<>>, parser = undefined}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Handle result of parsing received data
stream_body_chunk_result({ok, Chunk, NewParser}, Data) ->
    {ok, Chunk, Data#conn_data{parser = NewParser}};
stream_body_chunk_result({done, Rest}, Data) ->
    {done, Data#conn_data{buffer = Rest, parser = undefined}};
stream_body_chunk_result(done, Data) ->
    {done, Data#conn_data{buffer = <<>>, parser = undefined}};
stream_body_chunk_result({more, NewParser}, Data) ->
    stream_body_chunk(Data#conn_data{parser = NewParser});
stream_body_chunk_result({more, NewParser, _Buffer}, Data) ->
    stream_body_chunk(Data#conn_data{parser = NewParser});
stream_body_chunk_result({error, Reason}, _Data) ->
    {error, Reason}.

%% @private Receive data from socket
recv_data(#conn_data{transport = Transport, socket = Socket, recv_timeout = Timeout}) ->
    Transport:recv(Socket, 0, Timeout).
