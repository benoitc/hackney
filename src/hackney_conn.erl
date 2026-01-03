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
    %% Request/Response (sync)
    request/5,
    request/6,
    body/1,
    body/2,
    stream_body/1,
    %% Streaming body (request body)
    send_request_headers/4,
    send_body_chunk/2,
    finish_send_body/1,
    start_response/1,
    %% Async streaming
    request_async/6,
    request_async/7,
    request_async/8,
    stream_next/1,
    stop_async/1,
    pause_stream/1,
    resume_stream/1,
    %% Socket operations
    setopts/2,
    peername/1,
    sockname/1,
    %% Low-level socket operations (for hackney_request/hackney_response)
    send/2,
    recv/2,
    recv/3,
    close/1,
    %% Response info
    response_headers/1,
    get_location/1,
    set_location/2,
    %% Pool management
    release_to_pool/1,
    verify_socket/1,
    is_ready/1
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
    streaming_body/3,
    receiving/3,
    streaming/3,
    streaming_once/3,
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

    %% Pool integration
    pool_pid :: pid() | undefined,  %% If set, connection is from a pool

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
    location :: binary() | undefined,  %% Stores the final URL after redirects

    %% Parser state
    parser :: #hparser{} | undefined,

    %% Async mode
    async = false :: false | true | once | paused,
    stream_to :: pid() | undefined,
    async_ref :: pid() | undefined,  %% Connection PID used as message correlation ref
    follow_redirect = false :: boolean()
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

%% @doc Send only the request headers (for streaming body mode).
%% After this, use send_body_chunk/2 and finish_send_body/1 to send the body,
%% then start_response/1 to receive the response.
-spec send_request_headers(pid(), binary(), binary(), list()) -> ok | {error, term()}.
send_request_headers(Pid, Method, Path, Headers) ->
    gen_statem:call(Pid, {send_headers, Method, Path, Headers}, infinity).

%% @doc Send a chunk of the request body.
-spec send_body_chunk(pid(), iodata()) -> ok | {error, term()}.
send_body_chunk(Pid, Data) ->
    gen_statem:call(Pid, {send_body_chunk, Data}, infinity).

%% @doc Finish sending the request body.
-spec finish_send_body(pid()) -> ok | {error, term()}.
finish_send_body(Pid) ->
    gen_statem:call(Pid, finish_send_body, infinity).

%% @doc Start receiving the response after sending the full body.
-spec start_response(pid()) -> {ok, integer(), list(), pid()} | {error, term()}.
start_response(Pid) ->
    gen_statem:call(Pid, start_response, infinity).

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

%% @doc Send an HTTP request asynchronously.
%% Returns {ok, Ref} immediately. Response is sent as messages:
%%   - {hackney_response, Ref, {status, Status, Reason}}
%%   - {hackney_response, Ref, {headers, Headers}}
%%   - {hackney_response, Ref, Data} (body chunks)
%%   - {hackney_response, Ref, done}
%%   - {hackney_response, Ref, {error, Reason}}
%% When follow_redirect is true and response is a redirect:
%%   - {hackney_response, Ref, {redirect, Location, Headers}} for 301,302,307,308
%%   - {hackney_response, Ref, {see_other, Location, Headers}} for 303 with POST
%% AsyncMode: true (continuous) or once (one message at a time, use stream_next/1)
-spec request_async(pid(), binary(), binary(), list(), binary() | iolist(), true | once) ->
    {ok, reference()} | {error, term()}.
request_async(Pid, Method, Path, Headers, Body, AsyncMode) ->
    request_async(Pid, Method, Path, Headers, Body, AsyncMode, self(), false).

-spec request_async(pid(), binary(), binary(), list(), binary() | iolist(), true | once, pid()) ->
    {ok, reference()} | {error, term()}.
request_async(Pid, Method, Path, Headers, Body, AsyncMode, StreamTo) ->
    request_async(Pid, Method, Path, Headers, Body, AsyncMode, StreamTo, false).

-spec request_async(pid(), binary(), binary(), list(), binary() | iolist(), true | once, pid(), boolean()) ->
    {ok, reference()} | {error, term()}.
request_async(Pid, Method, Path, Headers, Body, AsyncMode, StreamTo, FollowRedirect) ->
    gen_statem:call(Pid, {request_async, Method, Path, Headers, Body, AsyncMode, StreamTo, FollowRedirect}).

%% @doc Request the next message in {async, once} mode.
-spec stream_next(pid()) -> ok | {error, term()}.
stream_next(Pid) ->
    gen_statem:cast(Pid, stream_next).

%% @doc Stop async mode and return to sync mode.
-spec stop_async(pid()) -> ok | {error, term()}.
stop_async(Pid) ->
    gen_statem:call(Pid, stop_async).

%% @doc Pause async streaming (hibernate the stream).
-spec pause_stream(pid()) -> ok | {error, term()}.
pause_stream(Pid) ->
    gen_statem:cast(Pid, pause_stream).

%% @doc Resume paused async streaming.
-spec resume_stream(pid()) -> ok | {error, term()}.
resume_stream(Pid) ->
    gen_statem:cast(Pid, resume_stream).

%% @doc Set socket options on the underlying socket.
-spec setopts(pid(), list()) -> ok | {error, term()}.
setopts(Pid, Opts) ->
    gen_statem:call(Pid, {setopts, Opts}).

%% @doc Get the remote address and port.
-spec peername(pid()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
peername(Pid) ->
    gen_statem:call(Pid, peername).

%% @doc Get the local address and port.
-spec sockname(pid()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
sockname(Pid) ->
    gen_statem:call(Pid, sockname).

%% @doc Get the last response headers.
-spec response_headers(pid()) -> list() | undefined.
response_headers(Pid) ->
    gen_statem:call(Pid, response_headers).

%% @doc Get the stored location (final URL after redirects).
-spec get_location(pid()) -> binary() | undefined.
get_location(Pid) ->
    gen_statem:call(Pid, get_location).

%% @doc Set the location (used after following redirects).
-spec set_location(pid(), binary()) -> ok.
set_location(Pid, Location) ->
    gen_statem:call(Pid, {set_location, Location}).

%% @doc Send data through the connection process.
%% This is a low-level function used by hackney_request.
-spec send(pid(), iodata()) -> ok | {error, term()}.
send(Pid, Data) ->
    gen_statem:call(Pid, {send, Data}).

%% @doc Receive data from the connection process.
%% This is a low-level function used by hackney_response.
-spec recv(pid(), timeout()) -> {ok, binary()} | {error, term()}.
recv(Pid, Timeout) ->
    recv(Pid, 0, Timeout).

-spec recv(pid(), non_neg_integer(), timeout()) -> {ok, binary()} | {error, term()}.
recv(Pid, Length, Timeout) ->
    gen_statem:call(Pid, {recv, Length, Timeout}, Timeout + 5000).

%% @doc Close the connection.
%% This is a low-level function that closes the socket but keeps the process.
-spec close(pid()) -> ok.
close(Pid) ->
    gen_statem:call(Pid, close_socket).

%% @doc Release the connection back to the pool.
%% This notifies the pool that the connection is available for reuse.
%% Uses a synchronous call to ensure the pool has processed the checkin.
-spec release_to_pool(pid()) -> ok.
release_to_pool(Pid) ->
    gen_statem:call(Pid, release_to_pool, 5000).

%% @doc Check if the connection's socket is still healthy.
%% Returns ok if socket is open, {error, closed} otherwise.
-spec verify_socket(pid()) -> ok | {error, closed | term()}.
verify_socket(Pid) ->
    gen_statem:call(Pid, verify_socket).

%% @doc Check if the connection is ready for a new request.
%% Returns {ok, connected} if ready, or error/closed status.
%% This combines state check and socket verification in one call.
-spec is_ready(pid()) -> {ok, connected} | {ok, closed} | {error, term()}.
is_ready(Pid) ->
    gen_statem:call(Pid, is_ready).

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

    %% Check if a pre-established socket is provided (e.g., from proxy)
    Socket = maps:get(socket, Opts, undefined),

    Data = #conn_data{
        owner = Owner,
        owner_mon = OwnerMon,
        host = Host,
        port = Port,
        netloc = Netloc,
        transport = Transport,
        socket = Socket,
        connect_timeout = maps:get(connect_timeout, Opts, ?CONNECT_TIMEOUT),
        recv_timeout = maps:get(recv_timeout, Opts, ?RECV_TIMEOUT),
        idle_timeout = maps:get(idle_timeout, Opts, ?IDLE_TIMEOUT),
        connect_options = maps:get(connect_options, Opts, []),
        ssl_options = maps:get(ssl_options, Opts, []),
        pool_pid = maps:get(pool_pid, Opts, undefined)
    },

    %% If socket is provided, start in connected state; otherwise start in idle
    case Socket of
        undefined ->
            {ok, idle, Data};
        _ ->
            {ok, connected, Data}
    end.

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
        ssl_options = SslOpts0
    } = Data,

    %% Build connection options
    Opts = case Transport of
        hackney_ssl ->
            %% Get default SSL options with hostname verification
            DefaultSslOpts = hackney_ssl:check_hostname_opts(Host),
            %% Merge user-provided SSL options (they override defaults)
            MergedSslOpts = hackney_util:merge_opts(DefaultSslOpts, SslOpts0),
            %% Pass SSL options under ssl_options key as expected by hackney_ssl
            ConnectOpts ++ [{ssl_options, MergedSslOpts}];
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

connected({call, From}, release_to_pool, Data) ->
    %% Notify pool that connection is available for reuse (sync)
    notify_pool_available_sync(Data),
    {keep_state_and_data, [{reply, From, ok}]};

connected(state_timeout, idle_timeout, Data) ->
    %% Idle timeout - close connection
    {next_state, closed, Data};

connected({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, connected}}]};

connected({call, From}, verify_socket, #conn_data{socket = undefined} = Data) ->
    %% Socket not connected
    {next_state, closed, Data, [{reply, From, {error, closed}}]};
connected({call, From}, verify_socket, #conn_data{transport = Transport, socket = Socket} = Data) ->
    %% Check if socket has any pending data or close messages
    case check_socket_health(Transport, Socket) of
        ok -> {keep_state_and_data, [{reply, From, ok}]};
        {error, Reason} ->
            {next_state, closed, Data#conn_data{socket = undefined}, [{reply, From, {error, Reason}}]}
    end;

connected({call, From}, is_ready, #conn_data{socket = undefined} = Data) ->
    %% Socket not connected
    {next_state, closed, Data, [{reply, From, {ok, closed}}]};
connected({call, From}, is_ready, #conn_data{transport = Transport, socket = Socket}) ->
    %% Combined state and socket check
    case check_socket_health(Transport, Socket) of
        ok -> {keep_state_and_data, [{reply, From, {ok, connected}}]};
        {error, _} -> {keep_state_and_data, [{reply, From, {ok, closed}}]}
    end;

connected({call, From}, {request, Method, Path, Headers, Body}, Data) ->
    %% Start a new sync request
    NewData = Data#conn_data{
        request_from = From,
        method = Method,
        path = Path,
        parser = undefined,
        version = undefined,
        status = undefined,
        reason = undefined,
        response_headers = undefined,
        buffer = <<>>,
        async = false,
        async_ref = undefined,
        stream_to = undefined
    },
    {next_state, sending, NewData, [{next_event, internal, {send_request, Method, Path, Headers, Body}}]};

connected({call, From}, {request_async, Method, Path, Headers, Body, AsyncMode, StreamTo}, Data) ->
    %% Start a new async request (no redirect following)
    do_request_async(From, Method, Path, Headers, Body, AsyncMode, StreamTo, false, Data);

connected({call, From}, {request_async, Method, Path, Headers, Body, AsyncMode, StreamTo, FollowRedirect}, Data) ->
    %% Start a new async request with redirect option
    do_request_async(From, Method, Path, Headers, Body, AsyncMode, StreamTo, FollowRedirect, Data);

connected({call, From}, {send_headers, Method, Path, Headers}, Data) ->
    %% Send only headers for streaming body mode
    NewData = Data#conn_data{
        request_from = From,
        method = Method,
        path = Path,
        parser = undefined,
        version = undefined,
        status = undefined,
        reason = undefined,
        response_headers = undefined,
        buffer = <<>>,
        async = false,
        async_ref = undefined,
        stream_to = undefined
    },
    %% Transition to streaming_body state
    {next_state, streaming_body, NewData, [{next_event, internal, {send_headers_only, Method, Path, Headers}}]};

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
    case do_send_request(Method, Path, Headers, Body, Data) of
        {ok, NewData} ->
            {next_state, receiving, NewData, [{next_event, internal, do_recv_response}]};
        {error, Reason} ->
            {next_state, closed, Data, [{reply, Data#conn_data.request_from, {error, Reason}}]}
    end;

sending(internal, {send_request_async, Method, Path, Headers, Body}, Data) ->
    case do_send_request(Method, Path, Headers, Body, Data) of
        {ok, NewData} ->
            %% Reply with ref immediately, then start async receiving
            From = NewData#conn_data.request_from,
            Ref = NewData#conn_data.async_ref,
            {next_state, receiving, NewData#conn_data{request_from = undefined},
             [{reply, From, {ok, Ref}}, {next_event, internal, do_recv_response_async}]};
        {error, Reason} ->
            From = Data#conn_data.request_from,
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
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
%% State: streaming_body - Streaming request body
%%====================================================================

streaming_body(enter, connected, _Data) ->
    keep_state_and_data;

streaming_body(internal, {send_headers_only, Method, Path, Headers}, Data) ->
    %% Send only headers, then return ok and wait for body chunks
    #conn_data{host = Host, port = Port, transport = Transport, socket = Socket} = Data,
    %% Build request line and headers (with Transfer-Encoding: chunked for streaming)
    HeadersObj = hackney_headers_new:from_list(Headers),
    %% Add Transfer-Encoding: chunked if not present
    HeadersWithTE = case hackney_headers_new:get_value(<<"transfer-encoding">>, HeadersObj) of
        undefined -> hackney_headers_new:store(<<"Transfer-Encoding">>, <<"chunked">>, HeadersObj);
        _ -> HeadersObj
    end,
    HeadersList = hackney_headers_new:to_list(HeadersWithTE),
    RequestLine = build_request_line(Method, Path),
    HeaderLines = [[Name, <<": ">>, Value, <<"\r\n">>] || {Name, Value} <- HeadersList],
    HeadersData = [RequestLine, HeaderLines, <<"\r\n">>],
    case Transport:send(Socket, HeadersData) of
        ok ->
            From = Data#conn_data.request_from,
            {keep_state, Data#conn_data{request_from = undefined}, [{reply, From, ok}]};
        {error, Reason} ->
            From = Data#conn_data.request_from,
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
    end;

streaming_body({call, From}, {send_body_chunk, BodyData}, Data) ->
    #conn_data{transport = Transport, socket = Socket} = Data,
    %% Send as chunked encoding
    ChunkData = encode_chunk(BodyData),
    case Transport:send(Socket, ChunkData) of
        ok ->
            {keep_state_and_data, [{reply, From, ok}]};
        {error, Reason} ->
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
    end;

streaming_body({call, From}, finish_send_body, Data) ->
    #conn_data{transport = Transport, socket = Socket} = Data,
    %% Send final chunk marker
    case Transport:send(Socket, <<"0\r\n\r\n">>) of
        ok ->
            {keep_state, Data#conn_data{request_from = From}, [{reply, From, ok}]};
        {error, Reason} ->
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
    end;

streaming_body({call, From}, start_response, Data) ->
    %% Transition to receiving state and get response
    %% Use {do_recv_response, include_pid} to include pid in response
    NewData = Data#conn_data{request_from = From},
    {next_state, receiving, NewData, [{next_event, internal, {do_recv_response, include_pid}}]};

streaming_body({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, streaming_body}}]};

streaming_body(info, {tcp_closed, Socket}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming_body(info, {ssl_closed, Socket}, #conn_data{socket = Socket} = Data) ->
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming_body(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

streaming_body(EventType, Event, Data) ->
    handle_common(EventType, Event, streaming_body, Data).

%%====================================================================
%% State: receiving - Awaiting/streaming response
%%====================================================================

receiving(enter, sending, _Data) ->
    %% Just enter state, request handling happens in internal event
    keep_state_and_data;

receiving(enter, streaming_body, _Data) ->
    %% Coming from streaming body state
    keep_state_and_data;

receiving(enter, receiving, _Data) ->
    %% Re-entering after body streaming
    keep_state_and_data;

receiving(internal, do_recv_response, Data) ->
    %% Receive and parse response status and headers (sync mode, no pid in response)
    do_recv_response_impl(Data, false);

receiving(internal, {do_recv_response, include_pid}, Data) ->
    %% Receive and parse response status and headers (sync mode, include pid in response)
    do_recv_response_impl(Data, true);

receiving(internal, do_recv_response_async, Data) ->
    %% Receive and parse response status and headers (async mode)
    Parser = hackney_http:parser([response]),
    DataWithParser = Data#conn_data{parser = Parser},
    #conn_data{async_ref = Ref, stream_to = StreamTo, async = AsyncMode,
               follow_redirect = FollowRedirect, method = Method} = Data,
    case recv_status_and_headers(DataWithParser) of
        {ok, Status, Headers, NewData} ->
            HeadersList = hackney_headers_new:to_list(Headers),
            %% Check if this is a redirect and we should handle it
            case maybe_handle_async_redirect(Status, Method, Headers, FollowRedirect) of
                {redirect, Location} ->
                    %% Skip body and send redirect message
                    _ = skip_response_body(NewData),
                    StreamTo ! {hackney_response, Ref, {redirect, Location, HeadersList}},
                    notify_pool_available(NewData),
                    {next_state, connected, NewData#conn_data{response_headers = Headers}};
                {see_other, Location} ->
                    %% Skip body and send see_other message
                    _ = skip_response_body(NewData),
                    StreamTo ! {hackney_response, Ref, {see_other, Location, HeadersList}},
                    notify_pool_available(NewData),
                    {next_state, connected, NewData#conn_data{response_headers = Headers}};
                no_redirect ->
                    %% Normal response - send status and headers
                    StreamTo ! {hackney_response, Ref, {status, Status, NewData#conn_data.reason}},
                    StreamTo ! {hackney_response, Ref, {headers, HeadersList}},
                    %% Transition to appropriate streaming state based on mode
                    NextState = case AsyncMode of
                        true -> streaming;
                        once -> streaming_once
                    end,
                    {next_state, NextState, NewData#conn_data{response_headers = Headers}}
            end;
        {error, Reason} ->
            StreamTo ! {hackney_response, Ref, {error, Reason}},
            {next_state, closed, Data}
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
%% State: streaming - Continuous async body streaming (async=true)
%%====================================================================

streaming(enter, receiving, Data) ->
    %% Start streaming immediately by sending self a cast
    gen_statem:cast(self(), do_stream),
    {keep_state, Data};

streaming(enter, streaming, _Data) ->
    keep_state_and_data;

streaming(cast, do_stream, Data) ->
    %% Stream body chunks continuously
    #conn_data{async_ref = Ref, stream_to = StreamTo} = Data,
    case stream_body_chunk(Data) of
        {ok, Chunk, NewData} ->
            StreamTo ! {hackney_response, Ref, Chunk},
            %% Continue streaming
            gen_statem:cast(self(), do_stream),
            {keep_state, NewData};
        {done, NewData} ->
            StreamTo ! {hackney_response, Ref, done},
            %% Check Connection: close header and pool status
            finish_async_streaming(NewData);
        {error, Reason} ->
            StreamTo ! {hackney_response, Ref, {error, Reason}},
            {next_state, closed, Data}
    end;

streaming(cast, pause_stream, Data) ->
    %% Pause - transition to streaming_once which waits for explicit next
    {next_state, streaming_once, Data};

streaming({call, From}, stop_async, Data) ->
    {next_state, receiving, reset_async(Data), [{reply, From, ok}]};

streaming({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, streaming}}]};

streaming(info, {tcp_closed, Socket}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, closed}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming(info, {ssl_closed, Socket}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, closed}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming(info, {tcp_error, Socket, Reason}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, Reason}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming(info, {ssl_error, Socket, Reason}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, Reason}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

streaming(EventType, Event, Data) ->
    handle_common(EventType, Event, streaming, Data).

%%====================================================================
%% State: streaming_once - On-demand async streaming (async=once)
%%====================================================================

streaming_once(enter, _OldState, _Data) ->
    %% Wait for stream_next
    keep_state_and_data;

streaming_once(cast, stream_next, Data) ->
    %% Stream one chunk
    #conn_data{async_ref = Ref, stream_to = StreamTo} = Data,
    case stream_body_chunk(Data) of
        {ok, Chunk, NewData} ->
            StreamTo ! {hackney_response, Ref, Chunk},
            {keep_state, NewData};
        {done, NewData} ->
            StreamTo ! {hackney_response, Ref, done},
            %% Check Connection: close header and pool status
            finish_async_streaming(NewData);
        {error, Reason} ->
            StreamTo ! {hackney_response, Ref, {error, Reason}},
            {next_state, closed, Data}
    end;

streaming_once(cast, resume_stream, Data) ->
    %% Switch to continuous mode
    {next_state, streaming, Data#conn_data{async = true}};

streaming_once({call, From}, stop_async, Data) ->
    {next_state, receiving, reset_async(Data), [{reply, From, ok}]};

streaming_once({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, streaming_once}}]};

streaming_once(info, {tcp_closed, Socket}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, closed}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming_once(info, {ssl_closed, Socket}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, closed}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming_once(info, {tcp_error, Socket, Reason}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, Reason}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming_once(info, {ssl_error, Socket, Reason}, #conn_data{socket = Socket, async_ref = Ref, stream_to = StreamTo} = Data) ->
    StreamTo ! {hackney_response, Ref, {error, Reason}},
    {next_state, closed, Data#conn_data{socket = undefined}};

streaming_once(info, {'DOWN', Ref, process, _Pid, _Reason}, #conn_data{owner_mon = Ref} = Data) ->
    {stop, normal, Data};

streaming_once(EventType, Event, Data) ->
    handle_common(EventType, Event, streaming_once, Data).

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

%% Socket operations - available in any state with a socket
handle_common({call, From}, {setopts, Opts}, _State, #conn_data{transport = Transport, socket = Socket} = _Data)
  when Socket =/= undefined ->
    Result = Transport:setopts(Socket, Opts),
    {keep_state_and_data, [{reply, From, Result}]};

handle_common({call, From}, peername, _State, #conn_data{transport = Transport, socket = Socket} = _Data)
  when Socket =/= undefined ->
    Result = Transport:peername(Socket),
    {keep_state_and_data, [{reply, From, Result}]};

handle_common({call, From}, sockname, _State, #conn_data{transport = Transport, socket = Socket} = _Data)
  when Socket =/= undefined ->
    Result = Transport:sockname(Socket),
    {keep_state_and_data, [{reply, From, Result}]};

%% Low-level send operation
handle_common({call, From}, {send, Data}, _State, #conn_data{transport = Transport, socket = Socket} = _Data)
  when Socket =/= undefined ->
    Result = Transport:send(Socket, Data),
    {keep_state_and_data, [{reply, From, Result}]};

%% Low-level recv operation
handle_common({call, From}, {recv, Length, Timeout}, _State, #conn_data{transport = Transport, socket = Socket} = _Data)
  when Socket =/= undefined ->
    Result = Transport:recv(Socket, Length, Timeout),
    {keep_state_and_data, [{reply, From, Result}]};

%% Close socket but keep process
handle_common({call, From}, close_socket, _State, #conn_data{transport = Transport, socket = Socket} = Data)
  when Socket =/= undefined ->
    Transport:close(Socket),
    {next_state, closed, Data#conn_data{socket = undefined}, [{reply, From, ok}]};

handle_common({call, From}, close_socket, _State, Data) ->
    {next_state, closed, Data, [{reply, From, ok}]};

handle_common({call, From}, {send, _SendData}, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

handle_common({call, From}, {recv, _Length, _Timeout}, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

handle_common({call, From}, {setopts, _Opts}, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

handle_common({call, From}, peername, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

handle_common({call, From}, sockname, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};

handle_common({call, From}, response_headers, _State, #conn_data{response_headers = Headers}) ->
    {keep_state_and_data, [{reply, From, Headers}]};

handle_common({call, From}, get_location, _State, #conn_data{location = Location}) ->
    {keep_state_and_data, [{reply, From, Location}]};

handle_common({call, From}, {set_location, Location}, _State, Data) ->
    {keep_state, Data#conn_data{location = Location}, [{reply, From, ok}]};

handle_common({call, From}, _, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

handle_common(info, _Msg, _State, _Data) ->
    keep_state_and_data.

%% @private Reply to caller and stop
reply_and_stop(undefined, _Reply, Data) ->
    {stop, normal, Data};
reply_and_stop(From, Reply, _Data) ->
    {stop_and_reply, normal, [{reply, From, Reply}]}.

%% @private Reset async state
reset_async(Data) ->
    Data#conn_data{
        async = false,
        async_ref = undefined,
        stream_to = undefined
    }.

%% @private Check if connection should be closed based on response headers
should_close_connection(#conn_data{response_headers = undefined}) ->
    false;
should_close_connection(#conn_data{response_headers = Headers}) ->
    case hackney_headers_new:get_value(<<"connection">>, Headers) of
        undefined -> false;
        Value -> hackney_bstr:to_lower(Value) =:= <<"close">>
    end.

%% @private Finish async streaming - close or return to connected based on Connection header
finish_async_streaming(Data) ->
    #conn_data{transport = Transport, socket = Socket, pool_pid = PoolPid} = Data,
    case should_close_connection(Data) of
        true ->
            %% Connection: close - close socket and stop process
            case Socket of
                undefined -> ok;
                _ -> Transport:close(Socket)
            end,
            {stop, normal, reset_async(Data#conn_data{socket = undefined})};
        false when PoolPid =:= undefined ->
            %% No pool and no Connection: close - still stop since no reuse
            case Socket of
                undefined -> ok;
                _ -> Transport:close(Socket)
            end,
            {stop, normal, reset_async(Data#conn_data{socket = undefined})};
        false ->
            %% Has pool or keep-alive - return to connected for reuse
            {next_state, connected, reset_async(Data)}
    end.

%% @private Handle receiving response status and headers
%% IncludePid determines whether to include pid in the response
do_recv_response_impl(Data, IncludePid) ->
    Parser = hackney_http:parser([response]),
    DataWithParser = Data#conn_data{parser = Parser},
    case recv_status_and_headers(DataWithParser) of
        {ok, Status, Headers, NewData} ->
            From = NewData#conn_data.request_from,
            HeadersList = hackney_headers_new:to_list(Headers),
            Reply = case IncludePid of
                true -> {ok, Status, HeadersList, self()};
                false -> {ok, Status, HeadersList}
            end,
            {keep_state, NewData#conn_data{
                request_from = undefined,
                response_headers = Headers
            }, [{reply, From, Reply}]};
        {error, Reason} ->
            From = Data#conn_data.request_from,
            {next_state, closed, Data, [{reply, From, {error, Reason}}]}
    end.

%% @private Send HTTP request (shared by sync and async)
do_send_request(Method, Path, Headers, Body, Data) ->
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
    RequestData = <<RequestLine/binary, HeadersBin/binary>>,

    %% Send request
    case Transport:send(Socket, RequestData) of
        ok ->
            %% Send body if present
            case send_body(Transport, Socket, Body) of
                ok ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

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

%% @private Build request line
build_request_line(Method, Path) ->
    [Method, <<" ">>, Path, <<" HTTP/1.1\r\n">>].

%% @private Encode a chunk for chunked transfer encoding
encode_chunk(Data) when is_binary(Data) ->
    Size = byte_size(Data),
    SizeHex = integer_to_binary(Size, 16),
    [SizeHex, <<"\r\n">>, Data, <<"\r\n">>];
encode_chunk(Data) when is_list(Data) ->
    Size = iolist_size(Data),
    SizeHex = integer_to_binary(Size, 16),
    [SizeHex, <<"\r\n">>, Data, <<"\r\n">>].

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
send_body(_Transport, _Socket, stream) ->
    %% Stream body mode - body will be sent separately
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

%% @private Check if socket is healthy (not closed by peer)
check_socket_health(Transport, Socket) ->
    %% Use a non-blocking recv with 0 timeout to check for pending close
    case Transport:recv(Socket, 0, 0) of
        {error, timeout} ->
            %% No data pending, socket is healthy
            ok;
        {error, closed} ->
            %% Socket is closed
            {error, closed};
        {error, Reason} ->
            %% Some other error
            {error, Reason};
        {ok, _ExtraData} ->
            %% Unexpected data - could be server closing with data
            %% Treat as ok for now - the next request will handle it
            ok
    end.

%% @private Notify pool that connection is available for reuse (async)
notify_pool_available(#conn_data{pool_pid = undefined}) ->
    %% Not from a pool, nothing to do
    ok;
notify_pool_available(#conn_data{pool_pid = PoolPid}) ->
    %% Tell the pool this connection is available
    gen_server:cast(PoolPid, {checkin, nil, self()}).

%% @private Notify pool that connection is available for reuse (sync)
%% This ensures the pool has processed the checkin before returning.
notify_pool_available_sync(#conn_data{pool_pid = undefined}) ->
    ok;
notify_pool_available_sync(#conn_data{pool_pid = PoolPid}) ->
    gen_server:call(PoolPid, {checkin_sync, self()}, 5000).

%% @private Start an async request
do_request_async(From, Method, Path, Headers, Body, AsyncMode, StreamTo, FollowRedirect, Data) ->
    %% Use self() (connection PID) as the async ref for message correlation
    Ref = self(),
    NewData = Data#conn_data{
        request_from = From,
        method = Method,
        path = Path,
        parser = undefined,
        version = undefined,
        status = undefined,
        reason = undefined,
        response_headers = undefined,
        buffer = <<>>,
        async = AsyncMode,
        async_ref = Ref,
        stream_to = StreamTo,
        follow_redirect = FollowRedirect
    },
    {next_state, sending, NewData, [{next_event, internal, {send_request_async, Method, Path, Headers, Body}}]}.

%% @private Check if response is a redirect that should be handled
maybe_handle_async_redirect(Status, Method, Headers, true) when
        Status =:= 301; Status =:= 302; Status =:= 307; Status =:= 308 ->
    %% Redirect status - get location
    case hackney_headers_new:get_value(<<"location">>, Headers) of
        undefined -> no_redirect;
        Location -> {redirect, Location}
    end;
maybe_handle_async_redirect(303, <<"POST">>, Headers, true) ->
    %% 303 See Other for POST - should redirect as GET
    case hackney_headers_new:get_value(<<"location">>, Headers) of
        undefined -> no_redirect;
        Location -> {see_other, Location}
    end;
maybe_handle_async_redirect(_, _, _, _) ->
    no_redirect.

%% @private Skip the response body
skip_response_body(Data) ->
    case read_full_body(Data, <<>>) of
        {ok, _, NewData} -> NewData;
        {error, _} -> Data
    end.
