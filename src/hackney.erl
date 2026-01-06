%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Simplified hackney API using process-per-connection architecture.
%%% Connection handles are now hackney_conn process PIDs.

-module(hackney).

-export([connect/1, connect/2, connect/3, connect/4,
         close/1,
         peername/1,
         sockname/1,
         request/1, request/2, request/3, request/4, request/5,
         send_request/2,
         cookies/1,
         body/1, body/2, skip_body/1,
         stream_body/1,
         send_body/2, finish_send_body/1, start_response/1,
         setopts/2]).

%% WebSocket API
-export([ws_connect/1, ws_connect/2,
         ws_send/2,
         ws_recv/1, ws_recv/2,
         ws_setopts/2,
         ws_close/1, ws_close/2]).

-export([redirect_location/1, location/1]).

-export([get_version/0]).
-export([default_ua/0]).

%% Async streaming
-export([stream_next/1,
         stop_async/1,
         pause_stream/1,
         resume_stream/1]).

-export([parse_proxy_url/1]).

-ifdef(TEST).
-export([get_proxy_env/1, do_get_proxy_env/1]).
-export([get_proxy_config/3]).
-export([check_no_proxy/2]).
-export([start_conn_with_socket/5]).
-endif.

-define(METHOD_TPL(Method),
  -export([Method/1, Method/2, Method/3, Method/4])).
-include("hackney_methods.hrl").

-include("hackney.hrl").
-include("hackney_lib.hrl").
-include_lib("hackney_internal.hrl").


-type url() :: #hackney_url{} | binary().
-type conn() :: pid().
-type request_ret() ::
    {ok, integer(), list(), conn()} |     %% normal response with body to read
    {ok, integer(), list(), binary()} |   %% with_body option
    {ok, integer(), list()} |             %% HEAD request
    {ok, reference()} |                   %% async mode
    {ok, conn()} |                        %% streaming body mode (body = stream)
    {error, term()}.
-export_type([url/0, conn/0, request_ret/0]).

%%====================================================================
%% Connection API
%%====================================================================

connect(URL) ->
  connect(URL, []).

connect(#hackney_url{}=URL, Options) ->
  #hackney_url{transport=Transport,
    host=Host,
    port=Port} = URL,
  connect(Transport, Host, Port, Options);
connect(URL, Options) when is_binary(URL) orelse is_list(URL) ->
  connect(hackney_url:parse_url(URL), Options).

%% @doc Connect to a host and return a connection handle (hackney_conn PID).
-spec connect(module(), string(), inet:port_number()) -> {ok, conn()} | {error, term()}.
connect(Transport, Host, Port) ->
  connect(Transport, Host, Port, []).

-spec connect(module(), string(), inet:port_number(), list()) -> {ok, conn()} | {error, term()}.
connect(Transport, Host, Port, Options) ->
  %% Check if using a pool
  UsePool = use_pool(Options),
  case UsePool of
    false ->
      %% Direct connection - start a hackney_conn process
      connect_direct(Transport, Host, Port, Options);
    _PoolName ->
      %% Pool mode with per-host load regulation
      connect_pool(Transport, Host, Port, Options)
  end.

%% @private Direct connection without pool
connect_direct(Transport, Host, Port, Options) ->
  %% Build connect_options including protocols for ALPN
  BaseConnectOpts = proplists:get_value(connect_options, Options, []),
  Protocols = proplists:get_value(protocols, Options, undefined),
  ConnectOpts = case Protocols of
    undefined -> BaseConnectOpts;
    _ -> [{protocols, Protocols} | BaseConnectOpts]
  end,
  ConnOpts = #{
    host => Host,
    port => Port,
    transport => Transport,
    connect_timeout => proplists:get_value(connect_timeout, Options, 8000),
    recv_timeout => proplists:get_value(recv_timeout, Options, 5000),
    connect_options => ConnectOpts,
    ssl_options => proplists:get_value(ssl_options, Options, [])
  },
  case hackney_conn_sup:start_conn(ConnOpts) of
    {ok, ConnPid} ->
      case hackney_conn:connect(ConnPid) of
        ok ->
          hackney_manager:start_request(Host),
          {ok, ConnPid};
        {error, Reason} ->
          catch hackney_conn:stop(ConnPid),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @private Pool connection with load regulation
%% Flow:
%% 1. For SSL: Check if existing HTTP/2 connection can be reused (multiplexing)
%% 2. Acquire slot from load_regulation (blocks if at per-host limit)
%% 3. Get TCP connection from pool (always TCP, pool doesn't store SSL)
%% 4. Upgrade to SSL if needed (in-place upgrade)
%% 5. If HTTP/2 negotiated, register for multiplexing
%% Note: load_regulation slot is released when connection is checked in or dies
connect_pool(Transport, Host, Port, Options) ->
  PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),

  %% For SSL connections, check if we can reuse an HTTP/2 connection
  case Transport of
    hackney_ssl ->
      case PoolHandler:checkout_h2(Host, Port, Transport, Options) of
        {ok, H2Pid} ->
          %% Reuse existing HTTP/2 connection (multiplexed)
          hackney_manager:start_request(Host),
          {ok, H2Pid};
        none ->
          %% No HTTP/2 connection, proceed with normal pool checkout
          connect_pool_new(Transport, Host, Port, Options, PoolHandler)
      end;
    _ ->
      %% Non-SSL, use normal pool
      connect_pool_new(Transport, Host, Port, Options, PoolHandler)
  end.

connect_pool_new(Transport, Host, Port, Options, PoolHandler) ->
  MaxPerHost = proplists:get_value(max_per_host, Options, 50),
  CheckoutTimeout = proplists:get_value(checkout_timeout, Options,
                      proplists:get_value(connect_timeout, Options, 8000)),

  %% 1. Acquire per-host slot (blocks with backoff until available)
  case hackney_load_regulation:acquire(Host, Port, MaxPerHost, CheckoutTimeout) of
    ok ->
      %% Slot acquired - now get connection from pool
      %% Always checkout as TCP - pool only stores TCP connections
      case PoolHandler:checkout(Host, Port, hackney_tcp, Options) of
        {ok, _PoolRef, ConnPid} ->
          %% Got TCP connection - upgrade to SSL if needed
          case maybe_upgrade_ssl(Transport, ConnPid, Host, Options) of
            ok ->
              %% Check if HTTP/2 was negotiated, register for multiplexing
              maybe_register_h2(ConnPid, Host, Port, Transport, Options, PoolHandler),
              hackney_manager:start_request(Host),
              {ok, ConnPid};
            {error, Reason} ->
              %% Upgrade failed - release slot and close connection
              hackney_load_regulation:release(Host, Port),
              catch hackney_conn:stop(ConnPid),
              {error, Reason}
          end;
        {error, Reason} ->
          %% Checkout failed - release slot
          hackney_load_regulation:release(Host, Port),
          {error, Reason}
      end;
    {error, timeout} ->
      {error, checkout_timeout}
  end.

%% @private Register HTTP/2 connection for multiplexing if applicable
maybe_register_h2(ConnPid, Host, Port, Transport, Options, PoolHandler) ->
  case hackney_conn:get_protocol(ConnPid) of
    http2 ->
      %% HTTP/2 negotiated - register for connection sharing
      PoolHandler:register_h2(Host, Port, Transport, ConnPid, Options);
    http1 ->
      ok
  end.

%% @private Upgrade TCP connection to SSL if needed
maybe_upgrade_ssl(hackney_ssl, ConnPid, Host, Options) ->
  SslOpts = proplists:get_value(ssl_options, Options, []),
  %% Add protocols option for ALPN negotiation if specified
  Protocols = proplists:get_value(protocols, Options, undefined),
  SslOpts2 = case Protocols of
    undefined -> SslOpts;
    _ -> [{protocols, Protocols} | SslOpts]
  end,
  %% Check if connection is already SSL (e.g., reused SSL connection)
  case catch hackney_conn:is_upgraded_ssl(ConnPid) of
    true ->
      %% Already SSL, no upgrade needed
      ok;
    _ ->
      %% Upgrade TCP to SSL with ALPN
      hackney_conn:upgrade_to_ssl(ConnPid, [{server_name_indication, Host} | SslOpts2])
  end;
maybe_upgrade_ssl(_, _ConnPid, _Host, _Options) ->
  %% Not SSL, no upgrade needed
  ok.

%% @doc Close a connection.
-spec close(conn()) -> ok.
close(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:stop(ConnPid).

%% @doc Start a connection with a pre-established socket.
%% Used for proxy connections where the tunnel is established first.
%% Socket can be a raw socket or a {Transport, Socket} tuple from proxy modules.
-spec start_conn_with_socket(string(), inet:port_number(), module(),
                              inet:socket() | {module(), inet:socket()}, list()) ->
  {ok, conn()} | {error, term()}.
start_conn_with_socket(Host, Port, _Transport, {SocketTransport, Socket}, Options) ->
  %% Handle {Transport, Socket} tuple from proxy modules
  %% Use the socket's transport for operations
  ActualTransport = normalize_transport(SocketTransport),
  start_conn_with_socket_internal(Host, Port, ActualTransport, Socket, Options);
start_conn_with_socket(Host, Port, Transport, Socket, Options) ->
  %% Raw socket
  ActualTransport = normalize_transport(Transport),
  start_conn_with_socket_internal(Host, Port, ActualTransport, Socket, Options).

start_conn_with_socket_internal(Host, Port, Transport, Socket, Options) ->
  %% Build connect_options including protocols for ALPN
  BaseConnectOpts = proplists:get_value(connect_options, Options, []),
  Protocols = proplists:get_value(protocols, Options, undefined),
  ConnectOpts = case Protocols of
    undefined -> BaseConnectOpts;
    _ -> [{protocols, Protocols} | BaseConnectOpts]
  end,
  ConnOpts = #{
    host => Host,
    port => Port,
    transport => Transport,
    socket => Socket,
    connect_timeout => proplists:get_value(connect_timeout, Options, 8000),
    recv_timeout => proplists:get_value(recv_timeout, Options, 5000),
    connect_options => ConnectOpts,
    ssl_options => proplists:get_value(ssl_options, Options, [])
  },
  case hackney_conn_sup:start_conn(ConnOpts) of
    {ok, ConnPid} ->
      hackney_manager:start_request(Host),
      {ok, ConnPid};
    {error, Reason} ->
      {error, Reason}
  end.

%% Normalize transport atoms (e.g., ssl -> hackney_ssl, gen_tcp -> hackney_tcp)
normalize_transport(hackney_tcp) -> hackney_tcp;
normalize_transport(hackney_ssl) -> hackney_ssl;
normalize_transport(gen_tcp) -> hackney_tcp;
normalize_transport(ssl) -> hackney_ssl;
normalize_transport(Other) -> Other.

%% @doc Get the remote address and port.
-spec peername(conn()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
peername(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:peername(ConnPid).

%% @doc Get the local address and port.
-spec sockname(conn()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
sockname(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:sockname(ConnPid).

%% @doc Set socket options.
-spec setopts(conn(), list()) -> ok | {error, term()}.
setopts(ConnPid, Options) when is_pid(ConnPid) ->
  hackney_conn:setopts(ConnPid, Options).

%%====================================================================
%% Request API
%%====================================================================

%% @doc Make a request.
-spec request(url()) -> request_ret().
request(URL) ->
  request(get, URL).

-spec request(atom() | binary(), url()) -> request_ret().
request(Method, URL) ->
  request(Method, URL, [], <<>>, []).

-spec request(atom() | binary(), url(), list()) -> request_ret().
request(Method, URL, Headers) ->
  request(Method, URL, Headers, <<>>, []).

-spec request(atom() | binary(), url(), list(), term()) -> request_ret().
request(Method, URL, Headers, Body) ->
  request(Method, URL, Headers, Body, []).

%% @doc Make a request.
%%
%% Args:
%% - Method: HTTP method (get, post, put, delete, etc.)
%% - URL: Full URL or parsed hackney_url record
%% - Headers: List of headers
%% - Body: Request body (binary, iolist, {form, KVs}, {file, Path}, etc.)
%% - Options: Request options
%%
%% Options:
%% - with_body: If true, return full body in response
%% - async: true | once - Receive response asynchronously
%% - stream_to: PID to receive async messages
%% - follow_redirect: Follow redirects automatically
%% - max_redirect: Maximum number of redirects (default 5)
%% - pool: Pool name or false for no pooling
%% - connect_timeout: Connection timeout in ms (default 8000)
%% - recv_timeout: Receive timeout in ms (default 5000)
%%
%% Returns:
%% - {ok, Status, Headers, ConnPid}: Success, use body/1 or stream_body/1 to get body
%% - {ok, Status, Headers, Body}: Success with with_body option
%% - {ok, Status, Headers}: HEAD request
%% - {ok, Ref}: Async mode - use stream_next/1 to receive messages
%% - {ok, ConnPid}: Streaming body mode (body = stream) - use send_body/2, finish_send_body/1
%% - {error, Reason}: Error
-spec request(atom() | binary(), url(), list(), term(), list()) -> request_ret().
request(Method, #hackney_url{}=URL0, Headers0, Body, Options0) ->
  PathEncodeFun = proplists:get_value(path_encode_fun, Options0,
    fun hackney_url:pathencode/1),

  %% Normalize the URL
  URL = hackney_url:normalize(URL0, PathEncodeFun),

  ?report_trace("request", [{method, Method},
                            {url, URL},
                            {headers, Headers0},
                            {body, Body},
                            {options, Options0}]),

  #hackney_url{transport=Transport,
               host = Host,
               port = Port,
               user = User,
               password = Password,
               path = Path,
               qs = Query} = URL,

  Options = case User of
              <<>> -> Options0;
              _ -> lists:keystore(basic_auth, 1, Options0, {basic_auth, {User, Password}})
            end,

  %% Build final path
  FinalPath = case Query of
                <<>> -> Path;
                _ -> <<Path/binary, "?", Query/binary>>
              end,

  %% Check for proxy
  case maybe_proxy(Transport, URL#hackney_url.scheme, Host, Port, Options) of
    {ok, ConnPid} ->
      do_request(ConnPid, Method, FinalPath, Headers0, Body, Options, URL, Host);
    {ok, ConnPid, {http_proxy, TargetScheme, TargetHost, TargetPort, ProxyAuth}} ->
      %% HTTP proxy mode - use absolute URLs
      AbsolutePath = build_absolute_url(TargetScheme, TargetHost, TargetPort, FinalPath),
      Headers1 = add_proxy_auth_header(Headers0, ProxyAuth),
      do_request(ConnPid, Method, AbsolutePath, Headers1, Body, Options, URL, Host);
    Error ->
      Error
  end;
request(Method, URL, Headers, Body, Options) when is_binary(URL) orelse is_list(URL) ->
  request(Method, hackney_url:parse_url(URL), Headers, Body, Options).

%% @doc Send a request on an existing connection.
-spec send_request(conn(), {atom(), binary(), list(), term()}) ->
    {ok, integer(), list(), conn()} | {ok, integer(), list()} | {error, term()}.
send_request(ConnPid, {Method, Path, Headers, Body}) when is_pid(ConnPid) ->
  %% Convert method to binary
  MethodBin = hackney_bstr:to_upper(hackney_bstr:to_binary(Method)),
  case hackney_conn:request(ConnPid, MethodBin, Path, Headers, Body) of
    {ok, Status, RespHeaders} ->
      %% HEAD request or no body
      case MethodBin of
        <<"HEAD">> -> {ok, Status, RespHeaders};
        _ -> {ok, Status, RespHeaders, ConnPid}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%%====================================================================
%% Response Body API
%%====================================================================

%% @doc Get the full response body.
-spec body(conn()) -> {ok, binary()} | {error, term()}.
body(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:body(ConnPid).

-spec body(conn(), timeout()) -> {ok, binary()} | {error, term()}.
body(ConnPid, Timeout) when is_pid(ConnPid) ->
  hackney_conn:body(ConnPid, Timeout).

%% @doc Stream the response body in chunks.
-spec stream_body(conn()) -> {ok, binary()} | done | {error, term()}.
stream_body(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:stream_body(ConnPid).

%% @doc Skip the response body and close the connection.
%% The connection is closed rather than returned to pool since we
%% can't guarantee the socket state after skipping.
-spec skip_body(conn()) -> ok | {error, term()}.
skip_body(ConnPid) when is_pid(ConnPid) ->
  case body(ConnPid) of
    {ok, _} ->
      %% Stop the connection process so pool gets DOWN message and decrements in_use
      hackney_conn:stop(ConnPid),
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%%====================================================================
%% Streaming Request Body API
%%====================================================================

%% @doc Send a chunk of the request body.
%% Used when request was initiated with body = stream.
-spec send_body(conn(), iodata()) -> ok | {error, term()}.
send_body(ConnPid, Data) when is_pid(ConnPid) ->
  hackney_conn:send_body_chunk(ConnPid, Data).

%% @doc Finish sending the streaming request body.
-spec finish_send_body(conn()) -> ok | {error, term()}.
finish_send_body(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:finish_send_body(ConnPid).

%% @doc Start receiving the response after sending the full body.
%% Returns {ok, Status, Headers, ConnPid}.
-spec start_response(conn()) -> {ok, integer(), list(), conn()} | {error, term()}.
start_response(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:start_response(ConnPid).

%%====================================================================
%% Async Streaming API
%%====================================================================

%% @doc Request next chunk in {async, once} mode.
-spec stream_next(conn()) -> ok.
stream_next(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:stream_next(ConnPid).

%% @doc Stop async mode and return to sync mode.
-spec stop_async(conn()) -> ok | {error, term()}.
stop_async(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:stop_async(ConnPid).

%% @doc Pause async streaming.
-spec pause_stream(conn()) -> ok.
pause_stream(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:pause_stream(ConnPid).

%% @doc Resume async streaming.
-spec resume_stream(conn()) -> ok.
resume_stream(ConnPid) when is_pid(ConnPid) ->
  hackney_conn:resume_stream(ConnPid).

%%====================================================================
%% WebSocket API
%%====================================================================

%% @doc Connect to a WebSocket server.
%% URL should use ws:// or wss:// scheme.
%%
%% Options:
%% <ul>
%%   <li>active: false | true | once (default false)</li>
%%   <li>headers: Extra headers for upgrade request</li>
%%   <li>protocols: Sec-WebSocket-Protocol values</li>
%%   <li>connect_timeout: Connection timeout in ms (default 8000)</li>
%%   <li>recv_timeout: Receive timeout in ms (default infinity)</li>
%%   <li>connect_options: Options passed to transport connect</li>
%%   <li>ssl_options: Additional SSL options</li>
%% </ul>
%%
%% Returns `{ok, WsPid}' on success, where WsPid is the hackney_ws process.
-spec ws_connect(binary() | string()) -> {ok, pid()} | {error, term()}.
ws_connect(URL) ->
  ws_connect(URL, []).

-spec ws_connect(binary() | string(), list()) -> {ok, pid()} | {error, term()}.
ws_connect(URL, Options) when is_binary(URL) orelse is_list(URL) ->
  #hackney_url{
    transport = Transport,
    scheme = Scheme,
    host = Host,
    port = Port,
    path = Path0,
    qs = Query
  } = hackney_url:parse_url(URL),

  %% Validate scheme
  case Scheme of
    ws -> ok;
    wss -> ok;
    _ -> error({invalid_websocket_scheme, Scheme})
  end,

  %% Build path with query string
  Path = case Query of
    <<>> -> Path0;
    _ -> <<Path0/binary, "?", Query/binary>>
  end,

  %% Get proxy configuration (WebSocket always uses tunnel mode)
  ProxyConfig = get_ws_proxy_config(Scheme, Host, Options),

  %% Build connection options
  WsOpts = #{
    host => Host,
    port => Port,
    transport => Transport,
    path => Path,
    connect_timeout => proplists:get_value(connect_timeout, Options, 8000),
    recv_timeout => proplists:get_value(recv_timeout, Options, infinity),
    connect_options => proplists:get_value(connect_options, Options, []),
    ssl_options => proplists:get_value(ssl_options, Options, []),
    active => proplists:get_value(active, Options, false),
    headers => normalize_ws_headers(proplists:get_value(headers, Options, [])),
    protocols => proplists:get_value(protocols, Options, []),
    proxy => ProxyConfig
  },

  %% Start WebSocket process and connect
  case hackney_ws:start_link(WsOpts) of
    {ok, WsPid} ->
      Timeout = maps:get(connect_timeout, WsOpts),
      try hackney_ws:connect(WsPid, Timeout) of
        ok ->
          {ok, WsPid};
        {error, Reason} ->
          catch exit(WsPid, shutdown),
          {error, Reason}
      catch
        exit:{timeout, _} ->
          catch exit(WsPid, shutdown),
          {error, connect_timeout};
        exit:{noproc, _} ->
          {error, {ws_process_died, noproc}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Send a WebSocket frame.
%% Frame types:
%%   - {text, Data} - Text message
%%   - {binary, Data} - Binary message
%%   - ping | {ping, Data} - Ping frame
%%   - pong | {pong, Data} - Pong frame
%%   - close | {close, Code, Reason} - Close frame
-spec ws_send(pid(), hackney_ws:ws_frame()) -> ok | {error, term()}.
ws_send(WsPid, Frame) when is_pid(WsPid) ->
  hackney_ws:send(WsPid, Frame).

%% @doc Receive a WebSocket frame (passive mode only).
%% Blocks until a frame is received or timeout.
%% Returns {ok, Frame} or {error, Reason}.
-spec ws_recv(pid()) -> {ok, hackney_ws:ws_frame()} | {error, term()}.
ws_recv(WsPid) when is_pid(WsPid) ->
  hackney_ws:recv(WsPid).

-spec ws_recv(pid(), timeout()) -> {ok, hackney_ws:ws_frame()} | {error, term()}.
ws_recv(WsPid, Timeout) when is_pid(WsPid) ->
  hackney_ws:recv(WsPid, Timeout).

%% @doc Set WebSocket options.
%% Supported options: [{active, true | false | once}]
-spec ws_setopts(pid(), list()) -> ok | {error, term()}.
ws_setopts(WsPid, Opts) when is_pid(WsPid) ->
  hackney_ws:setopts(WsPid, Opts).

%% @doc Close WebSocket connection gracefully.
-spec ws_close(pid()) -> ok.
ws_close(WsPid) when is_pid(WsPid) ->
  hackney_ws:close(WsPid).

-spec ws_close(pid(), {integer(), binary()}) -> ok.
ws_close(WsPid, {Code, Reason}) when is_pid(WsPid) ->
  hackney_ws:close(WsPid, {Code, Reason}).

%% @private Normalize WebSocket headers to {binary(), binary()} format
normalize_ws_headers(Headers) ->
  [{hackney_bstr:to_binary(Name), hackney_bstr:to_binary(Value)}
   || {Name, Value} <- Headers].

%% @private Get proxy configuration for WebSocket.
%% WebSocket always uses tunnel mode (CONNECT or SOCKS5), never simple HTTP proxy.
get_ws_proxy_config(Scheme, Host, Options) ->
  %% Map ws/wss to http/https for proxy env var lookup
  HttpScheme = case Scheme of
    ws -> http;
    wss -> https
  end,
  case get_proxy_config(HttpScheme, Host, Options) of
    false ->
      false;
    {http, ProxyHost, ProxyPort, ProxyAuth} ->
      %% Simple HTTP proxy - convert to CONNECT tunnel for WebSocket
      {connect, ProxyHost, ProxyPort, ProxyAuth};
    {connect, _, _, _} = Config ->
      Config;
    {socks5, _, _, _} = Config ->
      Config
  end.

%%====================================================================
%% Helpers
%%====================================================================

%% @doc Parse cookies from response headers.
-spec cookies(list()) -> list().
cookies(Headers) ->
  lists:foldl(fun({K, V}, Acc) ->
                  case hackney_bstr:to_lower(K) of
                    <<"set-cookie">> ->
                      case hackney_cookie:parse_cookie(V) of
                        {error, _} -> Acc;
                        [{Name, _} | _]=Cookie ->
                          [{Name, Cookie} | Acc]
                      end;
                    _ ->
                      Acc
                  end
              end, [], Headers).

%% @doc Get redirect location from headers.
redirect_location(Headers) when is_list(Headers) ->
  redirect_location(hackney_headers:from_list(Headers));
redirect_location(Headers) ->
  hackney_headers:get_value(<<"location">>, Headers).

%% @doc Get the final URL after following redirects.
%% First checks the stored location (set after redirects),
%% then falls back to the Location header from the last response.
-spec location(conn()) -> binary() | undefined.
location(ConnPid) when is_pid(ConnPid) ->
  case hackney_conn:get_location(ConnPid) of
    undefined ->
      %% No stored location, check response headers
      case hackney_conn:response_headers(ConnPid) of
        undefined -> undefined;
        Headers -> redirect_location(Headers)
      end;
    Location ->
      Location
  end.

%%====================================================================
%% Internal functions
%%====================================================================

do_request(ConnPid, Method, Path, Headers0, Body, Options, URL, Host) ->
  %% Build headers
  Headers1 = hackney_headers:new(Headers0),
  Headers2 = add_host_header(URL, Headers1),

  %% Add default headers (User-Agent, Authorization, Cookies)
  Headers3 = add_default_headers(Headers2, Options, URL),

  %% Check for async mode
  Async = proplists:get_value(async, Options, false),
  StreamTo = proplists:get_value(stream_to, Options, self()),
  WithBody = proplists:get_value(with_body, Options, false),
  FollowRedirect = proplists:get_value(follow_redirect, Options, false),
  MaxRedirect = proplists:get_value(max_redirect, Options, 5),
  RedirectCount = proplists:get_value(redirect_count, Options, 0),

  %% Convert method to binary
  MethodBin = hackney_bstr:to_upper(hackney_bstr:to_binary(Method)),

  StartTime = os:timestamp(),

  Result = case Async of
    false ->
      %% Sync request with redirect handling
      sync_request_with_redirect(ConnPid, MethodBin, Path, Headers3, Body, WithBody,
                                 Options, URL, FollowRedirect, MaxRedirect, RedirectCount);
    _ ->
      %% Async request with optional redirect handling
      async_request(ConnPid, MethodBin, Path, Headers3, Body, Async, StreamTo, FollowRedirect)
  end,

  case Result of
    {ok, _, _, _} ->
      hackney_manager:finish_request(Host, StartTime),
      Result;
    {ok, _, _} ->
      hackney_manager:finish_request(Host, StartTime),
      Result;
    {ok, _} ->
      Result;  % Async - don't finish yet
    {error, _} ->
      hackney_manager:finish_request(Host, StartTime),
      Result
  end.

sync_request_with_redirect(ConnPid, Method, Path, Headers, Body, WithBody, Options, URL,
                           FollowRedirect, MaxRedirect, RedirectCount) ->
  %% Handle body encoding
  {FinalHeaders, FinalBody} = encode_body(Headers, Body, Options),
  HeadersList = hackney_headers:to_list(FinalHeaders),

  %% Check if this is a streaming body request
  case FinalBody of
    stream ->
      %% For streaming body, just send headers and return immediately
      case hackney_conn:send_request_headers(ConnPid, Method, Path, HeadersList) of
        ok -> {ok, ConnPid};
        {error, Reason} -> {error, Reason}
      end;
    _ ->
      sync_request_with_redirect_body(ConnPid, Method, Path, HeadersList, FinalBody,
                                      WithBody, Options, URL, FollowRedirect, MaxRedirect, RedirectCount)
  end.

sync_request_with_redirect_body(ConnPid, Method, Path, HeadersList, FinalBody,
                                WithBody, Options, URL, FollowRedirect, MaxRedirect, RedirectCount) ->
  case hackney_conn:request(ConnPid, Method, Path, HeadersList, FinalBody) of
    %% HTTP/2 returns body directly - handle 4-tuple first
    {ok, Status, RespHeaders, RespBody} when Status >= 301, Status =< 303; Status =:= 307; Status =:= 308 ->
      %% HTTP/2 redirect status
      case FollowRedirect of
        true when RedirectCount < MaxRedirect ->
          follow_redirect(ConnPid, Method, FinalBody, WithBody, Options, URL,
                         RespHeaders, Status, MaxRedirect, RedirectCount);
        true ->
          {error, {max_redirect, RedirectCount}};
        false ->
          {ok, Status, RespHeaders, RespBody}
      end;
    {ok, Status, RespHeaders, RespBody} ->
      %% HTTP/2 response with body - already have body
      case Method of
        <<"HEAD">> ->
          {ok, Status, RespHeaders};
        _ when WithBody ->
          {ok, Status, RespHeaders, RespBody};
        _ ->
          %% Caller wants connection ref but HTTP/2 already consumed body
          %% Return body anyway since we have it
          {ok, Status, RespHeaders, RespBody}
      end;
    %% HTTP/1.1 returns 3-tuple, body fetched separately
    {ok, Status, RespHeaders} when Status >= 301, Status =< 303; Status =:= 307; Status =:= 308 ->
      %% Redirect status
      case FollowRedirect of
        true when RedirectCount < MaxRedirect ->
          %% Skip the body if any
          _ = hackney_conn:body(ConnPid),
          %% Follow redirect
          follow_redirect(ConnPid, Method, FinalBody, WithBody, Options, URL,
                         RespHeaders, Status, MaxRedirect, RedirectCount);
        true ->
          {error, {max_redirect, RedirectCount}};
        false ->
          %% Return the redirect response
          {ok, Status, RespHeaders, ConnPid}
      end;
    {ok, Status, RespHeaders} ->
      case Method of
        <<"HEAD">> ->
          %% HEAD responses have no body - release connection to pool
          hackney_conn:release_to_pool(ConnPid),
          {ok, Status, RespHeaders};
        _ when WithBody ->
          case hackney_conn:body(ConnPid) of
            {ok, RespBody} ->
              %% Body read - release connection to pool
              hackney_conn:release_to_pool(ConnPid),
              {ok, Status, RespHeaders, RespBody};
            {error, Reason} ->
              {error, Reason}
          end;
        _ ->
          {ok, Status, RespHeaders, ConnPid}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

follow_redirect(_ConnPid, Method, Body, WithBody, Options, CurrentURL, RespHeaders, Status,
                MaxRedirect, RedirectCount) ->
  %% Get the Location header
  Location = redirect_location(RespHeaders),
  case Location of
    undefined ->
      {error, no_location_header};
    LocationBin ->
      %% Parse the new URL (could be relative or absolute)
      NewURL = resolve_redirect_url(CurrentURL, LocationBin),
      %% Get the full URL as a binary for storing
      FinalLocation = hackney_url:unparse_url(NewURL),
      %% Determine method for redirect (301, 302, 303 -> GET, 307, 308 -> same method)
      NewMethod = case Status of
        S when S =:= 301; S =:= 302; S =:= 303 ->
          <<"GET">>;
        _ ->
          Method
      end,
      NewBody = case NewMethod of
        <<"GET">> -> <<>>;
        _ -> Body
      end,
      %% Make new request to the redirect URL
      %% Remove old redirect_count and add incremented one
      Options1 = proplists:delete(redirect_count, Options),
      case request(NewMethod, NewURL, [], NewBody,
                   [{follow_redirect, true}, {max_redirect, MaxRedirect},
                    {redirect_count, RedirectCount + 1}, {with_body, WithBody} | Options1]) of
        {ok, Status2, Headers2, NewConnPid} when is_pid(NewConnPid) ->
          %% Store the final location in the connection
          hackney_conn:set_location(NewConnPid, FinalLocation),
          {ok, Status2, Headers2, NewConnPid};
        {ok, Status2, Headers2, Body2} ->
          %% Body was returned (with_body option)
          {ok, Status2, Headers2, Body2};
        {ok, Status2, Headers2} ->
          {ok, Status2, Headers2};
        Error ->
          Error
      end
  end.

resolve_redirect_url(CurrentURL, Location) when is_binary(Location) ->
  case Location of
    <<"http://", _/binary>> -> hackney_url:parse_url(Location);
    <<"https://", _/binary>> -> hackney_url:parse_url(Location);
    <<"/", _/binary>> ->
      %% Relative path - use current host
      CurrentURL#hackney_url{path = Location, qs = <<>>};
    _ ->
      %% Relative path without leading slash
      CurrentPath = CurrentURL#hackney_url.path,
      BasePath = filename:dirname(binary_to_list(CurrentPath)),
      NewPath = iolist_to_binary([BasePath, "/", Location]),
      CurrentURL#hackney_url{path = NewPath, qs = <<>>}
  end.

async_request(ConnPid, Method, Path, Headers, Body, AsyncMode, StreamTo, FollowRedirect) ->
  %% Handle body encoding
  {FinalHeaders, FinalBody} = encode_body(Headers, Body, []),
  HeadersList = hackney_headers:to_list(FinalHeaders),

  case hackney_conn:request_async(ConnPid, Method, Path, HeadersList, FinalBody, AsyncMode, StreamTo, FollowRedirect) of
    {ok, Ref} ->
      {ok, Ref};
    {error, Reason} ->
      {error, Reason}
  end.

encode_body(Headers, <<>>, _Options) ->
  {Headers, <<>>};
encode_body(Headers, [], _Options) ->
  {Headers, <<>>};
encode_body(Headers, {form, KVs}, _Options) ->
  {CLen, CType, EncodedBody} = encode_form(KVs),
  Headers1 = hackney_headers:store(<<"Content-Type">>, CType, Headers),
  Headers2 = hackney_headers:store(<<"Content-Length">>, integer_to_binary(CLen), Headers1),
  {Headers2, EncodedBody};
encode_body(Headers, {multipart, Parts}, _Options) ->
  %% Encode multipart body
  Boundary = hackney_multipart:boundary(),
  {MpBody, MpSize} = hackney_multipart:encode_form(Parts, Boundary),
  %% Add Content-Type with boundary
  ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,
  Headers1 = hackney_headers:store(<<"Content-Type">>, ContentType, Headers),
  Headers2 = hackney_headers:store(<<"Content-Length">>, integer_to_binary(MpSize), Headers1),
  {Headers2, MpBody};
encode_body(Headers, Body, _Options) when is_binary(Body) ->
  case hackney_headers:get_value(<<"content-length">>, Headers) of
    undefined ->
      Headers1 = hackney_headers:store(<<"Content-Length">>, integer_to_binary(byte_size(Body)), Headers),
      {Headers1, Body};
    _ ->
      {Headers, Body}
  end;
encode_body(Headers, Body, _Options) when is_list(Body) ->
  Bin = iolist_to_binary(Body),
  encode_body(Headers, Bin, _Options);
encode_body(Headers, Body, _Options) ->
  {Headers, Body}.

%% @doc Encode form data as application/x-www-form-urlencoded
encode_form(KVs) ->
  Lines = hackney_url:qs(KVs),
  CType = <<"application/x-www-form-urlencoded; charset=utf-8">>,
  {byte_size(Lines), CType, Lines}.

add_host_header(#hackney_url{transport=Transport, netloc=Netloc}, Headers) ->
  HostValue = case Transport of
                hackney_local_tcp -> hackney_url:urlencode(Netloc);
                _ -> Netloc
              end,
  {_, Headers1} = hackney_headers:store_new(<<"Host">>, HostValue, Headers),
  Headers1.

%% Add default headers: User-Agent, Authorization (basic auth), Cookies
add_default_headers(Headers, Options, URL) ->
  %% Add User-Agent
  {_, Headers1} = hackney_headers:store_new(<<"User-Agent">>, default_ua(), Headers),

  %% Add basic auth if present
  Headers2 = case proplists:get_value(basic_auth, Options) of
    undefined ->
      Headers1;
    {User, Pwd} ->
      %% Check if basic auth over HTTP is allowed
      Transport = URL#hackney_url.transport,
      AllowInsecureAuth = proplists:get_value(insecure_basic_auth, Options,
                            hackney_app:get_app_env(insecure_basic_auth, true)),
      case {Transport, AllowInsecureAuth} of
        {hackney_ssl, _} ->
          %% HTTPS - always safe
          add_basic_auth_header(User, Pwd, Headers1);
        {_, true} ->
          %% HTTP with explicit bypass
          add_basic_auth_header(User, Pwd, Headers1);
        {_, false} ->
          erlang:error({insecure_basic_auth,
                       "Basic authentication over HTTP is insecure. "
                       "Use HTTPS, add {insecure_basic_auth, true} option, or set "
                       "application:set_env(hackney, insecure_basic_auth, true) to bypass this check."})
      end
  end,

  %% Add cookies if present
  case proplists:get_value(cookie, Options, []) of
    [] -> Headers2;
    Cookies -> add_cookies_header(Cookies, Headers2)
  end.

add_basic_auth_header(User, Pwd, Headers) ->
  User1 = hackney_bstr:to_binary(User),
  Pwd1 = hackney_bstr:to_binary(Pwd),
  Credentials = base64:encode(<<User1/binary, ":", Pwd1/binary>>),
  hackney_headers:store(<<"Authorization">>, <<"Basic ", Credentials/binary>>, Headers).

add_cookies_header([], Headers) ->
  Headers;
add_cookies_header(Cookies, Headers) when is_list(Cookies) ->
  %% Format cookies as "name1=value1; name2=value2"
  CookieStr = format_cookies(Cookies),
  case CookieStr of
    <<>> -> Headers;
    _ -> hackney_headers:store(<<"Cookie">>, CookieStr, Headers)
  end;
add_cookies_header(Cookie, Headers) when is_binary(Cookie) ->
  hackney_headers:store(<<"Cookie">>, Cookie, Headers).

format_cookies([]) ->
  <<>>;
format_cookies([{Name, Value} | Rest]) ->
  First = iolist_to_binary([Name, <<"=">>, Value]),
  format_cookies(Rest, First);
format_cookies([Cookie | Rest]) when is_binary(Cookie) ->
  format_cookies(Rest, Cookie).

format_cookies([], Acc) ->
  Acc;
format_cookies([{Name, Value} | Rest], Acc) ->
  format_cookies(Rest, <<Acc/binary, "; ", (iolist_to_binary([Name, <<"=">>, Value]))/binary>>);
format_cookies([Cookie | Rest], Acc) when is_binary(Cookie) ->
  format_cookies(Rest, <<Acc/binary, "; ", Cookie/binary>>).

default_ua() ->
  <<"hackney/", (list_to_binary(get_version()))/binary>>.

get_version() ->
  case application:get_key(hackney, vsn) of
    {ok, Vsn} -> Vsn;
    undefined -> "1.0.0"
  end.

use_pool(Options) ->
  UseDefaultPool = case application:get_env(hackney, use_default_pool) of
                     {ok, Val} -> Val;
                     _ -> true
                   end,
  case proplists:get_value(pool, Options) of
    false -> false;
    undefined when UseDefaultPool =:= false -> false;
    undefined -> default;
    PoolName -> PoolName
  end.

maybe_proxy(Transport, Scheme, Host, Port, Options) ->
  case get_proxy_config(Scheme, Host, Options) of
    false ->
      %% No proxy configured, direct connection
      connect(Transport, Host, Port, Options);
    {connect, ProxyHost, ProxyPort, ProxyAuth} ->
      %% HTTP CONNECT tunnel (for HTTPS through HTTP proxy)
      connect_via_connect_proxy(Transport, Host, Port, ProxyHost, ProxyPort, ProxyAuth, Options);
    {socks5, ProxyHost, ProxyPort, ProxyAuth} ->
      %% SOCKS5 proxy
      connect_via_socks5_proxy(Transport, Host, Port, ProxyHost, ProxyPort, ProxyAuth, Options);
    {http, ProxyHost, ProxyPort, ProxyAuth} ->
      %% Simple HTTP proxy - connect to proxy, use absolute URLs
      connect_via_http_proxy(Scheme, Host, Port, ProxyHost, ProxyPort, ProxyAuth, Options)
  end.

%% @doc Connect through HTTP CONNECT proxy (tunnel).
%% Used for HTTPS requests through HTTP proxy.
connect_via_connect_proxy(Transport, Host, Port, ProxyHost, ProxyPort, ProxyAuth, Options) ->
  %% Build options for hackney_http_connect
  ConnectOpts0 = [
    {connect_host, Host},
    {connect_port, Port},
    {connect_transport, Transport}
  ],
  ConnectOpts1 = case ProxyAuth of
    undefined -> ConnectOpts0;
    {User, Pass} -> [{connect_user, User}, {connect_pass, Pass} | ConnectOpts0]
  end,
  %% Add SSL options if connecting to HTTPS target
  ConnectOpts2 = case Transport of
    hackney_ssl ->
      SslOpts = proplists:get_value(ssl_options, Options, []),
      [{ssl_options, SslOpts} | ConnectOpts1];
    _ ->
      ConnectOpts1
  end,
  %% Add other connection options
  ConnectOpts = ConnectOpts2 ++ proplists:get_value(connect_options, Options, []),
  Timeout = proplists:get_value(connect_timeout, Options, 8000),

  case hackney_http_connect:connect(ProxyHost, ProxyPort, ConnectOpts, Timeout) of
    {ok, ProxySocket} ->
      %% Start hackney_conn with the pre-established socket
      start_conn_with_socket(Host, Port, Transport, ProxySocket, Options);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Connect through SOCKS5 proxy.
connect_via_socks5_proxy(Transport, Host, Port, ProxyHost, ProxyPort, ProxyAuth, Options) ->
  %% Build options for hackney_socks5
  Socks5Opts0 = [
    {socks5_host, ProxyHost},
    {socks5_port, ProxyPort},
    {socks5_transport, Transport}
  ],
  Socks5Opts1 = case ProxyAuth of
    undefined -> Socks5Opts0;
    {User, Pass} -> [{socks5_user, User}, {socks5_pass, Pass} | Socks5Opts0]
  end,
  %% Add SSL options if connecting to HTTPS target
  Socks5Opts2 = case Transport of
    hackney_ssl ->
      SslOpts = proplists:get_value(ssl_options, Options, []),
      [{ssl_options, SslOpts} | Socks5Opts1];
    _ ->
      Socks5Opts1
  end,
  %% Add socks5_resolve option if specified
  Socks5Opts3 = case proplists:get_value(socks5_resolve, Options) of
    undefined -> Socks5Opts2;
    Resolve -> [{socks5_resolve, Resolve} | Socks5Opts2]
  end,
  %% Add other connection options
  Socks5Opts = Socks5Opts3 ++ proplists:get_value(connect_options, Options, []),
  Timeout = proplists:get_value(connect_timeout, Options, 8000),

  case hackney_socks5:connect(Host, Port, Socks5Opts, Timeout) of
    {ok, ProxySocket} ->
      %% Start hackney_conn with the pre-established socket
      start_conn_with_socket(Host, Port, Transport, ProxySocket, Options);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Connect through simple HTTP proxy (no tunneling).
%% Used for HTTP requests through HTTP proxy. Requests use absolute URLs.
connect_via_http_proxy(TargetScheme, TargetHost, TargetPort, ProxyHost, ProxyPort, ProxyAuth, Options) ->
  %% Connect directly to the proxy server
  case connect(hackney_tcp, ProxyHost, ProxyPort, Options) of
    {ok, ConnPid} ->
      %% Return connection with proxy info for absolute URL mode
      {ok, ConnPid, {http_proxy, TargetScheme, TargetHost, TargetPort, ProxyAuth}};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Build absolute URL for HTTP proxy requests.
build_absolute_url(Scheme, Host, Port, Path) ->
  SchemeBin = atom_to_binary(Scheme),
  HostBin = hackney_bstr:to_binary(Host),
  %% Include port only if non-default
  HostPort = case {Scheme, Port} of
    {http, 80} -> HostBin;
    {https, 443} -> HostBin;
    _ -> <<HostBin/binary, ":", (integer_to_binary(Port))/binary>>
  end,
  <<SchemeBin/binary, "://", HostPort/binary, Path/binary>>.

%% @doc Add Proxy-Authorization header if auth is configured.
add_proxy_auth_header(Headers, undefined) ->
  Headers;
add_proxy_auth_header(Headers, {User, Pass}) ->
  Credentials = base64:encode(<<User/binary, ":", Pass/binary>>),
  AuthHeader = {<<"Proxy-Authorization">>, <<"Basic ", Credentials/binary>>},
  [AuthHeader | Headers].

%% @doc Extract proxy configuration from options.
%% Returns: false | {Type, Host, Port, Auth}
%% Type: http | connect | socks5
%% Auth: undefined | {User, Pass}
%% TargetHost is used to check NO_PROXY for environment variable proxies.
-spec get_proxy_config(atom(), string() | binary(), list()) ->
  false | {http | connect | socks5, string(), inet:port_number(),
           undefined | {binary(), binary()}}.
get_proxy_config(Scheme, TargetHost, Options) ->
  case proplists:get_value(proxy, Options) of
    undefined ->
      %% No explicit proxy option, check environment variables
      get_proxy_from_env(Scheme, TargetHost, Options);
    false ->
      false;
    ProxyUrl when is_binary(ProxyUrl); is_list(ProxyUrl) ->
      parse_proxy_option(ProxyUrl, Scheme, Options);
    {ProxyHost, ProxyPort} when is_list(ProxyHost), is_integer(ProxyPort) ->
      %% Simple tuple: use HTTP proxy for http, CONNECT for https
      ProxyAuth = proplists:get_value(proxy_auth, Options),
      {proxy_type_for_scheme(Scheme), ProxyHost, ProxyPort, ProxyAuth};
    {connect, ProxyHost, ProxyPort} when is_list(ProxyHost), is_integer(ProxyPort) ->
      %% Explicit CONNECT tunnel
      ProxyAuth = proplists:get_value(proxy_auth, Options),
      {connect, ProxyHost, ProxyPort, ProxyAuth};
    {socks5, ProxyHost, ProxyPort} when is_list(ProxyHost), is_integer(ProxyPort) ->
      %% SOCKS5 proxy
      User = proplists:get_value(socks5_user, Options),
      Pass = proplists:get_value(socks5_pass, Options, <<>>),
      Auth = case User of
        undefined -> undefined;
        _ -> {User, Pass}
      end,
      {socks5, ProxyHost, ProxyPort, Auth};
    _ ->
      false
  end.

%% Parse proxy URL and determine type based on target scheme
parse_proxy_option(ProxyUrl, Scheme, Options) ->
  case parse_proxy_url(ProxyUrl) of
    {ok, #{scheme := ProxyScheme, host := ProxyHost, port := ProxyPort,
           user := User, password := Pass}} ->
      Auth = case User of
        undefined -> proplists:get_value(proxy_auth, Options);
        _ -> {User, Pass}
      end,
      Type = case ProxyScheme of
        socks5 -> socks5;
        _ -> proxy_type_for_scheme(Scheme)
      end,
      {Type, ProxyHost, ProxyPort, Auth};
    {error, _} ->
      false
  end.

%% @doc Get proxy configuration from environment variables.
%% Checks HTTP_PROXY, HTTPS_PROXY, ALL_PROXY based on scheme.
%% Returns false if NO_PROXY matches the target host.
get_proxy_from_env(Scheme, TargetHost, Options) ->
  case get_proxy_env(Scheme) of
    false ->
      false;
    {ok, ProxyUrl} ->
      %% Check if target host is in NO_PROXY list
      case check_no_proxy(TargetHost, get_no_proxy()) of
        true ->
          %% Host is in NO_PROXY, don't use proxy
          false;
        false ->
          parse_proxy_option(ProxyUrl, Scheme, Options)
      end
  end.

%% @doc Get proxy URL from environment variables.
get_proxy_env(Scheme) ->
  case Scheme of
    https -> get_proxy_env_https();
    _ -> get_proxy_env_http()
  end.

get_proxy_env_https() ->
  do_get_proxy_env(?HTTPS_PROXY_ENV_VARS).

get_proxy_env_http() ->
  do_get_proxy_env(?HTTP_PROXY_ENV_VARS).

do_get_proxy_env([Var | Rest]) when is_list(Var) ->
  case os:getenv(Var) of
    false -> do_get_proxy_env(Rest);
    Url ->
      %% Trim whitespace from the URL
      TrimmedUrl = string:trim(Url),
      case TrimmedUrl of
        "" -> do_get_proxy_env(Rest);
        _ -> {ok, TrimmedUrl}
      end
  end;
do_get_proxy_env([]) ->
  false.

%% @doc Get NO_PROXY list from environment variables.
%% Returns list of hosts/domains to bypass proxy.
get_no_proxy() ->
  case do_get_proxy_env(?HTTP_NO_PROXY_ENV_VARS) of
    false -> [];
    {ok, NoProxyStr} ->
      %% Split by comma and trim each entry
      Entries = string:split(NoProxyStr, ",", all),
      [string:trim(E) || E <- Entries, string:trim(E) =/= ""]
  end.

%% @doc Check if a host matches any NO_PROXY entry.
%% Supports exact match, suffix match (with leading dot), and wildcard (*).
%% Returns true if proxy should be bypassed for this host.
%% Note: binary clause is for API flexibility (used in tests).
-dialyzer({nowarn_function, check_no_proxy/2}).
check_no_proxy(_Host, []) ->
  false;
check_no_proxy(Host, NoProxyList) when is_binary(Host) ->
  check_no_proxy(binary_to_list(Host), NoProxyList);
check_no_proxy(Host, NoProxyList) ->
  LowerHost = string:lowercase(Host),
  lists:any(fun(Entry) -> matches_no_proxy(LowerHost, Entry) end, NoProxyList).

%% Check if host matches a single NO_PROXY entry
matches_no_proxy(_Host, "*") ->
  %% Wildcard matches everything
  true;
matches_no_proxy(Host, Entry) ->
  LowerEntry = string:lowercase(Entry),
  case LowerEntry of
    [$. | Domain] ->
      %% Leading dot: match any subdomain
      string:find(Host, Domain, trailing) =:= Domain;
    _ ->
      %% Exact match or suffix match
      Host =:= LowerEntry orelse
        lists:suffix("." ++ LowerEntry, Host)
  end.

%% Determine proxy type based on target scheme
proxy_type_for_scheme(https) -> connect;
proxy_type_for_scheme(_) -> http.

%% HTTP method helpers
-define(METHOD_TPL(Method),
  Method(URL) ->
  hackney:request(Method, URL)).
-include("hackney_methods.hrl").

-define(METHOD_TPL(Method),
  Method(URL, Headers) ->
  hackney:request(Method, URL, Headers)).
-include("hackney_methods.hrl").

-define(METHOD_TPL(Method),
  Method(URL, Headers, Body) ->
  hackney:request(Method, URL, Headers, Body)).
-include("hackney_methods.hrl").

-define(METHOD_TPL(Method),
  Method(URL, Headers, Body, Options) ->
  hackney:request(Method, URL, Headers, Body, Options)).
-include("hackney_methods.hrl").


%% @doc Parse a proxy URL and extract host, port, and optional credentials.
%% Supports URLs like:
%% - "http://proxy.example.com:8080"
%% - "http://user:pass@proxy.example.com:8080"
%% - "https://admin:secret@secure-proxy.example.com:443"
%% - "socks5://socks.example.com:1080"
%% - "socks5://user:pass@socks.example.com:1080"
%%
%% Returns a map with keys: scheme, host, port, user, password
%% Fixes issue #741: Extract proxy basic auth from URL
-spec parse_proxy_url(binary() | string()) ->
  {ok, #{scheme := atom(),
         host := string(),
         port := inet:port_number(),
         user := binary() | undefined,
         password := binary() | undefined}} |
  {error, invalid_proxy_url}.
parse_proxy_url(Url) when is_list(Url) ->
  parse_proxy_url(list_to_binary(Url));
parse_proxy_url(Url) when is_binary(Url) ->
  try
    #hackney_url{
      scheme = Scheme,
      host = Host,
      port = Port,
      user = User,
      password = Password
    } = hackney_url:parse_url(Url),
    {ok, #{
      scheme => Scheme,
      host => Host,
      port => Port,
      user => case User of <<>> -> undefined; _ -> User end,
      password => case Password of <<>> -> undefined; _ -> Password end
    }}
  catch
    _:_ -> {error, invalid_proxy_url}
  end.
