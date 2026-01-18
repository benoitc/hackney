%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2014 Benoît Chesneau <benoitc@e-engura.org>
%%%
%%%
-module(hackney_http_connect).

-export([messages/1,
  connect/3, connect/4,
  recv/2, recv/3,
  send/2,
  setopts/2,
  controlling_process/2,
  peername/1,
  close/1,
  shutdown/2,
  sockname/1]).

-define(DEFAULT_RECV_TIMEOUT, infinity).

-type http_socket() :: {atom(), inet:socket()}.
-export_type([http_socket/0]).

%% Use hackney_ssl for SSL options (was hackney_connection)
ssl_opts(Host, Opts) ->
  hackney_ssl:ssl_opts(Host, Opts).

%% @doc Atoms used to identify messages in {active, once | true} mode.
messages({hackney_ssl, _}) ->
  {ssl, ssl_closed, ssl_error};
messages({_, _}) ->
  {tcp, tcp_closed, tcp_error}.


connect(ProxyHost, ProxyPort, Opts) ->
  connect(ProxyHost, ProxyPort, Opts, infinity).

connect(ProxyHost, ProxyPort, Opts, Timeout)
  when is_list(ProxyHost), is_integer(ProxyPort),
       (Timeout =:= infinity orelse is_integer(Timeout)) ->

  %% get the host and port to connect from the options
  Host = proplists:get_value(connect_host, Opts),
  Port = proplists:get_value(connect_port, Opts),
  Transport = proplists:get_value(connect_transport, Opts),
  ProxyTransport = proplists:get_value(proxy_transport, Opts, tcp),

  %% filter connection options
  AcceptedOpts =  [linger, nodelay, send_timeout,
    send_timeout_close, raw, inet6],
  BaseOpts = [binary, {active, false}, {packet, 0}, {keepalive,  true},
    {nodelay, true}],
  ConnectOpts = hackney_util:filter_options(Opts, AcceptedOpts, BaseOpts),

  %% connect to the proxy (TCP or TLS based on proxy_transport)
  case connect_to_proxy(ProxyHost, ProxyPort, ProxyTransport, ConnectOpts, Opts) of
    {ok, ProxySocket} ->
      case do_handshake(ProxySocket, ProxyTransport, Host, Port, Opts) of
        ok ->
          %% if we are connecting to a remote https source, we
          %% upgrade the tunnel to handle SSL (end-to-end encryption).
          case Transport of
            hackney_ssl ->
              SSLOpts = ssl_opts(Host, Opts),
              %% upgrade the tunnel to TLS
              case ssl:connect(ProxySocket, SSLOpts) of
                {ok, SslSocket} ->
                  {ok, {Transport, SslSocket}};
                Error ->
                  _ = close_proxy_socket(ProxySocket, ProxyTransport),
                  Error
              end;
            _ ->
              %% For HTTP targets, wrap the socket appropriately
              case ProxyTransport of
                ssl ->
                  %% Proxy connection is TLS, but target is HTTP
                  {ok, {hackney_ssl, ProxySocket}};
                _ ->
                  {ok, {Transport, ProxySocket}}
              end
          end;
        Error ->
          _ = close_proxy_socket(ProxySocket, ProxyTransport),
          Error
      end;
    Error ->
      Error
  end.

%% Connect to proxy using TCP or TLS
connect_to_proxy(ProxyHost, ProxyPort, ssl, ConnectOpts, Opts) ->
  %% HTTPS proxy: connect with TLS
  case hackney_happy:connect(ProxyHost, ProxyPort, ConnectOpts) of
    {ok, TcpSocket} ->
      ProxySslOpts = proplists:get_value(proxy_ssl_options, Opts, []),
      %% Add SNI for the proxy
      SslOpts = [{server_name_indication, ProxyHost} | ProxySslOpts],
      case ssl:connect(TcpSocket, SslOpts) of
        {ok, SslSocket} ->
          {ok, SslSocket};
        Error ->
          gen_tcp:close(TcpSocket),
          Error
      end;
    Error ->
      Error
  end;
connect_to_proxy(ProxyHost, ProxyPort, tcp, ConnectOpts, _Opts) ->
  %% HTTP proxy: plain TCP connection
  hackney_happy:connect(ProxyHost, ProxyPort, ConnectOpts).

%% Close proxy socket based on transport type
close_proxy_socket(Socket, ssl) ->
  ssl:close(Socket);
close_proxy_socket(Socket, tcp) ->
  gen_tcp:close(Socket).

recv(Socket, Length) ->
  recv(Socket, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_tcp:recv/3
-spec recv(http_socket(), non_neg_integer(), timeout())
    -> {ok, any()} | {error, closed | atom()}.
recv({Transport, Socket}, Length, Timeout) ->
  Transport:recv(Socket, Length, Timeout).


%% @doc Send a packet on a socket.
%% @see gen_tcp:send/2
-spec send(http_socket(), iolist()) -> ok | {error, atom()}.
send({Transport, Socket}, Packet) ->
  Transport:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(http_socket(), list()) -> ok | {error, atom()}.
setopts({Transport, Socket}, Opts) ->
  Transport:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_tcp:controlling_process/2
-spec controlling_process(http_socket(), pid())
    -> ok | {error, closed | not_owner | atom()}.
controlling_process({Transport, Socket}, Pid) ->
  Transport:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(http_socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername({Transport, Socket}) ->
  Transport:peername(Socket).

%% @doc Close a socks5 socket.
%% @see gen_tcp:close/1
-spec close(http_socket()) -> ok.
close({Transport, Socket}) ->
  Transport:close(Socket).

%% @doc Immediately close a socket in one or two directions.
%% @see gen_tcp:shutdown/2
-spec shutdown(http_socket(), read | write | read_write) -> ok.
shutdown({Transport, Socket}, How) ->
  Transport:shutdown(Socket, How).


%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(http_socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname({Transport, Socket}) ->
  Transport:sockname(Socket).

%% private functions
do_handshake(Socket, ProxyTransport, Host, Port, Options) ->
  ProxyUser = proplists:get_value(connect_user, Options),
  ProxyPass = proplists:get_value(connect_pass, Options, <<>>),
  ProxyPort = proplists:get_value(connect_port, Options),
  RecvTimeout = proplists:get_value(recv_timeout, Options, ?DEFAULT_RECV_TIMEOUT),
  OnConnectResponse = proplists:get_value(on_connect_response, Options),
  ProxyAuthFun = proplists:get_value(proxy_auth_fun, Options),

  %% set defaults headers
  HostHdr = case ProxyPort of
              80 ->
                list_to_binary(Host);
              _ ->
                iolist_to_binary([Host, ":", integer_to_list(Port)])
            end,
  UA = hackney:default_ua(),
  Headers0 = [<<"Host: ", HostHdr/binary>>,
    <<"User-Agent: ", UA/binary >>],

  Headers = case ProxyUser of
              undefined ->
                Headers0;
              _ ->
                Credentials = base64:encode(<<ProxyUser/binary, ":",
                  ProxyPass/binary>>),
                Headers0 ++ [<< "Proxy-Authorization: Basic ", Credentials/binary >>]
            end,

  do_handshake_with_auth(Socket, ProxyTransport, Host, Port, Headers,
                         RecvTimeout, OnConnectResponse, ProxyAuthFun, 0).

%% Handshake with optional proxy authentication callback (issue #115)
%% MaxRetries limits challenge-response rounds to prevent infinite loops
-define(MAX_AUTH_RETRIES, 5).

do_handshake_with_auth(_Socket, _ProxyTransport, _Host, _Port, _Headers,
                       _RecvTimeout, _OnConnectResponse, _ProxyAuthFun, Retries)
    when Retries >= ?MAX_AUTH_RETRIES ->
  {error, proxy_auth_failed};
do_handshake_with_auth(Socket, ProxyTransport, Host, Port, Headers,
                       RecvTimeout, OnConnectResponse, ProxyAuthFun, Retries) ->
  Path = iolist_to_binary([Host, ":", integer_to_list(Port)]),
  Payload = [<< "CONNECT ", Path/binary, " HTTP/1.1", "\r\n" >>,
    hackney_bstr:join(lists:reverse(Headers), <<"\r\n">>),
    <<"\r\n\r\n">>],
  case proxy_send(Socket, ProxyTransport, Payload) of
    ok ->
      case check_response(Socket, ProxyTransport, RecvTimeout) of
        {ok, Status, RespHeaders} ->
          %% Success - call the callback if provided (issue #438)
          maybe_call_on_connect_response(OnConnectResponse, Status, RespHeaders),
          ok;
        {proxy_auth_required, StatusCode, RespHeaders} when ProxyAuthFun =/= undefined ->
          %% 407 response - try custom auth callback
          handle_proxy_auth(Socket, ProxyTransport, Host, Port, Headers,
                           RecvTimeout, OnConnectResponse, ProxyAuthFun,
                           StatusCode, RespHeaders, Retries);
        {proxy_auth_required, _StatusCode, _RespHeaders} ->
          %% 407 but no auth callback provided
          {error, proxy_auth_required};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

%% Handle 407 Proxy Authentication Required
handle_proxy_auth(Socket, ProxyTransport, Host, Port, Headers,
                  RecvTimeout, OnConnectResponse, ProxyAuthFun,
                  StatusCode, RespHeaders, Retries) ->
  %% Call the user's auth function
  case ProxyAuthFun(StatusCode, RespHeaders) of
    {ok, AuthHeader} when is_binary(AuthHeader) ->
      %% Remove any existing Proxy-Authorization and add new one
      Headers1 = lists:filter(fun(H) -> not is_proxy_auth_header(H) end, Headers),
      Headers2 = Headers1 ++ [<<"Proxy-Authorization: ", AuthHeader/binary>>],
      %% Retry with new auth header
      do_handshake_with_auth(Socket, ProxyTransport, Host, Port, Headers2,
                            RecvTimeout, OnConnectResponse, ProxyAuthFun, Retries + 1);
    {error, Reason} ->
      {error, {proxy_auth_error, Reason}};
    _ ->
      {error, proxy_auth_failed}
  end.

%% Check if a header is a Proxy-Authorization header
is_proxy_auth_header(Header) when is_binary(Header) ->
  case binary:match(hackney_bstr:to_lower(Header), <<"proxy-authorization">>) of
    {0, _} -> true;
    _ -> false
  end;
is_proxy_auth_header(_) ->
  false.

%% Call the on_connect_response callback if provided
maybe_call_on_connect_response(undefined, _Status, _Headers) ->
  ok;
maybe_call_on_connect_response(Fun, Status, Headers) when is_function(Fun, 2) ->
  Fun(Status, Headers);
maybe_call_on_connect_response(_Invalid, _Status, _Headers) ->
  ok.

%% Send data over proxy socket (TCP or TLS)
proxy_send(Socket, ssl, Data) ->
  ssl:send(Socket, Data);
proxy_send(Socket, tcp, Data) ->
  gen_tcp:send(Socket, Data).

%% Receive data from proxy socket (TCP or TLS)
proxy_recv(Socket, ssl, Length, Timeout) ->
  ssl:recv(Socket, Length, Timeout);
proxy_recv(Socket, tcp, Length, Timeout) ->
  gen_tcp:recv(Socket, Length, Timeout).

%% Read the full HTTP response (until \r\n\r\n) before returning.
%% This fixes issue #536 where partial reads cause SSL handshake failures.
%% RecvTimeout is used to respect recv_timeout during proxy handshake (issue #569).
check_response(Socket, ProxyTransport, RecvTimeout) ->
  check_response(Socket, ProxyTransport, RecvTimeout, <<>>).

check_response(Socket, ProxyTransport, RecvTimeout, Buffer) ->
  case proxy_recv(Socket, ProxyTransport, 0, RecvTimeout) of
    {ok, Data} ->
      NewBuffer = <<Buffer/binary, Data/binary>>,
      case binary:match(NewBuffer, <<"\r\n\r\n">>) of
        {_Pos, 4} ->
          %% Found end of headers, now parse the response
          parse_response(NewBuffer);
        nomatch ->
          %% Keep reading until we get the full response headers
          check_response(Socket, ProxyTransport, RecvTimeout, NewBuffer)
      end;
    {error, timeout} ->
      %% Return timeout error instead of generic closed error
      {error, timeout};
    Error ->
      Error
  end.

%% Parse the HTTP response and extract status and headers (issue #438)
parse_response(Response) ->
  case binary:split(Response, <<"\r\n">>) of
    [StatusLine, Rest] ->
      Headers = parse_headers(Rest),
      case check_status_code(StatusLine) of
        ok ->
          {ok, StatusLine, Headers};
        {auth_required, Code} ->
          %% 407 Proxy Authentication Required - return specially for auth callback
          {proxy_auth_required, Code, Headers};
        Error ->
          Error
      end;
    _ ->
      {error, proxy_error}
  end.

%% Check if status code indicates success or auth required
check_status_code(<< "HTTP/1.1 200", _/bits >>) -> ok;
check_status_code(<< "HTTP/1.1 201", _/bits >>) -> ok;
check_status_code(<< "HTTP/1.0 200", _/bits >>) -> ok;
check_status_code(<< "HTTP/1.0 201", _/bits >>) -> ok;
check_status_code(<< "HTTP/1.1 407", _/bits >>) -> {auth_required, 407};
check_status_code(<< "HTTP/1.0 407", _/bits >>) -> {auth_required, 407};
check_status_code(StatusLine) ->
  error_logger:error_msg("proxy error: ~w~n", [StatusLine]),
  {error, proxy_error}.

%% Parse headers into a list of {Name, Value} tuples
parse_headers(Data) ->
  %% Remove trailing \r\n\r\n
  HeaderData = case binary:split(Data, <<"\r\n\r\n">>) of
    [H, _] -> H;
    [H] -> H
  end,
  Lines = binary:split(HeaderData, <<"\r\n">>, [global]),
  parse_header_lines(Lines, []).

parse_header_lines([], Acc) ->
  lists:reverse(Acc);
parse_header_lines([<<>> | Rest], Acc) ->
  parse_header_lines(Rest, Acc);
parse_header_lines([Line | Rest], Acc) ->
  case binary:split(Line, <<": ">>) of
    [Name, Value] ->
      parse_header_lines(Rest, [{Name, Value} | Acc]);
    [Name] ->
      %% Header with no value or malformed, try splitting on just ":"
      case binary:split(Name, <<":">>) of
        [N, V] ->
          parse_header_lines(Rest, [{N, hackney_bstr:trim(V)} | Acc]);
        _ ->
          parse_header_lines(Rest, Acc)
      end
  end.
