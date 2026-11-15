%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%

%% @doc socks 5 transport

-module(hackney_socks5).

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

-type socks5_socket() :: {atom(), inet:socket()}.
-export_type([socks5_socket/0]).

%% Use hackney_ssl for SSL options (was hackney_connect/hackney_connection)
ssl_opts(Host, Opts) ->
  hackney_ssl:ssl_opts(Host, Opts).

%% @doc Atoms used to identify messages in {active, once | true} mode.
messages({hackney_ssl, _}) ->
  {ssl, ssl_closed, ssl_error};
messages({_, _}) ->
  {tcp, tcp_closed, tcp_error}.


connect(Host, Port, Opts) ->
  connect(Host, Port, Opts, infinity).


connect(Host, Port, Opts, Timeout) when is_list(Host), is_integer(Port),
                                        (Timeout =:= infinity orelse is_integer(Timeout)) ->
  %% get the proxy host and port from the options
  ProxyHost = proplists:get_value(socks5_host, Opts),
  ProxyPort = proplists:get_value(socks5_port, Opts),
  Transport = proplists:get_value(socks5_transport, Opts),

  %% filter connection options
  AcceptedOpts =  [linger, nodelay, send_timeout,
    send_timeout_close, raw, inet6],
  BaseOpts = [binary, {active, false}, {packet, 0}, {keepalive,  true},
    {nodelay, true}],
  ConnectOpts0 = hackney_util:filter_options(Opts, AcceptedOpts, BaseOpts),
  %% The caller's socket options (connect_options), all of them, over the
  %% base ones: the list above only lets a few through (#970).
  ConnectOpts = hackney_util:merge_opts(
                  ConnectOpts0,
                  hackney_util:dial_socket_opts(proplists:get_value(connect_options, Opts, []))),

  %% connect to the socks 5 proxy
  case hackney_happy:connect(ProxyHost, ProxyPort, ConnectOpts, Timeout) of
    {ok, Socket} ->
      %% The SOCKS5 replies are read under the same timeout, as one
      %% deadline: a proxy that accepts the connection and then stops
      %% answering would otherwise block the caller forever.
      Deadline = deadline(Timeout),
      case do_handshake(Socket, Host, Port, Opts, Deadline) of
        ok ->
          case Transport of
            hackney_ssl ->
              SSLOpts = ssl_opts(Host, Opts),
              %% upgrade the tcp connection. GHSA-gp9c: forward the
              %% caller's timeout; the 2-arg ssl:connect defaults to
              %% infinity, so a stalled proxy upstream could pin the
              %% process and socket on the TLS handshake forever.
              case ssl:connect(Socket, SSLOpts, Timeout) of
                {ok, SslSocket} ->
                  {ok, {Transport, SslSocket}};
                Error ->
                  gen_tcp:close(Socket),
                  Error
              end;
            _ ->
              {ok, {Transport, Socket}}
          end;
        Error ->
          gen_tcp:close(Socket),
          Error
      end;
    Error ->
      Error
  end.


recv(Socket, Length) ->
  recv(Socket, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_tcp:recv/3
-spec recv(socks5_socket(), non_neg_integer(), timeout())
    -> {ok, any()} | {error, closed | atom()}.
recv({Transport, Socket}, Length, Timeout) ->
  Transport:recv(Socket, Length, Timeout).


%% @doc Send a packet on a socket.
%% @see gen_tcp:send/2
-spec send(socks5_socket(), iolist()) -> ok | {error, atom()}.
send({Transport, Socket}, Packet) ->
  Transport:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(socks5_socket(), list()) -> ok | {error, atom()}.
setopts({Transport, Socket}, Opts) ->
  Transport:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_tcp:controlling_process/2
-spec controlling_process(socks5_socket(), pid())
    -> ok | {error, closed | not_owner | atom()}.
controlling_process({Transport, Socket}, Pid) ->
  Transport:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(socks5_socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername({Transport, Socket}) ->
  Transport:peername(Socket).

%% @doc Close a socks5 socket.
%% @see gen_tcp:close/1
-spec close(socks5_socket()) -> ok.
close({Transport, Socket}) ->
  Transport:close(Socket).

%% @doc Immediately close a socket in one or two directions.
%% @see gen_tcp:shutdown/2
-spec shutdown(socks5_socket(), read | write | read_write) -> ok.
shutdown({Transport, Socket}, How) ->
  Transport:shutdown(Socket, How).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(socks5_socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname({Transport, Socket}) ->
  Transport:sockname(Socket).

%% private functions
do_handshake(Socket, Host, Port, Options, Deadline) ->
  ProxyUser = proplists:get_value(socks5_user, Options),
  ProxyPass = proplists:get_value(socks5_pass, Options, <<>>),
  case ProxyUser of
    undefined ->
      %% no auth
      ok = gen_tcp:send(Socket, << 5, 1, 0 >>),
      case recv_reply(Socket, 2, Deadline) of
        {ok, << 5, 0 >>} ->
          do_connection(Socket, Host, Port, Options, Deadline);
        {ok, _Reply} ->
          {error, unknown_reply};
        Error ->
          Error
      end;
    _ ->
      case do_authentication(Socket, ProxyUser, ProxyPass, Deadline) of
        ok ->
          do_connection(Socket, Host, Port, Options, Deadline);
        Error ->
          Error
      end
  end.

do_authentication(Socket, User, Pass, Deadline) ->
  ok = gen_tcp:send(Socket, << 5, 1, 2 >>),
  case recv_reply(Socket, 2, Deadline) of
    {ok, <<5, 0>>} ->
      ok;
    {ok, <<5, 2>>} ->
      UserLength = byte_size(User),
      PassLength = byte_size(Pass),
      Msg = iolist_to_binary([<< 1, UserLength >>,
        User, << PassLength >>,
        Pass]),
      ok = gen_tcp:send(Socket, Msg),
      case recv_reply(Socket, 2, Deadline) of
        {ok, <<1, 0>>} ->
          ok;
        {error, timeout} ->
          {error, timeout};
        _ ->
          {error, not_authenticated}
      end;
    {error, timeout} ->
      {error, timeout};
    _ ->
      {error, not_authenticated}
  end.


do_connection(Socket, Host, Port, Options, Deadline) ->
  Resolve = proplists:get_value(socks5_resolve, Options, remote),
  case addr(Host, Port, Resolve) of
    Addr when is_binary(Addr) ->
      ok = gen_tcp:send(Socket, << 5, 1, 0, Addr/binary >>),
      case recv_reply(Socket, 4, Deadline) of
        {ok, << 5, 0, 0, AType>>} ->
          case recv_addr_port(AType, Socket, Deadline) of
            {error, _} = Error ->
              Error;
            BoundAddr ->
              check_connection(BoundAddr)
          end;
        {ok, _} ->
          {error, badarg};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

addr(Host, Port, Resolve) ->
  case inet_parse:address(Host) of
    {ok, {IP1, IP2, IP3, IP4}} ->
      << 1, IP1, IP2, IP3, IP4, Port:16 >>;
    {ok, {IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}} ->
      << 4, IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, Port:16 >>;
    _ -> %% domain name
      case Resolve of
        local ->
          case inet:getaddr(Host, inet) of
            {ok, {IP1, IP2, IP3, IP4}} ->
              << 1, IP1, IP2, IP3, IP4, Port:16 >>;
            Error ->
              case inet:getaddr(Host, inet6) of
                {ok, {IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}} ->
                  << 4, IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, Port:16 >>;
                _ ->
                  Error
              end
          end;
        _Remote ->
          Host1 = list_to_binary(Host),
          HostLength = byte_size(Host1),
          << 3, HostLength, Host1/binary, Port:16 >>
      end
  end.

recv_addr_port(1 = AType, Socket, Deadline) -> % IPv4
  case recv_reply(Socket, 6, Deadline) of
    {ok, Data} -> <<AType, Data/binary>>;
    Error -> Error
  end;
recv_addr_port(4 = AType, Socket, Deadline) -> % IPv6
  case recv_reply(Socket, 18, Deadline) of
    {ok, Data} -> <<AType, Data/binary>>;
    Error -> Error
  end;
recv_addr_port(3 = AType, Socket, Deadline) -> % Domain
  case recv_reply(Socket, 1, Deadline) of
    {ok, <<DLen/integer>>} ->
      case recv_reply(Socket, DLen + 2, Deadline) of
        {ok, AddrPort} -> <<AType, DLen, AddrPort/binary>>;
        Error -> Error
      end;
    Error ->
      Error
  end;
recv_addr_port(_, _, _) ->
  error.

%% Deadline for the whole handshake, from the connect timeout.
deadline(infinity) -> infinity;
deadline(Timeout) -> erlang:monotonic_time(millisecond) + Timeout.

%% Read a proxy reply within what is left of the deadline.
recv_reply(Socket, Length, infinity) ->
  gen_tcp:recv(Socket, Length, infinity);
recv_reply(Socket, Length, Deadline) ->
  Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
  gen_tcp:recv(Socket, Length, Left).

check_connection(<< 3, _DomainLen:8, _Domain/binary >>) ->
  ok;
check_connection(<< 1, _Addr:32, _Port:16 >>) ->
  ok;
check_connection(<< 4, _Addr:128, _Port:16 >>) ->
  ok;
check_connection(_) ->
  {error, no_connection}.
