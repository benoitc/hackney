%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>

-module(hackney_ssl).
-export([messages/1,
  connect/3, connect/4,
  recv/3, recv/2,
  send/2,
  setopts/2,
  controlling_process/2,
  peername/1,
  close/1,
  shutdown/2,
  sockname/1]).

%% from https://wiki.mozilla.org/Security/Server_Side_TLS
-define(DEFAULT_CIPHERS,
  ["ECDHE-ECDSA-AES256-GCM-SHA384","ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-AES256-SHA384","ECDHE-RSA-AES256-SHA384", "ECDHE-ECDSA-DES-CBC3-SHA",
    "ECDH-ECDSA-AES256-GCM-SHA384","ECDH-RSA-AES256-GCM-SHA384","ECDH-ECDSA-AES256-SHA384",
    "ECDH-RSA-AES256-SHA384","DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
    "AES256-GCM-SHA384","AES256-SHA256","ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES128-GCM-SHA256","ECDHE-ECDSA-AES128-SHA256","ECDHE-RSA-AES128-SHA256",
    "ECDH-ECDSA-AES128-GCM-SHA256","ECDH-RSA-AES128-GCM-SHA256","ECDH-ECDSA-AES128-SHA256",
    "ECDH-RSA-AES128-SHA256","DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
    "AES128-GCM-SHA256","AES128-SHA256","ECDHE-ECDSA-AES256-SHA",
    "ECDHE-RSA-AES256-SHA","DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
    "ECDH-RSA-AES256-SHA","AES256-SHA","ECDHE-ECDSA-AES128-SHA",
    "ECDHE-RSA-AES128-SHA","DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
    "ECDH-RSA-AES128-SHA","AES128-SHA"]).

-define(WITHOUT_ECC_CIPHERS,
  ["DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
    "AES256-GCM-SHA384","AES256-SHA256", "DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
    "AES128-GCM-SHA256","AES128-SHA256", "DHE-DSS-AES256-SHA", "AES256-SHA",
    "DHE-DSS-AES128-SHA", "AES128-SHA"]).



%% @doc Atoms used to identify messages in {active, once | true} mode.
messages(_) -> {ssl, ssl_closed, ssl_error}.

%% @doc The ssl:connect/4 (and related) doesn't work with textual representation
%% of IP addresses. It accepts either a string with a DNS-resolvable name or a
%% tuple with parts of an IP as numbers. This function attempts to parse given
%% string and either returns such tuple, or the string if it's impossible to
%% parse.
parse_address(Host) when is_list(Host) ->
  case inet:parse_address(Host) of
    {ok, Address} -> Address;
    {error, _} -> Host
  end.


connect(Host, Port, Opts) ->
  connect(Host, Port, Opts, infinity).

connect(Host, Port, Opts, Timeout) when is_list(Host), is_integer(Port),
                                        (Timeout =:= infinity orelse is_integer(Timeout)) ->

  BaseOpts = [binary, {active, false}, {packet, raw},
    {secure_renegotiate, true},
    {reuse_sessions, true},
    {honor_cipher_order, true},
    {versions,['tlsv1.2', 'tlsv1.1', tlsv1, sslv3]},
    {ciphers, ciphers()}],
  Opts1 = hackney_util:merge_opts(BaseOpts, Opts),
  Host1 = parse_address(Host),
  %% connect
  ssl:connect(Host1, Port, Opts1, Timeout).


ciphers() ->
  case lists:keymember(ecdh_rsa, 1, ssl:cipher_suites()) of
    true -> ?DEFAULT_CIPHERS;
    false ->
      error_logger:warning_msg("hackney_ssl: ECC not enabled"),
      ?WITHOUT_ECC_CIPHERS
  end.

recv(Socket, Length) ->
  recv(Socket, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
%% @see ssl:recv/3
-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
    -> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
  ssl:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see ssl:send/2
-spec send(ssl:sslsocket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
  ssl:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see ssl:setopts/2
-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
  ssl:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see ssl:controlling_process/2
-spec controlling_process(ssl:sslsocket(), pid())
    -> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
  ssl:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see ssl:peername/1
-spec peername(ssl:sslsocket()) ->
  {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
  ssl:peername(Socket).

%% @doc Close a TCP socket.
%% @see ssl:close/1
-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
  ssl:close(Socket).

%% @doc Immediately close a socket in one or two directions.
%% @see ssl:shutdown/2
-spec shutdown(ssl:socket(), read | write | read_write) -> ok.
shutdown(Socket, How) ->
  ssl:shutdown(Socket, How).

%% @doc Get the local address and port of a socket
%% @see ssl:sockname/1
-spec sockname(ssl:sslsocket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
  ssl:sockname(Socket).
