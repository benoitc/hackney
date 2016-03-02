%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>

-module(hackney_ssl_transport).
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

%% @doc Atoms used to identify messages in {active, once | true} mode.
messages(_) -> {ssl, ssl_closed, ssl_error}.

connect(Host, Port, Opts) ->
	connect(Host, Port, Opts, infinity).

connect(Host, Port, Opts, Timeout) when is_list(Host), is_integer(Port),
	(Timeout =:= infinity orelse is_integer(Timeout)) ->

    %% filter options
    AcceptedOpts =  [cacertfile, cacerts, cert, certfile, ciphers,
                     fail_if_no_peer_cert, hibernate_after, key, keyfile,
                     linger, next_protocols_advertised, nodelay, password, raw,
                     reuse_session, reuse_sessions, secure_renegotiate,
                     send_timeout, send_timeout_close, verify, partial_chain,
                     verify_fun, inet6, versions, server_name_indication,
                     depth, reuseaddr, ip, ip_address],
    BaseOpts = [binary, {active, false}, {packet, raw}],
    Opts1 = hackney_util:filter_options(Opts, AcceptedOpts, BaseOpts),

    %% connect
	ssl:connect(Host, Port, Opts1, Timeout).

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
-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
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
