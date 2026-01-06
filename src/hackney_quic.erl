%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2025 Benoit Chesneau
%%%
%%% @doc QUIC NIF wrapper for HTTP/3 support.
%%%
%%% This module provides the Erlang interface to the QUIC NIF.
%%% The NIF handles QUIC transport and HTTP/3 using lsquic.
%%%
%%% == Connection Options ==
%%%
%%% The `Opts' map passed to `connect/4' may contain:
%%% <ul>
%%%   <li>`socket_fd' - An existing UDP socket file descriptor (integer).
%%%       If provided, the NIF will use this socket instead of creating
%%%       a new one. This allows pre-warming connections and H3 detection
%%%       in Erlang before handing off to the NIF. Use `get_fd/1' to
%%%       extract the FD from a gen_udp socket.</li>
%%%   <li>`verify' - Boolean indicating whether to verify server certificate
%%%       (default: false)</li>
%%% </ul>
%%%
%%% == Messages ==
%%%
%%% Messages sent from NIF to owner process:
%%% <ul>
%%%   <li>`{quic, ConnRef, {connected, Info}}' - Connection established</li>
%%%   <li>`{quic, ConnRef, {stream_opened, StreamId}}' - Stream opened</li>
%%%   <li>`{quic, ConnRef, {closed, Reason}}' - Connection closed</li>
%%%   <li>`{quic, ConnRef, {transport_error, Code, Reason}}' - Transport error</li>
%%%   <li>`{quic, ConnRef, {stream_headers, StreamId, Headers, Fin}}' - Headers received</li>
%%%   <li>`{quic, ConnRef, {stream_data, StreamId, Bin, Fin}}' - Data received</li>
%%%   <li>`{quic, ConnRef, {stream_reset, StreamId, ErrorCode}}' - Stream reset</li>
%%%   <li>`{quic, ConnRef, {stop_sending, StreamId, ErrorCode}}' - Stop sending</li>
%%%   <li>`{quic, ConnRef, {goaway, LastStreamId, ErrorCode, Debug}}' - GoAway received</li>
%%%   <li>`{quic, ConnRef, {session_ticket, Ticket}}' - Session ticket for 0-RTT</li>
%%%   <li>`{quic, ConnRef, {send_ready, StreamId}}' - Stream ready to write</li>
%%%   <li>`{quic, ConnRef, {timer, NextTimeoutMs}}' - Timer notification</li>
%%% </ul>

-module(hackney_quic).

-export([
    connect/4,
    close/2,
    open_stream/1,
    send_headers/4,
    send_data/4,
    reset_stream/3,
    handle_timeout/2,
    peername/1,
    sockname/1,
    setopts/2
]).

-export([is_available/0, get_fd/1]).

-on_load(init/0).

-define(NIF_NOT_LOADED, erlang:nif_error(nif_not_loaded)).

%%====================================================================
%% NIF Loading
%%====================================================================

init() ->
    PrivDir = case code:priv_dir(hackney) of
        {error, bad_name} ->
            %% Fallback for development
            case filelib:is_dir(filename:join(["..", "priv"])) of
                true -> filename:join(["..", "priv"]);
                false -> "priv"
            end;
        Dir -> Dir
    end,
    SoName = filename:join(PrivDir, "hackney_quic"),
    case erlang:load_nif(SoName, 0) of
        ok -> ok;
        {error, {load_failed, _}} -> ok;  % NIF not built yet
        {error, {reload, _}} -> ok;
        {error, Reason} ->
            error_logger:warning_msg("Failed to load hackney_quic NIF: ~p~n", [Reason]),
            ok
    end.

%%====================================================================
%% API
%%====================================================================

%% @doc Check if QUIC/HTTP3 support is available.
%% Returns true if the NIF is loaded and ready.
-spec is_available() -> boolean().
is_available() ->
    try
        %% Try to call connect - if NIF is loaded it will return {ok, Ref}
        %% or {error, _}. If NIF is not loaded it will throw nif_not_loaded.
        case connect(<<"test">>, 443, #{}, self()) of
            {ok, ConnRef} ->
                %% NIF is loaded, clean up the test connection
                catch close(ConnRef, normal),
                true;
            {error, _} ->
                %% NIF is loaded, just failed (expected for invalid params)
                true
        end
    catch
        error:nif_not_loaded -> false;
        _:_ -> false
    end.

%% @doc Get the file descriptor from a gen_udp socket.
%% This can be used to pass an existing UDP socket to the QUIC NIF
%% via the `socket_fd' option.
%%
%% Example:
%% ```
%% {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
%% {ok, Fd} = hackney_quic:get_fd(Socket),
%% {ok, ConnRef} = hackney_quic:connect(Host, Port, #{socket_fd => Fd}, self()).
%% '''
%%
%% Note: After passing the FD to the NIF, do NOT close the gen_udp socket
%% as the NIF now owns the file descriptor. The socket will be closed
%% when the QUIC connection is closed.
-spec get_fd(gen_udp:socket()) -> {ok, integer()} | {error, term()}.
get_fd(Socket) ->
    case inet:getfd(Socket) of
        {ok, Fd} -> {ok, Fd};
        Error -> Error
    end.

%% @doc Connect to a QUIC server.
%% Returns {ok, ConnRef} on success.
%% The owner process will receive {quic, ConnRef, {connected, Info}}
%% when the connection is established.
%%
%% Options:
%% <ul>
%%   <li>`socket_fd' - Use an existing UDP socket FD (see `get_fd/1')</li>
%%   <li>`verify' - Verify server certificate (default: false)</li>
%% </ul>
-spec connect(Host, Port, Opts, Owner) -> {ok, reference()} | {error, term()}
    when Host :: binary() | string(),
         Port :: inet:port_number(),
         Opts :: map(),
         Owner :: pid().
connect(Host, Port, Opts, Owner) when is_list(Host) ->
    connect(list_to_binary(Host), Port, Opts, Owner);
connect(Host, Port, Opts, Owner) when is_binary(Host), is_integer(Port),
                                       Port > 0, Port =< 65535,
                                       is_map(Opts), is_pid(Owner) ->
    connect_nif(Host, Port, Opts, Owner);
connect(_Host, _Port, _Opts, _Owner) ->
    {error, badarg}.

connect_nif(_Host, _Port, _Opts, _Owner) ->
    ?NIF_NOT_LOADED.

%% @doc Close a QUIC connection.
-spec close(ConnRef, Reason) -> ok
    when ConnRef :: reference(),
         Reason :: term().
close(ConnRef, Reason) ->
    close_nif(ConnRef, Reason).

close_nif(_ConnRef, _Reason) ->
    ?NIF_NOT_LOADED.

%% @doc Open a new bidirectional stream.
%% Returns {ok, StreamId} on success. The StreamId may be 0 if the stream
%% creation is pending; the actual stream ID will be provided via the
%% on_new_stream callback message.
-spec open_stream(ConnRef) -> {ok, non_neg_integer()} | {error, term()}
    when ConnRef :: reference().
open_stream(ConnRef) ->
    open_stream_nif(ConnRef).

open_stream_nif(_ConnRef) ->
    ?NIF_NOT_LOADED.

%% @doc Send HTTP/3 headers on a stream.
%% Headers should be [{Name, Value}] with binary keys/values.
%% Fin indicates if this is the final frame on the stream.
-spec send_headers(ConnRef, StreamId, Headers, Fin) -> ok | {error, term()}
    when ConnRef :: reference(),
         StreamId :: non_neg_integer(),
         Headers :: [{binary(), binary()}],
         Fin :: boolean().
send_headers(ConnRef, StreamId, Headers, Fin) when is_list(Headers), is_boolean(Fin) ->
    send_headers_nif(ConnRef, StreamId, Headers, Fin);
send_headers(_ConnRef, _StreamId, _Headers, _Fin) ->
    {error, badarg}.

send_headers_nif(_ConnRef, _StreamId, _Headers, _Fin) ->
    ?NIF_NOT_LOADED.

%% @doc Send data on a stream.
%% Fin indicates if this is the final frame on the stream.
-spec send_data(ConnRef, StreamId, Data, Fin) -> ok | {error, term()}
    when ConnRef :: reference(),
         StreamId :: non_neg_integer(),
         Data :: iodata(),
         Fin :: boolean().
send_data(ConnRef, StreamId, Data, Fin) when is_boolean(Fin) ->
    send_data_nif(ConnRef, StreamId, Data, Fin);
send_data(_ConnRef, _StreamId, _Data, _Fin) ->
    {error, badarg}.

send_data_nif(_ConnRef, _StreamId, _Data, _Fin) ->
    ?NIF_NOT_LOADED.

%% @doc Reset a stream with an error code.
-spec reset_stream(ConnRef, StreamId, ErrorCode) -> ok | {error, term()}
    when ConnRef :: reference(),
         StreamId :: non_neg_integer(),
         ErrorCode :: non_neg_integer().
reset_stream(ConnRef, StreamId, ErrorCode) when is_integer(ErrorCode), ErrorCode >= 0 ->
    reset_stream_nif(ConnRef, StreamId, ErrorCode);
reset_stream(_ConnRef, _StreamId, _ErrorCode) ->
    {error, badarg}.

reset_stream_nif(_ConnRef, _StreamId, _ErrorCode) ->
    ?NIF_NOT_LOADED.

%% @doc Handle connection timeout.
%% Should be called when timer expires.
%% Returns next timeout in ms or 'infinity'.
-spec handle_timeout(ConnRef, NowMs) -> non_neg_integer() | infinity
    when ConnRef :: reference(),
         NowMs :: non_neg_integer().
handle_timeout(ConnRef, NowMs) when is_integer(NowMs) ->
    handle_timeout_nif(ConnRef, NowMs);
handle_timeout(_ConnRef, _NowMs) ->
    infinity.

handle_timeout_nif(_ConnRef, _NowMs) ->
    ?NIF_NOT_LOADED.

%% @doc Get the remote address of the connection.
-spec peername(ConnRef) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}
    when ConnRef :: reference().
peername(ConnRef) ->
    peername_nif(ConnRef).

peername_nif(_ConnRef) ->
    ?NIF_NOT_LOADED.

%% @doc Get the local address of the connection.
-spec sockname(ConnRef) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}
    when ConnRef :: reference().
sockname(ConnRef) ->
    sockname_nif(ConnRef).

sockname_nif(_ConnRef) ->
    ?NIF_NOT_LOADED.

%% @doc Set connection options.
-spec setopts(ConnRef, Opts) -> ok | {error, term()}
    when ConnRef :: reference(),
         Opts :: [{atom(), term()}].
setopts(ConnRef, Opts) when is_list(Opts) ->
    setopts_nif(ConnRef, Opts);
setopts(_ConnRef, _Opts) ->
    {error, badarg}.

setopts_nif(_ConnRef, _Opts) ->
    ?NIF_NOT_LOADED.
