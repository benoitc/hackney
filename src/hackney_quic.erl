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
%%% The NIF handles QUIC transport (ngtcp2) and HTTP/3 (nghttp3).
%%%
%%% Messages sent from NIF to owner process:
%%% - {quic, ConnRef, {connected, Info}}
%%% - {quic, ConnRef, {closed, Reason}}
%%% - {quic, ConnRef, {transport_error, Code, Reason}}
%%% - {quic, ConnRef, {stream_headers, StreamId, Headers, Fin}}
%%% - {quic, ConnRef, {stream_data, StreamId, Bin, Fin}}
%%% - {quic, ConnRef, {stream_reset, StreamId, ErrorCode}}
%%% - {quic, ConnRef, {stop_sending, StreamId, ErrorCode}}
%%% - {quic, ConnRef, {goaway, LastStreamId, ErrorCode, Debug}}
%%% - {quic, ConnRef, {session_ticket, Ticket}}
%%% - {quic, ConnRef, {send_ready, StreamId}}
%%% - {quic, ConnRef, {timer, NextTimeoutMs}}

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

-export([is_available/0]).

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
        _ = connect(<<"test">>, 443, #{}, self()),
        false  % If we get here, NIF isn't really working
    catch
        error:nif_not_loaded -> false;
        error:badarg -> true;  % NIF loaded, just bad args
        _:_ -> true
    end.

%% @doc Connect to a QUIC server.
%% Returns {ok, ConnRef} on success.
%% The owner process will receive {quic, ConnRef, {connected, Info}}
%% when the connection is established.
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
%% Returns {ok, StreamId} on success.
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
