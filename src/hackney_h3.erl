%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2025 Benoit Chesneau
%%%
%%% @doc HTTP/3 thin wrapper for hackney_conn.
%%%
%%% This module provides HTTP/3-specific operations to be used by hackney_conn.
%%% It handles stream management, header encoding, and request/response
%%% handling for HTTP/3 connections over QUIC.
%%%

-module(hackney_h3).

-export([
    %% Request operations
    send_request/6,
    send_request_headers/5,
    send_body_chunk/4,
    finish_send_body/3,
    %% Stream management
    new_stream/1,
    close_stream/2,
    get_stream_state/2,
    update_stream_state/3,
    %% Response parsing
    parse_response_headers/1,
    %% Connection close
    close/1,
    close/2
]).

-type h3_conn() :: reference().
-type stream_id() :: non_neg_integer().
-type method() :: get | post | put | delete | head | options | patch | atom() | binary().
-type headers() :: [{binary(), binary()}].
-type stream_state() :: waiting_headers | {receiving_body, integer(), headers(), binary()} | done.
-type streams_map() :: #{stream_id() => {term(), stream_state()}}.

-export_type([h3_conn/0, stream_id/0, stream_state/0, streams_map/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Send a complete HTTP/3 request (headers + body).
%% Returns {ok, StreamId, UpdatedStreams} or {error, Reason}.
-spec send_request(h3_conn(), method(), binary(), binary(), headers(), binary()) ->
    {ok, stream_id(), streams_map()} | {error, term()}.
send_request(ConnRef, Method, Host, Path, Headers, Body) ->
    case hackney_quic:open_stream(ConnRef) of
        {ok, StreamId} ->
            AllHeaders = build_request_headers(Method, Host, Path, Headers),
            HasBody = Body =/= <<>> andalso Body =/= [],
            Fin = not HasBody,
            case hackney_quic:send_headers(ConnRef, StreamId, AllHeaders, Fin) of
                ok when HasBody ->
                    case hackney_quic:send_data(ConnRef, StreamId, Body, true) of
                        ok ->
                            {ok, StreamId, #{StreamId => {undefined, waiting_headers}}};
                        {error, _} = Error ->
                            Error
                    end;
                ok ->
                    {ok, StreamId, #{StreamId => {undefined, waiting_headers}}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Send HTTP/3 request headers only (for streaming body).
%% Returns {ok, StreamId, UpdatedStreams} or {error, Reason}.
-spec send_request_headers(h3_conn(), method(), binary(), binary(), headers()) ->
    {ok, stream_id(), streams_map()} | {error, term()}.
send_request_headers(ConnRef, Method, Host, Path, Headers) ->
    case hackney_quic:open_stream(ConnRef) of
        {ok, StreamId} ->
            AllHeaders = build_request_headers(Method, Host, Path, Headers),
            case hackney_quic:send_headers(ConnRef, StreamId, AllHeaders, false) of
                ok ->
                    {ok, StreamId, #{StreamId => {undefined, waiting_headers}}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Send a chunk of body data on a stream.
-spec send_body_chunk(h3_conn(), stream_id(), binary(), boolean()) ->
    ok | {error, term()}.
send_body_chunk(ConnRef, StreamId, Data, Fin) ->
    hackney_quic:send_data(ConnRef, StreamId, Data, Fin).

%% @doc Finish sending body (close stream for writing).
-spec finish_send_body(h3_conn(), stream_id(), streams_map()) ->
    {ok, streams_map()} | {error, term()}.
finish_send_body(ConnRef, StreamId, Streams) ->
    case hackney_quic:send_data(ConnRef, StreamId, <<>>, true) of
        ok ->
            {ok, Streams};
        {error, _} = Error ->
            Error
    end.

%% @doc Open a new stream for a request.
-spec new_stream(h3_conn()) -> {ok, stream_id()} | {error, term()}.
new_stream(ConnRef) ->
    hackney_quic:open_stream(ConnRef).

%% @doc Close a specific stream.
-spec close_stream(h3_conn(), stream_id()) -> ok.
close_stream(_ConnRef, _StreamId) ->
    %% HTTP/3 streams are closed when fin is sent/received
    %% No explicit close needed
    ok.

%% @doc Get the state of a specific stream.
-spec get_stream_state(stream_id(), streams_map()) ->
    {ok, {term(), stream_state()}} | {error, not_found}.
get_stream_state(StreamId, Streams) ->
    case maps:find(StreamId, Streams) of
        {ok, State} -> {ok, State};
        error -> {error, not_found}
    end.

%% @doc Update the state of a stream.
-spec update_stream_state(stream_id(), {term(), stream_state()}, streams_map()) ->
    streams_map().
update_stream_state(StreamId, State, Streams) ->
    maps:put(StreamId, State, Streams).

%% @doc Parse response headers from a QUIC stream_headers event.
%% Returns {ok, Status, ResponseHeaders} or {error, Reason}.
-spec parse_response_headers(headers()) -> {ok, integer(), headers()} | {error, term()}.
parse_response_headers(Headers) ->
    case get_status_from_headers(Headers) of
        {ok, Status} ->
            RespHeaders = filter_pseudo_headers(Headers),
            {ok, Status, RespHeaders};
        {error, _} = Error ->
            Error
    end.

%% @doc Close the HTTP/3 connection.
-spec close(h3_conn()) -> ok.
close(ConnRef) ->
    close(ConnRef, normal).

%% @doc Close the HTTP/3 connection with a reason.
-spec close(h3_conn(), term()) -> ok.
close(ConnRef, Reason) ->
    hackney_quic:close(ConnRef, Reason).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Build HTTP/3 request headers including pseudo-headers.
-spec build_request_headers(method(), binary(), binary(), headers()) -> headers().
build_request_headers(Method, Host, Path, Headers) ->
    MethodBin = method_to_binary(Method),
    PseudoHeaders = [
        {<<":method">>, MethodBin},
        {<<":path">>, Path},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, Host}
    ],
    PseudoHeaders ++ normalize_headers(Headers).

%% @private Convert HTTP method to binary.
-spec method_to_binary(method()) -> binary().
method_to_binary(get) -> <<"GET">>;
method_to_binary(post) -> <<"POST">>;
method_to_binary(put) -> <<"PUT">>;
method_to_binary(delete) -> <<"DELETE">>;
method_to_binary(head) -> <<"HEAD">>;
method_to_binary(options) -> <<"OPTIONS">>;
method_to_binary(patch) -> <<"PATCH">>;
method_to_binary(Method) when is_atom(Method) ->
    string:uppercase(atom_to_binary(Method));
method_to_binary(Method) when is_binary(Method) ->
    Method.

%% @private Normalize headers to binary key-value pairs.
-spec normalize_headers(headers()) -> headers().
normalize_headers(Headers) ->
    [{ensure_binary(K), ensure_binary(V)} || {K, V} <- Headers].

%% @private Ensure value is binary.
-spec ensure_binary(term()) -> binary().
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A).

%% @private Extract status from HTTP/3 response headers.
-spec get_status_from_headers(headers()) -> {ok, integer()} | {error, no_status | {invalid_status, binary()}}.
get_status_from_headers(Headers) ->
    case lists:keyfind(<<":status">>, 1, Headers) of
        {_, StatusBin} ->
            try
                {ok, binary_to_integer(StatusBin)}
            catch
                error:badarg -> {error, {invalid_status, StatusBin}}
            end;
        false ->
            {error, no_status}
    end.

%% @private Filter out pseudo-headers from response.
-spec filter_pseudo_headers(headers()) -> headers().
filter_pseudo_headers(Headers) ->
    [{K, V} || {K, V} <- Headers, not is_pseudo_header(K)].

%% @private Check if header is a pseudo-header (starts with ':').
-spec is_pseudo_header(binary()) -> boolean().
is_pseudo_header(<<$:, _/binary>>) -> true;
is_pseudo_header(_) -> false.
