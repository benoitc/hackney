%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2025 Benoit Chesneau
%%%
%%% @doc HTTP/3 support for hackney.
%%%
%%% This module provides HTTP/3 functionality using the QUIC NIF.
%%% HTTP/3 requests are made over QUIC connections using lsquic.
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Check if HTTP/3 is available
%%% hackney_http3:is_available() -> boolean()
%%%
%%% %% Make a simple GET request
%%% {ok, Status, Headers, Body} = hackney_http3:request(get, "https://cloudflare.com/")
%%%
%%% %% Make a request with options
%%% {ok, Status, Headers, Body} = hackney_http3:request(get, "https://example.com/",
%%%     [{<<"user-agent">>, <<"hackney/1.0">>}], <<>>, #{timeout => 30000})
%%% '''
%%%
%%% == Connection Pooling ==
%%%
%%% HTTP/3 connections are multiplexed - multiple requests can share the same
%%% connection. Future versions will integrate with hackney's connection pool.
%%%

-module(hackney_http3).

-export([
    is_available/0,
    request/2, request/3, request/4, request/5,
    connect/2, connect/3,
    close/1,
    send_request/5,
    await_response/2
]).

-type method() :: get | post | put | delete | head | options | patch | atom() | binary().
-type url() :: binary() | string().
-type headers() :: [{binary(), binary()}].
-type body() :: binary() | iodata().
-type response() :: {ok, integer(), headers(), binary()} | {error, term()}.

%%====================================================================
%% API
%%====================================================================

%% @doc Check if HTTP/3/QUIC support is available.
-spec is_available() -> boolean().
is_available() ->
    hackney_quic:is_available().

%% @doc Make an HTTP/3 request with default options.
-spec request(method(), url()) -> response().
request(Method, Url) ->
    request(Method, Url, [], <<>>, #{}).

%% @doc Make an HTTP/3 request with headers.
-spec request(method(), url(), headers()) -> response().
request(Method, Url, Headers) ->
    request(Method, Url, Headers, <<>>, #{}).

%% @doc Make an HTTP/3 request with headers and body.
-spec request(method(), url(), headers(), body()) -> response().
request(Method, Url, Headers, Body) ->
    request(Method, Url, Headers, Body, #{}).

%% @doc Make an HTTP/3 request with all options.
%%
%% Options:
%%   - timeout: Request timeout in milliseconds (default: 30000)
%%   - recv_timeout: Response receive timeout (default: 30000)
%%
-spec request(method(), url(), headers(), body(), map()) -> response().
request(Method, Url, Headers, Body, Opts) ->
    case parse_url(Url) of
        {ok, Host, Port, Path} ->
            Timeout = maps:get(timeout, Opts, 30000),
            case connect(Host, Port, Opts) of
                {ok, Conn} ->
                    try
                        Result = do_request(Conn, Method, Host, Path, Headers, Body, Timeout),
                        Result
                    after
                        close(Conn)
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Connect to an HTTP/3 server.
-spec connect(binary() | string(), inet:port_number()) -> {ok, reference()} | {error, term()}.
connect(Host, Port) ->
    connect(Host, Port, #{}).

%% @doc Connect to an HTTP/3 server with options.
%% lsquic handles its own UDP socket creation and DNS resolution.
-spec connect(binary() | string(), inet:port_number(), map()) -> {ok, reference()} | {error, term()}.
connect(Host, Port, Opts) when is_list(Host) ->
    connect(list_to_binary(Host), Port, Opts);
connect(Host, Port, Opts) when is_binary(Host) ->
    Timeout = maps:get(timeout, Opts, 30000),
    %% Let lsquic create its own UDP socket
    case hackney_quic:connect(Host, Port, Opts, self()) of
        {ok, ConnRef} ->
            %% Drive event loop until connected or error
            wait_connected(ConnRef, Timeout, erlang:monotonic_time(millisecond));
        {error, _} = Error ->
            Error
    end.

%% Drive QUIC event loop until connected
wait_connected(ConnRef, Timeout, StartTime) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    Remaining = max(0, Timeout - Elapsed),
    receive
        {select, _Resource, _Ref, ready_input} ->
            _ = hackney_quic:process(ConnRef),
            wait_connected(ConnRef, Timeout, StartTime);
        {quic, ConnRef, {connected, _Info}} ->
            {ok, ConnRef};
        {quic, ConnRef, {closed, Reason}} ->
            {error, {connection_closed, Reason}};
        {quic, ConnRef, {transport_error, Code, Msg}} ->
            {error, {transport_error, Code, Msg}}
    after Remaining ->
        hackney_quic:close(ConnRef, timeout),
        {error, timeout}
    end.

%% @doc Close an HTTP/3 connection.
-spec close(reference()) -> ok.
close(ConnRef) ->
    hackney_quic:close(ConnRef, normal).

%% @doc Send an HTTP/3 request on an existing connection.
%%
%% Returns {ok, StreamId} on success. Use await_response/2 to get the response.
-spec send_request(reference(), method(), binary(), binary(), headers()) ->
    {ok, non_neg_integer()} | {error, term()}.
send_request(ConnRef, Method, Host, Path, Headers) ->
    case hackney_quic:open_stream(ConnRef) of
        {ok, StreamId} ->
            %% Build HTTP/3 pseudo-headers
            MethodBin = method_to_binary(Method),
            ReqHeaders = [
                {<<":method">>, MethodBin},
                {<<":path">>, Path},
                {<<":scheme">>, <<"https">>},
                {<<":authority">>, Host}
            | Headers],
            case hackney_quic:send_headers(ConnRef, StreamId, ReqHeaders, true) of
                ok ->
                    {ok, StreamId};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Wait for an HTTP/3 response.
-spec await_response(reference(), non_neg_integer()) ->
    {ok, integer(), headers(), binary()} | {error, term()}.
await_response(ConnRef, StreamId) ->
    await_response(ConnRef, StreamId, 30000, undefined, <<>>).

%%====================================================================
%% Internal functions
%%====================================================================

do_request(ConnRef, Method, Host, Path, Headers, Body, Timeout) ->
    case hackney_quic:open_stream(ConnRef) of
        {ok, StreamId} ->
            %% Build HTTP/3 pseudo-headers
            MethodBin = method_to_binary(Method),
            AllHeaders = [
                {<<":method">>, MethodBin},
                {<<":path">>, Path},
                {<<":scheme">>, <<"https">>},
                {<<":authority">>, Host}
            | normalize_headers(Headers)],

            %% Determine if we have a body
            HasBody = Body =/= <<>> andalso Body =/= [],
            Fin = not HasBody,

            case hackney_quic:send_headers(ConnRef, StreamId, AllHeaders, Fin) of
                ok when HasBody ->
                    %% Send body
                    case hackney_quic:send_data(ConnRef, StreamId, Body, true) of
                        ok ->
                            await_response(ConnRef, StreamId, Timeout, undefined, <<>>);
                        {error, _} = Error ->
                            Error
                    end;
                ok ->
                    await_response(ConnRef, StreamId, Timeout, undefined, <<>>);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

await_response(ConnRef, StreamId, Timeout, Status, AccBody) ->
    await_response(ConnRef, StreamId, Timeout, Status, [], AccBody).

await_response(ConnRef, StreamId, Timeout, Status, Headers, AccBody) ->
    await_response(ConnRef, StreamId, Timeout, Status, Headers, AccBody, erlang:monotonic_time(millisecond)).

await_response(ConnRef, StreamId, Timeout, Status, Headers, AccBody, StartTime) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    Remaining = max(0, Timeout - Elapsed),
    receive
        {select, _Resource, _Ref, ready_input} ->
            %% Drive the event loop
            _ = hackney_quic:process(ConnRef),
            await_response(ConnRef, StreamId, Timeout, Status, Headers, AccBody, StartTime);
        {quic, ConnRef, {stream_headers, StreamId, RespHeaders, _Fin}} ->
            %% Got response headers
            NewStatus = get_status(RespHeaders),
            FilteredHeaders = filter_pseudo_headers(RespHeaders),
            await_response(ConnRef, StreamId, Timeout, NewStatus, FilteredHeaders, AccBody, StartTime);
        {quic, ConnRef, {stream_data, StreamId, Data, Fin}} ->
            %% Got response data
            NewBody = <<AccBody/binary, Data/binary>>,
            case Fin of
                true ->
                    {ok, Status, Headers, NewBody};
                false ->
                    await_response(ConnRef, StreamId, Timeout, Status, Headers, NewBody, StartTime)
            end;
        {quic, ConnRef, {stream_reset, StreamId, _ErrorCode}} ->
            {error, stream_reset};
        {quic, ConnRef, {closed, Reason}} ->
            {error, {connection_closed, Reason}}
    after Remaining ->
        {error, timeout}
    end.

parse_url(Url) when is_list(Url) ->
    parse_url(list_to_binary(Url));
parse_url(<<"https://", Rest/binary>>) ->
    parse_host_path(Rest);
parse_url(_) ->
    {error, {invalid_url, only_https_supported}}.

parse_host_path(UrlPart) ->
    case binary:split(UrlPart, <<"/">>) of
        [HostPort] ->
            {Host, Port} = parse_host_port(HostPort),
            {ok, Host, Port, <<"/">>};
        [HostPort, Path] ->
            {Host, Port} = parse_host_port(HostPort),
            {ok, Host, Port, <<"/", Path/binary>>}
    end.

parse_host_port(HostPort) ->
    case binary:split(HostPort, <<":">>) of
        [Host] ->
            {Host, 443};
        [Host, PortBin] ->
            {Host, binary_to_integer(PortBin)}
    end.

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

normalize_headers(Headers) ->
    [{ensure_binary(K), ensure_binary(V)} || {K, V} <- Headers].

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A).

get_status(Headers) ->
    case lists:keyfind(<<":status">>, 1, Headers) of
        {_, StatusBin} ->
            binary_to_integer(StatusBin);
        false ->
            0
    end.

filter_pseudo_headers(Headers) ->
    [{K, V} || {K, V} <- Headers, not is_pseudo_header(K)].

is_pseudo_header(<<$:, _/binary>>) -> true;
is_pseudo_header(_) -> false.
