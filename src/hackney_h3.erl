%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc HTTP/3 support for hackney.
%%%
%%% This module provides HTTP/3 functionality using pure Erlang QUIC.
%%% It handles stream management, header encoding, and request/response
%%% handling for HTTP/3 connections over QUIC.
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Make a simple GET request
%%% {ok, Status, Headers, Body} = hackney_h3:request(get, "https://cloudflare.com/")
%%%
%%% %% Make a request with options
%%% {ok, Status, Headers, Body} = hackney_h3:request(get, "https://example.com/",
%%%     [{<<"user-agent">>, <<"hackney/2.0">>}], <<>>, #{timeout => 30000})
%%% '''
%%%

-module(hackney_h3).

-behaviour(gen_server).

-include("hackney_lib.hrl").

-export([
    %% High-level API
    is_available/0,
    request/2, request/3, request/4, request/5,
    connect/2, connect/3,
    await_response/2,
    %% Request operations (used by hackney_conn)
    send_request/6,
    send_request_headers/5,
    send_body_chunk/4,
    finish_send_body/3,
    %% Stream management
    get_stream_state/2,
    update_stream_state/3,
    %% Response parsing
    parse_response_headers/1,
    %% Connection close
    close/1,
    close/2,
    %% Low-level adapter API
    connect/4,
    send_request/3,
    send_data/4,
    reset_stream/3,
    handle_timeout/2,
    process/1,
    get_fd/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    h3_conn :: pid() | undefined,
    conn_ref :: reference(),
    owner :: pid(),
    owner_mon :: reference()
}).

-define(CONN_TABLE, hackney_h3_conns).

-type h3_conn() :: reference().
-type stream_id() :: non_neg_integer().
-type method() :: get | post | put | delete | head | options | patch | atom() | binary().
-type url() :: binary() | string().
-type headers() :: [{binary(), binary()}].
-type body() :: binary() | iodata().
-type response() :: {ok, integer(), headers(), binary()} | {error, term()}.
-type stream_state() :: waiting_headers | {receiving_body, integer(), headers(), binary()} | done.
-type streams_map() :: #{stream_id() => {term(), stream_state()}}.

-export_type([h3_conn/0, stream_id/0, stream_state/0, streams_map/0]).

%%====================================================================
%% High-level API
%%====================================================================

%% @doc Check if HTTP/3/QUIC support is available.
%% Always returns true as pure Erlang implementation is always available.
-spec is_available() -> true.
is_available() -> true.

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
%%   - follow_redirect: boolean to follow redirects (default: false)
%%   - max_redirect: maximum number of redirects to follow (default: 5)
%%
-spec request(method(), url(), headers(), body(), map()) -> response().
request(Method, Url, Headers, Body, Opts) ->
    FollowRedirect = maps:get(follow_redirect, Opts, false),
    MaxRedirect = maps:get(max_redirect, Opts, 5),
    do_request_with_redirect(Method, Url, Headers, Body, Opts, FollowRedirect, MaxRedirect, 0).

%% @private Handle request with redirect following
do_request_with_redirect(Method, Url, Headers, Body, Opts, FollowRedirect, MaxRedirect, RedirectCount) ->
    #hackney_url{host = Host, port = Port, path = Path, qs = Qs} = hackney_url:parse_url(Url),
    HostBin = list_to_binary(Host),
    PathBin = case Path of
        <<>> -> <<"/">>;
        _ -> Path
    end,
    %% Include query string if present
    FullPath = case Qs of
        <<>> -> PathBin;
        _ -> <<PathBin/binary, "?", Qs/binary>>
    end,
    Timeout = maps:get(timeout, Opts, 30000),
    case connect(HostBin, Port, Opts) of
        {ok, Conn} ->
            try
                case do_request(Conn, Method, HostBin, FullPath, Headers, Body, Timeout) of
                    {ok, Status, RespHeaders, RespBody} when Status >= 301, Status =< 308 ->
                        %% Redirect response
                        handle_redirect(Status, RespHeaders, RespBody, Method, Url, Headers, Body, Opts,
                                        FollowRedirect, MaxRedirect, RedirectCount);
                    Other ->
                        Other
                end
            after
                close(Conn)
            end;
        {error, _} = Error ->
            Error
    end.

%% @private Handle redirect response
handle_redirect(Status, RespHeaders, RespBody, _Method, _Url, _Headers, _Body, _Opts, false, _MaxRedirect, _RedirectCount) ->
    %% follow_redirect is false, return redirect response as-is
    {ok, Status, RespHeaders, RespBody};
handle_redirect(_Status, _RespHeaders, _RespBody, _Method, _Url, _Headers, _Body, _Opts, true, MaxRedirect, RedirectCount)
  when RedirectCount >= MaxRedirect ->
    {error, {max_redirect, RedirectCount}};
handle_redirect(Status, RespHeaders, _RespBody, Method, Url, Headers, Body, Opts, true, MaxRedirect, RedirectCount) ->
    case get_redirect_location(RespHeaders) of
        {ok, Location} ->
            %% Resolve Location to absolute URL
            NewUrl = resolve_redirect_url(Location, Url),
            %% Determine new method based on status code
            NewMethod = redirect_method(Status, Method),
            %% Clear body for POST->GET redirects
            NewBody = case NewMethod of
                get when Method =/= get -> <<>>;
                _ -> Body
            end,
            %% Follow the redirect
            do_request_with_redirect(NewMethod, NewUrl, Headers, NewBody, Opts,
                                     true, MaxRedirect, RedirectCount + 1);
        {error, no_location} ->
            %% No Location header, return as-is
            {ok, Status, RespHeaders, <<>>}
    end.

%% @private Get Location header from response
get_redirect_location(Headers) ->
    case lists:keyfind(<<"location">>, 1, Headers) of
        {_, Location} -> {ok, Location};
        false ->
            %% Try case-insensitive search
            case lists:keyfind(<<"Location">>, 1, Headers) of
                {_, Location} -> {ok, Location};
                false -> {error, no_location}
            end
    end.

%% @private Resolve redirect URL relative to original URL
resolve_redirect_url(Location, OriginalUrl) when is_binary(Location) ->
    case Location of
        <<"http://", _/binary>> -> Location;
        <<"https://", _/binary>> -> Location;
        <<"/", _/binary>> ->
            %% Absolute path - use original scheme, host, port
            #hackney_url{scheme = Scheme, host = Host, port = Port} = hackney_url:parse_url(OriginalUrl),
            SchemeBin = atom_to_binary(Scheme),
            HostBin = list_to_binary(Host),
            case {Scheme, Port} of
                {https, 443} -> <<SchemeBin/binary, "://", HostBin/binary, Location/binary>>;
                {http, 80} -> <<SchemeBin/binary, "://", HostBin/binary, Location/binary>>;
                _ ->
                    PortBin = integer_to_binary(Port),
                    <<SchemeBin/binary, "://", HostBin/binary, ":", PortBin/binary, Location/binary>>
            end;
        _ ->
            %% Relative path
            #hackney_url{scheme = Scheme, host = Host, port = Port, path = BasePath} = hackney_url:parse_url(OriginalUrl),
            SchemeBin = atom_to_binary(Scheme),
            HostBin = list_to_binary(Host),
            %% Get directory of base path
            BaseDir = case binary:split(BasePath, <<"/">>, [global, trim]) of
                [] -> <<"/">>;
                Parts ->
                    DirParts = lists:droplast(Parts),
                    case DirParts of
                        [] -> <<"/">>;
                        _ -> iolist_to_binary(["/", lists:join("/", DirParts), "/"])
                    end
            end,
            FullPath = <<BaseDir/binary, Location/binary>>,
            case {Scheme, Port} of
                {https, 443} -> <<SchemeBin/binary, "://", HostBin/binary, FullPath/binary>>;
                {http, 80} -> <<SchemeBin/binary, "://", HostBin/binary, FullPath/binary>>;
                _ ->
                    PortBin = integer_to_binary(Port),
                    <<SchemeBin/binary, "://", HostBin/binary, ":", PortBin/binary, FullPath/binary>>
            end
    end.

%% @private Determine HTTP method after redirect
%% 301, 302, 303: POST/PUT/DELETE -> GET (de facto standard)
%% 307, 308: Preserve original method
redirect_method(Status, Method) when Status =:= 301; Status =:= 302; Status =:= 303 ->
    case Method of
        get -> get;
        head -> head;
        _ -> get  %% POST, PUT, DELETE, etc. become GET
    end;
redirect_method(_Status, Method) ->
    %% 307, 308 preserve method
    Method.

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
    case connect(Host, Port, Opts, self()) of
        {ok, ConnRef} ->
            wait_connected(ConnRef, Timeout, erlang:monotonic_time(millisecond));
        {error, _} = Error ->
            Error
    end.

%% @doc Wait for an HTTP/3 response.
-spec await_response(reference(), non_neg_integer()) ->
    {ok, integer(), headers(), binary()} | {error, term()}.
await_response(ConnRef, StreamId) ->
    await_response_loop(ConnRef, StreamId, 30000, undefined, [], <<>>).

%%====================================================================
%% Request operations (used by hackney_conn)
%%====================================================================

%% @doc Send a complete HTTP/3 request (headers + body).
%% Returns {ok, StreamId, UpdatedStreams} or {error, Reason}.
-spec send_request(h3_conn(), method(), binary(), binary(), headers(), binary()) ->
    {ok, stream_id(), streams_map()} | {error, term()}.
send_request(ConnRef, Method, Host, Path, Headers, Body) ->
    AllHeaders = build_request_headers(Method, Host, Path, Headers),
    HasBody = Body =/= <<>> andalso Body =/= [],
    Fin = not HasBody,
    case send_request(ConnRef, AllHeaders, Fin) of
        {ok, StreamId} when HasBody ->
            case send_data(ConnRef, StreamId, Body, true) of
                ok ->
                    {ok, StreamId, #{StreamId => {undefined, waiting_headers}}};
                {error, _} = Error ->
                    Error
            end;
        {ok, StreamId} ->
            {ok, StreamId, #{StreamId => {undefined, waiting_headers}}};
        {error, _} = Error ->
            Error
    end.

%% @doc Send HTTP/3 request headers only (for streaming body).
%% Returns {ok, StreamId, UpdatedStreams} or {error, Reason}.
-spec send_request_headers(h3_conn(), method(), binary(), binary(), headers()) ->
    {ok, stream_id(), streams_map()} | {error, term()}.
send_request_headers(ConnRef, Method, Host, Path, Headers) ->
    AllHeaders = build_request_headers(Method, Host, Path, Headers),
    case send_request(ConnRef, AllHeaders, false) of
        {ok, StreamId} ->
            {ok, StreamId, #{StreamId => {undefined, waiting_headers}}};
        {error, _} = Error ->
            Error
    end.

%% @doc Send a chunk of body data on a stream.
-spec send_body_chunk(h3_conn(), stream_id(), binary(), boolean()) ->
    ok | {error, term()}.
send_body_chunk(ConnRef, StreamId, Data, Fin) ->
    send_data(ConnRef, StreamId, Data, Fin).

%% @doc Finish sending body (close stream for writing).
-spec finish_send_body(h3_conn(), stream_id(), streams_map()) ->
    {ok, streams_map()} | {error, term()}.
finish_send_body(ConnRef, StreamId, Streams) ->
    case send_data(ConnRef, StreamId, <<>>, true) of
        ok ->
            {ok, Streams};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Stream management
%%====================================================================

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

%%====================================================================
%% Response parsing
%%====================================================================

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

%%====================================================================
%% Connection close
%%====================================================================

%% @doc Close the HTTP/3 connection.
-spec close(h3_conn()) -> ok.
close(ConnRef) ->
    close(ConnRef, normal).

%% @doc Close the HTTP/3 connection with a reason.
-spec close(h3_conn(), term()) -> ok.
close(ConnRef, Reason) ->
    with_pid(ConnRef, fun(Pid) -> gen_server:cast(Pid, {close, Reason}) end, ok).

%%====================================================================
%% Internal functions
%%====================================================================

%% Drive QUIC event loop until connected
wait_connected(ConnRef, Timeout, StartTime) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    Remaining = max(0, Timeout - Elapsed),
    receive
        {select, _Resource, _Ref, ready_input} ->
            _ = process(ConnRef),
            wait_connected(ConnRef, Timeout, StartTime);
        {h3, ConnRef, {connected, _Info}} ->
            {ok, ConnRef};
        {h3, ConnRef, {closed, Reason}} ->
            {error, {connection_closed, Reason}};
        {h3, ConnRef, {transport_error, Code, Msg}} ->
            {error, {transport_error, Code, Msg}};
        {h3, ConnRef, {settings, _Settings}} ->
            %% HTTP/3 SETTINGS frame - ignore and continue waiting
            wait_connected(ConnRef, Timeout, StartTime)
    after Remaining ->
        close(ConnRef, timeout),
        {error, timeout}
    end.

do_request(ConnRef, Method, Host, Path, Headers, Body, Timeout) ->
    AllHeaders = build_request_headers(Method, Host, Path, Headers),
    HasBody = Body =/= <<>> andalso Body =/= [],
    Fin = not HasBody,
    case send_request(ConnRef, AllHeaders, Fin) of
        {ok, StreamId} when HasBody ->
            case send_data(ConnRef, StreamId, Body, true) of
                ok ->
                    await_response_loop(ConnRef, StreamId, Timeout, undefined, [], <<>>);
                {error, _} = Error ->
                    Error
            end;
        {ok, StreamId} ->
            await_response_loop(ConnRef, StreamId, Timeout, undefined, [], <<>>);
        {error, _} = Error ->
            Error
    end.

%% @private Wait for HTTP/3 response.
%% Timeout is per-chunk - resets each time data is received.
%% This allows large responses to complete as long as data keeps flowing.
%% Note: For very large responses, use hackney_conn with streaming mode instead.
await_response_loop(ConnRef, StreamId, Timeout, Status, Headers, AccBody) ->
    receive
        {select, _Resource, _Ref, ready_input} ->
            _ = process(ConnRef),
            await_response_loop(ConnRef, StreamId, Timeout, Status, Headers, AccBody);
        {h3, ConnRef, {stream_headers, StreamId, RespHeaders, Fin}} ->
            NewStatus = get_status(RespHeaders),
            FilteredHeaders = filter_pseudo_headers(RespHeaders),
            case Fin of
                true ->
                    %% Headers with Fin=true means no body (e.g., HEAD response, 204, 304)
                    {ok, NewStatus, FilteredHeaders, AccBody};
                false ->
                    await_response_loop(ConnRef, StreamId, Timeout, NewStatus, FilteredHeaders, AccBody)
            end;
        {h3, ConnRef, {stream_data, StreamId, Data, Fin}} ->
            NewBody = <<AccBody/binary, Data/binary>>,
            case Fin of
                true ->
                    {ok, Status, Headers, NewBody};
                false ->
                    await_response_loop(ConnRef, StreamId, Timeout, Status, Headers, NewBody)
            end;
        {h3, ConnRef, {stream_reset, StreamId, _ErrorCode}} ->
            {error, stream_reset};
        {h3, ConnRef, {closed, Reason}} ->
            {error, {connection_closed, Reason}};
        {h3, ConnRef, {settings, _Settings}} ->
            %% HTTP/3 SETTINGS frame - ignore and continue waiting
            await_response_loop(ConnRef, StreamId, Timeout, Status, Headers, AccBody);
        {h3, ConnRef, {goaway, _StreamId2}} ->
            %% GOAWAY frame - connection is shutting down
            {error, goaway}
    after Timeout ->
        {error, timeout}
    end.

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

%% @private Extract status from HTTP/3 response headers (returns integer or 0).
-spec get_status(headers()) -> integer().
get_status(Headers) ->
    case lists:keyfind(<<":status">>, 1, Headers) of
        {_, StatusBin} ->
            binary_to_integer(StatusBin);
        false ->
            0
    end.

%% @private Extract status from HTTP/3 response headers (returns ok/error tuple).
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

%%====================================================================
%% Low-level adapter API (gen_server-backed)
%%
%% Messages delivered to the owner:
%%   {h3, ConnRef, {connected, Info}}
%%   {h3, ConnRef, {stream_headers, StreamId, Headers, Fin}}
%%   {h3, ConnRef, {stream_data, StreamId, Bin, Fin}}
%%   {h3, ConnRef, {stream_reset, StreamId, ErrorCode}}
%%   {h3, ConnRef, {goaway, LastStreamId}}
%%   {h3, ConnRef, {closed, Reason}}
%%   {h3, ConnRef, {transport_error, Code, Reason}}
%%====================================================================

-spec get_fd(gen_udp:socket()) -> {ok, integer()} | {error, term()}.
get_fd(Socket) ->
    inet:getfd(Socket).

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
    case gen_server:start(?MODULE, {Host, Port, Opts, Owner}, []) of
        {ok, Pid} ->
            gen_server:call(Pid, get_conn_ref);
        {error, _} = Error ->
            Error
    end;
connect(_Host, _Port, _Opts, _Owner) ->
    {error, badarg}.

-spec send_request(reference(), [{binary(), binary()}], boolean()) ->
    {ok, non_neg_integer()} | {error, term()}.
send_request(ConnRef, Headers, Fin) when is_list(Headers), is_boolean(Fin) ->
    with_pid(ConnRef,
             fun(Pid) -> gen_server:call(Pid, {send_request, Headers, Fin}) end,
             {error, not_connected}).

-spec send_data(reference(), non_neg_integer(), iodata(), boolean()) ->
    ok | {error, term()}.
send_data(ConnRef, StreamId, Data, Fin) when is_boolean(Fin) ->
    with_pid(ConnRef,
             fun(Pid) -> gen_server:call(Pid, {send_data, StreamId, Data, Fin}) end,
             {error, not_connected});
send_data(_ConnRef, _StreamId, _Data, _Fin) ->
    {error, badarg}.

-spec reset_stream(reference(), non_neg_integer(), non_neg_integer()) ->
    ok | {error, term()}.
reset_stream(ConnRef, StreamId, ErrorCode)
  when is_integer(ErrorCode), ErrorCode >= 0 ->
    with_pid(ConnRef,
             fun(Pid) -> gen_server:call(Pid, {reset_stream, StreamId, ErrorCode}) end,
             {error, not_connected});
reset_stream(_ConnRef, _StreamId, _ErrorCode) ->
    {error, badarg}.

-spec handle_timeout(reference(), non_neg_integer()) -> non_neg_integer() | infinity.
handle_timeout(_ConnRef, _NowMs) ->
    infinity.

-spec process(reference()) -> non_neg_integer() | infinity.
process(_ConnRef) ->
    infinity.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Host, Port, Opts, Owner}) ->
    MonRef = erlang:monitor(process, Owner),
    H3Opts = build_h3_opts(Host, Opts),
    case quic_h3:connect(Host, Port, H3Opts) of
        {ok, H3Conn} ->
            ConnRef = make_ref(),
            _ = ensure_table(),
            ets:insert(?CONN_TABLE, {ConnRef, self()}),
            {ok, #state{h3_conn = H3Conn,
                        conn_ref = ConnRef,
                        owner = Owner,
                        owner_mon = MonRef}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_conn_ref, _From, #state{conn_ref = Ref} = State) ->
    {reply, {ok, Ref}, State};

handle_call({send_request, Headers, Fin}, _From, #state{h3_conn = Conn} = State) ->
    case quic_h3:request(Conn, Headers, #{end_stream => Fin}) of
        {ok, StreamId} ->
            {reply, {ok, StreamId}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({send_data, StreamId, Data, Fin}, _From, #state{h3_conn = Conn} = State) ->
    Bin = iolist_to_binary(Data),
    {reply, quic_h3:send_data(Conn, StreamId, Bin, Fin), State};

handle_call({reset_stream, StreamId, ErrorCode}, _From, #state{h3_conn = Conn} = State) ->
    {reply, quic_h3:cancel(Conn, StreamId, ErrorCode), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({close, _Reason}, #state{h3_conn = Conn} = State) ->
    catch quic_h3:close(Conn),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({quic_h3, Conn, connected},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {h3, Ref, {connected, #{}}},
    {noreply, State};

handle_info({quic_h3, Conn, {response, StreamId, Status, Headers}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Full = [{<<":status">>, integer_to_binary(Status)} | Headers],
    Owner ! {h3, Ref, {stream_headers, StreamId, Full, false}},
    {noreply, State};

handle_info({quic_h3, Conn, {data, StreamId, Data, Fin}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {h3, Ref, {stream_data, StreamId, Data, Fin}},
    {noreply, State};

handle_info({quic_h3, Conn, {trailers, StreamId, Trailers}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {h3, Ref, {stream_headers, StreamId, Trailers, true}},
    {noreply, State};

handle_info({quic_h3, Conn, {stream_reset, StreamId, ErrorCode}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {h3, Ref, {stream_reset, StreamId, ErrorCode}},
    {noreply, State};

handle_info({quic_h3, Conn, {goaway, LastStreamId}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {h3, Ref, {goaway, LastStreamId}},
    {noreply, State};

handle_info({quic_h3, Conn, {goaway_sent, _}}, #state{h3_conn = Conn} = State) ->
    {noreply, State};

handle_info({quic_h3, Conn, closed},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {h3, Ref, {closed, normal}},
    {stop, normal, State};

handle_info({quic_h3, Conn, {error, Code, Reason}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {h3, Ref, {transport_error, Code, Reason}},
    {stop, {transport_error, Code}, State};

handle_info({'DOWN', MonRef, process, _Pid, _Reason},
            #state{owner_mon = MonRef, h3_conn = Conn} = State) ->
    catch quic_h3:close(Conn),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn_ref = Ref, h3_conn = Conn}) ->
    case ets:whereis(?CONN_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?CONN_TABLE, Ref)
    end,
    case Conn of
        undefined -> ok;
        _ -> catch quic_h3:close(Conn)
    end,
    ok.

%%====================================================================
%% Internal adapter helpers
%%====================================================================

build_h3_opts(Host, Opts) ->
    HostStr = binary_to_list(Host),
    Verify = case maps:get(insecure_skip_verify, Opts, false) of
        true -> verify_none;
        false ->
            case maps:get(verify, Opts, verify_peer) of
                verify_peer -> verify_peer;
                verify_none -> verify_none;
                true -> verify_peer;
                false -> verify_none
            end
    end,
    QuicOpts0 = #{server_name_indication => HostStr},
    QuicOpts = case maps:get(cacerts, Opts, undefined) of
        undefined ->
            case maps:get(cacertfile, Opts, undefined) of
                undefined -> QuicOpts0;
                File -> QuicOpts0#{cacertfile => File}
            end;
        CACerts -> QuicOpts0#{cacerts => CACerts}
    end,
    Base = #{verify => Verify, quic_opts => QuicOpts},
    case maps:get(settings, Opts, undefined) of
        undefined -> Base;
        Settings -> Base#{settings => Settings}
    end.

ensure_table() ->
    case ets:whereis(?CONN_TABLE) of
        undefined ->
            try
                ets:new(?CONN_TABLE,
                        [named_table, public, set, {read_concurrency, true}])
            catch
                error:badarg -> ok
            end;
        _ -> ok
    end.

with_pid(ConnRef, Fun, Default) ->
    case ets:whereis(?CONN_TABLE) of
        undefined -> Default;
        _ ->
            case ets:lookup(?CONN_TABLE, ConnRef) of
                [{ConnRef, Pid}] -> Fun(Pid);
                [] -> Default
            end
    end.
