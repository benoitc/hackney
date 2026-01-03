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
-export([get_proxy_config/2]).
-export([start_conn_with_socket/5]).
-endif.

-define(METHOD_TPL(Method),
  -export([Method/1, Method/2, Method/3, Method/4])).
-include("hackney_methods.hrl").

-include("hackney.hrl").
-include("hackney_lib.hrl").
-include_lib("hackney_internal.hrl").


-type url() :: #hackney_url{} | binary().
-export_type([url/0]).

%% Connection handle is now the hackney_conn process PID
-type conn() :: pid().
-export_type([conn/0]).

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
      ConnOpts = #{
        host => Host,
        port => Port,
        transport => Transport,
        connect_timeout => proplists:get_value(connect_timeout, Options, 8000),
        recv_timeout => proplists:get_value(recv_timeout, Options, 5000),
        connect_options => proplists:get_value(connect_options, Options, []),
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
      end;
    _PoolName ->
      %% Get connection from pool
      PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),
      case PoolHandler:checkout(Host, Port, Transport, Options) of
        {ok, _PoolRef, ConnPid} ->
          hackney_manager:start_request(Host),
          {ok, ConnPid};
        {error, Reason} ->
          {error, Reason}
      end
  end.

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
start_conn_with_socket(Host, Port, Transport, {SocketTransport, Socket}, Options) ->
  %% Handle {Transport, Socket} tuple from proxy modules
  %% Use the socket's transport for operations
  ActualTransport = normalize_transport(SocketTransport),
  start_conn_with_socket_internal(Host, Port, ActualTransport, Socket, Options);
start_conn_with_socket(Host, Port, Transport, Socket, Options) ->
  %% Raw socket
  ActualTransport = normalize_transport(Transport),
  start_conn_with_socket_internal(Host, Port, ActualTransport, Socket, Options).

start_conn_with_socket_internal(Host, Port, Transport, Socket, Options) ->
  ConnOpts = #{
    host => Host,
    port => Port,
    transport => Transport,
    socket => Socket,
    connect_timeout => proplists:get_value(connect_timeout, Options, 8000),
    recv_timeout => proplists:get_value(recv_timeout, Options, 5000),
    connect_options => proplists:get_value(connect_options, Options, []),
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
-spec request(url()) -> {ok, integer(), list(), conn()} | {ok, integer(), list()} | {error, term()}.
request(URL) ->
  request(get, URL).

-spec request(atom() | binary(), url()) -> {ok, integer(), list(), conn()} | {ok, integer(), list()} | {error, term()}.
request(Method, URL) ->
  request(Method, URL, [], <<>>, []).

-spec request(atom() | binary(), url(), list()) -> {ok, integer(), list(), conn()} | {ok, integer(), list()} | {error, term()}.
request(Method, URL, Headers) ->
  request(Method, URL, Headers, <<>>, []).

-spec request(atom() | binary(), url(), list(), term()) -> {ok, integer(), list(), conn()} | {ok, integer(), list()} | {error, term()}.
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
%% - {error, Reason}: Error
-spec request(atom() | binary(), url(), list(), term(), list()) ->
    {ok, integer(), list(), conn()} |
    {ok, integer(), list(), binary()} |
    {ok, integer(), list()} |
    {error, term()}.
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
  redirect_location(hackney_headers_new:from_list(Headers));
redirect_location(Headers) ->
  hackney_headers_new:get_value(<<"location">>, Headers).

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
  Headers1 = hackney_headers_new:new(Headers0),
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
  HeadersList = hackney_headers_new:to_list(FinalHeaders),

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
  HeadersList = hackney_headers_new:to_list(FinalHeaders),

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
  Headers1 = hackney_headers_new:store(<<"Content-Type">>, CType, Headers),
  Headers2 = hackney_headers_new:store(<<"Content-Length">>, integer_to_binary(CLen), Headers1),
  {Headers2, EncodedBody};
encode_body(Headers, {multipart, Parts}, _Options) ->
  %% Encode multipart body
  Boundary = hackney_multipart:boundary(),
  {MpBody, MpSize} = hackney_multipart:encode_form(Parts, Boundary),
  %% Add Content-Type with boundary
  ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,
  Headers1 = hackney_headers_new:store(<<"Content-Type">>, ContentType, Headers),
  Headers2 = hackney_headers_new:store(<<"Content-Length">>, integer_to_binary(MpSize), Headers1),
  {Headers2, MpBody};
encode_body(Headers, Body, _Options) when is_binary(Body) ->
  case hackney_headers_new:get_value(<<"content-length">>, Headers) of
    undefined ->
      Headers1 = hackney_headers_new:store(<<"Content-Length">>, integer_to_binary(byte_size(Body)), Headers),
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
  {_, Headers1} = hackney_headers_new:store_new(<<"Host">>, HostValue, Headers),
  Headers1.

%% Add default headers: User-Agent, Authorization (basic auth), Cookies
add_default_headers(Headers, Options, URL) ->
  %% Add User-Agent
  {_, Headers1} = hackney_headers_new:store_new(<<"User-Agent">>, default_ua(), Headers),

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
  hackney_headers_new:store(<<"Authorization">>, <<"Basic ", Credentials/binary>>, Headers).

add_cookies_header([], Headers) ->
  Headers;
add_cookies_header(Cookies, Headers) when is_list(Cookies) ->
  %% Format cookies as "name1=value1; name2=value2"
  CookieStr = format_cookies(Cookies),
  case CookieStr of
    <<>> -> Headers;
    _ -> hackney_headers_new:store(<<"Cookie">>, CookieStr, Headers)
  end;
add_cookies_header(Cookie, Headers) when is_binary(Cookie) ->
  hackney_headers_new:store(<<"Cookie">>, Cookie, Headers).

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
  case get_proxy_config(Scheme, Options) of
    false ->
      %% No proxy configured, direct connection
      connect(Transport, Host, Port, Options);
    _ProxyConfig ->
      %% Proxy configured - for now just connect directly
      %% TODO: Implement full proxy support
      ?report_debug("proxy configured but not fully implemented", []),
      connect(Transport, Host, Port, Options)
  end.

%% @doc Extract proxy configuration from options.
%% Returns: false | {Type, Host, Port, Auth}
%% Type: http | connect | socks5
%% Auth: undefined | {User, Pass}
-spec get_proxy_config(atom(), list()) ->
  false | {http | connect | socks5, string(), inet:port_number(),
           undefined | {binary(), binary()}}.
get_proxy_config(Scheme, Options) ->
  case proplists:get_value(proxy, Options) of
    undefined ->
      false;
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

%% Test exports
-ifdef(TEST).
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
-endif.

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
