-module(hackney_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

-define(PORT, 8126).

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?PORT))/binary, Path/binary>>.

all_tests() ->
  [fun get_request/0,
   fun request_with_body/0,
   fun head_request/0,
   fun no_content_response/0,
   fun not_modified_response/0,
   fun no_content_body_read_then_next_request/0,
   fun not_modified_body_read_then_next_request/0,
   fun chunked_body_read_then_next_request/0,
   fun chunked_large_body_read/0,
   fun connection_close_response/0,
   fun connection_close_large_body/0,
   fun basic_auth_request_failed/0,
   fun basic_auth_request/0,
   fun basic_auth_url_request/0,
   fun basic_auth_app_variable/0,
   fun set_cookie_request/0,
   fun send_cookies_request/0,
   fun send_multiple_cookies_request/0,
   fun absolute_redirect_request_no_follow/0,
   fun absolute_redirect_request_follow/0,
   fun relative_redirect_request_follow/0,
   fun test_duplicate_headers/0,
   fun test_custom_host_headers/0,
   fun async_request/0,
   fun async_head_request/0,
   fun async_no_content_request/0,
   fun test_frees_manager_ets_when_body_is_in_client/0,
   fun test_frees_manager_ets_when_body_is_in_response/0,
   fun test_307_redirect_pool_cleanup/0,
   fun iolist_body_request/0,
   fun json_encode_body_request/0].

http_requests_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun({ok, _}) ->
         {inorder, all_tests()}
     end}.

start() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Host = '_',
    Routes = [
        {"/[...]", test_http_resource, []}
    ],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    cowboy:start_clear(integration_test_server, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}).

stop({ok, _Pid}) ->
    cowboy:stop_listener(integration_test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

get_request() ->
    URL = url(<<"/get">>),
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?assertEqual(200, StatusCode).

request_with_body() ->
    URL = url(<<"/robots.txt">>),
    ExpectedBody = <<"User-agent: *\nDisallow: /deny\n">>,
    {ok, 200, _, Body} = hackney:request(get, URL, [], <<>>, [{with_body, true}]),
    ?assertEqual(ExpectedBody, Body).

head_request() ->
    URL = url(<<"/get">>),
    {ok, StatusCode, _} = hackney:request(head, URL, [], <<>>, []),
    ?assertEqual(200, StatusCode).

no_content_response() ->
    URL = url(<<"/status/204">>),
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?assertEqual(204, StatusCode).

not_modified_response() ->
    URL = url(<<"/status/304">>),
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?assertEqual(304, StatusCode).

%% Test for issue #434: 204 response followed by next request should work correctly
%% Per RFC 7230 3.3.3, 204 and 304 responses have no body regardless of headers
no_content_body_read_then_next_request() ->
    URL204 = url(<<"/status/204">>),
    URLGet = url(<<"/get">>),
    %% First request: 204 (body returned directly, should be empty)
    {ok, Status1, _, Body1} = hackney:request(get, URL204, [], <<>>, []),
    ?assertEqual(204, Status1),
    ?assertEqual(<<>>, Body1),
    %% Second request: should succeed (connection closed after 204, new connection used)
    {ok, Status2, _, _} = hackney:request(get, URLGet, [], <<>>, []),
    ?assertEqual(200, Status2).

%% Same test for 304 Not Modified
not_modified_body_read_then_next_request() ->
    URL304 = url(<<"/status/304">>),
    URLGet = url(<<"/get">>),
    %% First request: 304 (body returned directly, should be empty)
    {ok, Status1, _, Body1} = hackney:request(get, URL304, [], <<>>, []),
    ?assertEqual(304, Status1),
    ?assertEqual(<<>>, Body1),
    %% Second request: should succeed
    {ok, Status2, _, _} = hackney:request(get, URLGet, [], <<>>, []),
    ?assertEqual(200, Status2).

%% Test for issue #403: Chunked transfer encoding should not hang
%% Tests that reading body from a chunked response works correctly
%% and doesn't hang or corrupt subsequent requests
chunked_body_read_then_next_request() ->
    URLChunked = url(<<"/chunked/1000">>),
    URLGet = url(<<"/get">>),
    %% First request: chunked response (body returned directly)
    {ok, Status1, Headers1, Body1} = hackney:request(get, URLChunked, [], <<>>, []),
    ?assertEqual(200, Status1),
    %% Verify it's chunked (no content-length header in Cowboy stream_reply)
    ?assertEqual(undefined, hackney_headers:get_value(<<"content-length">>, hackney_headers:from_list(Headers1))),
    ?assertEqual(1000, byte_size(Body1)),
    %% Second request: should succeed without hanging
    {ok, Status2, _, _} = hackney:request(get, URLGet, [], <<>>, []),
    ?assertEqual(200, Status2).

%% Test larger chunked response (closer to original issue)
chunked_large_body_read() ->
    %% Test with 100KB body similar to original issue
    URLChunked = url(<<"/chunked/102400">>),
    {ok, Status, _, Body} = hackney:request(get, URLChunked, [], <<>>, [{recv_timeout, 10000}]),
    ?assertEqual(200, Status),
    ?assertEqual(102400, byte_size(Body)).

%% Test for issue #439: Connection: close responses should not be lost
%% Tests that responses with Connection: close header are received correctly
connection_close_response() ->
    URL = url(<<"/connection-close">>),
    {ok, Status, Headers, Body} = hackney:request(get, URL, [], <<>>, []),
    ?assertEqual(200, Status),
    %% Verify Connection: close header
    ?assertEqual(<<"close">>, hackney_headers:get_value(<<"connection">>, hackney_headers:from_list(Headers))),
    ?assertEqual(<<"{\"connection\": \"close\"}">>, Body).

%% Test larger Connection: close response
connection_close_large_body() ->
    URL = url(<<"/connection-close/102400">>),
    {ok, Status, Headers, Body} = hackney:request(get, URL, [], <<>>, [{recv_timeout, 10000}]),
    ?assertEqual(200, Status),
    ?assertEqual(<<"close">>, hackney_headers:get_value(<<"connection">>, hackney_headers:from_list(Headers))),
    ?assertEqual(102400, byte_size(Body)).

basic_auth_request() ->
    URL = url(<<"/basic-auth/username/password">>),
    %% Use application variable instead of per-request option
    application:set_env(hackney, insecure_basic_auth, true),
    Options = [{basic_auth, {<<"username">>, <<"password">>}}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    application:unset_env(hackney, insecure_basic_auth),
    ?assertEqual(200, StatusCode).

basic_auth_url_request() ->
    %% URL with embedded credentials (simpler password without special chars)
    BaseURL = <<"http://username:secret@localhost:", (integer_to_binary(?PORT))/binary, "/basic-auth/username/secret">>,
    Options = [{insecure_basic_auth, true}],
    {ok, StatusCode, _, _} = hackney:request(get, BaseURL, [], <<>>, Options),
    ?assertEqual(200, StatusCode).

basic_auth_request_failed() ->
    URL = url(<<"/basic-auth/username/password">>),
    Options = [{basic_auth, {<<"wrong">>, <<"auth">>}}, {insecure_basic_auth, true}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    ?assertEqual(401, StatusCode).

set_cookie_request() ->
    URL = url(<<"/cookies/set?k1=v1">>),
    {ok, _, Headers, _} = hackney:request(get, URL, [], <<>>, []),
    Cookies = hackney:cookies(Headers),
    ExpectedCookies = [{<<"k1">>, [{<<"k1">>,<<"v1">>},{<<"Path">>,<<"/">>}]}],
    ?assertEqual(ExpectedCookies, Cookies).

send_cookies_request() ->
    URL = url(<<"/cookies">>),
    Options = [{cookie, [{<<"SESSION">>, <<"123">>}]}],
    {ok, _, _, Body} = hackney:request(get, URL, [], <<>>, Options),
    Match = re:run(Body, <<".*SESSION.*123.*">>),
    ?assertMatch({match, _}, Match).

%% Test for issue #719: Multiple cookies should be sent in a single Cookie header
%% RFC 6265 says "the user agent MUST NOT attach more than one Cookie header field"
send_multiple_cookies_request() ->
    URL = url(<<"/cookies">>),
    Options = [{cookie, [{<<"cookie1">>, <<"value1">>}, {<<"cookie2">>, <<"value2">>}]}],
    {ok, _, _, Body} = hackney:request(get, URL, [], <<>>, Options),
    %% Both cookies should be present in the response
    Match1 = re:run(Body, <<"cookie1">>),
    Match2 = re:run(Body, <<"cookie2">>),
    ?assertMatch({match, _}, Match1),
    ?assertMatch({match, _}, Match2).

absolute_redirect_request_no_follow() ->
    RedirectTarget = url(<<"/get">>),
    URL = <<"http://localhost:", (integer_to_binary(?PORT))/binary, "/redirect-to?url=", RedirectTarget/binary>>,
    Options = [{follow_redirect, false}],
    {ok, StatusCode, Headers, _Body} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:redirect_location(Headers),
    ?assertEqual(302, StatusCode),
    ?assertEqual(RedirectTarget, Location).

absolute_redirect_request_follow() ->
    RedirectTarget = url(<<"/get">>),
    URL = <<"http://localhost:", (integer_to_binary(?PORT))/binary, "/redirect-to?url=", RedirectTarget/binary>>,
    Options = [{follow_redirect, true}],
    {ok, StatusCode, _Headers, _Body} = hackney:request(get, URL, [], <<>>, Options),
    %% After following redirect, we're at the final URL, not the redirect URL
    ?assertEqual(200, StatusCode).

relative_redirect_request_follow() ->
    URL = url(<<"/redirect-to?url=/get">>),
    Options = [{follow_redirect, true}],
    {ok, StatusCode, _Headers, _Body} = hackney:request(get, URL, [], <<>>, Options),
    %% After following redirect, we're at the final URL
    ?assertEqual(200, StatusCode).

async_request() ->
    URL = url(<<"/get">>),
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    ?assertEqual(200, StatusCode),
    ?assertEqual([body, headers, status], Keys).

async_head_request() ->
    URL = url(<<"/get">>),
    Options = [async],
    {ok, ClientRef} = hackney:head(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    ?assertEqual(200, StatusCode),
    ?assertEqual([headers, status], Keys).

async_no_content_request() ->
    %% Test async handling for 204 No Content responses
    URL = url(<<"/status/204">>),
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    ?assertEqual(204, StatusCode),
    %% 204 should have status and headers but no body
    ?assertEqual([headers, status], Keys).

test_duplicate_headers() ->
  %% Test that Content-Type header is properly sent
  URL = url(<<"/post">>),
  Headers = [{<<"Content-Type">>, <<"text/plain">>}],
  Body = <<"test body">>,
  %% Use pool=false to prevent this connection from polluting the pool
  Options = [with_body, {pool, false}],
  {ok, 200, _H, JsonBody} = hackney:post(URL, Headers, Body, Options),
  Obj = jsx:decode(JsonBody, [return_maps]),
  ReqHeaders = maps:get(<<"headers">>, Obj),
  ?assertEqual(<<"text/plain">>, maps:get(<<"content-type">>, ReqHeaders)).

test_custom_host_headers() ->
  URL = url(<<"/get">>),
  Headers = [{<<"Host">>, <<"myhost.com">>}],
  Options = [with_body],
  {ok, 200, _H, JsonBody} = hackney:get(URL, Headers, <<>>, Options),
  Obj = jsx:decode(JsonBody, [return_maps]),
  ReqHeaders = maps:get(<<"headers">>, Obj),
  ?assertEqual(<<"myhost.com">>, maps:get(<<"host">>, ReqHeaders)).

test_frees_manager_ets_when_body_is_in_client() ->
    %% In 3.x, body is always returned directly, so this test now verifies
    %% that the body is returned and is binary, not a pid.
    URL = url(<<"/get">>),
    {ok, 200, _, Body} = hackney:get(URL),
    %% Body is now returned directly (not a pid)
    ?assert(is_binary(Body)),
    ok.

test_frees_manager_ets_when_body_is_in_response() ->
    %% Body is now always returned directly regardless of with_body option
    %% (option is deprecated and ignored)
    URL = url(<<"/get">>),
    Headers = [],
    Options = [with_body],
    {ok, 200, _, Body} = hackney:get(URL, Headers, <<>>, Options),
    %% Body should be the response content, not a pid
    ?assert(is_binary(Body)),
    ok.

%% Test for issue #307: Pool connections not freed when response code is 307
%% Tests that redirect responses properly clean up pool connections
test_307_redirect_pool_cleanup() ->
    %% Create a small test pool to monitor connection usage
    PoolName = test_pool_307_cleanup,
    PoolOpts = [{pool_size, 2}, {timeout, 5000}],
    ok = hackney_pool:start_pool(PoolName, PoolOpts),

    %% URL that returns a 307 redirect
    RedirectTarget = url(<<"/get">>),
    URL = <<"http://localhost:", (integer_to_binary(?PORT))/binary, "/redirect-to?url=", RedirectTarget/binary, "&status_code=307">>,
    RequestOpts = [{pool, PoolName}, {follow_redirect, false}],

    %% Get initial pool stats
    InitialStats = hackney_pool:get_stats(PoolName),
    InitialInUse = proplists:get_value(in_use_count, InitialStats),

    %% Make a GET request that should get a 307 redirect
    %% With follow_redirect=false, this should return the redirect response directly
    %% In 3.x, body is returned directly (connection auto-released to pool)
    {ok, Status1, _Headers1, _Body1} = hackney:request(get, URL, [], <<"">>, RequestOpts),
    ?assertEqual(307, Status1),

    %% Allow time for checkin to complete
    timer:sleep(10),

    %% Make a second request to verify pool connections are available
    {ok, Status2, _Headers2, _Body2} = hackney:request(get, URL, [], <<"">>, RequestOpts),
    ?assertEqual(307, Status2),

    %% Allow time for async cleanup to complete
    timer:sleep(10),

    %% Check final pool stats - connections should be returned
    FinalStats = hackney_pool:get_stats(PoolName),
    FinalInUse = proplists:get_value(in_use_count, FinalStats),

    %% The key test: in_use_count should return to initial value
    ?assertEqual(InitialInUse, FinalInUse),

    %% Clean up test pool
    hackney_pool:stop_pool(PoolName).

basic_auth_app_variable() ->
    URL = url(<<"/basic-auth/username/password">>),
    %% Test application variable for global insecure basic auth setting
    application:set_env(hackney, insecure_basic_auth, true),
    Options = [{basic_auth, {<<"username">>, <<"password">>}}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    application:unset_env(hackney, insecure_basic_auth),
    ?assertEqual(200, StatusCode).

iolist_body_request() ->
    URL = url(<<"/post">>),
    Headers = [{<<"Content-Type">>, <<"text/plain">>}],
    %% Test various iolist formats: strings, binaries, nested lists
    IolistBody = ["hello", <<" ">>, "world", [" - ", <<"nested">>]],
    ExpectedBody = <<"hello world - nested">>,
    Options = [with_body, {pool, false}],
    {ok, 200, _H, JsonBody} = hackney:post(URL, Headers, IolistBody, Options),
    Obj = jsx:decode(JsonBody, [return_maps]),
    ReceivedBody = maps:get(<<"data">>, Obj),
    ?assertEqual(ExpectedBody, ReceivedBody).

json_encode_body_request() ->
    URL = url(<<"/post">>),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    %% json:encode returns an iolist, not a binary
    Data = #{<<"key">> => <<"value">>, <<"number">> => 42},
    JsonBody = json:encode(Data),
    Options = [with_body, {pool, false}],
    {ok, 200, _H, ResponseBody} = hackney:post(URL, Headers, JsonBody, Options),
    %% Parse the response and verify the server received valid JSON
    ResponseObj = jsx:decode(ResponseBody, [return_maps]),
    %% Test server returns the raw body in "data" field
    ReceivedRaw = maps:get(<<"data">>, ResponseObj),
    ReceivedData = json:decode(ReceivedRaw),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, ReceivedData)),
    ?assertEqual(42, maps:get(<<"number">>, ReceivedData)).

%% Helpers

receive_response(Ref) ->
    Dict = receive_response(Ref, orddict:new()),
    Keys = orddict:fetch_keys(Dict),
    StatusCode = orddict:fetch(status, Dict),
    {StatusCode, Keys}.

receive_response(Ref, Dict0) ->
    receive
        {hackney_response, Ref, {status, Status, _Reason}} ->
            Dict1 = orddict:store(status, Status, Dict0),
            receive_response(Ref, Dict1);
        {hackney_response, Ref, {headers, Headers}} ->
            Dict1 = orddict:store(headers, Headers, Dict0),
            receive_response(Ref, Dict1);
        {hackney_response, Ref, done} -> Dict0;
        {hackney_response, Ref, Bin} ->
            Dict1 = orddict:append(body, Bin, Dict0),
            receive_response(Ref, Dict1)
    after 10000 ->
        {error, timeout}
    end.
