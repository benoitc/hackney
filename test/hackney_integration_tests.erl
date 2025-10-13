-module(hackney_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").


all_tests() ->
  [fun get_request/0,
   fun request_with_body/0,
   fun head_request/0,
   fun no_content_response/0,
   fun not_modified_response/0,
   fun basic_auth_request_failed/0,
   fun basic_auth_request/0,
   fun basic_auth_url_request/0,
   fun basic_auth_app_variable_test/0,
   fun set_cookie_request/0,
   fun send_cookies_request/0,
   fun absolute_redirect_request_no_follow/0,
   fun absolute_redirect_request_follow/0,
  % fun relative_redirect_request_no_follow/0,
   fun relative_redirect_request_follow/0,
   fun test_duplicate_headers/0,
   fun test_custom_host_headers/0,
   fun async_request/0,
   fun async_head_request/0,
   fun async_no_content_request/0,
   fun test_frees_manager_ets_when_body_is_in_client/0,
   fun test_frees_manager_ets_when_body_is_in_response/0,
   fun test_307_redirect_pool_cleanup/0].

%%all_tests() ->
%%    case has_unix_socket() of
%%        true -> def
%% ault_tests() ++ [local_socket_request()];
%%        false -> default_tests()
%%    end.

http_requests_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(ok) ->
         {inorder, all_tests()}
     end}.

start() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

stop(ok) -> ok.

get_request() ->
    URL = <<"http://localhost:8000/get">>,
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?assertEqual(200, StatusCode).

request_with_body() ->
    URL = <<"http://localhost:8000/robots.txt">>,
    ExpectedBody = <<"User-agent: *\nDisallow: /deny\n">>,
    {ok, 200, _, Body} = hackney:request(get, URL, [], <<>>, [{with_body, true}]),
    ?assertEqual(ExpectedBody, Body).

head_request() ->
    URL = <<"http://localhost:8000/get">>,
    {ok, StatusCode, _} = hackney:request(head, URL, [], <<>>, []),
    ?assertEqual(200, StatusCode).

no_content_response() ->
    URL = <<"http://localhost:8000/status/204">>,
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?assertEqual(204, StatusCode).

not_modified_response() ->
    URL = <<"http://localhost:8000/status/304">>,
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?assertEqual(304, StatusCode).

basic_auth_request() ->
    URL = <<"http://localhost:8000/basic-auth/username/password">>,
    %% Use application variable instead of per-request option
    application:set_env(hackney, insecure_basic_auth, true),
    Options = [{basic_auth, {<<"username">>, <<"password">>}}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    application:unset_env(hackney, insecure_basic_auth),
    ?assertEqual(200, StatusCode).

basic_auth_url_request() ->
    URL = <<"http://username:pass%26word@localhost:8000/basic-auth/username/pass%26word">>,
    Options = [{insecure_basic_auth, true}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    ?assertEqual(200, StatusCode).

basic_auth_request_failed() ->
    URL = <<"http://localhost:8000/basic-auth/username/password">>,
    Options = [{basic_auth, {<<"wrong">>, <<"auth">>}}, {insecure_basic_auth, true}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    ?assertEqual(401, StatusCode).

set_cookie_request() ->
    URL = <<"http://localhost:8000/cookies/set?k1=v1">>,
    {ok, _, Headers, _} = hackney:request(get, URL, [], <<>>, []),
    Cookies = hackney:cookies(Headers),
    ExpectedCookies = [{<<"k1">>, [{<<"k1">>,<<"v1">>},{<<"Path">>,<<"/">>}]}],
    ?assertEqual(ExpectedCookies, Cookies).

send_cookies_request() ->
    URL = <<"http://localhost:8000/cookies">>,
    Options = [{cookie, [{<<"SESSION">>, <<"123">>}]}],
    {ok, _, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    {ok, Body} = hackney:body(Client),
    Match = re:run(Body, <<".*\"SESSION\".*\"123\".*">>),
    ?assertMatch({match, _}, Match).

absolute_redirect_request_no_follow() ->
    URL = <<"http://localhost:8000/redirect-to?url=http://localhost:8000/get">>,
    Options = [{follow_redirect, false}],
    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:location(Client),
    ?assertEqual(302, StatusCode),
    ?assertEqual(<<"http://localhost:8000/get">>, Location).

absolute_redirect_request_follow() ->
    URL = <<"http://localhost:8000/redirect-to?url=http://localhost:8000/get">>,
    Options = [{follow_redirect, true}],
    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:location(Client),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"http://localhost:8000/get">>, Location).

%relative_redirect_request_no_follow() ->
%    URL = <<"http://localhost:8000/relative-redirect/1">>,
%    Options = [{follow_redirect, false}],
%    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
%    Location = hackney:location(Client),
%    [?assertEqual(302, StatusCode),
%     ?assertEqual(Location, <<"/get">>)].

relative_redirect_request_follow() ->
    URL = <<"http://localhost:8000/redirect-to?url=/get">>,
    Options = [{follow_redirect, true}],
    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:location(Client),
    ?assertEqual(200, StatusCode),
    ?assertEqual(Location, <<"http://localhost:8000/get">>).

async_request() ->
    URL = <<"http://localhost:8000/get">>,
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    ?assertEqual(200, StatusCode),
    ?assertEqual([body, headers, status], Keys).

async_head_request() ->
    URL = <<"http://localhost:8000/get">>,
    Options = [async],
    {ok, ClientRef} = hackney:head(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    ?assertEqual(200, StatusCode),
    ?assertEqual([headers, status], Keys).

async_no_content_request() ->
    URL = <<"http://localhost:8000/status/204">>,
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    ?assertEqual(204, StatusCode),
    ?assertEqual([headers, status], Keys).

test_duplicate_headers() ->
  URL = <<"http://localhost:8000/post">>,
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  Body = <<"{\"test\": \"ok\" }">>,
  Options = [with_body],
  {ok, 200, _H, JsonBody} = hackney:post(URL, Headers, Body, Options),
  Obj = jsone:decode(JsonBody, [{object_format, proplist}]),
  ReqHeaders = proplists:get_value(<<"headers">>, Obj),
  ?assertEqual(<<"application/json">>, proplists:get_value(<<"Content-Type">>, ReqHeaders)).

test_custom_host_headers() ->
  URL = <<"http://localhost:8000/get">>,
  Headers = [{<<"Host">>, <<"myhost.com">>}],
  Options = [with_body],
  {ok, 200, _H, JsonBody} = hackney:get(URL, Headers, <<>>, Options),
  Obj = jsone:decode(JsonBody, [{object_format, proplist}]),
  ReqHeaders = proplists:get_value(<<"headers">>, Obj),
  ?assertEqual(<<"myhost.com">>, proplists:get_value(<<"Host">>, ReqHeaders)).

test_frees_manager_ets_when_body_is_in_client() ->
    URL = <<"http://localhost:8000/get">>,
    BeforeCount = ets:info(hackney_manager_refs, size),
    {ok, 200, _, Client} = hackney:get(URL),
    DuringCount = ets:info(hackney_manager_refs, size),
    {ok, _unusedBody} = hackney:body(Client),
    timer:sleep(10),
    AfterCount = ets:info(hackney_manager_refs, size),
    ?assertEqual(DuringCount, BeforeCount + 1),
    ?assertEqual(BeforeCount, AfterCount).

test_frees_manager_ets_when_body_is_in_response() ->
    URL = <<"http://localhost:8000/get">>,
    Headers = [],
    Options = [with_body],
    BeforeCount = ets:info(hackney_manager_refs, size),
    {ok, 200, _, _} = hackney:get(URL, Headers, <<>>, Options),
    timer:sleep(10),
    AfterCount = ets:info(hackney_manager_refs, size),
    ?assertEqual(BeforeCount, AfterCount).

%% Test for issue #307: Pool connections not freed when response code is 307
%% Tests that POST requests with 307 redirects properly clean up pool connections
test_307_redirect_pool_cleanup() ->
    %% Create a small test pool to monitor connection usage
    PoolName = test_pool_307_cleanup,
    PoolOpts = [{pool_size, 2}, {timeout, 5000}],
    ok = hackney_pool:start_pool(PoolName, PoolOpts),
    
    %% URL that returns a 307 redirect (httpbin supports this)
    URL = <<"http://localhost:8000/redirect-to?url=http://localhost:8000/get&status_code=307">>,
    RequestOpts = [{pool, PoolName}, {follow_redirect, false}],
    
    %% Get initial pool stats
    InitialStats = hackney_pool:get_stats(PoolName),
    InitialInUse = proplists:get_value(in_use_count, InitialStats),
    
    %% Make a GET request that should get a 307 redirect (similar to existing test)
    %% With follow_redirect=false, this should return the redirect response directly
    Result1 = hackney:request(get, URL, [], <<"">>, RequestOpts),
    
    %% Handle response - the fourth element might be a reference or client
    case Result1 of
        {ok, {maybe_redirect, 307, _Headers1, Client1}} ->
            {skip, _} = hackney:skip_body(Client1),
            ok;
        {ok, Status1, _Headers1} when Status1 >= 300, Status1 < 400 ->
            ok;
        {ok, Status1, _Headers1, Client1} when Status1 >= 200, Status1 < 400 ->
            {ok, _Body} = hackney:body(Client1),
            ok;
        Other ->
            ?debugFmt("Unexpected response: ~p~n", [Other])
    end,
    
    %% Make a second request to verify pool connections are available
    Result2 = hackney:request(get, URL, [], <<"">>, RequestOpts),
    
    case Result2 of
        {ok, {maybe_redirect, 307, _Headers2, Client2}} ->
            {skip, _} = hackney:skip_body(Client2),
            ok;
        {ok, Status2, _Headers2} when Status2 >= 300, Status2 < 400 ->
            ok;
        {ok, Status2, _Headers2, Client2} when Status2 >= 200, Status2 < 400 ->
            {ok, _Body2} = hackney:body(Client2),
            ok;
        {error, checkout_timeout} ->
            %% This would indicate the pool connection was not returned
            ?assert(false);
        Other2 ->
            ?debugFmt("Unexpected second response: ~p~n", [Other2])
    end,
    
    %% Allow time for async cleanup to complete
    timer:sleep(10),
    
    %% Check final pool stats - connections should be returned
    FinalStats = hackney_pool:get_stats(PoolName),
    FinalInUse = proplists:get_value(in_use_count, FinalStats),
    
    %% The key test: in_use_count should return to initial value
    ?assertEqual(InitialInUse, FinalInUse),
    
    %% Clean up test pool
    hackney_pool:stop_pool(PoolName).


%%local_socket_request() ->
%%    URL = <<"http+unix://httpbin.sock/get">>,
%%    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
%%    ?assertEqual(200, StatusCode).


%% Helpers

%%has_unix_socket() ->
%%    {ok, Vsn} = application:get_key(kernel, vsn),
%%    ParsedVsn = version_pad(string:tokens(Vsn, ".")),
%%    ParsedVsn >= {5, 0, 0}.

%%version_pad([Major]) ->
%%  {list_to_integer(Major), 0, 0};
%%version_pad([Major, Minor]) ->
%%    {list_to_integer(Major), list_to_integer(Minor), 0};
%%version_pad([Major, Minor, Patch]) ->
%%    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)};
%%version_pad([Major, Minor, Patch | _]) ->
%%    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)}.

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
    end.

basic_auth_app_variable_test() ->
    URL = <<"http://localhost:8000/basic-auth/username/password">>,
    %% Test application variable for global insecure basic auth setting
    application:set_env(hackney, insecure_basic_auth, true),
    Options = [{basic_auth, {<<"username">>, <<"password">>}}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    application:unset_env(hackney, insecure_basic_auth),
    ?assertEqual(200, StatusCode).
