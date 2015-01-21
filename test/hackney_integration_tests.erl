-module(hackney_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

http_requests_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(SetupData) ->
         {inparallel, [get_request(SetupData),
                       head_request(SetupData),
                       no_content_response(SetupData),
                       not_modified_response(SetupData),
                       basic_auth_request_failed(SetupData),
                       basic_auth_request(SetupData),
                       set_cookie_request(SetupData),
                       send_cookies_request(SetupData),
                       absolute_redirect_request_no_follow(SetupData),
                       absolute_redirect_request_follow(SetupData),
                       relative_redirect_request_no_follow(SetupData),
                       relative_redirect_request_follow(SetupData),
                       async_request(SetupData),
                       async_no_content_request(SetupData)]}
     end}.

start() -> hackney:start().

stop(_) -> ok.

get_request(_) ->
    URL = <<"http://localhost:8000/get">>,
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?_assertEqual(200, StatusCode).

head_request(_) ->
    URL = <<"http://localhost:8000/get">>,
    {ok, StatusCode, _} = hackney:request(head, URL, [], <<>>, []),
    ?_assertEqual(200, StatusCode).

no_content_response(_) ->
    URL = <<"http://localhost:8000/status/204">>,
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?_assertEqual(204, StatusCode).

not_modified_response(_) ->
    URL = <<"http://localhost:8000/status/304">>,
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
    ?_assertEqual(304, StatusCode).

basic_auth_request(_) ->
    URL = <<"http://localhost:8000/basic-auth/username/password">>,
    Options = [{basic_auth, {<<"username">>, <<"password">>}}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    ?_assertEqual(200, StatusCode).

basic_auth_request_failed(_) ->
    URL = <<"http://localhost:8000/basic-auth/username/password">>,
    Options = [{basic_auth, {<<"wrong">>, <<"auth">>}}],
    {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
    ?_assertEqual(401, StatusCode).

set_cookie_request(_) ->
    URL = <<"http://localhost:8000/cookies/set?k1=v1">>,
    {ok, _, Headers, _} = hackney:request(get, URL, [], <<>>, []),
    Cookies = hackney:cookies(Headers),
    ExpectedCookies = [{<<"k1">>, [{<<"k1">>,<<"v1">>},{<<"Path">>,<<"/">>}]}],
    ?_assertEqual(ExpectedCookies, Cookies).

send_cookies_request(_) ->
    URL = <<"http://localhost:8000/cookies">>,
    Options = [{cookie, [{<<"SESSION">>, <<"123">>}]}],
    {ok, _, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    {ok, Body} = hackney:body(Client),
    Match = re:run(Body, <<".*\"SESSION\".*\"123\".*">>),
    ?_assertMatch({match, _}, Match).

absolute_redirect_request_no_follow(_) ->
    URL = <<"http://localhost:8000/redirect-to?url=http://localhost:8000/get">>,
    Options = [{follow_redirect, false}],
    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:location(Client),
    [?_assertEqual(302, StatusCode), ?_assertEqual(<<"http://localhost:8000/get">>, Location)].

absolute_redirect_request_follow(_) ->
    URL = <<"http://localhost:8000/redirect-to?url=http://localhost:8000/get">>,
    Options = [{follow_redirect, true}],
    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:location(Client),
    [?_assertEqual(200, StatusCode), ?_assertEqual(<<"http://localhost:8000/get">>, Location)].

relative_redirect_request_no_follow(_) ->
    URL = <<"http://localhost:8000/relative-redirect/1">>,
    Options = [{follow_redirect, false}],
    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:location(Client),
    [?_assertEqual(302, StatusCode), ?_assertEqual(<<"/get">>, Location)].

relative_redirect_request_follow(_) ->
    URL = <<"http://localhost:8000/redirect-to?url=/get">>,
    Options = [{follow_redirect, true}],
    {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
    Location = hackney:location(Client),
    [?_assertEqual(200, StatusCode), ?_assertEqual(<<"/get">>, Location)].

async_request(_) ->
    URL = <<"http://localhost:8000/get">>,
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    Dict = receive_response(ClientRef, orddict:new()),
    Keys = orddict:fetch_keys(Dict),
    StatusCode = orddict:fetch(status, Dict),
    [?_assertEqual(200, StatusCode),
     ?_assertEqual([body, headers, status], Keys)].

async_no_content_request(_) ->
    URL = <<"http://localhost:8000/status/204">>,
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    Dict = receive_response(ClientRef, orddict:new()),
    Keys = orddict:fetch_keys(Dict),
    StatusCode = orddict:fetch(status, Dict),
    [?_assertEqual(204, StatusCode),
     ?_assertEqual([headers, status], Keys)].

%% Helpers

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
