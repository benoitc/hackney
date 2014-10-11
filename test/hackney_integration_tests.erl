-module(hackney_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

http_requests_test_() ->
  {setup,
   fun start/0,
   fun stop/1,
   fun(SetupData) ->
     {inparallel, [get_request(SetupData),
      basic_auth_request_failed(SetupData),
      basic_auth_request(SetupData),
      set_cookie_request(SetupData),
      send_cookies_request(SetupData),
      absolute_redirect_request_no_follow(SetupData),
      absolute_redirect_request_follow(SetupData),
      relative_redirect_request_no_follow(SetupData),
      relative_redirect_request_follow(SetupData),
      async_request(SetupData)]}
   end}.

start() -> hackney:start().

stop(_) -> ok.

get_request(_) ->
  URL = <<"http://localhost:8000/get">>,
  {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, []),
  ?_assertEqual(StatusCode, 200).

basic_auth_request(_) ->
  URL = <<"http://localhost:8000/basic-auth/username/password">>,
  Options = [{basic_auth, {<<"username">>, <<"password">>}}],
  {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
  ?_assertEqual(StatusCode, 200).

basic_auth_request_failed(_) ->
  URL = <<"http://localhost:8000/basic-auth/username/password">>,
  Options = [{basic_auth, {<<"wrong">>, <<"auth">>}}],
  {ok, StatusCode, _, _} = hackney:request(get, URL, [], <<>>, Options),
  ?_assertEqual(StatusCode, 401).

set_cookie_request(_) ->
  URL = <<"http://localhost:8000/cookies/set?k1=v1">>,
  {ok, _, Headers, _} = hackney:request(get, URL, [], <<>>, []),
  Cookies = hackney:cookies(Headers),
  ExpectedCookies = [{<<"k1">>, [{<<"k1">>,<<"v1">>},{<<"Path">>,<<"/">>}]}],
  ?_assertEqual(Cookies, ExpectedCookies).

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
  [?_assertEqual(StatusCode, 302), ?_assertEqual(Location, <<"http://localhost:8000/get">>)].

absolute_redirect_request_follow(_) ->
  URL = <<"http://localhost:8000/redirect-to?url=http://localhost:8000/get">>,
  Options = [{follow_redirect, true}],
  {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
  Location = hackney:location(Client),
  [?_assertEqual(StatusCode, 200), ?_assertEqual(Location, <<"http://localhost:8000/get">>)].

relative_redirect_request_no_follow(_) ->
  URL = <<"http://localhost:8000/relative-redirect/1">>,
  Options = [{follow_redirect, false}],
  {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
  Location = hackney:location(Client),
  [?_assertEqual(StatusCode, 302), ?_assertEqual(Location, <<"/get">>)].

relative_redirect_request_follow(_) ->
  URL = <<"http://localhost:8000/redirect-to?url=/get">>,
  Options = [{follow_redirect, true}],
  {ok, StatusCode, _, Client} = hackney:request(get, URL, [], <<>>, Options),
  Location = hackney:location(Client),
  [?_assertEqual(StatusCode, 200), ?_assertEqual(Location, <<"/get">>)].

async_request(_) ->
  URL = <<"http://localhost:8000/get">>,
  Opts = [async],
  LoopFun = fun(Loop, Ref, Dict) ->
                receive
                  {hackney_response, Ref, {status, StatusInt, _Reason}} ->
                    Dict2 = orddict:store(status, StatusInt, Dict),
                    Loop(Loop, Ref, Dict2);
                  {hackney_response, Ref, {headers, Headers}} ->
                    Dict2 = orddict:store(headers, Headers, Dict),
                    Loop(Loop, Ref, Dict2);
                  {hackney_response, Ref, done} -> Dict;
                  {hackney_response, Ref, Bin} ->
                    Dict2 = orddict:append(body, Bin, Dict),
                    Loop(Loop, Ref, Dict2)
                end
            end,
  {ok, ClientRef} = hackney:get(URL, [], <<>>, Opts),
  Dict = LoopFun(LoopFun, ClientRef, orddict:new()),
  Keys = orddict:fetch_keys(Dict),
  Status = orddict:fetch(status, Dict),
  [?_assertEqual(Keys, [body, headers, status]), ?_assertEqual(Status, 200)].
