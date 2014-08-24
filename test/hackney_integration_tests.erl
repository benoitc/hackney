-module(hackney_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

http_requests_test_() ->
  {setup,
   fun start/0,
   fun stop/1,
   fun(SetupData) ->
     [get_request(SetupData),
      basic_auth_request_failed(SetupData),
      basic_auth_request(SetupData)]
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
