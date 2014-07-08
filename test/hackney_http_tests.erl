-module(hackney_http_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

parse_response_correct_200_test() ->
	Response = <<"HTTP/1.1 200 OK">>,
	St = #hparser{},
	{response, Version, StatusInt, Reason, NState} = hackney_http:parse_response_version(Response, St),
	?assertEqual(StatusInt, 200),
	?assertEqual(Reason, <<"OK">>).

parse_response_incomplete_200_test() ->
	Response = <<"HTTP/1.1 200 ">>,
	St = #hparser{},
	{response, _Version, StatusInt, Reason, _NState} =  hackney_http:parse_response_version(Response, St),
	?assertEqual(StatusInt, 200),
	?assertEqual(Reason, <<"">>).
