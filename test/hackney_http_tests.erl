-module(hackney_http_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

parse_response_correct_200_test() ->
	Response = <<"HTTP/1.1 200 OK">>,
	St = #hparser{},
	{response, _Version, StatusInt, Reason, _NState} = hackney_http:parse_response_version(Response, St),
	?assertEqual(StatusInt, 200),
	?assertEqual(Reason, <<"OK">>).

parse_response_incomplete_200_test() ->
	Response = <<"HTTP/1.1 200 ">>,
	St = #hparser{},
	{response, _Version, StatusInt, Reason, _NState} =  hackney_http:parse_response_version(Response, St),
	?assertEqual(StatusInt, 200),
	?assertEqual(Reason, <<"">>).

parse_response_missing_reason_phrase_test() ->
    	Response = <<"HTTP/1.1 200">>,
	St = #hparser{},
	{response, _Version, StatusInt, Reason, _NState} =  hackney_http:parse_response_version(Response, St),
	?assertEqual(StatusInt, 200),
	?assertEqual(Reason, <<"">>).

parse_response_header_with_continuation_line_test() ->
	Response = <<"HTTP/1.1 200\r\nContent-Type: multipart/related;\r\n\tboundary=\"--:\"\r\n\r\n">>,
	ST1 = #hparser{},
	{response, _Version, _StatusInt, _Reason, ST2} = hackney_http:execute(ST1, Response),
	{header, Header, ST3} = hackney_http:execute(ST2),
	?assertEqual({<<"Content-Type">>, <<"multipart/related; boundary=\"--:\"">>}, Header),
	{headers_complete, _ST4} = hackney_http:execute(ST3).
