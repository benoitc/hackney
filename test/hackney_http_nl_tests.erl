-module(hackney_http_nl_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

parse_response_header_with_continuation_line_test() ->
	Response = <<"HTTP/1.1 200\nContent-Type: multipart/related;\n\tboundary=\"--:\"\nOther-Header: test\n\n">>,
	ST1 = #hparser{},
	{response, _Version, _StatusInt, _Reason, ST2} = hackney_http:execute(ST1, Response),
	{header, Header, ST3} = hackney_http:execute(ST2),
	?assertEqual({<<"Content-Type">>, <<"multipart/related; boundary=\"--:\"">>}, Header),
  {header, Header1, ST4} = hackney_http:execute(ST3),
  ?assertEqual({<<"Other-Header">>, <<"test">>}, Header1),
	{headers_complete, _ST5} = hackney_http:execute(ST4).

parse_request_correct_leading_newlines_test() ->
	Request = <<"\nGET / HTTP/1.1\n\n">>,
	ST1 = #hparser{},
	{request, Verb, Resource, Version, _ST2} = hackney_http:execute(ST1, Request),
	?assertEqual(Verb, <<"GET">>),
	?assertEqual(Resource, <<"/">>),
	?assertEqual(Version, {1,1}).

parse_request_error_too_many_newlines_test() ->
	Request = <<"\nGET / HTTP/1.1\n\n">>,
	St = #hparser{max_empty_lines = 0},
	{error, bad_request} = hackney_http:execute(St, Request).

parse_chunked_response_crlf_test() ->
	P0 = hackney_http:parser([response]),
	{_, _, _, _, P1} = hackney_http:execute(P0, <<"HTTP/1.1 200 OK\n">>),
	{_, _, P2} = hackney_http:execute(P1, <<"Transfer-Encoding: chunked\n">>),
	{_, P3} = hackney_http:execute(P2, <<"\n">>),

	?assertEqual({done, <<>>}, hackney_http:execute(P3, <<"0\n\n">>)),
	?assertEqual({done, <<"a">>}, hackney_http:execute(P3, <<"0\n\na">>)),
	{more, P4_1} = hackney_http:execute(P3, <<"0\n">>),
	?assertEqual({done, <<>>}, hackney_http:execute(P4_1, <<"\n">>)),
	{more, P4_2} = hackney_http:execute(P3, <<"0\n\r">>),
	?assertEqual({done, <<>>}, hackney_http:execute(P4_2, <<"\n">>)).

parse_chunked_response_trailers_test() ->
	P0 = hackney_http:parser([response]),
	{_, _, _, _, P1} = hackney_http:execute(P0, <<"HTTP/1.1 200 OK\n">>),
	{_, _, P2} = hackney_http:execute(P1, <<"Transfer-Encoding: chunked\n">>),
	{_, P3} = hackney_http:execute(P2, <<"\n">>),
	{more, P4} = hackney_http:execute(P3, <<"0\nFoo: ">>),
	?assertEqual({done, <<>>}, hackney_http:execute(P4, <<"Bar\n\n">>)).
