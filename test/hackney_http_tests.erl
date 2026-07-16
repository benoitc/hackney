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
	Response = <<"HTTP/1.1 200\r\nContent-Type: multipart/related;\r\n\tboundary=\"--:\"\r\nOther-Header: test\r\n\r\n">>,
	ST1 = #hparser{},
	{response, _Version, _StatusInt, _Reason, ST2} = hackney_http:execute(ST1, Response),
	{header, Header, ST3} = hackney_http:execute(ST2),
	?assertEqual({<<"Content-Type">>, <<"multipart/related; boundary=\"--:\"">>}, Header),
  {header, Header1, ST4} = hackney_http:execute(ST3),
  ?assertEqual({<<"Other-Header">>, <<"test">>}, Header1),
	{headers_complete, _ST5} = hackney_http:execute(ST4).

parse_request_correct_leading_newlines_test() ->
	Request = <<"\r\nGET / HTTP/1.1\r\n\r\n">>,
	ST1 = #hparser{},
	{request, Verb, Resource, Version, _ST2} = hackney_http:execute(ST1, Request),
	?assertEqual(Verb, <<"GET">>),
	?assertEqual(Resource, <<"/">>),
	?assertEqual(Version, {1,1}).

parse_request_error_too_many_newlines_test() ->
	Request = <<"\r\nGET / HTTP/1.1\r\n\r\n">>,
	St = #hparser{max_empty_lines = 0},
	{error, bad_request} = hackney_http:execute(St, Request).

parse_chunked_response_crlf_test() ->
	P0 = hackney_http:parser([response]),
	{_, _, _, _, P1} = hackney_http:execute(P0, <<"HTTP/1.1 200 OK\r\n">>),
	{_, _, P2} = hackney_http:execute(P1, <<"Transfer-Encoding: chunked\r\n">>),
	{_, P3} = hackney_http:execute(P2, <<"\r\n">>),

	?assertEqual({done, <<>>}, hackney_http:execute(P3, <<"0\r\n\r\n">>)),
	?assertEqual({done, <<"a">>}, hackney_http:execute(P3, <<"0\r\n\r\na">>)),
	{more, P4_1} = hackney_http:execute(P3, <<"0\r\n">>),
	?assertEqual({done, <<>>}, hackney_http:execute(P4_1, <<"\r\n">>)),
	{more, P4_2} = hackney_http:execute(P3, <<"0\r\n\r">>),
	?assertEqual({done, <<>>}, hackney_http:execute(P4_2, <<"\n">>)).

parse_chunked_response_trailers_test() ->
	P0 = hackney_http:parser([response]),
	{_, _, _, _, P1} = hackney_http:execute(P0, <<"HTTP/1.1 200 OK\r\n">>),
	{_, _, P2} = hackney_http:execute(P1, <<"Transfer-Encoding: chunked\r\n">>),
	{_, P3} = hackney_http:execute(P2, <<"\r\n">>),
	{more, P4} = hackney_http:execute(P3, <<"0\r\nFoo: ">>),
	?assertEqual({done, <<>>}, hackney_http:execute(P4, <<"Bar\r\n\r\n">>)).

%% Issue #901: the CRLF terminating a chunk-size line is split across two
%% reads, leaving the buffer on a lone \r. The parser must wait for the \n
%% instead of failing with invalid_chunk_size.
parse_chunked_size_crlf_split_test() ->
	?assertEqual({done, <<"HELLO">>},
	             run_chunked([<<"5\r">>, <<"\nHELLO\r\n0\r\n\r\n">>])).

parse_chunked_last_chunk_crlf_split_test() ->
	?assertEqual({done, <<"HELLO">>},
	             run_chunked([<<"5\r\nHELLO\r\n0\r">>, <<"\n\r\n">>])).

%% Exhaustive read-boundary sweep: a two-chunk body with a trailer, split at
%% every possible byte position, must always decode to the same body.
parse_chunked_split_sweep_test() ->
	Body = <<"5\r\nHELLO\r\n6\r\n WORLD\r\n0\r\nX-Trail: 1\r\n\r\n">>,
	[?assertEqual({done, <<"HELLO WORLD">>},
	              run_chunked([binary:part(Body, 0, I),
	                           binary:part(Body, I, byte_size(Body) - I)]))
	 || I <- lists:seq(0, byte_size(Body))].

%% A genuinely malformed size line must fail cleanly, not crash te_chunked.
parse_chunked_invalid_size_test() ->
	?assertEqual({error, invalid_chunk_size},
	             run_chunked([<<"5\rX\nHELLO\r\n0\r\n\r\n">>])).

%% A malformed chunk terminator must fail cleanly as well
%% (read_chunk's error was unhandled in te_chunked).
parse_chunked_bad_chunk_terminator_test() ->
	?assertEqual({error, poorly_formatted_chunked_size},
	             run_chunked([<<"5\r\nHELLOXX0\r\n\r\n">>])).

%% Drive a chunked response body through the parser, feeding Segments as
%% separate socket reads. Contract: after {ok, Chunk, P}, drain buffered data
%% with execute/1 until the parser asks for more, only then feed the next
%% segment. Returns {done, Body} when the response completed with no
%% leftover bytes.
run_chunked(Segments) ->
	P0 = hackney_http:parser([response]),
	{_, _, _, _, P1} = hackney_http:execute(P0, <<"HTTP/1.1 200 OK\r\n">>),
	{_, _, P2} = hackney_http:execute(P1, <<"Transfer-Encoding: chunked\r\n">>),
	{headers_complete, P3} = hackney_http:execute(P2, <<"\r\n">>),
	feed_chunked(P3, Segments, <<>>).

feed_chunked(_Parser, [], Acc) ->
	{incomplete, Acc};
feed_chunked(Parser, [Seg | Rest], Acc) ->
	chunked_step(hackney_http:execute(Parser, Seg), Rest, Acc).

chunked_step({ok, Chunk, Parser}, Segments, Acc) ->
	chunked_step(hackney_http:execute(Parser), Segments, <<Acc/binary, Chunk/binary>>);
chunked_step({more, Parser}, Segments, Acc) ->
	feed_chunked(Parser, Segments, Acc);
chunked_step({more, Parser, _Buffer}, Segments, Acc) ->
	feed_chunked(Parser, Segments, Acc);
chunked_step({done, <<>>}, _Segments, Acc) ->
	{done, Acc};
chunked_step({done, Leftover}, _Segments, Acc) ->
	{done_with_leftover, Acc, Leftover};
chunked_step(done, _Segments, Acc) ->
	{done, Acc};
chunked_step({error, Reason}, _Segments, _Acc) ->
	{error, Reason}.

%% Issue #697: Handle non-standard decimal status codes (e.g., 401.1 from IIS)
parse_response_decimal_status_code_test() ->
	Response = <<"HTTP/1.1 401.1 Access Denied">>,
	St = #hparser{},
	{response, _Version, StatusInt, Reason, _NState} = hackney_http:parse_response_version(Response, St),
	?assertEqual(401, StatusInt),
	?assertEqual(<<"Access Denied">>, Reason).

parse_response_decimal_status_code_two_digits_test() ->
	Response = <<"HTTP/1.1 403.14 Forbidden">>,
	St = #hparser{},
	{response, _Version, StatusInt, Reason, _NState} = hackney_http:parse_response_version(Response, St),
	?assertEqual(403, StatusInt),
	?assertEqual(<<"Forbidden">>, Reason).
