%% @doc Tests for automatic gzip/deflate decompression (issue #155)
-module(hackney_decompress_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 9879).

%% Setup/teardown for integration tests
setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(test_decompress_http, [{port, ?PORT}], #{
        env => #{dispatch => Dispatch}
    }),
    ok.

cleanup(_) ->
    cowboy:stop_listener(test_decompress_http),
    ok.

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?PORT))/binary, Path/binary>>.

%% =============================================================================
%% Issue #155: Transparent gzip/deflate decompression
%% =============================================================================

auto_decompress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"gzip response decompressed correctly", fun test_gzip_decompression/0},
      {"deflate response decompressed correctly", fun test_deflate_decompression/0},
      {"no decompression when option not set", fun test_no_decompression_without_option/0},
      {"uncompressed response unchanged", fun test_uncompressed_response/0},
      {"Accept-Encoding header added when auto_decompress is set", fun test_accept_encoding_header/0}
     ]}.

%% Test gzip response is decompressed correctly
test_gzip_decompression() ->
    {ok, StatusCode, _Headers, Body} = hackney:request(get, url(<<"/compressed/gzip">>), [], <<>>,
                                                       [{with_body, true}, {auto_decompress, true}]),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"Hello, this is gzip compressed data!">>, Body).

%% Test deflate response is decompressed correctly
test_deflate_decompression() ->
    {ok, StatusCode, _Headers, Body} = hackney:request(get, url(<<"/compressed/deflate">>), [], <<>>,
                                                       [{with_body, true}, {auto_decompress, true}]),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"Hello, this is deflate compressed data!">>, Body).

%% Test that without auto_decompress option, body is not decompressed
test_no_decompression_without_option() ->
    {ok, StatusCode, _Headers, Body} = hackney:request(get, url(<<"/compressed/gzip">>), [], <<>>,
                                                       [{with_body, true}]),  %% No auto_decompress
    ?assertEqual(200, StatusCode),
    %% Body should still be compressed (not the plain text)
    ?assertNotEqual(<<"Hello, this is gzip compressed data!">>, Body),
    %% Verify it's actually gzip data by checking magic bytes
    <<16#1f, 16#8b, _/binary>> = Body.  %% gzip magic number

%% Test uncompressed response is unchanged
test_uncompressed_response() ->
    {ok, StatusCode, _Headers, Body} = hackney:request(get, url(<<"/compressed/none">>), [], <<>>,
                                                       [{with_body, true}, {auto_decompress, true}]),
    ?assertEqual(200, StatusCode),
    ?assertEqual(<<"Hello, this is uncompressed data!">>, Body).

%% Test that Accept-Encoding header is added when auto_decompress is set
test_accept_encoding_header() ->
    %% Make request to /get which echoes headers back as JSON
    {ok, StatusCode, _Headers, Body} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                                       [{with_body, true}, {auto_decompress, true}]),
    ?assertEqual(200, StatusCode),
    %% Parse JSON response and check for accept-encoding header
    BodyMap = jsx:decode(Body, [return_maps]),
    HeadersMap = maps:get(<<"headers">>, BodyMap),
    AcceptEncoding = maps:get(<<"accept-encoding">>, HeadersMap, undefined),
    ?assertNotEqual(undefined, AcceptEncoding),
    %% Should contain gzip and deflate
    ?assert(binary:match(AcceptEncoding, <<"gzip">>) =/= nomatch),
    ?assert(binary:match(AcceptEncoding, <<"deflate">>) =/= nomatch).
