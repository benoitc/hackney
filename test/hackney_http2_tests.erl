%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2025 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/2 frame parsing/building and HPACK encoding/decoding.
%%%

-module(hackney_http2_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% HTTP/2 Frame Parsing Tests
%%====================================================================

%% Test connection preface parsing
parse_connection_preface_test() ->
    Preface = <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>,
    ?assertEqual({ok, <<>>}, hackney_cow_http2:parse_sequence(Preface)),
    ?assertEqual({ok, <<"extra">>}, hackney_cow_http2:parse_sequence(<<Preface/binary, "extra">>)),
    ?assertEqual(more, hackney_cow_http2:parse_sequence(<<"PRI * HTTP/2.0\r\n">>)),
    ?assertMatch({connection_error, protocol_error, _},
                 hackney_cow_http2:parse_sequence(<<"INVALID PREFACE HERE!!!">>)).

%% Test DATA frame parsing
parse_data_frame_test() ->
    %% Build and parse a DATA frame
    StreamID = 1,
    Data = <<"Hello, HTTP/2!">>,
    Frame = hackney_cow_http2:data(StreamID, nofin, Data),
    FrameBin = iolist_to_binary(Frame),
    ?assertMatch({ok, {data, StreamID, nofin, Data}, <<>>},
                 hackney_cow_http2:parse(FrameBin)).

parse_data_frame_with_fin_test() ->
    StreamID = 3,
    Data = <<"Goodbye">>,
    Frame = hackney_cow_http2:data(StreamID, fin, Data),
    FrameBin = iolist_to_binary(Frame),
    ?assertMatch({ok, {data, StreamID, fin, Data}, <<>>},
                 hackney_cow_http2:parse(FrameBin)).

%% Test HEADERS frame parsing
parse_headers_frame_test() ->
    StreamID = 1,
    HeaderBlock = <<1, 2, 3, 4, 5>>,
    Frame = hackney_cow_http2:headers(StreamID, nofin, HeaderBlock),
    FrameBin = iolist_to_binary(Frame),
    ?assertMatch({ok, {headers, StreamID, nofin, head_fin, HeaderBlock}, <<>>},
                 hackney_cow_http2:parse(FrameBin)).

%% Test SETTINGS frame parsing
parse_settings_frame_test() ->
    Settings = #{
        header_table_size => 4096,
        enable_push => true,
        max_concurrent_streams => 100,
        initial_window_size => 65535,
        max_frame_size => 16384
    },
    Frame = hackney_cow_http2:settings(Settings),
    FrameBin = iolist_to_binary(Frame),
    {ok, {settings, ParsedSettings}, <<>>} = hackney_cow_http2:parse(FrameBin),
    ?assertEqual(4096, maps:get(header_table_size, ParsedSettings)),
    ?assertEqual(true, maps:get(enable_push, ParsedSettings)),
    ?assertEqual(100, maps:get(max_concurrent_streams, ParsedSettings)).

%% Test SETTINGS ACK
parse_settings_ack_test() ->
    Frame = hackney_cow_http2:settings_ack(),
    ?assertMatch({ok, settings_ack, <<>>}, hackney_cow_http2:parse(Frame)).

%% Test PING frame
parse_ping_frame_test() ->
    Opaque = 1234567890,
    Frame = hackney_cow_http2:ping(Opaque),
    ?assertMatch({ok, {ping, Opaque}, <<>>}, hackney_cow_http2:parse(Frame)).

%% Test PING ACK
parse_ping_ack_test() ->
    Opaque = 9876543210,
    Frame = hackney_cow_http2:ping_ack(Opaque),
    ?assertMatch({ok, {ping_ack, Opaque}, <<>>}, hackney_cow_http2:parse(Frame)).

%% Test GOAWAY frame
parse_goaway_frame_test() ->
    LastStreamID = 5,
    ErrorCode = no_error,
    DebugData = <<"goodbye">>,
    Frame = hackney_cow_http2:goaway(LastStreamID, ErrorCode, DebugData),
    FrameBin = iolist_to_binary(Frame),
    ?assertMatch({ok, {goaway, LastStreamID, no_error, DebugData}, <<>>},
                 hackney_cow_http2:parse(FrameBin)).

%% Test WINDOW_UPDATE frame (connection level)
parse_window_update_connection_test() ->
    Increment = 65535,
    Frame = hackney_cow_http2:window_update(Increment),
    ?assertMatch({ok, {window_update, Increment}, <<>>},
                 hackney_cow_http2:parse(Frame)).

%% Test WINDOW_UPDATE frame (stream level)
parse_window_update_stream_test() ->
    StreamID = 1,
    Increment = 32768,
    Frame = hackney_cow_http2:window_update(StreamID, Increment),
    ?assertMatch({ok, {window_update, StreamID, Increment}, <<>>},
                 hackney_cow_http2:parse(Frame)).

%% Test RST_STREAM frame
parse_rst_stream_test() ->
    StreamID = 1,
    Frame = hackney_cow_http2:rst_stream(StreamID, cancel),
    ?assertMatch({ok, {rst_stream, StreamID, cancel}, <<>>},
                 hackney_cow_http2:parse(Frame)).

%% Test PRIORITY frame
parse_priority_test() ->
    StreamID = 3,
    DepStreamID = 1,
    Weight = 16,
    Frame = hackney_cow_http2:priority(StreamID, shared, DepStreamID, Weight),
    ?assertMatch({ok, {priority, StreamID, shared, DepStreamID, _}, <<>>},
                 hackney_cow_http2:parse(Frame)).

%% Test incomplete frame returns more
parse_incomplete_frame_test() ->
    %% Send only part of a frame
    PartialFrame = <<0, 0, 10, 0, 0, 0, 0, 0, 1, "hel">>,
    ?assertEqual(more, hackney_cow_http2:parse(PartialFrame)).

%% Test frame with trailing data
parse_frame_with_extra_data_test() ->
    Opaque = 42,
    Frame = hackney_cow_http2:ping(Opaque),
    FrameWithExtra = <<Frame/binary, "extra">>,
    ?assertMatch({ok, {ping, Opaque}, <<"extra">>},
                 hackney_cow_http2:parse(FrameWithExtra)).

%%====================================================================
%% HTTP/2 Frame Building Tests
%%====================================================================

build_data_frame_test() ->
    Frame = hackney_cow_http2:data(1, nofin, <<"test">>),
    FrameBin = iolist_to_binary(Frame),
    %% Verify frame header: length=4, type=0 (DATA), flags=0, stream_id=1
    <<4:24, 0:8, 0:8, 0:1, 1:31, "test">> = FrameBin.

build_headers_frame_test() ->
    HeaderBlock = <<"headers">>,
    Frame = hackney_cow_http2:headers(1, fin, HeaderBlock),
    FrameBin = iolist_to_binary(Frame),
    %% Verify frame type is 1 (HEADERS), END_STREAM and END_HEADERS flags set
    <<7:24, 1:8, _Flags:8, 0:1, 1:31, "headers">> = FrameBin.

build_settings_frame_test() ->
    Settings = #{initial_window_size => 65535},
    Frame = hackney_cow_http2:settings(Settings),
    FrameBin = iolist_to_binary(Frame),
    %% Verify frame type is 4 (SETTINGS)
    <<_:24, 4:8, _/binary>> = FrameBin.

%%====================================================================
%% HPACK Encoding/Decoding Tests
%%====================================================================

%% Test HPACK state initialization
hpack_init_test() ->
    State = hackney_cow_hpack:init(),
    ?assert(is_tuple(State)).

hpack_init_with_max_size_test() ->
    State = hackney_cow_hpack:init(8192),
    ?assert(is_tuple(State)).

%% Test HPACK encode/decode roundtrip
hpack_encode_decode_test() ->
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"https">>},
        {<<":path">>, <<"/">>},
        {<<":authority">>, <<"example.com">>}
    ],
    {Encoded, _State1} = hackney_cow_hpack:encode(Headers),
    EncodedBin = iolist_to_binary(Encoded),
    {Decoded, _State2} = hackney_cow_hpack:decode(EncodedBin),
    ?assertEqual(Headers, Decoded).

%% Test HPACK with custom headers
hpack_custom_headers_test() ->
    Headers = [
        {<<":method">>, <<"POST">>},
        {<<":scheme">>, <<"https">>},
        {<<":path">>, <<"/api/v1/data">>},
        {<<":authority">>, <<"api.example.com">>},
        {<<"content-type">>, <<"application/json">>},
        {<<"x-custom-header">>, <<"custom-value">>}
    ],
    {Encoded, _State1} = hackney_cow_hpack:encode(Headers),
    EncodedBin = iolist_to_binary(Encoded),
    {Decoded, _State2} = hackney_cow_hpack:decode(EncodedBin),
    ?assertEqual(Headers, Decoded).

%% Test HPACK with static table entries
hpack_static_table_test() ->
    %% These headers should use indexed representation from static table
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"http">>},
        {<<":path">>, <<"/">>}
    ],
    {Encoded, _State} = hackney_cow_hpack:encode(Headers),
    EncodedBin = iolist_to_binary(Encoded),
    %% Static table entries should be very compact (1 byte each)
    ?assert(byte_size(EncodedBin) < 10).

%% Test HPACK encoding with huffman disabled
hpack_no_huffman_test() ->
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/test/path">>}
    ],
    {EncodedHuffman, _} = hackney_cow_hpack:encode(Headers),
    {EncodedRaw, _} = hackney_cow_hpack:encode(Headers, hackney_cow_hpack:init(), #{huffman => false}),
    %% Raw encoding should typically be larger than huffman
    _HuffmanSize = iolist_size(EncodedHuffman),
    _RawSize = iolist_size(EncodedRaw),
    %% Both should decode to the same headers
    {DecodedHuffman, _} = hackney_cow_hpack:decode(iolist_to_binary(EncodedHuffman)),
    {DecodedRaw, _} = hackney_cow_hpack:decode(iolist_to_binary(EncodedRaw)),
    ?assertEqual(DecodedHuffman, DecodedRaw).

%% Test HPACK dynamic table usage
hpack_dynamic_table_test() ->
    %% First request adds entries to dynamic table
    Headers1 = [
        {<<":method">>, <<"GET">>},
        {<<":authority">>, <<"www.example.com">>}
    ],
    {_Encoded1, State1} = hackney_cow_hpack:encode(Headers1),

    %% Second request should reuse entries from dynamic table
    Headers2 = [
        {<<":method">>, <<"GET">>},
        {<<":authority">>, <<"www.example.com">>}
    ],
    {Encoded2, _State2} = hackney_cow_hpack:encode(Headers2, State1),

    %% Second encoding should be more compact due to dynamic table
    {Decoded2, _} = hackney_cow_hpack:decode(iolist_to_binary(Encoded2), State1),
    ?assertEqual(Headers2, Decoded2).

%% Test HPACK max size update
hpack_set_max_size_test() ->
    State0 = hackney_cow_hpack:init(4096),
    State1 = hackney_cow_hpack:set_max_size(8192, State0),
    ?assert(is_tuple(State1)).

%% Test response headers encoding/decoding
hpack_response_headers_test() ->
    Headers = [
        {<<":status">>, <<"200">>},
        {<<"content-type">>, <<"text/html">>},
        {<<"content-length">>, <<"1234">>}
    ],
    {Encoded, _State} = hackney_cow_hpack:encode(Headers),
    {Decoded, _} = hackney_cow_hpack:decode(iolist_to_binary(Encoded)),
    ?assertEqual(Headers, Decoded).

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% Test DATA frame on stream 0 (invalid)
parse_data_stream_zero_test() ->
    %% Manually construct invalid DATA frame with stream ID 0
    InvalidFrame = <<0, 0, 5, 0, 0, 0, 0, 0, 0, "hello">>,
    ?assertMatch({connection_error, protocol_error, _},
                 hackney_cow_http2:parse(InvalidFrame)).

%% Test HEADERS frame on stream 0 (invalid)
parse_headers_stream_zero_test() ->
    InvalidFrame = <<0, 0, 5, 1, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5>>,
    ?assertMatch({connection_error, protocol_error, _},
                 hackney_cow_http2:parse(InvalidFrame)).

%% Test WINDOW_UPDATE with zero increment (invalid)
parse_window_update_zero_increment_test() ->
    InvalidFrame = <<4:24, 8:8, 0:9, 0:31, 0:1, 0:31>>,
    ?assertMatch({connection_error, protocol_error, _},
                 hackney_cow_http2:parse(InvalidFrame)).

%%====================================================================
%% All Frame Types Test Suite
%%====================================================================

all_frame_types_test_() ->
    [
        {"Connection preface", fun parse_connection_preface_test/0},
        {"DATA frame", fun parse_data_frame_test/0},
        {"DATA frame with fin", fun parse_data_frame_with_fin_test/0},
        {"HEADERS frame", fun parse_headers_frame_test/0},
        {"SETTINGS frame", fun parse_settings_frame_test/0},
        {"SETTINGS ACK", fun parse_settings_ack_test/0},
        {"PING frame", fun parse_ping_frame_test/0},
        {"PING ACK", fun parse_ping_ack_test/0},
        {"GOAWAY frame", fun parse_goaway_frame_test/0},
        {"WINDOW_UPDATE (connection)", fun parse_window_update_connection_test/0},
        {"WINDOW_UPDATE (stream)", fun parse_window_update_stream_test/0},
        {"RST_STREAM frame", fun parse_rst_stream_test/0},
        {"PRIORITY frame", fun parse_priority_test/0}
    ].

hpack_test_suite_test_() ->
    [
        {"HPACK init", fun hpack_init_test/0},
        {"HPACK init with max size", fun hpack_init_with_max_size_test/0},
        {"HPACK encode/decode roundtrip", fun hpack_encode_decode_test/0},
        {"HPACK custom headers", fun hpack_custom_headers_test/0},
        {"HPACK static table", fun hpack_static_table_test/0},
        {"HPACK no huffman", fun hpack_no_huffman_test/0},
        {"HPACK dynamic table", fun hpack_dynamic_table_test/0},
        {"HPACK set max size", fun hpack_set_max_size_test/0},
        {"HPACK response headers", fun hpack_response_headers_test/0}
    ].
