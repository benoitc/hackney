%%% -*- erlang -*-
%%%
%%% QPACK tests

-module(hackney_qpack_tests).

-include_lib("eunit/include/eunit.hrl").

%% Basic encoding tests

encode_empty_test() ->
    Encoded = hackney_qpack:encode([]),
    %% Just prefix (2 bytes)
    ?assertEqual(<<0, 0>>, Encoded).

encode_method_get_test() ->
    %% :method GET is in static table at index 17
    Encoded = hackney_qpack:encode([{<<":method">>, <<"GET">>}]),
    %% Prefix + indexed field line (static index 17)
    %% 11xxxxxx pattern for static indexed = 0xC0 | 17 = 0xD1
    ?assertEqual(<<0, 0, 16#D1>>, Encoded).

encode_status_200_test() ->
    %% :status 200 is in static table at index 25
    Encoded = hackney_qpack:encode([{<<":status">>, <<"200">>}]),
    %% 0xC0 | 25 = 0xD9
    ?assertEqual(<<0, 0, 16#D9>>, Encoded).

encode_scheme_https_test() ->
    %% :scheme https is in static table at index 23
    Encoded = hackney_qpack:encode([{<<":scheme">>, <<"https">>}]),
    %% 0xC0 | 23 = 0xD7
    ?assertEqual(<<0, 0, 16#D7>>, Encoded).

encode_path_root_test() ->
    %% :path / is in static table at index 1
    Encoded = hackney_qpack:encode([{<<":path">>, <<"/">>}]),
    %% 0xC0 | 1 = 0xC1
    ?assertEqual(<<0, 0, 16#C1>>, Encoded).

encode_literal_name_value_test() ->
    %% Custom header not in static table
    Encoded = hackney_qpack:encode([{<<"x-custom">>, <<"value">>}]),
    %% Prefix + literal instruction + name length + name + value length + value
    <<0, 0, 2#00100000, Rest/binary>> = Encoded,
    %% Name: length 8, "x-custom"
    <<8, "x-custom", 5, "value">> = Rest,
    ok.

encode_multiple_headers_test() ->
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"https">>}
    ],
    Encoded = hackney_qpack:encode(Headers),
    %% Should have prefix + 3 indexed entries
    <<0, 0, _M, _P, _S>> = Encoded,
    ok.

%% Basic decoding tests

decode_empty_test() ->
    {ok, Headers} = hackney_qpack:decode(<<0, 0>>),
    ?assertEqual([], Headers).

decode_indexed_static_test() ->
    %% :method GET at index 17 (0xD1)
    {ok, Headers} = hackney_qpack:decode(<<0, 0, 16#D1>>),
    ?assertEqual([{<<":method">>, <<"GET">>}], Headers).

decode_status_200_test() ->
    %% :status 200 at index 25 (0xD9)
    {ok, Headers} = hackney_qpack:decode(<<0, 0, 16#D9>>),
    ?assertEqual([{<<":status">>, <<"200">>}], Headers).

%% Roundtrip tests

roundtrip_static_exact_test() ->
    Original = [{<<":method">>, <<"GET">>}],
    Encoded = hackney_qpack:encode(Original),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(Original, Decoded).

roundtrip_multiple_static_test() ->
    Original = [
        {<<":method">>, <<"POST">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"https">>}
    ],
    Encoded = hackney_qpack:encode(Original),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(Original, Decoded).

roundtrip_literal_test() ->
    Original = [{<<"x-custom-header">>, <<"custom-value">>}],
    Encoded = hackney_qpack:encode(Original),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(Original, Decoded).

roundtrip_mixed_test() ->
    Original = [
        {<<":method">>, <<"GET">>},
        {<<":authority">>, <<"example.com">>},
        {<<":path">>, <<"/api/test">>},
        {<<":scheme">>, <<"https">>},
        {<<"accept">>, <<"*/*">>},
        {<<"user-agent">>, <<"hackney/3.0">>}
    ],
    Encoded = hackney_qpack:encode(Original),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(Original, Decoded).

%% HTTP/3 typical headers test

typical_request_test() ->
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, <<"cloudflare.com">>}
    ],
    Encoded = hackney_qpack:encode(Headers),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(Headers, Decoded).

typical_response_test() ->
    Headers = [
        {<<":status">>, <<"200">>},
        {<<"content-type">>, <<"text/html; charset=utf-8">>},
        {<<"server">>, <<"cloudflare">>}
    ],
    Encoded = hackney_qpack:encode(Headers),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(Headers, Decoded).
