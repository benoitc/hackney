-module(hackney_hpack_tests).
-include_lib("eunit/include/eunit.hrl").

-define(STATIC_TABLE_SIZE, 61).

encode_decode_roundtrip_test_() ->
    {"HPACK encode/decode round-trip", [
        {"single encode/decode cycle preserves headers",
         fun encode_decode_single/0},
        {"second encode uses dynamic table and still decodes correctly",
         fun encode_decode_reuses_dynamic_table/0},
        {"multiple sequential requests round-trip correctly",
         fun encode_decode_multiple_sequential/0},
        {"dynamic table indices start at STATIC_TABLE_SIZE + 1",
         fun dynamic_table_index_starts_at_62/0}
    ]}.

encode_decode_single() ->
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, <<"example.com">>}
    ],
    {Encoded, EncState} = hackney_hpack:encode(Headers),
    EncodedBin = iolist_to_binary(Encoded),
    {Decoded, _DecState} = hackney_hpack:decode(EncodedBin),
    ?assertEqual(Headers, Decoded),
    %% Verify encoder state is usable for next request
    ?assertNotEqual(hackney_hpack:init(), EncState).

encode_decode_reuses_dynamic_table() ->
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/api/test">>},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, <<"example.com">>},
        {<<"accept">>, <<"application/json">>}
    ],
    %% First encode populates the dynamic table
    {Encoded1, EncState1} = hackney_hpack:encode(Headers),
    Encoded1Bin = iolist_to_binary(Encoded1),
    {Decoded1, DecState1} = hackney_hpack:decode(Encoded1Bin),
    ?assertEqual(Headers, Decoded1),

    %% Second encode should use indexed references from dynamic table
    {Encoded2, _EncState2} = hackney_hpack:encode(Headers, EncState1),
    Encoded2Bin = iolist_to_binary(Encoded2),

    %% Second encoding should be smaller (using indexed references)
    ?assert(byte_size(Encoded2Bin) < byte_size(Encoded1Bin)),

    %% Must still decode to the same headers
    {Decoded2, _DecState2} = hackney_hpack:decode(Encoded2Bin, DecState1),
    ?assertEqual(Headers, Decoded2).

encode_decode_multiple_sequential() ->
    EncState0 = hackney_hpack:init(),
    DecState0 = hackney_hpack:init(),
    Requests = [
        [{<<":method">>, <<"GET">>},
         {<<":path">>, <<"/">>},
         {<<":scheme">>, <<"https">>},
         {<<":authority">>, <<"example.com">>}],
        [{<<":method">>, <<"GET">>},
         {<<":path">>, <<"/page2">>},
         {<<":scheme">>, <<"https">>},
         {<<":authority">>, <<"example.com">>}],
        [{<<":method">>, <<"POST">>},
         {<<":path">>, <<"/api/data">>},
         {<<":scheme">>, <<"https">>},
         {<<":authority">>, <<"example.com">>},
         {<<"content-type">>, <<"application/json">>}],
        [{<<":method">>, <<"GET">>},
         {<<":path">>, <<"/">>},
         {<<":scheme">>, <<"https">>},
         {<<":authority">>, <<"example.com">>}]
    ],
    lists:foldl(fun(Headers, {ES, DS}) ->
        {Encoded, ES2} = hackney_hpack:encode(Headers, ES),
        EncodedBin = iolist_to_binary(Encoded),
        {Decoded, DS2} = hackney_hpack:decode(EncodedBin, DS),
        ?assertEqual(Headers, Decoded),
        {ES2, DS2}
    end, {EncState0, DecState0}, Requests).

dynamic_table_index_starts_at_62() ->
    Headers = [{<<"x-custom">>, <<"value">>}],
    {Encoded1, EncState1} = hackney_hpack:encode(Headers),
    Encoded1Bin = iolist_to_binary(Encoded1),
    {_, DecState1} = hackney_hpack:decode(Encoded1Bin),

    %% Second encode should produce an indexed header field representation
    %% The index must be >= 62 (STATIC_TABLE_SIZE + 1)
    {Encoded2, _} = hackney_hpack:encode(Headers, EncState1),
    Encoded2Bin = iolist_to_binary(Encoded2),

    %% Indexed header field starts with 1-bit prefix: 1xxxxxxx
    %% Extract the index from the 7-bit prefix integer
    <<1:1, IndexBits:7, _/binary>> = Encoded2Bin,
    ?assert(IndexBits >= ?STATIC_TABLE_SIZE + 1),

    %% Verify it decodes correctly
    {Decoded2, _} = hackney_hpack:decode(Encoded2Bin, DecState1),
    ?assertEqual(Headers, Decoded2).
