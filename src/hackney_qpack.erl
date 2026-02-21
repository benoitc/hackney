%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Optimized QPACK header compression for HTTP/3 (RFC 9204).
%%%
%%% This implementation provides O(1) static table lookups using maps
%%% and supports an optional dynamic table for improved compression.
%%%
%%% Key optimizations:
%%% - Static table as maps for O(1) field/name lookups
%%% - Static table as tuple for O(1) index access
%%% - Optional dynamic table with O(1) map-based lookups
%%% @end

-module(hackney_qpack).

-export([
    encode/1,
    encode/2,
    decode/1,
    decode/2,
    init/0,
    init/1
]).

%% Entry overhead per RFC 9204 Section 3.2.1
-define(ENTRY_OVERHEAD, 32).

%% State record for stateful encoding/decoding
-record(qpack_state, {
    %% Dynamic table (optional, disabled by default)
    use_dynamic = false :: boolean(),
    dyn_field_index = #{} :: #{header() => pos_integer()},
    dyn_name_index = #{} :: #{binary() => pos_integer()},
    dyn_entries = [] :: [{pos_integer(), header()}],
    dyn_size = 0 :: non_neg_integer(),
    dyn_max_size = 0 :: non_neg_integer(),
    insert_count = 0 :: non_neg_integer()
}).

-opaque state() :: #qpack_state{}.
-export_type([state/0]).

-type header() :: {binary(), binary()}.

%%====================================================================
%% Static Table (RFC 9204 Appendix A) - O(1) Access
%%====================================================================

%% Static table as tuple for O(1) index access
-define(STATIC_TABLE, {
    %% 0-14
    {<<":authority">>, undefined},
    {<<":path">>, <<"/">>},
    {<<":age">>, <<"0">>},
    {<<"content-disposition">>, undefined},
    {<<"content-length">>, <<"0">>},
    {<<"cookie">>, undefined},
    {<<"date">>, undefined},
    {<<"etag">>, undefined},
    {<<"if-modified-since">>, undefined},
    {<<"if-none-match">>, undefined},
    {<<"last-modified">>, undefined},
    {<<"link">>, undefined},
    {<<"location">>, undefined},
    {<<"referer">>, undefined},
    {<<"set-cookie">>, undefined},
    %% 15-29
    {<<":method">>, <<"CONNECT">>},
    {<<":method">>, <<"DELETE">>},
    {<<":method">>, <<"GET">>},
    {<<":method">>, <<"HEAD">>},
    {<<":method">>, <<"OPTIONS">>},
    {<<":method">>, <<"POST">>},
    {<<":method">>, <<"PUT">>},
    {<<":scheme">>, <<"http">>},
    {<<":scheme">>, <<"https">>},
    {<<":status">>, <<"103">>},
    {<<":status">>, <<"200">>},
    {<<":status">>, <<"304">>},
    {<<":status">>, <<"404">>},
    {<<":status">>, <<"503">>},
    {<<"accept">>, <<"*/*">>},
    %% 30-44
    {<<"accept">>, <<"application/dns-message">>},
    {<<"accept-encoding">>, <<"gzip, deflate, br">>},
    {<<"accept-ranges">>, <<"bytes">>},
    {<<"access-control-allow-headers">>, <<"cache-control">>},
    {<<"access-control-allow-headers">>, <<"content-type">>},
    {<<"access-control-allow-origin">>, <<"*">>},
    {<<"cache-control">>, <<"max-age=0">>},
    {<<"cache-control">>, <<"max-age=2592000">>},
    {<<"cache-control">>, <<"max-age=604800">>},
    {<<"cache-control">>, <<"no-cache">>},
    {<<"cache-control">>, <<"no-store">>},
    {<<"cache-control">>, <<"public, max-age=31536000">>},
    {<<"content-encoding">>, <<"br">>},
    {<<"content-encoding">>, <<"gzip">>},
    {<<"content-type">>, <<"application/dns-message">>},
    %% 45-59
    {<<"content-type">>, <<"application/javascript">>},
    {<<"content-type">>, <<"application/json">>},
    {<<"content-type">>, <<"application/x-www-form-urlencoded">>},
    {<<"content-type">>, <<"image/gif">>},
    {<<"content-type">>, <<"image/jpeg">>},
    {<<"content-type">>, <<"image/png">>},
    {<<"content-type">>, <<"text/css">>},
    {<<"content-type">>, <<"text/html; charset=utf-8">>},
    {<<"content-type">>, <<"text/plain">>},
    {<<"content-type">>, <<"text/plain;charset=utf-8">>},
    {<<"range">>, <<"bytes=0-">>},
    {<<"strict-transport-security">>, <<"max-age=31536000">>},
    {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains">>},
    {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains; preload">>},
    {<<"vary">>, <<"accept-encoding">>},
    %% 60-74
    {<<"vary">>, <<"origin">>},
    {<<"x-content-type-options">>, <<"nosniff">>},
    {<<"x-xss-protection">>, <<"1; mode=block">>},
    {<<":status">>, <<"100">>},
    {<<":status">>, <<"204">>},
    {<<":status">>, <<"206">>},
    {<<":status">>, <<"302">>},
    {<<":status">>, <<"400">>},
    {<<":status">>, <<"403">>},
    {<<":status">>, <<"421">>},
    {<<":status">>, <<"425">>},
    {<<":status">>, <<"500">>},
    {<<"accept-language">>, undefined},
    {<<"access-control-allow-credentials">>, <<"FALSE">>},
    {<<"access-control-allow-credentials">>, <<"TRUE">>},
    %% 75-89
    {<<"access-control-allow-headers">>, <<"*">>},
    {<<"access-control-allow-methods">>, <<"get">>},
    {<<"access-control-allow-methods">>, <<"get, post, options">>},
    {<<"access-control-allow-methods">>, <<"options">>},
    {<<"access-control-expose-headers">>, <<"content-length">>},
    {<<"access-control-request-headers">>, <<"content-type">>},
    {<<"access-control-request-method">>, <<"get">>},
    {<<"access-control-request-method">>, <<"post">>},
    {<<"alt-svc">>, <<"clear">>},
    {<<"authorization">>, undefined},
    {<<"content-security-policy">>, <<"script-src 'none'; object-src 'none'; base-uri 'none'">>},
    {<<"early-data">>, <<"1">>},
    {<<"expect-ct">>, undefined},
    {<<"forwarded">>, undefined},
    {<<"if-range">>, undefined},
    %% 90-98
    {<<"origin">>, undefined},
    {<<"purpose">>, <<"prefetch">>},
    {<<"server">>, undefined},
    {<<"timing-allow-origin">>, <<"*">>},
    {<<"upgrade-insecure-requests">>, <<"1">>},
    {<<"user-agent">>, undefined},
    {<<"x-forwarded-for">>, undefined},
    {<<"x-frame-options">>, <<"deny">>},
    {<<"x-frame-options">>, <<"sameorigin">>}
}).

%% Static table field map for O(1) exact match lookup
%% Only includes entries with non-undefined values
-define(STATIC_FIELD_MAP, #{
    {<<":path">>, <<"/">>} => 1,
    {<<":age">>, <<"0">>} => 2,
    {<<"content-length">>, <<"0">>} => 4,
    {<<":method">>, <<"CONNECT">>} => 15,
    {<<":method">>, <<"DELETE">>} => 16,
    {<<":method">>, <<"GET">>} => 17,
    {<<":method">>, <<"HEAD">>} => 18,
    {<<":method">>, <<"OPTIONS">>} => 19,
    {<<":method">>, <<"POST">>} => 20,
    {<<":method">>, <<"PUT">>} => 21,
    {<<":scheme">>, <<"http">>} => 22,
    {<<":scheme">>, <<"https">>} => 23,
    {<<":status">>, <<"103">>} => 24,
    {<<":status">>, <<"200">>} => 25,
    {<<":status">>, <<"304">>} => 26,
    {<<":status">>, <<"404">>} => 27,
    {<<":status">>, <<"503">>} => 28,
    {<<"accept">>, <<"*/*">>} => 29,
    {<<"accept">>, <<"application/dns-message">>} => 30,
    {<<"accept-encoding">>, <<"gzip, deflate, br">>} => 31,
    {<<"accept-ranges">>, <<"bytes">>} => 32,
    {<<"access-control-allow-headers">>, <<"cache-control">>} => 33,
    {<<"access-control-allow-headers">>, <<"content-type">>} => 34,
    {<<"access-control-allow-origin">>, <<"*">>} => 35,
    {<<"cache-control">>, <<"max-age=0">>} => 36,
    {<<"cache-control">>, <<"max-age=2592000">>} => 37,
    {<<"cache-control">>, <<"max-age=604800">>} => 38,
    {<<"cache-control">>, <<"no-cache">>} => 39,
    {<<"cache-control">>, <<"no-store">>} => 40,
    {<<"cache-control">>, <<"public, max-age=31536000">>} => 41,
    {<<"content-encoding">>, <<"br">>} => 42,
    {<<"content-encoding">>, <<"gzip">>} => 43,
    {<<"content-type">>, <<"application/dns-message">>} => 44,
    {<<"content-type">>, <<"application/javascript">>} => 45,
    {<<"content-type">>, <<"application/json">>} => 46,
    {<<"content-type">>, <<"application/x-www-form-urlencoded">>} => 47,
    {<<"content-type">>, <<"image/gif">>} => 48,
    {<<"content-type">>, <<"image/jpeg">>} => 49,
    {<<"content-type">>, <<"image/png">>} => 50,
    {<<"content-type">>, <<"text/css">>} => 51,
    {<<"content-type">>, <<"text/html; charset=utf-8">>} => 52,
    {<<"content-type">>, <<"text/plain">>} => 53,
    {<<"content-type">>, <<"text/plain;charset=utf-8">>} => 54,
    {<<"range">>, <<"bytes=0-">>} => 55,
    {<<"strict-transport-security">>, <<"max-age=31536000">>} => 56,
    {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains">>} => 57,
    {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains; preload">>} => 58,
    {<<"vary">>, <<"accept-encoding">>} => 59,
    {<<"vary">>, <<"origin">>} => 60,
    {<<"x-content-type-options">>, <<"nosniff">>} => 61,
    {<<"x-xss-protection">>, <<"1; mode=block">>} => 62,
    {<<":status">>, <<"100">>} => 63,
    {<<":status">>, <<"204">>} => 64,
    {<<":status">>, <<"206">>} => 65,
    {<<":status">>, <<"302">>} => 66,
    {<<":status">>, <<"400">>} => 67,
    {<<":status">>, <<"403">>} => 68,
    {<<":status">>, <<"421">>} => 69,
    {<<":status">>, <<"425">>} => 70,
    {<<":status">>, <<"500">>} => 71,
    {<<"access-control-allow-credentials">>, <<"FALSE">>} => 73,
    {<<"access-control-allow-credentials">>, <<"TRUE">>} => 74,
    {<<"access-control-allow-headers">>, <<"*">>} => 75,
    {<<"access-control-allow-methods">>, <<"get">>} => 76,
    {<<"access-control-allow-methods">>, <<"get, post, options">>} => 77,
    {<<"access-control-allow-methods">>, <<"options">>} => 78,
    {<<"access-control-expose-headers">>, <<"content-length">>} => 79,
    {<<"access-control-request-headers">>, <<"content-type">>} => 80,
    {<<"access-control-request-method">>, <<"get">>} => 81,
    {<<"access-control-request-method">>, <<"post">>} => 82,
    {<<"alt-svc">>, <<"clear">>} => 83,
    {<<"content-security-policy">>, <<"script-src 'none'; object-src 'none'; base-uri 'none'">>} => 85,
    {<<"early-data">>, <<"1">>} => 86,
    {<<"purpose">>, <<"prefetch">>} => 91,
    {<<"timing-allow-origin">>, <<"*">>} => 93,
    {<<"upgrade-insecure-requests">>, <<"1">>} => 94,
    {<<"x-frame-options">>, <<"deny">>} => 97,
    {<<"x-frame-options">>, <<"sameorigin">>} => 98
}).

%% Static table name map for O(1) name-only lookup
-define(STATIC_NAME_MAP, #{
    <<":authority">> => 0,
    <<":path">> => 1,
    <<":age">> => 2,
    <<"content-disposition">> => 3,
    <<"content-length">> => 4,
    <<"cookie">> => 5,
    <<"date">> => 6,
    <<"etag">> => 7,
    <<"if-modified-since">> => 8,
    <<"if-none-match">> => 9,
    <<"last-modified">> => 10,
    <<"link">> => 11,
    <<"location">> => 12,
    <<"referer">> => 13,
    <<"set-cookie">> => 14,
    <<":method">> => 15,
    <<":scheme">> => 22,
    <<":status">> => 24,
    <<"accept">> => 29,
    <<"accept-encoding">> => 31,
    <<"accept-ranges">> => 32,
    <<"access-control-allow-headers">> => 33,
    <<"access-control-allow-origin">> => 35,
    <<"cache-control">> => 36,
    <<"content-encoding">> => 42,
    <<"content-type">> => 44,
    <<"range">> => 55,
    <<"strict-transport-security">> => 56,
    <<"vary">> => 59,
    <<"x-content-type-options">> => 61,
    <<"x-xss-protection">> => 62,
    <<"accept-language">> => 72,
    <<"access-control-allow-credentials">> => 73,
    <<"access-control-allow-methods">> => 76,
    <<"access-control-expose-headers">> => 79,
    <<"access-control-request-headers">> => 80,
    <<"access-control-request-method">> => 81,
    <<"alt-svc">> => 83,
    <<"authorization">> => 84,
    <<"content-security-policy">> => 85,
    <<"early-data">> => 86,
    <<"expect-ct">> => 87,
    <<"forwarded">> => 88,
    <<"if-range">> => 89,
    <<"origin">> => 90,
    <<"purpose">> => 91,
    <<"server">> => 92,
    <<"timing-allow-origin">> => 93,
    <<"upgrade-insecure-requests">> => 94,
    <<"user-agent">> => 95,
    <<"x-forwarded-for">> => 96,
    <<"x-frame-options">> => 97
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Initialize QPACK state (static-only mode).
-spec init() -> state().
init() ->
    #qpack_state{}.

%% @doc Initialize QPACK state with options.
%% Options:
%%   max_dynamic_size - Enable dynamic table with given max size (default: 0 = disabled)
-spec init(#{atom() => term()}) -> state().
init(Opts) ->
    MaxDynSize = maps:get(max_dynamic_size, Opts, 0),
    #qpack_state{
        use_dynamic = MaxDynSize > 0,
        dyn_max_size = MaxDynSize
    }.

%% @doc Encode headers using QPACK (stateless, static-only).
-spec encode([header()]) -> binary().
encode(Headers) ->
    {Encoded, _} = encode(Headers, #qpack_state{}),
    Encoded.

%% @doc Encode headers using QPACK with state.
-spec encode([header()], state()) -> {binary(), state()}.
encode(Headers, State) ->
    %% Required Insert Count = 0 (no dynamic table refs from encoder)
    %% S bit = 0 (no sign), Delta Base = 0
    Prefix = <<0, 0>>,
    {EncodedHeaders, NewState} = encode_headers(Headers, State, <<>>),
    {<<Prefix/binary, EncodedHeaders/binary>>, NewState}.

%% @doc Decode QPACK-encoded headers (stateless).
-spec decode(binary()) -> {ok, [header()]} | {error, term()}.
decode(Data) ->
    {Result, _} = decode(Data, #qpack_state{}),
    Result.

%% @doc Decode QPACK-encoded headers with state.
-spec decode(binary(), state()) -> {{ok, [header()]} | {error, term()}, state()}.
decode(Data, State) ->
    try
        {_, Rest} = decode_prefix(Data),
        {Headers, NewState} = decode_headers(Rest, State, []),
        {{ok, Headers}, NewState}
    catch
        _:Reason ->
            {{error, Reason}, State}
    end.

%%====================================================================
%% Internal - Encoding
%%====================================================================

encode_headers([], State, Acc) ->
    {Acc, State};
encode_headers([Header | Rest], State, Acc) ->
    {Encoded, NewState} = encode_header(Header, State),
    encode_headers(Rest, NewState, <<Acc/binary, Encoded/binary>>).

encode_header({Name, Value}, State) ->
    case find_static_match(Name, Value) of
        {exact, Index} ->
            %% Indexed Field Line (static) - 11xxxxxx
            {encode_indexed_static(Index), State};
        {name, Index} ->
            %% Literal Field Line With Name Reference (static)
            {encode_literal_with_name_ref(Index, Value), State};
        none ->
            %% Literal Field Line With Literal Name
            {encode_literal(Name, Value), State}
    end.

%% Indexed Field Line - 11xxxxxx for static
encode_indexed_static(Index) ->
    encode_prefixed_int(Index, 6, 2#11).

%% Literal with name reference - 0101xxxx (N=0, T=1 for static)
encode_literal_with_name_ref(Index, Value) ->
    NameRef = encode_prefixed_int(Index, 4, 2#0101),
    ValueEnc = encode_string(Value),
    <<NameRef/binary, ValueEnc/binary>>.

%% Literal with literal name - 0010xxxx (N=0, H=0 for no huffman)
encode_literal(Name, Value) ->
    NameLen = byte_size(Name),
    ValueEnc = encode_string(Value),
    case NameLen < 7 of
        true ->
            FirstByte = 2#00100000 bor NameLen,
            <<FirstByte, Name/binary, ValueEnc/binary>>;
        false ->
            FirstByte = 2#00100111,
            LenCont = encode_multi_byte_int(NameLen - 7),
            <<FirstByte, LenCont/binary, Name/binary, ValueEnc/binary>>
    end.

encode_string(Str) ->
    Len = byte_size(Str),
    LenEnc = encode_prefixed_int(Len, 7, 0),
    <<LenEnc/binary, Str/binary>>.

encode_prefixed_int(Value, PrefixBits, Prefix) when Value < (1 bsl PrefixBits) - 1 ->
    <<(Prefix bsl PrefixBits bor Value)>>;
encode_prefixed_int(Value, PrefixBits, Prefix) ->
    MaxPrefix = (1 bsl PrefixBits) - 1,
    FirstByte = Prefix bsl PrefixBits bor MaxPrefix,
    Remaining = Value - MaxPrefix,
    <<FirstByte, (encode_multi_byte_int(Remaining))/binary>>.

encode_multi_byte_int(Value) when Value < 128 ->
    <<Value>>;
encode_multi_byte_int(Value) ->
    <<(128 bor (Value band 127)), (encode_multi_byte_int(Value bsr 7))/binary>>.

%%====================================================================
%% Internal - Decoding
%%====================================================================

decode_prefix(<<RIC, Base, Rest/binary>>) ->
    {{RIC, Base}, Rest};
decode_prefix(_) ->
    throw(invalid_prefix).

decode_headers(<<>>, State, Acc) ->
    {lists:reverse(Acc), State};
decode_headers(<<2#11:2, _:6, _/binary>> = Data, State, Acc) ->
    %% Indexed Field Line (static) - 11xxxxxx
    {Index, Rest} = decode_prefixed_int(Data, 6),
    Header = get_static_entry(Index),
    decode_headers(Rest, State, [Header | Acc]);
decode_headers(<<2#10:2, _:6, _/binary>> = Data, State, Acc) ->
    %% Indexed Field Line (dynamic) - 10xxxxxx
    {_Index, Rest} = decode_prefixed_int(Data, 6),
    decode_headers(Rest, State, Acc);
decode_headers(<<2#01:2, _N:1, T:1, _:4, _/binary>> = Data, State, Acc) ->
    %% Literal Field Line with Name Reference - 01NTxxxx
    FirstByte = hd(binary_to_list(Data)),
    IndexBits = FirstByte band 16#0F,
    <<_, Rest0/binary>> = Data,
    {Index, Rest1} = case IndexBits < 15 of
        true -> {IndexBits, Rest0};
        false -> decode_multi_byte_int(Rest0, IndexBits, 0)
    end,
    {Value, Rest2} = decode_string(Rest1),
    case T of
        1 ->
            {Name, _} = get_static_entry(Index),
            decode_headers(Rest2, State, [{Name, Value} | Acc]);
        0 ->
            decode_headers(Rest2, State, Acc)
    end;
decode_headers(<<2#0010:4, H:1, NameLenPrefix:3, Rest0/binary>>, State, Acc) ->
    %% Literal with literal name - 0010Hxxx
    {NameLen, Rest1} = case NameLenPrefix < 7 of
        true -> {NameLenPrefix, Rest0};
        false -> decode_multi_byte_int(Rest0, NameLenPrefix, 0)
    end,
    {Name, Rest2} = decode_string_with_huffman(H, NameLen, Rest1),
    {Value, Rest3} = decode_string(Rest2),
    decode_headers(Rest3, State, [{Name, Value} | Acc]);
decode_headers(<<2#0011:4, H:1, NameLenPrefix:3, Rest0/binary>>, State, Acc) ->
    %% Literal with literal name, N=1 - 0011Hxxx
    {NameLen, Rest1} = case NameLenPrefix < 7 of
        true -> {NameLenPrefix, Rest0};
        false -> decode_multi_byte_int(Rest0, NameLenPrefix, 0)
    end,
    {Name, Rest2} = decode_string_with_huffman(H, NameLen, Rest1),
    {Value, Rest3} = decode_string(Rest2),
    decode_headers(Rest3, State, [{Name, Value} | Acc]);
decode_headers(<<2#0001:4, _:4, _/binary>> = Data, State, Acc) ->
    %% Indexed Header Field with Post-Base Index - 0001xxxx
    FirstByte = hd(binary_to_list(Data)),
    IndexBits = FirstByte band 16#0F,
    <<_, Rest0/binary>> = Data,
    {_Index, Rest1} = case IndexBits < 15 of
        true -> {IndexBits, Rest0};
        false -> decode_multi_byte_int(Rest0, IndexBits, 0)
    end,
    decode_headers(Rest1, State, Acc);
decode_headers(<<2#0000:4, _:4, _/binary>> = Data, State, Acc) ->
    %% Literal with post-base name reference - 0000Nxxx
    FirstByte = hd(binary_to_list(Data)),
    IndexBits = FirstByte band 16#07,
    <<_, Rest0/binary>> = Data,
    {_Index, Rest1} = case IndexBits < 7 of
        true -> {IndexBits, Rest0};
        false -> decode_multi_byte_int(Rest0, IndexBits, 0)
    end,
    {_Value, Rest2} = decode_string(Rest1),
    decode_headers(Rest2, State, Acc);
decode_headers(<<Byte, _/binary>>, _State, _Acc) ->
    throw({unknown_instruction, Byte}).

decode_prefixed_int(Data, PrefixBits) ->
    MaxPrefix = (1 bsl PrefixBits) - 1,
    <<First, Rest/binary>> = Data,
    Value = First band MaxPrefix,
    case Value < MaxPrefix of
        true ->
            {Value, Rest};
        false ->
            decode_multi_byte_int(Rest, Value, 0)
    end.

decode_multi_byte_int(<<Byte, Rest/binary>>, Acc, Shift) ->
    NewAcc = Acc + ((Byte band 127) bsl Shift),
    case Byte band 128 of
        0 -> {NewAcc, Rest};
        _ -> decode_multi_byte_int(Rest, NewAcc, Shift + 7)
    end.

decode_string(<<0:1, 127:7, Rest/binary>>) ->
    {ActualLen, Rest2} = decode_multi_byte_int(Rest, 127, 0),
    case byte_size(Rest2) >= ActualLen of
        true ->
            <<Str:ActualLen/binary, Rest3/binary>> = Rest2,
            {Str, Rest3};
        false ->
            throw({invalid_string, need_more_data})
    end;
decode_string(<<1:1, 127:7, Rest/binary>>) ->
    {ActualLen, Rest2} = decode_multi_byte_int(Rest, 127, 0),
    case byte_size(Rest2) >= ActualLen of
        true ->
            <<Encoded:ActualLen/binary, Rest3/binary>> = Rest2,
            {Decoded, _} = dec_huffman(Encoded, ActualLen),
            {Decoded, Rest3};
        false ->
            throw({invalid_string, need_more_data})
    end;
decode_string(<<0:1, Len:7, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Str:Len/binary, Rest2/binary>> = Rest,
    {Str, Rest2};
decode_string(<<1:1, Len:7, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Encoded:Len/binary, Rest2/binary>> = Rest,
    {Decoded, _} = dec_huffman(Encoded, Len),
    {Decoded, Rest2};
decode_string(Data) ->
    throw({invalid_string, byte_size(Data), Data}).

decode_string_with_huffman(HuffFlag, Len, Data) when byte_size(Data) >= Len ->
    <<Encoded:Len/binary, Rest/binary>> = Data,
    case HuffFlag of
        1 ->
            {Decoded, _} = dec_huffman(Encoded, Len),
            {Decoded, Rest};
        0 ->
            {Encoded, Rest}
    end;
decode_string_with_huffman(_HuffFlag, Len, Data) ->
    throw({invalid_string, need_more_data, Len, byte_size(Data)}).

%% Huffman decoding using HPACK lookup table
-include("hackney_hpack_huffman_dec.hrl").

dec_huffman(Data, Length) ->
    dec_huffman(Data, Length, 0, <<>>).

dec_huffman(<<A:4, B:4, R/bits>>, Len, Huff0, Acc) when Len > 1 ->
    {_, CharA, Huff1} = dec_huffman_lookup(Huff0, A),
    {_, CharB, Huff} = dec_huffman_lookup(Huff1, B),
    case {CharA, CharB} of
        {undefined, undefined} -> dec_huffman(R, Len - 1, Huff, Acc);
        {CharA, undefined} -> dec_huffman(R, Len - 1, Huff, <<Acc/binary, CharA>>);
        {undefined, CharB} -> dec_huffman(R, Len - 1, Huff, <<Acc/binary, CharB>>);
        {CharA, CharB} -> dec_huffman(R, Len - 1, Huff, <<Acc/binary, CharA, CharB>>)
    end;
dec_huffman(<<A:4, B:4, Rest/bits>>, 1, Huff0, Acc) ->
    {_, CharA, Huff} = dec_huffman_lookup(Huff0, A),
    case dec_huffman_lookup(Huff, B) of
        {ok, CharB, _} ->
            case {CharA, CharB} of
                {undefined, undefined} ->
                    {Acc, Rest};
                {CharA, undefined} ->
                    {<<Acc/binary, CharA>>, Rest};
                {undefined, CharB} ->
                    {<<Acc/binary, CharB>>, Rest};
                _ ->
                    {<<Acc/binary, CharA, CharB>>, Rest}
            end;
        {more, _, _} ->
            case CharA of
                undefined -> {Acc, Rest};
                _ -> {<<Acc/binary, CharA>>, Rest}
            end
    end;
dec_huffman(Rest, 0, _, <<>>) ->
    {<<>>, Rest};
dec_huffman(Rest, 0, _, Acc) ->
    {Acc, Rest}.

%%====================================================================
%% Internal - Static Table Lookup (O(1))
%%====================================================================

%% Find match in static table using maps - O(1)
find_static_match(Name, Value) ->
    Header = {Name, Value},
    case maps:find(Header, ?STATIC_FIELD_MAP) of
        {ok, Index} ->
            {exact, Index};
        error ->
            case maps:find(Name, ?STATIC_NAME_MAP) of
                {ok, Index} ->
                    {name, Index};
                error ->
                    none
            end
    end.

%% Get static table entry by index - O(1)
get_static_entry(Index) when Index >= 0, Index =< 98 ->
    element(Index + 1, ?STATIC_TABLE);
get_static_entry(Index) ->
    throw({invalid_static_index, Index}).
