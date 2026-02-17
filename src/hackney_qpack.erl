%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc QPACK header compression for HTTP/3 (RFC 9204).
%%%
%%% This is a simplified implementation using only the static table.
%%% Per RFC 9204, operating without the dynamic table is valid and
%%% results in slightly larger headers but simpler implementation.
%%%
%%% @end

-module(hackney_qpack).

-export([
    encode/1,
    decode/1
]).

%% Static table from RFC 9204 Appendix A
%% Index -> {Name, Value} or {Name, undefined}
-define(STATIC_TABLE, [
    % 0
    {<<":authority">>, undefined},
    % 1
    {<<":path">>, <<"/">>},
    % 2
    {<<":age">>, <<"0">>},
    % 3
    {<<"content-disposition">>, undefined},
    % 4
    {<<"content-length">>, <<"0">>},
    % 5
    {<<"cookie">>, undefined},
    % 6
    {<<"date">>, undefined},
    % 7
    {<<"etag">>, undefined},
    % 8
    {<<"if-modified-since">>, undefined},
    % 9
    {<<"if-none-match">>, undefined},
    % 10
    {<<"last-modified">>, undefined},
    % 11
    {<<"link">>, undefined},
    % 12
    {<<"location">>, undefined},
    % 13
    {<<"referer">>, undefined},
    % 14
    {<<"set-cookie">>, undefined},
    % 15
    {<<":method">>, <<"CONNECT">>},
    % 16
    {<<":method">>, <<"DELETE">>},
    % 17
    {<<":method">>, <<"GET">>},
    % 18
    {<<":method">>, <<"HEAD">>},
    % 19
    {<<":method">>, <<"OPTIONS">>},
    % 20
    {<<":method">>, <<"POST">>},
    % 21
    {<<":method">>, <<"PUT">>},
    % 22
    {<<":scheme">>, <<"http">>},
    % 23
    {<<":scheme">>, <<"https">>},
    % 24
    {<<":status">>, <<"103">>},
    % 25
    {<<":status">>, <<"200">>},
    % 26
    {<<":status">>, <<"304">>},
    % 27
    {<<":status">>, <<"404">>},
    % 28
    {<<":status">>, <<"503">>},
    % 29
    {<<"accept">>, <<"*/*">>},
    % 30
    {<<"accept">>, <<"application/dns-message">>},
    % 31
    {<<"accept-encoding">>, <<"gzip, deflate, br">>},
    % 32
    {<<"accept-ranges">>, <<"bytes">>},
    % 33
    {<<"access-control-allow-headers">>, <<"cache-control">>},
    % 34
    {<<"access-control-allow-headers">>, <<"content-type">>},
    % 35
    {<<"access-control-allow-origin">>, <<"*">>},
    % 36
    {<<"cache-control">>, <<"max-age=0">>},
    % 37
    {<<"cache-control">>, <<"max-age=2592000">>},
    % 38
    {<<"cache-control">>, <<"max-age=604800">>},
    % 39
    {<<"cache-control">>, <<"no-cache">>},
    % 40
    {<<"cache-control">>, <<"no-store">>},
    % 41
    {<<"cache-control">>, <<"public, max-age=31536000">>},
    % 42
    {<<"content-encoding">>, <<"br">>},
    % 43
    {<<"content-encoding">>, <<"gzip">>},
    % 44
    {<<"content-type">>, <<"application/dns-message">>},
    % 45
    {<<"content-type">>, <<"application/javascript">>},
    % 46
    {<<"content-type">>, <<"application/json">>},
    % 47
    {<<"content-type">>, <<"application/x-www-form-urlencoded">>},
    % 48
    {<<"content-type">>, <<"image/gif">>},
    % 49
    {<<"content-type">>, <<"image/jpeg">>},
    % 50
    {<<"content-type">>, <<"image/png">>},
    % 51
    {<<"content-type">>, <<"text/css">>},
    % 52
    {<<"content-type">>, <<"text/html; charset=utf-8">>},
    % 53
    {<<"content-type">>, <<"text/plain">>},
    % 54
    {<<"content-type">>, <<"text/plain;charset=utf-8">>},
    % 55
    {<<"range">>, <<"bytes=0-">>},
    % 56
    {<<"strict-transport-security">>, <<"max-age=31536000">>},
    % 57
    {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains">>},
    % 58
    {<<"strict-transport-security">>, <<"max-age=31536000; includesubdomains; preload">>},
    % 59
    {<<"vary">>, <<"accept-encoding">>},
    % 60
    {<<"vary">>, <<"origin">>},
    % 61
    {<<"x-content-type-options">>, <<"nosniff">>},
    % 62
    {<<"x-xss-protection">>, <<"1; mode=block">>},
    % 63
    {<<":status">>, <<"100">>},
    % 64
    {<<":status">>, <<"204">>},
    % 65
    {<<":status">>, <<"206">>},
    % 66
    {<<":status">>, <<"302">>},
    % 67
    {<<":status">>, <<"400">>},
    % 68
    {<<":status">>, <<"403">>},
    % 69
    {<<":status">>, <<"421">>},
    % 70
    {<<":status">>, <<"425">>},
    % 71
    {<<":status">>, <<"500">>},
    % 72
    {<<"accept-language">>, undefined},
    % 73
    {<<"access-control-allow-credentials">>, <<"FALSE">>},
    % 74
    {<<"access-control-allow-credentials">>, <<"TRUE">>},
    % 75
    {<<"access-control-allow-headers">>, <<"*">>},
    % 76
    {<<"access-control-allow-methods">>, <<"get">>},
    % 77
    {<<"access-control-allow-methods">>, <<"get, post, options">>},
    % 78
    {<<"access-control-allow-methods">>, <<"options">>},
    % 79
    {<<"access-control-expose-headers">>, <<"content-length">>},
    % 80
    {<<"access-control-request-headers">>, <<"content-type">>},
    % 81
    {<<"access-control-request-method">>, <<"get">>},
    % 82
    {<<"access-control-request-method">>, <<"post">>},
    % 83
    {<<"alt-svc">>, <<"clear">>},
    % 84
    {<<"authorization">>, undefined},
    % 85
    {<<"content-security-policy">>, <<"script-src 'none'; object-src 'none'; base-uri 'none'">>},
    % 86
    {<<"early-data">>, <<"1">>},
    % 87
    {<<"expect-ct">>, undefined},
    % 88
    {<<"forwarded">>, undefined},
    % 89
    {<<"if-range">>, undefined},
    % 90
    {<<"origin">>, undefined},
    % 91
    {<<"purpose">>, <<"prefetch">>},
    % 92
    {<<"server">>, undefined},
    % 93
    {<<"timing-allow-origin">>, <<"*">>},
    % 94
    {<<"upgrade-insecure-requests">>, <<"1">>},
    % 95
    {<<"user-agent">>, undefined},
    % 96
    {<<"x-forwarded-for">>, undefined},
    % 97
    {<<"x-frame-options">>, <<"deny">>},
    % 98
    {<<"x-frame-options">>, <<"sameorigin">>}
    % 99
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Encode headers using QPACK.
%% Returns encoded field section (without Required Insert Count or S bit prefix).
-spec encode([{binary(), binary()}]) -> binary().
encode(Headers) ->
    %% Required Insert Count = 0 (no dynamic table)
    %% S bit = 0 (no sign)
    %% Base = 0
    Prefix = <<0, 0>>,
    EncodedHeaders = lists:foldl(fun(Header, Acc) ->
        <<Acc/binary, (encode_header(Header))/binary>>
    end, <<>>, Headers),
    <<Prefix/binary, EncodedHeaders/binary>>.

%% @doc Decode QPACK-encoded headers.
%% Returns list of {Name, Value} tuples.
-spec decode(binary()) -> {ok, [{binary(), binary()}]} | {error, term()}.
decode(Data) ->
    try
        %% Skip prefix (Required Insert Count + Base)
        {_, Rest} = decode_prefix(Data),
        {ok, decode_headers(Rest, [])}
    catch
        _:Reason ->
            {error, Reason}
    end.

%%====================================================================
%% Internal - Encoding
%%====================================================================

encode_header({Name, Value}) ->
    case find_static_match(Name, Value) of
        {exact, Index} ->
            %% Indexed Field Line (static) - 1xxxxxxx
            encode_indexed_static(Index);
        {name, Index} ->
            %% Literal Field Line With Name Reference (static) - 01xxxxxx
            encode_literal_with_name_ref(Index, Value);
        none ->
            %% Literal Field Line With Literal Name - 001xxxxx
            encode_literal(Name, Value)
    end.

%% Indexed Field Line - 1T (T=1 for static)
encode_indexed_static(Index) ->
    %% 11xxxxxx for static table reference
    encode_prefixed_int(Index, 6, 2#11).

%% Literal with name reference - 01NT (N=0 never index, T=1 static)
encode_literal_with_name_ref(Index, Value) ->
    %% 0101xxxx for static name reference, never index
    NameRef = encode_prefixed_int(Index, 4, 2#0101),
    ValueEnc = encode_string(Value),
    <<NameRef/binary, ValueEnc/binary>>.

%% Literal with literal name - 001N (N=0 never index)
encode_literal(Name, Value) ->
    %% 0010xxxx for literal name, never index
    NameEnc = encode_string(Name),
    ValueEnc = encode_string(Value),
    <<2#00100000, NameEnc/binary, ValueEnc/binary>>.

%% Encode string (without Huffman for simplicity)
encode_string(Str) ->
    Len = byte_size(Str),
    %% H=0 (no Huffman)
    LenEnc = encode_prefixed_int(Len, 7, 0),
    <<LenEnc/binary, Str/binary>>.

%% Encode integer with prefix
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
    %% Simple case: RIC and Base fit in one byte each
    %% For dynamic table, would need to decode varint
    {{RIC, Base}, Rest};
decode_prefix(_) ->
    throw(invalid_prefix).

decode_headers(<<>>, Acc) ->
    lists:reverse(Acc);
decode_headers(<<2#11:2, _:6, _/binary>> = Data, Acc) ->
    %% Indexed Field Line (static) - 11xxxxxx
    {Index, Rest} = decode_prefixed_int(Data, 6),
    Header = get_static_entry(Index),
    decode_headers(Rest, [Header | Acc]);
decode_headers(<<2#10:2, _:6, _/binary>> = Data, Acc) ->
    %% Indexed Field Line (dynamic) - 10xxxxxx
    %% Not supported without dynamic table
    {_Index, Rest} = decode_prefixed_int(Data, 6),
    decode_headers(Rest, Acc);
decode_headers(<<2#01:2, _N:1, T:1, _:4, _/binary>> = Data, Acc) ->
    %% Literal Field Line with Name Reference - 01NTxxxx
    %% N = never index, T = 1 for static, 0 for dynamic
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
            %% Static table reference
            {Name, _} = get_static_entry(Index),
            decode_headers(Rest2, [{Name, Value} | Acc]);
        0 ->
            %% Dynamic table reference - skip (not supported)
            decode_headers(Rest2, Acc)
    end;
decode_headers(<<2#0010:4, H:1, NameLenPrefix:3, Rest0/binary>>, Acc) ->
    %% Literal with literal name - 0010Hxxx
    %% H = Huffman flag for name, xxx = 3-bit name length prefix
    {NameLen, Rest1} = case NameLenPrefix < 7 of
        true -> {NameLenPrefix, Rest0};
        false -> decode_multi_byte_int(Rest0, NameLenPrefix, 0)
    end,
    {Name, Rest2} = decode_string_with_huffman(H, NameLen, Rest1),
    {Value, Rest3} = decode_string(Rest2),
    decode_headers(Rest3, [{Name, Value} | Acc]);
decode_headers(<<2#0011:4, H:1, NameLenPrefix:3, Rest0/binary>>, Acc) ->
    %% Literal with literal name, N=1 - 0011Hxxx
    {NameLen, Rest1} = case NameLenPrefix < 7 of
        true -> {NameLenPrefix, Rest0};
        false -> decode_multi_byte_int(Rest0, NameLenPrefix, 0)
    end,
    {Name, Rest2} = decode_string_with_huffman(H, NameLen, Rest1),
    {Value, Rest3} = decode_string(Rest2),
    decode_headers(Rest3, [{Name, Value} | Acc]);
decode_headers(<<2#0001:4, _:4, _/binary>> = Data, Acc) ->
    %% Indexed Header Field with Post-Base Index - 0001xxxx
    %% Dynamic table reference - skip it (we don't support dynamic table)
    FirstByte = hd(binary_to_list(Data)),
    IndexBits = FirstByte band 16#0F,  %% 4-bit index
    <<_, Rest0/binary>> = Data,
    {_Index, Rest1} = case IndexBits < 15 of
        true -> {IndexBits, Rest0};
        false -> decode_multi_byte_int(Rest0, IndexBits, 0)
    end,
    decode_headers(Rest1, Acc);
decode_headers(<<2#0000:4, _:4, _/binary>> = Data, Acc) ->
    %% Literal with post-base name reference - 0000Nxxx
    %% Dynamic table reference - skip it (we don't support dynamic table)
    FirstByte = hd(binary_to_list(Data)),
    IndexBits = FirstByte band 16#07,  %% 3-bit index (after N bit)
    <<_, Rest0/binary>> = Data,
    {Index, Rest1} = case IndexBits < 7 of
        true -> {IndexBits, Rest0};
        false -> decode_multi_byte_int(Rest0, IndexBits, 0)
    end,
    _ = Index,  %% suppress unused variable warning
    {_Value, Rest2} = decode_string(Rest1),
    decode_headers(Rest2, Acc);
decode_headers(<<Byte, _/binary>>, _Acc) ->
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
    %% Length needs multi-byte encoding (non-huffman)
    {ActualLen, Rest2} = decode_multi_byte_int(Rest, 127, 0),
    case byte_size(Rest2) >= ActualLen of
        true ->
            <<Str:ActualLen/binary, Rest3/binary>> = Rest2,
            {Str, Rest3};
        false ->
            throw({invalid_string, need_more_data, ActualLen, byte_size(Rest2)})
    end;
decode_string(<<1:1, 127:7, Rest/binary>>) ->
    %% Length needs multi-byte encoding (Huffman)
    {ActualLen, Rest2} = decode_multi_byte_int(Rest, 127, 0),
    case byte_size(Rest2) >= ActualLen of
        true ->
            <<Encoded:ActualLen/binary, Rest3/binary>> = Rest2,
            {Decoded, _} = dec_huffman(Encoded, ActualLen),
            {Decoded, Rest3};
        false ->
            throw({invalid_string, need_more_data, ActualLen, byte_size(Rest2)})
    end;
decode_string(<<0:1, Len:7, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Str:Len/binary, Rest2/binary>> = Rest,
    {Str, Rest2};
decode_string(<<1:1, Len:7, Rest/binary>>) when byte_size(Rest) >= Len ->
    %% Huffman encoded
    <<Encoded:Len/binary, Rest2/binary>> = Rest,
    {Decoded, _} = dec_huffman(Encoded, Len),
    {Decoded, Rest2};
decode_string(<<_FirstByte, _/binary>> = Data) ->
    throw({invalid_string, byte_size(Data), Data});
decode_string(Data) ->
    throw({invalid_string, byte_size(Data), Data}).

%% Decode string where Huffman flag and length are already extracted
%% Used for literal-with-literal-name where these are in the instruction byte
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
-include("libs/hackney_cow_hpack_dec_huffman_lookup.hrl").

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
            %% Last nibble is padding - just emit CharA if present
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
%% Internal - Static Table
%%====================================================================

find_static_match(Name, Value) ->
    find_static_match(Name, Value, ?STATIC_TABLE, 0, none).

find_static_match(_Name, _Value, [], _Index, BestMatch) ->
    BestMatch;
find_static_match(Name, Value, [{Name, Value} | _], Index, _) ->
    {exact, Index};
find_static_match(Name, Value, [{Name, _} | Rest], Index, none) ->
    find_static_match(Name, Value, Rest, Index + 1, {name, Index});
find_static_match(Name, Value, [_ | Rest], Index, BestMatch) ->
    find_static_match(Name, Value, Rest, Index + 1, BestMatch).

get_static_entry(Index) when Index >= 0, Index =< 97 ->
    lists:nth(Index + 1, ?STATIC_TABLE);
get_static_entry(Index) ->
    throw({invalid_static_index, Index}).
