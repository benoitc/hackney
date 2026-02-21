%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Optimized HPACK header compression for HTTP/2 (RFC 7541).
%%%
%%% This implementation provides O(1) dynamic table lookups using
%%% a combination of ring buffer for index access and maps for
%%% field/name lookups during encoding.
%%%
%%% Key optimizations:
%%% - Ring buffer (tuple) for O(1) index-based access
%%% - Maps for O(1) field and name lookups
%%% - Pre-computed static table as maps
%%% - Lookup table for Huffman encoding
%%% @end

-module(hackney_hpack).
-dialyzer(no_improper_lists).

-export([init/0]).
-export([init/1]).
-export([set_max_size/2]).

-export([decode/1]).
-export([decode/2]).

-export([encode/1]).
-export([encode/2]).
-export([encode/3]).

%% Type definitions
-type header() :: {binary(), binary()}.
-type headers() :: [header()].
-type opts() :: map().

-export_type([state/0]).
-export_type([opts/0]).
-export_type([headers/0]).

%% Include Huffman lookup tables
-include("hackney_hpack_huffman.hrl").

%% Reuse existing Huffman decode lookup table from cowlib
-include("hackney_hpack_huffman_dec.hrl").

%% Dynamic table entry size overhead per RFC 7541 Section 4.1
-define(ENTRY_OVERHEAD, 32).

%% Static table size
-define(STATIC_TABLE_SIZE, 61).

%% Default ring buffer size (number of entries, will grow as needed)
-define(DEFAULT_RING_SIZE, 128).

%% State record with ring buffer + maps for O(1) operations
-record(state, {
    size = 0 :: non_neg_integer(),
    max_size = 4096 :: non_neg_integer(),
    configured_max_size = 4096 :: non_neg_integer(),
    %% Ring buffer for O(1) index access
    %% Entries stored as {EntrySize, {Name, Value}}
    entries = {} :: tuple(),
    head = 0 :: non_neg_integer(),        %% Position of newest entry
    count = 0 :: non_neg_integer(),       %% Number of entries
    %% Maps for O(1) lookup during encoding
    field_index = #{} :: #{header() => pos_integer()},  %% {Name, Value} => absolute_index
    name_index = #{} :: #{binary() => pos_integer()},   %% Name => absolute_index
    %% Monotonic counter for absolute indexing
    insert_count = 0 :: non_neg_integer()
}).

-opaque state() :: #state{}.

%%====================================================================
%% Static Table (RFC 7541 Appendix A)
%%====================================================================

%% Static table as a tuple for O(1) index access
-define(STATIC_TABLE, {
    {<<":authority">>, <<>>},
    {<<":method">>, <<"GET">>},
    {<<":method">>, <<"POST">>},
    {<<":path">>, <<"/">>},
    {<<":path">>, <<"/index.html">>},
    {<<":scheme">>, <<"http">>},
    {<<":scheme">>, <<"https">>},
    {<<":status">>, <<"200">>},
    {<<":status">>, <<"204">>},
    {<<":status">>, <<"206">>},
    {<<":status">>, <<"304">>},
    {<<":status">>, <<"400">>},
    {<<":status">>, <<"404">>},
    {<<":status">>, <<"500">>},
    {<<"accept-charset">>, <<>>},
    {<<"accept-encoding">>, <<"gzip, deflate">>},
    {<<"accept-language">>, <<>>},
    {<<"accept-ranges">>, <<>>},
    {<<"accept">>, <<>>},
    {<<"access-control-allow-origin">>, <<>>},
    {<<"age">>, <<>>},
    {<<"allow">>, <<>>},
    {<<"authorization">>, <<>>},
    {<<"cache-control">>, <<>>},
    {<<"content-disposition">>, <<>>},
    {<<"content-encoding">>, <<>>},
    {<<"content-language">>, <<>>},
    {<<"content-length">>, <<>>},
    {<<"content-location">>, <<>>},
    {<<"content-range">>, <<>>},
    {<<"content-type">>, <<>>},
    {<<"cookie">>, <<>>},
    {<<"date">>, <<>>},
    {<<"etag">>, <<>>},
    {<<"expect">>, <<>>},
    {<<"expires">>, <<>>},
    {<<"from">>, <<>>},
    {<<"host">>, <<>>},
    {<<"if-match">>, <<>>},
    {<<"if-modified-since">>, <<>>},
    {<<"if-none-match">>, <<>>},
    {<<"if-range">>, <<>>},
    {<<"if-unmodified-since">>, <<>>},
    {<<"last-modified">>, <<>>},
    {<<"link">>, <<>>},
    {<<"location">>, <<>>},
    {<<"max-forwards">>, <<>>},
    {<<"proxy-authenticate">>, <<>>},
    {<<"proxy-authorization">>, <<>>},
    {<<"range">>, <<>>},
    {<<"referer">>, <<>>},
    {<<"refresh">>, <<>>},
    {<<"retry-after">>, <<>>},
    {<<"server">>, <<>>},
    {<<"set-cookie">>, <<>>},
    {<<"strict-transport-security">>, <<>>},
    {<<"transfer-encoding">>, <<>>},
    {<<"user-agent">>, <<>>},
    {<<"vary">>, <<>>},
    {<<"via">>, <<>>},
    {<<"www-authenticate">>, <<>>}
}).

%% Static table field map for O(1) exact match lookup
-define(STATIC_FIELD_MAP, #{
    {<<":authority">>, <<>>} => 1,
    {<<":method">>, <<"GET">>} => 2,
    {<<":method">>, <<"POST">>} => 3,
    {<<":path">>, <<"/">>} => 4,
    {<<":path">>, <<"/index.html">>} => 5,
    {<<":scheme">>, <<"http">>} => 6,
    {<<":scheme">>, <<"https">>} => 7,
    {<<":status">>, <<"200">>} => 8,
    {<<":status">>, <<"204">>} => 9,
    {<<":status">>, <<"206">>} => 10,
    {<<":status">>, <<"304">>} => 11,
    {<<":status">>, <<"400">>} => 12,
    {<<":status">>, <<"404">>} => 13,
    {<<":status">>, <<"500">>} => 14,
    {<<"accept-charset">>, <<>>} => 15,
    {<<"accept-encoding">>, <<"gzip, deflate">>} => 16,
    {<<"accept-language">>, <<>>} => 17,
    {<<"accept-ranges">>, <<>>} => 18,
    {<<"accept">>, <<>>} => 19,
    {<<"access-control-allow-origin">>, <<>>} => 20,
    {<<"age">>, <<>>} => 21,
    {<<"allow">>, <<>>} => 22,
    {<<"authorization">>, <<>>} => 23,
    {<<"cache-control">>, <<>>} => 24,
    {<<"content-disposition">>, <<>>} => 25,
    {<<"content-encoding">>, <<>>} => 26,
    {<<"content-language">>, <<>>} => 27,
    {<<"content-length">>, <<>>} => 28,
    {<<"content-location">>, <<>>} => 29,
    {<<"content-range">>, <<>>} => 30,
    {<<"content-type">>, <<>>} => 31,
    {<<"cookie">>, <<>>} => 32,
    {<<"date">>, <<>>} => 33,
    {<<"etag">>, <<>>} => 34,
    {<<"expect">>, <<>>} => 35,
    {<<"expires">>, <<>>} => 36,
    {<<"from">>, <<>>} => 37,
    {<<"host">>, <<>>} => 38,
    {<<"if-match">>, <<>>} => 39,
    {<<"if-modified-since">>, <<>>} => 40,
    {<<"if-none-match">>, <<>>} => 41,
    {<<"if-range">>, <<>>} => 42,
    {<<"if-unmodified-since">>, <<>>} => 43,
    {<<"last-modified">>, <<>>} => 44,
    {<<"link">>, <<>>} => 45,
    {<<"location">>, <<>>} => 46,
    {<<"max-forwards">>, <<>>} => 47,
    {<<"proxy-authenticate">>, <<>>} => 48,
    {<<"proxy-authorization">>, <<>>} => 49,
    {<<"range">>, <<>>} => 50,
    {<<"referer">>, <<>>} => 51,
    {<<"refresh">>, <<>>} => 52,
    {<<"retry-after">>, <<>>} => 53,
    {<<"server">>, <<>>} => 54,
    {<<"set-cookie">>, <<>>} => 55,
    {<<"strict-transport-security">>, <<>>} => 56,
    {<<"transfer-encoding">>, <<>>} => 57,
    {<<"user-agent">>, <<>>} => 58,
    {<<"vary">>, <<>>} => 59,
    {<<"via">>, <<>>} => 60,
    {<<"www-authenticate">>, <<>>} => 61
}).

%% Static table name map for O(1) name-only lookup
-define(STATIC_NAME_MAP, #{
    <<":authority">> => 1,
    <<":method">> => 2,
    <<":path">> => 4,
    <<":scheme">> => 6,
    <<":status">> => 8,
    <<"accept-charset">> => 15,
    <<"accept-encoding">> => 16,
    <<"accept-language">> => 17,
    <<"accept-ranges">> => 18,
    <<"accept">> => 19,
    <<"access-control-allow-origin">> => 20,
    <<"age">> => 21,
    <<"allow">> => 22,
    <<"authorization">> => 23,
    <<"cache-control">> => 24,
    <<"content-disposition">> => 25,
    <<"content-encoding">> => 26,
    <<"content-language">> => 27,
    <<"content-length">> => 28,
    <<"content-location">> => 29,
    <<"content-range">> => 30,
    <<"content-type">> => 31,
    <<"cookie">> => 32,
    <<"date">> => 33,
    <<"etag">> => 34,
    <<"expect">> => 35,
    <<"expires">> => 36,
    <<"from">> => 37,
    <<"host">> => 38,
    <<"if-match">> => 39,
    <<"if-modified-since">> => 40,
    <<"if-none-match">> => 41,
    <<"if-range">> => 42,
    <<"if-unmodified-since">> => 43,
    <<"last-modified">> => 44,
    <<"link">> => 45,
    <<"location">> => 46,
    <<"max-forwards">> => 47,
    <<"proxy-authenticate">> => 48,
    <<"proxy-authorization">> => 49,
    <<"range">> => 50,
    <<"referer">> => 51,
    <<"refresh">> => 52,
    <<"retry-after">> => 53,
    <<"server">> => 54,
    <<"set-cookie">> => 55,
    <<"strict-transport-security">> => 56,
    <<"transfer-encoding">> => 57,
    <<"user-agent">> => 58,
    <<"vary">> => 59,
    <<"via">> => 60,
    <<"www-authenticate">> => 61
}).

%%====================================================================
%% State Initialization
%%====================================================================

-spec init() -> state().
init() ->
    #state{entries = erlang:make_tuple(?DEFAULT_RING_SIZE, undefined)}.

-spec init(non_neg_integer()) -> state().
init(MaxSize) ->
    #state{
        max_size = MaxSize,
        configured_max_size = MaxSize,
        entries = erlang:make_tuple(?DEFAULT_RING_SIZE, undefined)
    }.

-spec set_max_size(non_neg_integer(), State) -> State when State::state().
set_max_size(MaxSize, State) ->
    State#state{configured_max_size = MaxSize}.

%%====================================================================
%% Decoding
%%====================================================================

-spec decode(binary()) -> {headers(), state()}.
decode(Data) ->
    decode(Data, init()).

-spec decode(binary(), State) -> {headers(), State} when State::state().
%% Dynamic table size update is only allowed at the beginning of a HEADERS block.
decode(<<0:2, 1:1, Rest/bits>>, State = #state{configured_max_size = ConfigMaxSize}) ->
    {MaxSize, Rest2} = dec_int5(Rest),
    if
        MaxSize =< ConfigMaxSize ->
            State2 = table_update_size(MaxSize, State),
            decode(Rest2, State2)
    end;
decode(Data, State) ->
    decode(Data, State, []).

decode(<<>>, State, Acc) ->
    {lists:reverse(Acc), State};
%% Indexed header field representation.
decode(<<1:1, Rest/bits>>, State, Acc) ->
    dec_indexed(Rest, State, Acc);
%% Literal header field with incremental indexing: new name.
decode(<<0:1, 1:1, 0:6, Rest/bits>>, State, Acc) ->
    dec_lit_index_new_name(Rest, State, Acc);
%% Literal header field with incremental indexing: indexed name.
decode(<<0:1, 1:1, Rest/bits>>, State, Acc) ->
    dec_lit_index_indexed_name(Rest, State, Acc);
%% Literal header field without indexing: new name.
decode(<<0:8, Rest/bits>>, State, Acc) ->
    dec_lit_no_index_new_name(Rest, State, Acc);
%% Literal header field without indexing: indexed name.
decode(<<0:4, Rest/bits>>, State, Acc) ->
    dec_lit_no_index_indexed_name(Rest, State, Acc);
%% Literal header field never indexed: new name.
decode(<<0:3, 1:1, 0:4, Rest/bits>>, State, Acc) ->
    dec_lit_no_index_new_name(Rest, State, Acc);
%% Literal header field never indexed: indexed name.
decode(<<0:3, 1:1, Rest/bits>>, State, Acc) ->
    dec_lit_no_index_indexed_name(Rest, State, Acc).

%% Indexed header field representation.
dec_indexed(<<2#1111111:7, 0:1, Int:7, Rest/bits>>, State, Acc) ->
    {Name, Value} = table_get(127 + Int, State),
    decode(Rest, State, [{Name, Value} | Acc]);
dec_indexed(<<2#1111111:7, Rest0/bits>>, State, Acc) ->
    {Index, Rest} = dec_big_int(Rest0, 127, 0),
    {Name, Value} = table_get(Index, State),
    decode(Rest, State, [{Name, Value} | Acc]);
dec_indexed(<<Index:7, Rest/bits>>, State, Acc) ->
    {Name, Value} = table_get(Index, State),
    decode(Rest, State, [{Name, Value} | Acc]).

%% Literal header field with incremental indexing.
dec_lit_index_new_name(Rest, State, Acc) ->
    {Name, Rest2} = dec_str(Rest),
    dec_lit_index(Rest2, State, Acc, Name).

dec_lit_index_indexed_name(<<2#111111:6, 0:1, Int:7, Rest/bits>>, State, Acc) ->
    Name = table_get_name(63 + Int, State),
    dec_lit_index(Rest, State, Acc, Name);
dec_lit_index_indexed_name(<<2#111111:6, Rest0/bits>>, State, Acc) ->
    {Index, Rest} = dec_big_int(Rest0, 63, 0),
    Name = table_get_name(Index, State),
    dec_lit_index(Rest, State, Acc, Name);
dec_lit_index_indexed_name(<<Index:6, Rest/bits>>, State, Acc) ->
    Name = table_get_name(Index, State),
    dec_lit_index(Rest, State, Acc, Name).

dec_lit_index(Rest, State, Acc, Name) ->
    {Value, Rest2} = dec_str(Rest),
    State2 = table_insert({Name, Value}, State),
    decode(Rest2, State2, [{Name, Value} | Acc]).

%% Literal header field without indexing.
dec_lit_no_index_new_name(Rest, State, Acc) ->
    {Name, Rest2} = dec_str(Rest),
    dec_lit_no_index(Rest2, State, Acc, Name).

dec_lit_no_index_indexed_name(<<2#1111:4, 0:1, Int:7, Rest/bits>>, State, Acc) ->
    Name = table_get_name(15 + Int, State),
    dec_lit_no_index(Rest, State, Acc, Name);
dec_lit_no_index_indexed_name(<<2#1111:4, Rest0/bits>>, State, Acc) ->
    {Index, Rest} = dec_big_int(Rest0, 15, 0),
    Name = table_get_name(Index, State),
    dec_lit_no_index(Rest, State, Acc, Name);
dec_lit_no_index_indexed_name(<<Index:4, Rest/bits>>, State, Acc) ->
    Name = table_get_name(Index, State),
    dec_lit_no_index(Rest, State, Acc, Name).

dec_lit_no_index(Rest, State, Acc, Name) ->
    {Value, Rest2} = dec_str(Rest),
    decode(Rest2, State, [{Name, Value} | Acc]).

%% Integer decoding
dec_int5(<<2#11111:5, Rest/bits>>) ->
    dec_big_int(Rest, 31, 0);
dec_int5(<<Int:5, Rest/bits>>) ->
    {Int, Rest}.

dec_big_int(<<0:1, Value:7, Rest/bits>>, Int, M) ->
    {Int + (Value bsl M), Rest};
dec_big_int(<<1:1, Value:7, Rest/bits>>, Int, M) ->
    dec_big_int(Rest, Int + (Value bsl M), M + 7).

%% String decoding
dec_str(<<0:1, 2#1111111:7, Rest0/bits>>) ->
    {Length, Rest1} = dec_big_int(Rest0, 127, 0),
    <<Str:Length/binary, Rest/bits>> = Rest1,
    {Str, Rest};
dec_str(<<0:1, Length:7, Rest0/bits>>) ->
    <<Str:Length/binary, Rest/bits>> = Rest0,
    {Str, Rest};
dec_str(<<1:1, 2#1111111:7, Rest0/bits>>) ->
    {Length, Rest} = dec_big_int(Rest0, 127, 0),
    dec_huffman(Rest, Length, 0, <<>>);
dec_str(<<1:1, Length:7, Rest/bits>>) ->
    dec_huffman(Rest, Length, 0, <<>>).

%% Huffman decoding using lookup table
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
    {ok, CharB, _} = dec_huffman_lookup(Huff, B),
    case {CharA, CharB} of
        {CharA, undefined} ->
            {<<Acc/binary, CharA>>, Rest};
        {undefined, CharB} ->
            {<<Acc/binary, CharB>>, Rest};
        _ ->
            {<<Acc/binary, CharA, CharB>>, Rest}
    end;
dec_huffman(Rest, 0, _, <<>>) ->
    {<<>>, Rest}.

%%====================================================================
%% Encoding
%%====================================================================

-spec encode(headers()) -> {iodata(), state()}.
encode(Headers) ->
    encode(Headers, init(), huffman, []).

-spec encode(headers(), State) -> {iodata(), State} when State::state().
encode(Headers, State = #state{max_size = MaxSize, configured_max_size = MaxSize}) ->
    encode(Headers, State, huffman, []);
encode(Headers, State0 = #state{configured_max_size = MaxSize}) ->
    State1 = table_update_size(MaxSize, State0),
    {Data, State} = encode(Headers, State1, huffman, []),
    {[enc_int5(MaxSize, 2#001) | Data], State}.

-spec encode(headers(), State, opts()) -> {iodata(), State} when State::state().
encode(Headers, State = #state{max_size = MaxSize, configured_max_size = MaxSize}, Opts) ->
    encode(Headers, State, huffman_opt(Opts), []);
encode(Headers, State0 = #state{configured_max_size = MaxSize}, Opts) ->
    State1 = table_update_size(MaxSize, State0),
    {Data, State} = encode(Headers, State1, huffman_opt(Opts), []),
    {[enc_int5(MaxSize, 2#001) | Data], State}.

huffman_opt(#{huffman := false}) -> no_huffman;
huffman_opt(_) -> huffman.

encode([], State, _, Acc) ->
    {lists:reverse(Acc), State};
encode([{Name, Value0} | Tail], State, HuffmanOpt, Acc) ->
    Value = if
        is_binary(Value0) -> Value0;
        true -> iolist_to_binary(Value0)
    end,
    Header = {Name, Value},
    case table_find(Header, State) of
        %% Indexed header field representation.
        {field, Index} ->
            encode(Tail, State, HuffmanOpt,
                [enc_int7(Index, 2#1) | Acc]);
        %% Literal header field representation: indexed name.
        {name, Index} ->
            State2 = table_insert(Header, State),
            encode(Tail, State2, HuffmanOpt,
                [[enc_int6(Index, 2#01) | enc_str(Value, HuffmanOpt)] | Acc]);
        %% Literal header field representation: new name.
        not_found ->
            State2 = table_insert(Header, State),
            encode(Tail, State2, HuffmanOpt,
                [[<<0:1, 1:1, 0:6>> | [enc_str(Name, HuffmanOpt) | enc_str(Value, HuffmanOpt)]] | Acc])
    end.

%% Integer encoding
enc_int5(Int, Prefix) when Int < 31 ->
    <<Prefix:3, Int:5>>;
enc_int5(Int, Prefix) ->
    enc_big_int(Int - 31, <<Prefix:3, 2#11111:5>>).

enc_int6(Int, Prefix) when Int < 63 ->
    <<Prefix:2, Int:6>>;
enc_int6(Int, Prefix) ->
    enc_big_int(Int - 63, <<Prefix:2, 2#111111:6>>).

enc_int7(Int, Prefix) when Int < 127 ->
    <<Prefix:1, Int:7>>;
enc_int7(Int, Prefix) ->
    enc_big_int(Int - 127, <<Prefix:1, 2#1111111:7>>).

enc_big_int(Int, Acc) when Int < 128 ->
    <<Acc/binary, Int:8>>;
enc_big_int(Int, Acc) ->
    enc_big_int(Int bsr 7, <<Acc/binary, 1:1, Int:7>>).

%% String encoding
enc_str(Str, huffman) ->
    Encoded = enc_huffman(Str),
    [enc_int7(byte_size(Encoded), 2#1) | Encoded];
enc_str(Str, no_huffman) ->
    [enc_int7(byte_size(Str), 2#0) | Str].

%% Huffman encoding using lookup table
enc_huffman(Str) ->
    enc_huffman(Str, <<>>).

enc_huffman(<<>>, Acc) ->
    %% Add EOS padding
    case bit_size(Acc) rem 8 of
        0 -> Acc;
        N ->
            Padding = 8 - N,
            PadBits = (1 bsl Padding) - 1,
            <<Acc/bits, PadBits:Padding>>
    end;
enc_huffman(<<Byte, Rest/binary>>, Acc) ->
    {Code, Bits} = ?HUFFMAN_CODE(Byte),
    enc_huffman(Rest, <<Acc/bits, Code:Bits>>).

%%====================================================================
%% Table Operations
%%====================================================================

%% Find header in table - O(1) using maps
table_find(Header = {Name, _Value}, State) ->
    %% First check static table for exact match
    case maps:find(Header, ?STATIC_FIELD_MAP) of
        {ok, Index} ->
            {field, Index};
        error ->
            %% Check dynamic table for exact match
            case find_in_dynamic_field(Header, State) of
                {ok, Index} ->
                    {field, Index};
                error ->
                    %% Check static table for name match
                    case maps:find(Name, ?STATIC_NAME_MAP) of
                        {ok, Index} ->
                            {name, Index};
                        error ->
                            %% Check dynamic table for name match
                            case find_in_dynamic_name(Name, State) of
                                {ok, Index} ->
                                    {name, Index};
                                error ->
                                    not_found
                            end
                    end
            end
    end.

%% Find exact field match in dynamic table
find_in_dynamic_field(Header, #state{field_index = FieldIndex, insert_count = InsertCount, count = Count}) ->
    case maps:find(Header, FieldIndex) of
        {ok, AbsIndex} when InsertCount - AbsIndex < Count ->
            %% Convert absolute index to relative index
            %% Relative index = insert_count - abs_index + STATIC_TABLE_SIZE
            {ok, ?STATIC_TABLE_SIZE + (InsertCount - AbsIndex)};
        _ ->
            error
    end.

%% Find name match in dynamic table
find_in_dynamic_name(Name, #state{name_index = NameIndex, insert_count = InsertCount, count = Count}) ->
    case maps:find(Name, NameIndex) of
        {ok, AbsIndex} when InsertCount - AbsIndex < Count ->
            {ok, ?STATIC_TABLE_SIZE + (InsertCount - AbsIndex)};
        _ ->
            error
    end.

%% Get entry by index (1-based) - O(1)
table_get(Index, _State) when Index >= 1, Index =< ?STATIC_TABLE_SIZE ->
    element(Index, ?STATIC_TABLE);
table_get(Index, State) when Index > ?STATIC_TABLE_SIZE ->
    DynIndex = Index - ?STATIC_TABLE_SIZE,
    get_dynamic_entry(DynIndex, State).

%% Get name by index (1-based) - O(1)
table_get_name(Index, State) ->
    {Name, _Value} = table_get(Index, State),
    Name.

%% Get dynamic table entry by relative index (1 = newest)
get_dynamic_entry(RelIndex, #state{entries = Entries, head = Head, count = Count})
  when RelIndex >= 1, RelIndex =< Count ->
    RingSize = tuple_size(Entries),
    %% RelIndex 1 is at head, RelIndex 2 is at head-1, etc.
    Pos = ((Head - RelIndex + RingSize) rem RingSize) + 1,
    {_Size, Header} = element(Pos, Entries),
    Header.

%% Insert entry into dynamic table - O(1) amortized
table_insert(Entry = {Name, Value}, State = #state{
    size = Size,
    max_size = MaxSize,
    insert_count = InsertCount
}) ->
    EntrySize = byte_size(Name) + byte_size(Value) + ?ENTRY_OVERHEAD,
    if
        EntrySize > MaxSize ->
            %% Entry larger than table - clear everything
            State#state{
                size = 0,
                head = 0,
                count = 0,
                field_index = #{},
                name_index = #{},
                insert_count = InsertCount + 1
            };
        EntrySize + Size =< MaxSize ->
            %% Add entry without eviction
            add_entry(Entry, EntrySize, State);
        true ->
            %% Need to evict entries first
            State2 = evict_entries(State, EntrySize),
            add_entry(Entry, EntrySize, State2)
    end.

%% Add entry to ring buffer
add_entry(Entry = {Name, _Value}, EntrySize, State = #state{
    size = Size,
    entries = Entries,
    head = Head,
    count = Count,
    field_index = FieldIndex,
    name_index = NameIndex,
    insert_count = InsertCount
}) ->
    RingSize = tuple_size(Entries),
    NewInsertCount = InsertCount + 1,
    %% Calculate new head position
    NewHead = (Head rem RingSize) + 1,
    %% Grow ring buffer if needed
    {NewEntries, _NewRingSize} = case Count >= RingSize of
        true ->
            %% Double the ring buffer size
            grow_ring_buffer(Entries, RingSize);
        false ->
            {Entries, RingSize}
    end,
    %% Insert entry
    NewEntries2 = setelement(NewHead, NewEntries, {EntrySize, Entry}),
    State#state{
        size = Size + EntrySize,
        entries = NewEntries2,
        head = NewHead,
        count = Count + 1,
        field_index = FieldIndex#{Entry => NewInsertCount},
        name_index = NameIndex#{Name => NewInsertCount},
        insert_count = NewInsertCount
    }.

%% Grow ring buffer by doubling its size
grow_ring_buffer(Entries, OldSize) ->
    NewSize = OldSize * 2,
    NewEntries = erlang:make_tuple(NewSize, undefined),
    %% Copy existing entries
    NewEntries2 = copy_entries(Entries, NewEntries, 1, OldSize),
    {NewEntries2, NewSize}.

copy_entries(_Old, New, I, Size) when I > Size ->
    New;
copy_entries(Old, New, I, Size) ->
    copy_entries(Old, setelement(I, New, element(I, Old)), I + 1, Size).

%% Evict oldest entries to make room
evict_entries(State = #state{max_size = MaxSize}, NewEntrySize) ->
    TargetSize = MaxSize - NewEntrySize,
    evict_until(State, TargetSize).

evict_until(State = #state{size = Size}, TargetSize) when Size =< TargetSize ->
    State;
evict_until(State = #state{count = 0}, _TargetSize) ->
    State;
evict_until(State = #state{
    size = Size,
    entries = Entries,
    head = Head,
    count = Count,
    field_index = FieldIndex,
    name_index = NameIndex
}, TargetSize) ->
    %% Find oldest entry (tail of ring)
    RingSize = tuple_size(Entries),
    TailPos = ((Head - Count + RingSize) rem RingSize) + 1,
    {EntrySize, {Name, _Value} = Entry} = element(TailPos, Entries),
    %% Remove from maps (only if pointing to this entry)
    NewFieldIndex = maps:remove(Entry, FieldIndex),
    NewNameIndex = maps:remove(Name, NameIndex),
    %% Clear entry and update state
    NewState = State#state{
        size = Size - EntrySize,
        entries = setelement(TailPos, Entries, undefined),
        count = Count - 1,
        field_index = NewFieldIndex,
        name_index = NewNameIndex
    },
    evict_until(NewState, TargetSize).

%% Update table size
table_update_size(0, State) ->
    State#state{
        size = 0,
        max_size = 0,
        head = 0,
        count = 0,
        field_index = #{},
        name_index = #{}
    };
table_update_size(MaxSize, State = #state{size = CurrentSize}) when CurrentSize =< MaxSize ->
    State#state{max_size = MaxSize};
table_update_size(MaxSize, State) ->
    %% Need to evict entries
    evict_until(State#state{max_size = MaxSize}, MaxSize).
