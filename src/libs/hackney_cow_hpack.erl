%%% -*- erlang -*-
%%%
%%% Vendored from cowlib (https://github.com/ninenines/cowlib)
%%% Original module: cow_hpack
%%%
%% Copyright (c) 2015-2023, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% The current implementation is not suitable for use in
%% intermediaries as the information about headers that
%% should never be indexed is currently lost.

-module(hackney_cow_hpack).
-dialyzer(no_improper_lists).

-export([init/0]).
-export([init/1]).
-export([set_max_size/2]).

-export([decode/1]).
-export([decode/2]).

-export([encode/1]).
-export([encode/2]).
-export([encode/3]).

-record(state, {
	size = 0 :: non_neg_integer(),
	max_size = 4096 :: non_neg_integer(),
	configured_max_size = 4096 :: non_neg_integer(),
	dyn_table = [] :: [{pos_integer(), {binary(), binary()}}]
}).

-opaque state() :: #state{}.
-export_type([state/0]).

-type opts() :: map().
-export_type([opts/0]).

%% Type for HTTP headers (replaces cow_http:headers/0)
-type headers() :: [{binary(), binary()}].
-export_type([headers/0]).

%% Note: TEST block with proper include removed as proper is not a hackney dependency.
%% See cow_hpack.erl in cowlib for the original tests.

%% State initialization.

-spec init() -> state().
init() ->
	#state{}.

-spec init(non_neg_integer()) -> state().
init(MaxSize) ->
	#state{max_size=MaxSize, configured_max_size=MaxSize}.

%% Update the configured max size.
%%
%% When decoding, the local endpoint also needs to send a SETTINGS
%% frame with this value and it is then up to the remote endpoint
%% to decide what actual limit it will use. The actual limit is
%% signaled via dynamic table size updates in the encoded data.
%%
%% When encoding, the local endpoint will call this function after
%% receiving a SETTINGS frame with this value. The encoder will
%% then use this value as the new max after signaling via a dynamic
%% table size update. The value given as argument may be lower
%% than the one received in the SETTINGS.

-spec set_max_size(non_neg_integer(), State) -> State when State::state().
set_max_size(MaxSize, State) ->
	State#state{configured_max_size=MaxSize}.

%% Decoding.

-spec decode(binary()) -> {headers(), state()}.
decode(Data) ->
	decode(Data, init()).

-spec decode(binary(), State) -> {headers(), State} when State::state().
%% Dynamic table size update is only allowed at the beginning of a HEADERS block.
decode(<< 0:2, 1:1, Rest/bits >>, State=#state{configured_max_size=ConfigMaxSize}) ->
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
decode(<< 1:1, Rest/bits >>, State, Acc) ->
	dec_indexed(Rest, State, Acc);
%% Literal header field with incremental indexing: new name.
decode(<< 0:1, 1:1, 0:6, Rest/bits >>, State, Acc) ->
	dec_lit_index_new_name(Rest, State, Acc);
%% Literal header field with incremental indexing: indexed name.
decode(<< 0:1, 1:1, Rest/bits >>, State, Acc) ->
	dec_lit_index_indexed_name(Rest, State, Acc);
%% Literal header field without indexing: new name.
decode(<< 0:8, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_new_name(Rest, State, Acc);
%% Literal header field without indexing: indexed name.
decode(<< 0:4, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_indexed_name(Rest, State, Acc);
%% Literal header field never indexed: new name.
%% @todo Keep track of "never indexed" headers.
decode(<< 0:3, 1:1, 0:4, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_new_name(Rest, State, Acc);
%% Literal header field never indexed: indexed name.
%% @todo Keep track of "never indexed" headers.
decode(<< 0:3, 1:1, Rest/bits >>, State, Acc) ->
	dec_lit_no_index_indexed_name(Rest, State, Acc).

%% Indexed header field representation.

%% We do the integer decoding inline where appropriate, falling
%% back to dec_big_int for larger values.
dec_indexed(<<2#1111111:7, 0:1, Int:7, Rest/bits>>, State, Acc) ->
	{Name, Value} = table_get(127 + Int, State),
	decode(Rest, State, [{Name, Value}|Acc]);
dec_indexed(<<2#1111111:7, Rest0/bits>>, State, Acc) ->
	{Index, Rest} = dec_big_int(Rest0, 127, 0),
	{Name, Value} = table_get(Index, State),
	decode(Rest, State, [{Name, Value}|Acc]);
dec_indexed(<<Index:7, Rest/bits>>, State, Acc) ->
	{Name, Value} = table_get(Index, State),
	decode(Rest, State, [{Name, Value}|Acc]).

%% Literal header field with incremental indexing.

dec_lit_index_new_name(Rest, State, Acc) ->
	{Name, Rest2} = dec_str(Rest),
	dec_lit_index(Rest2, State, Acc, Name).

%% We do the integer decoding inline where appropriate, falling
%% back to dec_big_int for larger values.
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
	decode(Rest2, State2, [{Name, Value}|Acc]).

%% Literal header field without indexing.

dec_lit_no_index_new_name(Rest, State, Acc) ->
	{Name, Rest2} = dec_str(Rest),
	dec_lit_no_index(Rest2, State, Acc, Name).

%% We do the integer decoding inline where appropriate, falling
%% back to dec_big_int for larger values.
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
	decode(Rest2, State, [{Name, Value}|Acc]).

%% @todo Literal header field never indexed.

%% Decode an integer.

%% The HPACK format has 4 different integer prefixes length (from 4 to 7)
%% and each can be used to create an indefinite length integer if all bits
%% of the prefix are set to 1.

dec_int5(<< 2#11111:5, Rest/bits >>) ->
	dec_big_int(Rest, 31, 0);
dec_int5(<< Int:5, Rest/bits >>) ->
	{Int, Rest}.

dec_big_int(<< 0:1, Value:7, Rest/bits >>, Int, M) ->
	{Int + (Value bsl M), Rest};
dec_big_int(<< 1:1, Value:7, Rest/bits >>, Int, M) ->
	dec_big_int(Rest, Int + (Value bsl M), M + 7).

%% Decode a string.

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

%% We use a lookup table that allows us to benefit from
%% the binary match context optimization. A more naive
%% implementation using bit pattern matching cannot reuse
%% a match context because it wouldn't always match on
%% byte boundaries.
%%
%% See hackney_cow_hpack_dec_huffman_lookup.hrl for more details.

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
		%% {undefined, undefined} (> 7-bit final padding) is rejected with a crash.
		{CharA, undefined} ->
			{<<Acc/binary, CharA>>, Rest};
		{undefined, CharB} ->
			{<<Acc/binary, CharB>>, Rest};
		_ ->
			{<<Acc/binary, CharA, CharB>>, Rest}
	end;
%% Can only be reached when the string length to decode is 0.
dec_huffman(Rest, 0, _, <<>>) ->
	{<<>>, Rest}.

-include("hackney_cow_hpack_dec_huffman_lookup.hrl").

%% Encoding.

-spec encode(headers()) -> {iodata(), state()}.
encode(Headers) ->
	encode(Headers, init(), huffman, []).

-spec encode(headers(), State) -> {iodata(), State} when State::state().
encode(Headers, State=#state{max_size=MaxSize, configured_max_size=MaxSize}) ->
	encode(Headers, State, huffman, []);
encode(Headers, State0=#state{configured_max_size=MaxSize}) ->
	State1 = table_update_size(MaxSize, State0),
	{Data, State} = encode(Headers, State1, huffman, []),
	{[enc_int5(MaxSize, 2#001)|Data], State}.

-spec encode(headers(), State, opts()) -> {iodata(), State} when State::state().
encode(Headers, State=#state{max_size=MaxSize, configured_max_size=MaxSize}, Opts) ->
	encode(Headers, State, huffman_opt(Opts), []);
encode(Headers, State0=#state{configured_max_size=MaxSize}, Opts) ->
	State1 = table_update_size(MaxSize, State0),
	{Data, State} = encode(Headers, State1, huffman_opt(Opts), []),
	{[enc_int5(MaxSize, 2#001)|Data], State}.

huffman_opt(#{huffman := false}) -> no_huffman;
huffman_opt(_) -> huffman.

%% @todo Handle cases where no/never indexing is expected.
encode([], State, _, Acc) ->
	{lists:reverse(Acc), State};
encode([{Name, Value0}|Tail], State, HuffmanOpt, Acc) ->
	%% We conditionally call iolist_to_binary/1 because a small
	%% but noticeable speed improvement happens when we do this.
	Value = if
		is_binary(Value0) -> Value0;
		true -> iolist_to_binary(Value0)
	end,
	Header = {Name, Value},
	case table_find(Header, State) of
		%% Indexed header field representation.
		{field, Index} ->
			encode(Tail, State, HuffmanOpt,
				[enc_int7(Index, 2#1)|Acc]);
		%% Literal header field representation: indexed name.
		{name, Index} ->
			State2 = table_insert(Header, State),
			encode(Tail, State2, HuffmanOpt,
				[[enc_int6(Index, 2#01)|enc_str(Value, HuffmanOpt)]|Acc]);
		%% Literal header field representation: new name.
		not_found ->
			State2 = table_insert(Header, State),
			encode(Tail, State2, HuffmanOpt,
				[[<< 0:1, 1:1, 0:6 >>|[enc_str(Name, HuffmanOpt)|enc_str(Value, HuffmanOpt)]]|Acc])
	end.

%% Encode an integer.

enc_int5(Int, Prefix) when Int < 31 ->
	<< Prefix:3, Int:5 >>;
enc_int5(Int, Prefix) ->
	enc_big_int(Int - 31, << Prefix:3, 2#11111:5 >>).

enc_int6(Int, Prefix) when Int < 63 ->
	<< Prefix:2, Int:6 >>;
enc_int6(Int, Prefix) ->
	enc_big_int(Int - 63, << Prefix:2, 2#111111:6 >>).

enc_int7(Int, Prefix) when Int < 127 ->
	<< Prefix:1, Int:7 >>;
enc_int7(Int, Prefix) ->
	enc_big_int(Int - 127, << Prefix:1, 2#1111111:7 >>).

enc_big_int(Int, Acc) when Int < 128 ->
	<<Acc/binary, Int:8>>;
enc_big_int(Int, Acc) ->
	enc_big_int(Int bsr 7, <<Acc/binary, 1:1, Int:7>>).

%% Encode a string.

enc_str(Str, huffman) ->
	Str2 = enc_huffman(Str, <<>>),
	[enc_int7(byte_size(Str2), 2#1)|Str2];
enc_str(Str, no_huffman) ->
	[enc_int7(byte_size(Str), 2#0)|Str].

enc_huffman(<<>>, Acc) ->
	case bit_size(Acc) rem 8 of
		1 -> << Acc/bits, 2#1111111:7 >>;
		2 -> << Acc/bits, 2#111111:6 >>;
		3 -> << Acc/bits, 2#11111:5 >>;
		4 -> << Acc/bits, 2#1111:4 >>;
		5 -> << Acc/bits, 2#111:3 >>;
		6 -> << Acc/bits, 2#11:2 >>;
		7 -> << Acc/bits, 2#1:1 >>;
		0 -> Acc
	end;
enc_huffman(<< 0, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111000:13 >>);
enc_huffman(<< 1, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011000:23 >>);
enc_huffman(<< 2, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100010:28 >>);
enc_huffman(<< 3, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100011:28 >>);
enc_huffman(<< 4, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100100:28 >>);
enc_huffman(<< 5, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100101:28 >>);
enc_huffman(<< 6, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100110:28 >>);
enc_huffman(<< 7, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111100111:28 >>);
enc_huffman(<< 8, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101000:28 >>);
enc_huffman(<< 9, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101010:24 >>);
enc_huffman(<< 10, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111111111100:30 >>);
enc_huffman(<< 11, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101001:28 >>);
enc_huffman(<< 12, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101010:28 >>);
enc_huffman(<< 13, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111111111101:30 >>);
enc_huffman(<< 14, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101011:28 >>);
enc_huffman(<< 15, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101100:28 >>);
enc_huffman(<< 16, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101101:28 >>);
enc_huffman(<< 17, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101110:28 >>);
enc_huffman(<< 18, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111101111:28 >>);
enc_huffman(<< 19, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110000:28 >>);
enc_huffman(<< 20, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110001:28 >>);
enc_huffman(<< 21, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110010:28 >>);
enc_huffman(<< 22, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111111111110:30 >>);
enc_huffman(<< 23, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110011:28 >>);
enc_huffman(<< 24, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110100:28 >>);
enc_huffman(<< 25, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110101:28 >>);
enc_huffman(<< 26, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110110:28 >>);
enc_huffman(<< 27, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111110111:28 >>);
enc_huffman(<< 28, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111000:28 >>);
enc_huffman(<< 29, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111001:28 >>);
enc_huffman(<< 30, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111010:28 >>);
enc_huffman(<< 31, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111011:28 >>);
enc_huffman(<< 32, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010100:6 >>);
enc_huffman(<< 33, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111000:10 >>);
enc_huffman(<< 34, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111001:10 >>);
enc_huffman(<< 35, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111010:12 >>);
enc_huffman(<< 36, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111001:13 >>);
enc_huffman(<< 37, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010101:6 >>);
enc_huffman(<< 38, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111000:8 >>);
enc_huffman(<< 39, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111010:11 >>);
enc_huffman(<< 40, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111010:10 >>);
enc_huffman(<< 41, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111011:10 >>);
enc_huffman(<< 42, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111001:8 >>);
enc_huffman(<< 43, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111011:11 >>);
enc_huffman(<< 44, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111010:8 >>);
enc_huffman(<< 45, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010110:6 >>);
enc_huffman(<< 46, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#010111:6 >>);
enc_huffman(<< 47, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011000:6 >>);
enc_huffman(<< 48, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00000:5 >>);
enc_huffman(<< 49, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00001:5 >>);
enc_huffman(<< 50, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00010:5 >>);
enc_huffman(<< 51, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011001:6 >>);
enc_huffman(<< 52, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011010:6 >>);
enc_huffman(<< 53, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011011:6 >>);
enc_huffman(<< 54, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011100:6 >>);
enc_huffman(<< 55, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011101:6 >>);
enc_huffman(<< 56, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011110:6 >>);
enc_huffman(<< 57, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#011111:6 >>);
enc_huffman(<< 58, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011100:7 >>);
enc_huffman(<< 59, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111011:8 >>);
enc_huffman(<< 60, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111100:15 >>);
enc_huffman(<< 61, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100000:6 >>);
enc_huffman(<< 62, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111011:12 >>);
enc_huffman(<< 63, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111100:10 >>);
enc_huffman(<< 64, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111010:13 >>);
enc_huffman(<< 65, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100001:6 >>);
enc_huffman(<< 66, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011101:7 >>);
enc_huffman(<< 67, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011110:7 >>);
enc_huffman(<< 68, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1011111:7 >>);
enc_huffman(<< 69, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100000:7 >>);
enc_huffman(<< 70, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100001:7 >>);
enc_huffman(<< 71, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100010:7 >>);
enc_huffman(<< 72, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100011:7 >>);
enc_huffman(<< 73, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100100:7 >>);
enc_huffman(<< 74, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100101:7 >>);
enc_huffman(<< 75, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100110:7 >>);
enc_huffman(<< 76, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1100111:7 >>);
enc_huffman(<< 77, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101000:7 >>);
enc_huffman(<< 78, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101001:7 >>);
enc_huffman(<< 79, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101010:7 >>);
enc_huffman(<< 80, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101011:7 >>);
enc_huffman(<< 81, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101100:7 >>);
enc_huffman(<< 82, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101101:7 >>);
enc_huffman(<< 83, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101110:7 >>);
enc_huffman(<< 84, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1101111:7 >>);
enc_huffman(<< 85, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110000:7 >>);
enc_huffman(<< 86, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110001:7 >>);
enc_huffman(<< 87, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110010:7 >>);
enc_huffman(<< 88, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111100:8 >>);
enc_huffman(<< 89, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110011:7 >>);
enc_huffman(<< 90, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111101:8 >>);
enc_huffman(<< 91, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111011:13 >>);
enc_huffman(<< 92, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111110000:19 >>);
enc_huffman(<< 93, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111100:13 >>);
enc_huffman(<< 94, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111100:14 >>);
enc_huffman(<< 95, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100010:6 >>);
enc_huffman(<< 96, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111101:15 >>);
enc_huffman(<< 97, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00011:5 >>);
enc_huffman(<< 98, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100011:6 >>);
enc_huffman(<< 99, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00100:5 >>);
enc_huffman(<< 100, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100100:6 >>);
enc_huffman(<< 101, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00101:5 >>);
enc_huffman(<< 102, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100101:6 >>);
enc_huffman(<< 103, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100110:6 >>);
enc_huffman(<< 104, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#100111:6 >>);
enc_huffman(<< 105, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00110:5 >>);
enc_huffman(<< 106, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110100:7 >>);
enc_huffman(<< 107, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110101:7 >>);
enc_huffman(<< 108, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101000:6 >>);
enc_huffman(<< 109, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101001:6 >>);
enc_huffman(<< 110, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101010:6 >>);
enc_huffman(<< 111, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#00111:5 >>);
enc_huffman(<< 112, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101011:6 >>);
enc_huffman(<< 113, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110110:7 >>);
enc_huffman(<< 114, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101100:6 >>);
enc_huffman(<< 115, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#01000:5 >>);
enc_huffman(<< 116, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#01001:5 >>);
enc_huffman(<< 117, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#101101:6 >>);
enc_huffman(<< 118, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1110111:7 >>);
enc_huffman(<< 119, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111000:7 >>);
enc_huffman(<< 120, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111001:7 >>);
enc_huffman(<< 121, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111010:7 >>);
enc_huffman(<< 122, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111011:7 >>);
enc_huffman(<< 123, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111110:15 >>);
enc_huffman(<< 124, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111100:11 >>);
enc_huffman(<< 125, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111101:14 >>);
enc_huffman(<< 126, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111101:13 >>);
enc_huffman(<< 127, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111100:28 >>);
enc_huffman(<< 128, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111100110:20 >>);
enc_huffman(<< 129, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010010:22 >>);
enc_huffman(<< 130, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111100111:20 >>);
enc_huffman(<< 131, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101000:20 >>);
enc_huffman(<< 132, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010011:22 >>);
enc_huffman(<< 133, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010100:22 >>);
enc_huffman(<< 134, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010101:22 >>);
enc_huffman(<< 135, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011001:23 >>);
enc_huffman(<< 136, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010110:22 >>);
enc_huffman(<< 137, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011010:23 >>);
enc_huffman(<< 138, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011011:23 >>);
enc_huffman(<< 139, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011100:23 >>);
enc_huffman(<< 140, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011101:23 >>);
enc_huffman(<< 141, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011110:23 >>);
enc_huffman(<< 142, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101011:24 >>);
enc_huffman(<< 143, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111011111:23 >>);
enc_huffman(<< 144, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101100:24 >>);
enc_huffman(<< 145, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101101:24 >>);
enc_huffman(<< 146, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111010111:22 >>);
enc_huffman(<< 147, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100000:23 >>);
enc_huffman(<< 148, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101110:24 >>);
enc_huffman(<< 149, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100001:23 >>);
enc_huffman(<< 150, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100010:23 >>);
enc_huffman(<< 151, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100011:23 >>);
enc_huffman(<< 152, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100100:23 >>);
enc_huffman(<< 153, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011100:21 >>);
enc_huffman(<< 154, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011000:22 >>);
enc_huffman(<< 155, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100101:23 >>);
enc_huffman(<< 156, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011001:22 >>);
enc_huffman(<< 157, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100110:23 >>);
enc_huffman(<< 158, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111100111:23 >>);
enc_huffman(<< 159, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111101111:24 >>);
enc_huffman(<< 160, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011010:22 >>);
enc_huffman(<< 161, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011101:21 >>);
enc_huffman(<< 162, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101001:20 >>);
enc_huffman(<< 163, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011011:22 >>);
enc_huffman(<< 164, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011100:22 >>);
enc_huffman(<< 165, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101000:23 >>);
enc_huffman(<< 166, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101001:23 >>);
enc_huffman(<< 167, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011110:21 >>);
enc_huffman(<< 168, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101010:23 >>);
enc_huffman(<< 169, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011101:22 >>);
enc_huffman(<< 170, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011110:22 >>);
enc_huffman(<< 171, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110000:24 >>);
enc_huffman(<< 172, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111011111:21 >>);
enc_huffman(<< 173, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111011111:22 >>);
enc_huffman(<< 174, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101011:23 >>);
enc_huffman(<< 175, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101100:23 >>);
enc_huffman(<< 176, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100000:21 >>);
enc_huffman(<< 177, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100001:21 >>);
enc_huffman(<< 178, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100000:22 >>);
enc_huffman(<< 179, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100010:21 >>);
enc_huffman(<< 180, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101101:23 >>);
enc_huffman(<< 181, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100001:22 >>);
enc_huffman(<< 182, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101110:23 >>);
enc_huffman(<< 183, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111101111:23 >>);
enc_huffman(<< 184, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101010:20 >>);
enc_huffman(<< 185, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100010:22 >>);
enc_huffman(<< 186, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100011:22 >>);
enc_huffman(<< 187, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100100:22 >>);
enc_huffman(<< 188, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110000:23 >>);
enc_huffman(<< 189, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100101:22 >>);
enc_huffman(<< 190, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100110:22 >>);
enc_huffman(<< 191, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110001:23 >>);
enc_huffman(<< 192, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100000:26 >>);
enc_huffman(<< 193, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100001:26 >>);
enc_huffman(<< 194, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101011:20 >>);
enc_huffman(<< 195, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111110001:19 >>);
enc_huffman(<< 196, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111100111:22 >>);
enc_huffman(<< 197, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110010:23 >>);
enc_huffman(<< 198, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101000:22 >>);
enc_huffman(<< 199, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101100:25 >>);
enc_huffman(<< 200, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100010:26 >>);
enc_huffman(<< 201, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100011:26 >>);
enc_huffman(<< 202, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100100:26 >>);
enc_huffman(<< 203, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111011110:27 >>);
enc_huffman(<< 204, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111011111:27 >>);
enc_huffman(<< 205, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100101:26 >>);
enc_huffman(<< 206, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110001:24 >>);
enc_huffman(<< 207, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101101:25 >>);
enc_huffman(<< 208, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111110010:19 >>);
enc_huffman(<< 209, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100011:21 >>);
enc_huffman(<< 210, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100110:26 >>);
enc_huffman(<< 211, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100000:27 >>);
enc_huffman(<< 212, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100001:27 >>);
enc_huffman(<< 213, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111100111:26 >>);
enc_huffman(<< 214, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100010:27 >>);
enc_huffman(<< 215, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110010:24 >>);
enc_huffman(<< 216, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100100:21 >>);
enc_huffman(<< 217, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100101:21 >>);
enc_huffman(<< 218, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101000:26 >>);
enc_huffman(<< 219, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101001:26 >>);
enc_huffman(<< 220, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111101:28 >>);
enc_huffman(<< 221, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100011:27 >>);
enc_huffman(<< 222, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100100:27 >>);
enc_huffman(<< 223, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100101:27 >>);
enc_huffman(<< 224, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101100:20 >>);
enc_huffman(<< 225, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110011:24 >>);
enc_huffman(<< 226, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111101101:20 >>);
enc_huffman(<< 227, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100110:21 >>);
enc_huffman(<< 228, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101001:22 >>);
enc_huffman(<< 229, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111100111:21 >>);
enc_huffman(<< 230, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111101000:21 >>);
enc_huffman(<< 231, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110011:23 >>);
enc_huffman(<< 232, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101010:22 >>);
enc_huffman(<< 233, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111101011:22 >>);
enc_huffman(<< 234, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101110:25 >>);
enc_huffman(<< 235, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111101111:25 >>);
enc_huffman(<< 236, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110100:24 >>);
enc_huffman(<< 237, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111110101:24 >>);
enc_huffman(<< 238, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101010:26 >>);
enc_huffman(<< 239, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111110100:23 >>);
enc_huffman(<< 240, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101011:26 >>);
enc_huffman(<< 241, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100110:27 >>);
enc_huffman(<< 242, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101100:26 >>);
enc_huffman(<< 243, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101101:26 >>);
enc_huffman(<< 244, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111100111:27 >>);
enc_huffman(<< 245, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101000:27 >>);
enc_huffman(<< 246, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101001:27 >>);
enc_huffman(<< 247, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101010:27 >>);
enc_huffman(<< 248, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101011:27 >>);
enc_huffman(<< 249, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#1111111111111111111111111110:28 >>);
enc_huffman(<< 250, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101100:27 >>);
enc_huffman(<< 251, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101101:27 >>);
enc_huffman(<< 252, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101110:27 >>);
enc_huffman(<< 253, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111101111:27 >>);
enc_huffman(<< 254, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#111111111111111111111110000:27 >>);
enc_huffman(<< 255, R/bits >>, A) -> enc_huffman(R, << A/bits, 2#11111111111111111111101110:26 >>).

%% Static and dynamic tables.

%% @todo There must be a more efficient way.
table_find(Header = {Name, _}, State) ->
	case table_find_field(Header, State) of
		not_found ->
			case table_find_name(Name, State) of
				NotFound = not_found ->
					NotFound;
				Found ->
					{name, Found}
			end;
		Found ->
			{field, Found}
	end.

table_find_field({<<":authority">>, <<>>}, _) -> 1;
table_find_field({<<":method">>, <<"GET">>}, _) -> 2;
table_find_field({<<":method">>, <<"POST">>}, _) -> 3;
table_find_field({<<":path">>, <<"/">>}, _) -> 4;
table_find_field({<<":path">>, <<"/index.html">>}, _) -> 5;
table_find_field({<<":scheme">>, <<"http">>}, _) -> 6;
table_find_field({<<":scheme">>, <<"https">>}, _) -> 7;
table_find_field({<<":status">>, <<"200">>}, _) -> 8;
table_find_field({<<":status">>, <<"204">>}, _) -> 9;
table_find_field({<<":status">>, <<"206">>}, _) -> 10;
table_find_field({<<":status">>, <<"304">>}, _) -> 11;
table_find_field({<<":status">>, <<"400">>}, _) -> 12;
table_find_field({<<":status">>, <<"404">>}, _) -> 13;
table_find_field({<<":status">>, <<"500">>}, _) -> 14;
table_find_field({<<"accept-charset">>, <<>>}, _) -> 15;
table_find_field({<<"accept-encoding">>, <<"gzip, deflate">>}, _) -> 16;
table_find_field({<<"accept-language">>, <<>>}, _) -> 17;
table_find_field({<<"accept-ranges">>, <<>>}, _) -> 18;
table_find_field({<<"accept">>, <<>>}, _) -> 19;
table_find_field({<<"access-control-allow-origin">>, <<>>}, _) -> 20;
table_find_field({<<"age">>, <<>>}, _) -> 21;
table_find_field({<<"allow">>, <<>>}, _) -> 22;
table_find_field({<<"authorization">>, <<>>}, _) -> 23;
table_find_field({<<"cache-control">>, <<>>}, _) -> 24;
table_find_field({<<"content-disposition">>, <<>>}, _) -> 25;
table_find_field({<<"content-encoding">>, <<>>}, _) -> 26;
table_find_field({<<"content-language">>, <<>>}, _) -> 27;
table_find_field({<<"content-length">>, <<>>}, _) -> 28;
table_find_field({<<"content-location">>, <<>>}, _) -> 29;
table_find_field({<<"content-range">>, <<>>}, _) -> 30;
table_find_field({<<"content-type">>, <<>>}, _) -> 31;
table_find_field({<<"cookie">>, <<>>}, _) -> 32;
table_find_field({<<"date">>, <<>>}, _) -> 33;
table_find_field({<<"etag">>, <<>>}, _) -> 34;
table_find_field({<<"expect">>, <<>>}, _) -> 35;
table_find_field({<<"expires">>, <<>>}, _) -> 36;
table_find_field({<<"from">>, <<>>}, _) -> 37;
table_find_field({<<"host">>, <<>>}, _) -> 38;
table_find_field({<<"if-match">>, <<>>}, _) -> 39;
table_find_field({<<"if-modified-since">>, <<>>}, _) -> 40;
table_find_field({<<"if-none-match">>, <<>>}, _) -> 41;
table_find_field({<<"if-range">>, <<>>}, _) -> 42;
table_find_field({<<"if-unmodified-since">>, <<>>}, _) -> 43;
table_find_field({<<"last-modified">>, <<>>}, _) -> 44;
table_find_field({<<"link">>, <<>>}, _) -> 45;
table_find_field({<<"location">>, <<>>}, _) -> 46;
table_find_field({<<"max-forwards">>, <<>>}, _) -> 47;
table_find_field({<<"proxy-authenticate">>, <<>>}, _) -> 48;
table_find_field({<<"proxy-authorization">>, <<>>}, _) -> 49;
table_find_field({<<"range">>, <<>>}, _) -> 50;
table_find_field({<<"referer">>, <<>>}, _) -> 51;
table_find_field({<<"refresh">>, <<>>}, _) -> 52;
table_find_field({<<"retry-after">>, <<>>}, _) -> 53;
table_find_field({<<"server">>, <<>>}, _) -> 54;
table_find_field({<<"set-cookie">>, <<>>}, _) -> 55;
table_find_field({<<"strict-transport-security">>, <<>>}, _) -> 56;
table_find_field({<<"transfer-encoding">>, <<>>}, _) -> 57;
table_find_field({<<"user-agent">>, <<>>}, _) -> 58;
table_find_field({<<"vary">>, <<>>}, _) -> 59;
table_find_field({<<"via">>, <<>>}, _) -> 60;
table_find_field({<<"www-authenticate">>, <<>>}, _) -> 61;
table_find_field(Header, #state{dyn_table=DynamicTable}) ->
	table_find_field_dyn(Header, DynamicTable, 62).

table_find_field_dyn(_, [], _) -> not_found;
table_find_field_dyn(Header, [{_, Header}|_], Index) -> Index;
table_find_field_dyn(Header, [_|Tail], Index) -> table_find_field_dyn(Header, Tail, Index + 1).

table_find_name(<<":authority">>, _) -> 1;
table_find_name(<<":method">>, _) -> 2;
table_find_name(<<":path">>, _) -> 4;
table_find_name(<<":scheme">>, _) -> 6;
table_find_name(<<":status">>, _) -> 8;
table_find_name(<<"accept-charset">>, _) -> 15;
table_find_name(<<"accept-encoding">>, _) -> 16;
table_find_name(<<"accept-language">>, _) -> 17;
table_find_name(<<"accept-ranges">>, _) -> 18;
table_find_name(<<"accept">>, _) -> 19;
table_find_name(<<"access-control-allow-origin">>, _) -> 20;
table_find_name(<<"age">>, _) -> 21;
table_find_name(<<"allow">>, _) -> 22;
table_find_name(<<"authorization">>, _) -> 23;
table_find_name(<<"cache-control">>, _) -> 24;
table_find_name(<<"content-disposition">>, _) -> 25;
table_find_name(<<"content-encoding">>, _) -> 26;
table_find_name(<<"content-language">>, _) -> 27;
table_find_name(<<"content-length">>, _) -> 28;
table_find_name(<<"content-location">>, _) -> 29;
table_find_name(<<"content-range">>, _) -> 30;
table_find_name(<<"content-type">>, _) -> 31;
table_find_name(<<"cookie">>, _) -> 32;
table_find_name(<<"date">>, _) -> 33;
table_find_name(<<"etag">>, _) -> 34;
table_find_name(<<"expect">>, _) -> 35;
table_find_name(<<"expires">>, _) -> 36;
table_find_name(<<"from">>, _) -> 37;
table_find_name(<<"host">>, _) -> 38;
table_find_name(<<"if-match">>, _) -> 39;
table_find_name(<<"if-modified-since">>, _) -> 40;
table_find_name(<<"if-none-match">>, _) -> 41;
table_find_name(<<"if-range">>, _) -> 42;
table_find_name(<<"if-unmodified-since">>, _) -> 43;
table_find_name(<<"last-modified">>, _) -> 44;
table_find_name(<<"link">>, _) -> 45;
table_find_name(<<"location">>, _) -> 46;
table_find_name(<<"max-forwards">>, _) -> 47;
table_find_name(<<"proxy-authenticate">>, _) -> 48;
table_find_name(<<"proxy-authorization">>, _) -> 49;
table_find_name(<<"range">>, _) -> 50;
table_find_name(<<"referer">>, _) -> 51;
table_find_name(<<"refresh">>, _) -> 52;
table_find_name(<<"retry-after">>, _) -> 53;
table_find_name(<<"server">>, _) -> 54;
table_find_name(<<"set-cookie">>, _) -> 55;
table_find_name(<<"strict-transport-security">>, _) -> 56;
table_find_name(<<"transfer-encoding">>, _) -> 57;
table_find_name(<<"user-agent">>, _) -> 58;
table_find_name(<<"vary">>, _) -> 59;
table_find_name(<<"via">>, _) -> 60;
table_find_name(<<"www-authenticate">>, _) -> 61;
table_find_name(Name, #state{dyn_table=DynamicTable}) ->
	table_find_name_dyn(Name, DynamicTable, 62).

table_find_name_dyn(_, [], _) -> not_found;
table_find_name_dyn(Name, [{Name, _}|_], Index) -> Index;
table_find_name_dyn(Name, [_|Tail], Index) -> table_find_name_dyn(Name, Tail, Index + 1).

table_get(1, _) -> {<<":authority">>, <<>>};
table_get(2, _) -> {<<":method">>, <<"GET">>};
table_get(3, _) -> {<<":method">>, <<"POST">>};
table_get(4, _) -> {<<":path">>, <<"/">>};
table_get(5, _) -> {<<":path">>, <<"/index.html">>};
table_get(6, _) -> {<<":scheme">>, <<"http">>};
table_get(7, _) -> {<<":scheme">>, <<"https">>};
table_get(8, _) -> {<<":status">>, <<"200">>};
table_get(9, _) -> {<<":status">>, <<"204">>};
table_get(10, _) -> {<<":status">>, <<"206">>};
table_get(11, _) -> {<<":status">>, <<"304">>};
table_get(12, _) -> {<<":status">>, <<"400">>};
table_get(13, _) -> {<<":status">>, <<"404">>};
table_get(14, _) -> {<<":status">>, <<"500">>};
table_get(15, _) -> {<<"accept-charset">>, <<>>};
table_get(16, _) -> {<<"accept-encoding">>, <<"gzip, deflate">>};
table_get(17, _) -> {<<"accept-language">>, <<>>};
table_get(18, _) -> {<<"accept-ranges">>, <<>>};
table_get(19, _) -> {<<"accept">>, <<>>};
table_get(20, _) -> {<<"access-control-allow-origin">>, <<>>};
table_get(21, _) -> {<<"age">>, <<>>};
table_get(22, _) -> {<<"allow">>, <<>>};
table_get(23, _) -> {<<"authorization">>, <<>>};
table_get(24, _) -> {<<"cache-control">>, <<>>};
table_get(25, _) -> {<<"content-disposition">>, <<>>};
table_get(26, _) -> {<<"content-encoding">>, <<>>};
table_get(27, _) -> {<<"content-language">>, <<>>};
table_get(28, _) -> {<<"content-length">>, <<>>};
table_get(29, _) -> {<<"content-location">>, <<>>};
table_get(30, _) -> {<<"content-range">>, <<>>};
table_get(31, _) -> {<<"content-type">>, <<>>};
table_get(32, _) -> {<<"cookie">>, <<>>};
table_get(33, _) -> {<<"date">>, <<>>};
table_get(34, _) -> {<<"etag">>, <<>>};
table_get(35, _) -> {<<"expect">>, <<>>};
table_get(36, _) -> {<<"expires">>, <<>>};
table_get(37, _) -> {<<"from">>, <<>>};
table_get(38, _) -> {<<"host">>, <<>>};
table_get(39, _) -> {<<"if-match">>, <<>>};
table_get(40, _) -> {<<"if-modified-since">>, <<>>};
table_get(41, _) -> {<<"if-none-match">>, <<>>};
table_get(42, _) -> {<<"if-range">>, <<>>};
table_get(43, _) -> {<<"if-unmodified-since">>, <<>>};
table_get(44, _) -> {<<"last-modified">>, <<>>};
table_get(45, _) -> {<<"link">>, <<>>};
table_get(46, _) -> {<<"location">>, <<>>};
table_get(47, _) -> {<<"max-forwards">>, <<>>};
table_get(48, _) -> {<<"proxy-authenticate">>, <<>>};
table_get(49, _) -> {<<"proxy-authorization">>, <<>>};
table_get(50, _) -> {<<"range">>, <<>>};
table_get(51, _) -> {<<"referer">>, <<>>};
table_get(52, _) -> {<<"refresh">>, <<>>};
table_get(53, _) -> {<<"retry-after">>, <<>>};
table_get(54, _) -> {<<"server">>, <<>>};
table_get(55, _) -> {<<"set-cookie">>, <<>>};
table_get(56, _) -> {<<"strict-transport-security">>, <<>>};
table_get(57, _) -> {<<"transfer-encoding">>, <<>>};
table_get(58, _) -> {<<"user-agent">>, <<>>};
table_get(59, _) -> {<<"vary">>, <<>>};
table_get(60, _) -> {<<"via">>, <<>>};
table_get(61, _) -> {<<"www-authenticate">>, <<>>};
table_get(Index, #state{dyn_table=DynamicTable}) ->
	{_, Header} = lists:nth(Index - 61, DynamicTable),
	Header.

table_get_name(1, _) -> <<":authority">>;
table_get_name(2, _) -> <<":method">>;
table_get_name(3, _) -> <<":method">>;
table_get_name(4, _) -> <<":path">>;
table_get_name(5, _) -> <<":path">>;
table_get_name(6, _) -> <<":scheme">>;
table_get_name(7, _) -> <<":scheme">>;
table_get_name(8, _) -> <<":status">>;
table_get_name(9, _) -> <<":status">>;
table_get_name(10, _) -> <<":status">>;
table_get_name(11, _) -> <<":status">>;
table_get_name(12, _) -> <<":status">>;
table_get_name(13, _) -> <<":status">>;
table_get_name(14, _) -> <<":status">>;
table_get_name(15, _) -> <<"accept-charset">>;
table_get_name(16, _) -> <<"accept-encoding">>;
table_get_name(17, _) -> <<"accept-language">>;
table_get_name(18, _) -> <<"accept-ranges">>;
table_get_name(19, _) -> <<"accept">>;
table_get_name(20, _) -> <<"access-control-allow-origin">>;
table_get_name(21, _) -> <<"age">>;
table_get_name(22, _) -> <<"allow">>;
table_get_name(23, _) -> <<"authorization">>;
table_get_name(24, _) -> <<"cache-control">>;
table_get_name(25, _) -> <<"content-disposition">>;
table_get_name(26, _) -> <<"content-encoding">>;
table_get_name(27, _) -> <<"content-language">>;
table_get_name(28, _) -> <<"content-length">>;
table_get_name(29, _) -> <<"content-location">>;
table_get_name(30, _) -> <<"content-range">>;
table_get_name(31, _) -> <<"content-type">>;
table_get_name(32, _) -> <<"cookie">>;
table_get_name(33, _) -> <<"date">>;
table_get_name(34, _) -> <<"etag">>;
table_get_name(35, _) -> <<"expect">>;
table_get_name(36, _) -> <<"expires">>;
table_get_name(37, _) -> <<"from">>;
table_get_name(38, _) -> <<"host">>;
table_get_name(39, _) -> <<"if-match">>;
table_get_name(40, _) -> <<"if-modified-since">>;
table_get_name(41, _) -> <<"if-none-match">>;
table_get_name(42, _) -> <<"if-range">>;
table_get_name(43, _) -> <<"if-unmodified-since">>;
table_get_name(44, _) -> <<"last-modified">>;
table_get_name(45, _) -> <<"link">>;
table_get_name(46, _) -> <<"location">>;
table_get_name(47, _) -> <<"max-forwards">>;
table_get_name(48, _) -> <<"proxy-authenticate">>;
table_get_name(49, _) -> <<"proxy-authorization">>;
table_get_name(50, _) -> <<"range">>;
table_get_name(51, _) -> <<"referer">>;
table_get_name(52, _) -> <<"refresh">>;
table_get_name(53, _) -> <<"retry-after">>;
table_get_name(54, _) -> <<"server">>;
table_get_name(55, _) -> <<"set-cookie">>;
table_get_name(56, _) -> <<"strict-transport-security">>;
table_get_name(57, _) -> <<"transfer-encoding">>;
table_get_name(58, _) -> <<"user-agent">>;
table_get_name(59, _) -> <<"vary">>;
table_get_name(60, _) -> <<"via">>;
table_get_name(61, _) -> <<"www-authenticate">>;
table_get_name(Index, #state{dyn_table=DynamicTable}) ->
	{_, {Name, _}} = lists:nth(Index - 61, DynamicTable),
	Name.

table_insert(Entry = {Name, Value}, State=#state{size=Size, max_size=MaxSize, dyn_table=DynamicTable}) ->
	EntrySize = byte_size(Name) + byte_size(Value) + 32,
	if
		EntrySize + Size =< MaxSize ->
			%% Add entry without eviction
			State#state{size=Size + EntrySize, dyn_table=[{EntrySize, Entry}|DynamicTable]};
		EntrySize =< MaxSize ->
			%% Evict, then add entry
			{DynamicTable2, Size2} = table_resize(DynamicTable, MaxSize - EntrySize, 0, []),
			State#state{size=Size2 + EntrySize, dyn_table=[{EntrySize, Entry}|DynamicTable2]};
		EntrySize > MaxSize ->
			%% "an attempt to add an entry larger than the
			%% maximum size causes the table to be emptied
			%% of all existing entries and results in an
			%% empty table" (RFC 7541, 4.4)
			State#state{size=0, dyn_table=[]}
	end.

table_resize([], _, Size, Acc) ->
	{lists:reverse(Acc), Size};
table_resize([{EntrySize, _}|_], MaxSize, Size, Acc) when Size + EntrySize > MaxSize ->
	{lists:reverse(Acc), Size};
table_resize([Entry = {EntrySize, _}|Tail], MaxSize, Size, Acc) ->
	table_resize(Tail, MaxSize, Size + EntrySize, [Entry|Acc]).

table_update_size(0, State) ->
	State#state{size=0, max_size=0, dyn_table=[]};
table_update_size(MaxSize, State=#state{size=CurrentSize})
		when CurrentSize =< MaxSize ->
	State#state{max_size=MaxSize};
table_update_size(MaxSize, State=#state{dyn_table=DynTable}) ->
	{DynTable2, Size} = table_resize(DynTable, MaxSize, 0, []),
	State#state{size=Size, max_size=MaxSize, dyn_table=DynTable2}.
