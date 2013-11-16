%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_util).

-export([is_ipv6/1,
         to_binary/1,
         to_lower/1, to_upper/1,
         char_to_lower/1, char_to_upper/1,
         join/2,
         to_hex/1,
         token_ci/2, token/2,
         content_type/1]).


is_ipv6(Host) ->
    case inet_parse:address(Host) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            true;
        {ok, {_, _, _, _}} ->
            false;
        _ ->
            case inet:getaddr(Host, inet) of
                {ok, _} ->
                    false;
                _ ->
                    case inet:getaddr(Host, inet6) of
                        {ok, _} ->
                            true;
                        _ ->
                            false
                    end
            end
    end.


to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
to_binary(V) when is_binary(V) ->
    V.

%% @doc Convert a binary string to lowercase.
-spec to_lower(binary()) -> binary().
to_lower(L) ->
	<< << (char_to_lower(C)) >> || << C >> <= L >>.

to_upper(U) ->
    << << (char_to_upper(C)) >> || << C >> <= U >>.


%% @doc Convert [A-Z] characters to lowercase.
%% @end
%% We gain noticeable speed by matching each value directly.
-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.

%% @doc Convert [a-z] characters to uppercase.
-spec char_to_upper(char()) -> char().
char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(Ch) -> Ch.

join([], _Separator) ->
    <<>>;
join([S], _separator) ->
    S;
join(L, Separator) ->
    iolist_to_binary(join(lists:reverse(L), Separator, [])).

join([], _Separator, Acc) ->
    Acc;
join([S | Rest], Separator, []) ->
    join(Rest, Separator, [S]);
join([S | Rest], Separator, Acc) ->
    join(Rest, Separator, [S, Separator | Acc]).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.



%% @doc Parse a case-insensitive token.
%%
%% Changes all characters to lowercase.
-spec token_ci(binary(), fun()) -> any().
token_ci(Data, Fun) ->
	token(Data, Fun, ci, <<>>).

%% @doc Parse a token.
-spec token(binary(), fun()) -> any().
token(Data, Fun) ->
	token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) ->
	Fun(<<>>, Acc);
token(Data = << C, _Rest/binary >>, Fun, _Case, Acc)
		when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
			 C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
			 C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
			 C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
			 C < 32; C =:= 127 ->
	Fun(Data, Acc);
token(<< C, Rest/binary >>, Fun, Case = ci, Acc) ->
	C2 = char_to_lower(C),
	token(Rest, Fun, Case, << Acc/binary, C2 >>);
token(<< C, Rest/binary >>, Fun, Case, Acc) ->
	token(Rest, Fun, Case, << Acc/binary, C >>).


%% get content type
%%
%%
content_type(Name) ->
    case mimetypes:filename(Name) of
        [CT | _] ->
            CT;
        CT when is_binary(CT) ->
            CT
    end.
