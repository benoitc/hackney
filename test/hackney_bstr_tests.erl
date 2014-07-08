-module(hackney_bstr_tests).
-include_lib("eunit/include/eunit.hrl").

to_binary_test_() ->
    %% {Value, Result}.
    Tests = [
            {"string", <<"string">>},
            {123, <<"123">>},
            {string, <<"string">>},
            {<<"string">>, <<"string">>}
            ],
    [{R, fun() -> R = hackney_bstr:to_binary(V) end} || {V, R} <- Tests].

to_lower_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>, <<"abcdefghijklmnopqrstuvwxyz">>},
            {<<"~çÇ">>, <<"~çÇ">>}
            ],
    [{V, fun() -> R = hackney_bstr:to_lower(V) end} || {V, R} <- Tests].

to_upper_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"abcdefghijklmnopqrstuvwxyz">>, <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>},
            {<<"~çÇ">>, <<"~çÇ">>}
            ],
    [{V, fun() -> R = hackney_bstr:to_upper(V) end} || {V, R} <- Tests].

char_to_lower_test_() ->
    %% {Value, Result}.
    Tests = [
            {$A, $a},
            {$Ç, $Ç}
            ],
    [{<<V>>, fun() -> R = hackney_bstr:char_to_lower(V) end} || {V, R} <- Tests].

char_to_upper_test_() ->
    %% {Value, Result}.
    Tests = [
            {$a, $A},
            {$', $'}
            ],
    [{<<V>>, fun() -> R = hackney_bstr:char_to_upper(V) end} || {V, R} <- Tests].

join_test_() ->
    %% {{Value, Separator}, Result}.
    Tests = [
            {{[<<"a">>, <<"b">>], <<$,>>}, <<"a,b">>},
            {{[<<"a">>, <<"b">>], <<$->>}, <<"a-b">>}
            ],
    [{R, fun() -> R = hackney_bstr:join(V, S) end} || {{V, S}, R} <- Tests].

to_hex_test_() ->
    %% {Value, Result}.
    Tests = [
            {[1, 2, 3, 4, 5, 6, 7, 9, 0, 10, 11, 12, 13, 14, 15, 16], "0102030405060709000a0b0c0d0e0f10"},
            {"hackney_lib", "6861636b6e65795f6c6962"},
            {<<"hackney_lib">>, "6861636b6e65795f6c6962"}
            ],
    [{R, fun() -> R = hackney_bstr:to_hex(V) end} || {V, R} <- Tests].

token_ci_test_() ->
    F = fun(X, Acc) -> <<X/binary, Acc/binary>> end,
    %% {Value, Result}.
    Tests = [
            {<<"ABCDEF">>, <<"abcdef">>},
            {<<"(ABCDEF)">>, <<"(ABCDEF)">>}
            ],
    [{R, fun() -> R = hackney_bstr:token_ci(V, F) end} || {V, R} <- Tests].

token_test_() ->
    F = fun(X, Acc) -> <<X/binary, Acc/binary>> end,
    %% {Value, Result}.
    Tests = [
            {<<"ABCDEF">>, <<"ABCDEF">>},
            {<<"(ABCDEF)">>, <<"(ABCDEF)">>}
            ],
    [{R, fun() -> R = hackney_bstr:token(V, F) end} || {V, R} <- Tests].

params_test_() ->
    F = fun(X, Acc) -> [X | Acc] end,
    %% {Value, Result}.
    Tests = [
            {<<";c=d">>, [<<>>,{<<"c">>,<<"d">>}]},
            {<<";a=b;c=d">>, [<<>>,{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]},
            {<<";ab">>, {error, badarg}},
            {<<";a=b;">>, {error, badarg}},
            {<<" ;a=b;c=d">>, [<<>>,{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]}
            ],
    [{V, fun() -> R = hackney_bstr:params(V, F) end} || {V, R} <- Tests].

parameterized_tokens_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"a;c;d">>,[{<<"a">>,[<<"c">>,<<"d">>]}]},
            {<<"a;b=d;c;d">>, [{<<"a">>,[{<<"b">>,<<"d">>},<<"c">>,<<"d">>]}]},
            {<<";a;c;d;e;f">>, {error, badarg}}
            ],
    [{V, fun() -> R = hackney_bstr:parameterized_tokens(V) end} || {V, R} <- Tests].

whitespace_test_() ->
    F = fun(R) -> R end,
    %% {Value, Result}.
    Tests = [
            {<<" ABCDEF">>, <<"ABCDEF">>},
            {<<"\tABCDEF">>, <<"ABCDEF">>}
            ],
    [{V, fun() -> R = hackney_bstr:whitespace(V, F) end} || {V, R} <- Tests].

digits_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"13349">>, 13349},
            {<<"0">>, 0},
            {<<"-13349">>, {error, badarg}}
            ],
    [{V, fun() -> R = hackney_bstr:digits(V) end} || {V, R} <- Tests].

alpha_test_() ->
    F = fun(X, Acc) -> <<X/binary, Acc/binary>> end,
    %% {Value, Result}.
    Tests = [
            {<<"abcdef">>, <<"abcdef">>},
            {<<"ABCDEF">>, <<"abcdef">>},
            {<<"123ABCDEF">>, <<"123ABCDEF">>}
            ],
    [{V, fun() -> R = hackney_bstr:alpha(V, F) end} || {V, R} <- Tests].

word_test_() ->
    F = fun(X, Acc) -> <<Acc/binary, X/binary>> end,
    %% {Value, Result}.
    Tests = [
            {<<"\"ABCDEF\"">>, <<"ABCDEF">>},
            {<<"\\ABCDEF\\">>, {error, badarg}},
            {<<"\"ABC\" \"DEF\"">>, <<"ABC \"DEF\"">>}
            ],
    [{V, fun() -> R = hackney_bstr:word(V, F) end} || {V, R} <- Tests].
