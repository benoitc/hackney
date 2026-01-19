%%% -*- erlang -*-
%%%
%%% This file is part of hackney_lib released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(hackney_headers_test).
-author("benoitc").

-include_lib("eunit/include/eunit.hrl").

from_list_test() ->
  Headers = [
    {<<"d">>, <<"e">>},
    {<<"a">>, <<"b">>},
    {<<"c">>, <<"b">>}
  ],
  ?assertEqual(Headers, hackney_headers:to_list(hackney_headers:from_list(Headers))).

key_type_test() ->
  HeadersList = [
    {<<"d">>, <<"e">>},
    {a, <<"b">>},
    {"c", <<"f">>}
  ],
  Headers = hackney_headers:from_list(HeadersList),
  ?assertEqual(HeadersList, hackney_headers:to_list(Headers)),
  ?assertEqual(<<"e">>, hackney_headers:get_value(<<"d">>, Headers)),
  ?assertEqual(<<"e">>, hackney_headers:get_value(d, Headers)),
  ?assertEqual(<<"e">>, hackney_headers:get_value("d", Headers)),
  ?assertEqual(<<"b">>, hackney_headers:get_value(a, Headers)),
  ?assertEqual(<<"b">>, hackney_headers:get_value(<<"a">>, Headers)),
  ?assertEqual(<<"b">>, hackney_headers:get_value("a", Headers)),
  ?assertEqual(<<"f">>, hackney_headers:get_value("c", Headers)),
  ?assertEqual(<<"f">>, hackney_headers:get_value(<<"c">>, Headers)),
  ?assertEqual(<<"f">>, hackney_headers:get_value(c, Headers)).
  


store_test() ->
  A = [{<<"Foo">>, <<"Bar">>}],
  B = [{<<"Bar">>, <<"Baz">>}],
  C = [{<<"Baz">>, <<"Bar">>}],
  Ha = hackney_headers:from_list(A),
  Hb = hackney_headers:from_list(B),
  Hc = hackney_headers:from_list(C),

  Expected1 = [{<<"Foo">>, <<"Bar">>}, {<<"Bar">>, <<"Baz">>}],
  Expected2 = [{<<"Bar">>, <<"Baz">>}, {<<"Foo">>, <<"Bar">>}],
  Expected3 = [{<<"Baz">>, <<"Bar">>},
               {<<"Foo">>, <<"Bar">>},
               {<<"Foo">>, <<"Baz">>}],  
  
  ?assertEqual(
    Expected1,
    hackney_headers:to_list(hackney_headers:store(<<"Bar">>, <<"Baz">>, Ha))
  ),
  ?assertEqual(
    Expected2,
    hackney_headers:to_list(hackney_headers:store(<<"Foo">>, <<"Bar">>, Hb))
  ),
  ?assertEqual(
    Expected3,
    hackney_headers:to_list(hackney_headers:store(<<"Foo">>,
                                                          [<<"Bar">>, <<"Baz">>],
                                                          Hc))
  ).

merge_test() ->
  A = [{<<"Foo">>, <<"Bar">>}],
  B = [{<<"Bar">>, <<"Baz">>}, {<<"Foo">>, <<"Bar">>}],
  Ha = hackney_headers:from_list(A),
  Hb = hackney_headers:from_list(B),
  
  Expected1 = [{<<"Foo">>, <<"Bar">>}, {<<"Bar">>, <<"Baz">>}],
  ?assertEqual(
    Expected1,
    hackney_headers:to_list(hackney_headers:merge(Ha, Hb))
  ).

header_multiple_test() ->
  HList = [{<<"a">>, <<"1">>},
           {<<"x-a">>, <<"a, b,c">>},
           {<<"X-a">>, <<"e,f, g">>}],
  Headers = hackney_headers:from_list(HList),
  ?assertEqual(HList, hackney_headers:to_list(Headers)),
  ?assertEqual(
    << "a: 1\r\nx-a: a, b,c\r\nX-a: e,f, g\r\n\r\n" >>,
    hackney_headers:to_binary(Headers)
  ).

lookup_test() ->
  HList = [{<<"a">>, <<"1">>},
           {<<"x-a">>, <<"a, b,c">>},
           {<<"X-a">>, <<"e,f, g">>}],

  Headers = hackney_headers:from_list(HList),
  ?assertEqual([{<<"a">>, <<"1">>}], hackney_headers:lookup(<<"a">>, Headers)),
  ?assertEqual([{<<"x-a">>, <<"a, b,c">>},
                {<<"X-a">>, <<"e,f, g">>}], hackney_headers:lookup(<<"x-a">>, Headers)).

%% Test that newlines in header values are sanitized (issue #506)
%% This prevents HTTP header injection attacks
header_injection_sanitization_test() ->
  %% Header with newline in value - should be stripped
  HList1 = [{<<"Custom-Header">>, <<"Value\n">>}],
  Headers1 = hackney_headers:from_list(HList1),
  Binary1 = hackney_headers:to_binary(Headers1),
  ?assertEqual(<<"Custom-Header: Value\r\n\r\n">>, Binary1),

  %% Header with CR+LF injection attempt - should be stripped
  HList2 = [{<<"Custom-Header">>, <<"Value\r\nInjected-Header: malicious">>}],
  Headers2 = hackney_headers:from_list(HList2),
  Binary2 = hackney_headers:to_binary(Headers2),
  ?assertEqual(<<"Custom-Header: ValueInjected-Header: malicious\r\n\r\n">>, Binary2),

  %% Header with only CR - should be stripped
  HList3 = [{<<"Custom-Header">>, <<"Value\rMore">>}],
  Headers3 = hackney_headers:from_list(HList3),
  Binary3 = hackney_headers:to_binary(Headers3),
  ?assertEqual(<<"Custom-Header: ValueMore\r\n\r\n">>, Binary3),

  %% Multiple headers, one with injection attempt
  HList4 = [{<<"Normal-Header">>, <<"normal">>},
            {<<"Bad-Header">>, <<"value\r\n\r\nHTTP/1.1 200 OK">>}],
  Headers4 = hackney_headers:from_list(HList4),
  Binary4 = hackney_headers:to_binary(Headers4),
  ?assertEqual(<<"Normal-Header: normal\r\nBad-Header: valueHTTP/1.1 200 OK\r\n\r\n">>, Binary4).
  