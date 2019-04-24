%%% -*- erlang -*-
%%%
%%% This file is part of hackney_lib released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(hackney_headers_new_test).
-author("benoitc").

-include_lib("eunit/include/eunit.hrl").

from_list_test() ->
  Headers = [
    {<<"d">>, <<"e">>},
    {<<"a">>, <<"b">>},
    {<<"c">>, <<"b">>}
  ],
  ?assertEqual(Headers, hackney_headers_new:to_list(hackney_headers_new:from_list(Headers))).

key_type_test() ->
  HeadersList = [
    {<<"d">>, <<"e">>},
    {a, <<"b">>},
    {"c", <<"f">>}
  ],
  Headers = hackney_headers_new:from_list(HeadersList),
  ?assertEqual(HeadersList, hackney_headers_new:to_list(Headers)),
  ?assertEqual(<<"e">>, hackney_headers_new:get_value(<<"d">>, Headers)),
  ?assertEqual(<<"e">>, hackney_headers_new:get_value(d, Headers)),
  ?assertEqual(<<"e">>, hackney_headers_new:get_value("d", Headers)),
  ?assertEqual(<<"b">>, hackney_headers_new:get_value(a, Headers)),
  ?assertEqual(<<"b">>, hackney_headers_new:get_value(<<"a">>, Headers)),
  ?assertEqual(<<"b">>, hackney_headers_new:get_value("a", Headers)),
  ?assertEqual(<<"f">>, hackney_headers_new:get_value("c", Headers)),
  ?assertEqual(<<"f">>, hackney_headers_new:get_value(<<"c">>, Headers)),
  ?assertEqual(<<"f">>, hackney_headers_new:get_value(c, Headers)).
  


store_test() ->
  A = [{<<"Foo">>, <<"Bar">>}],
  B = [{<<"Bar">>, <<"Baz">>}],
  C = [{<<"Baz">>, <<"Bar">>}],
  Ha = hackney_headers_new:from_list(A),
  Hb = hackney_headers_new:from_list(B),
  Hc = hackney_headers_new:from_list(C),

  Expected1 = [{<<"Foo">>, <<"Bar">>}, {<<"Bar">>, <<"Baz">>}],
  Expected2 = [{<<"Bar">>, <<"Baz">>}, {<<"Foo">>, <<"Bar">>}],
  Expected3 = [{<<"Baz">>, <<"Bar">>},
               {<<"Foo">>, <<"Bar">>},
               {<<"Foo">>, <<"Baz">>}],  
  
  ?assertEqual(
    Expected1,
    hackney_headers_new:to_list(hackney_headers_new:store(<<"Bar">>, <<"Baz">>, Ha))
  ),
  ?assertEqual(
    Expected2,
    hackney_headers_new:to_list(hackney_headers_new:store(<<"Foo">>, <<"Bar">>, Hb))
  ),
  ?assertEqual(
    Expected3,
    hackney_headers_new:to_list(hackney_headers_new:store(<<"Foo">>,
                                                          [<<"Bar">>, <<"Baz">>],
                                                          Hc))
  ).

merge_test() ->
  A = [{<<"Foo">>, <<"Bar">>}],
  B = [{<<"Bar">>, <<"Baz">>}, {<<"Foo">>, <<"Bar">>}],
  Ha = hackney_headers_new:from_list(A),
  Hb = hackney_headers_new:from_list(B),
  
  Expected1 = [{<<"Foo">>, <<"Bar">>}, {<<"Bar">>, <<"Baz">>}],
  ?assertEqual(
    Expected1,
    hackney_headers_new:to_list(hackney_headers_new:merge(Ha, Hb))
  ).

header_multiple_test() ->
  HList = [{<<"a">>, <<"1">>},
           {<<"x-a">>, <<"a, b,c">>},
           {<<"X-a">>, <<"e,f, g">>}],
  Headers = hackney_headers_new:from_list(HList),
  ?assertEqual(HList, hackney_headers_new:to_list(Headers)),
  ?assertEqual(
    << "a: 1\r\nx-a: a, b,c\r\nX-a: e,f, g\r\n\r\n" >>,
    hackney_headers_new:to_binary(Headers)
  ).

lookup_test() ->
  HList = [{<<"a">>, <<"1">>},
           {<<"x-a">>, <<"a, b,c">>},
           {<<"X-a">>, <<"e,f, g">>}],
  
  Headers = hackney_headers_new:from_list(HList),
  ?assertEqual([{<<"a">>, <<"1">>}], hackney_headers_new:lookup(<<"a">>, Headers)),
  ?assertEqual([{<<"x-a">>, <<"a, b,c">>},
                {<<"X-a">>, <<"e,f, g">>}], hackney_headers_new:lookup(<<"x-a">>, Headers)).
  