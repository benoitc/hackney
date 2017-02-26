-module(hackney_headers_tests).
-include_lib("eunit/include/eunit.hrl").


%% ... hm?

new_test() ->
    D = dict:new(),
    ?assertEqual(hackney_headers:new(), D).

to_list_test() ->
    A = [{<<"Foo">>, <<"Bar">>}],
    ?assertEqual(hackney_headers:to_list(hackney_headers:new(A)),A).

update_test() ->
    A = [{<<"Foo">>, <<"Bar">>}],
    B = [{<<"Bar">>, <<"Baz">>}],
    Ha = hackney_headers:new(A),
    Hb = hackney_headers:new(B),
    ?assertEqual(hackney_headers:update(Ha, B),
		 hackney_headers:update(Hb, A)).

get_value_test()->
    A =  <<"a">>,
    B =  <<"b">>,
    C =  <<"c">>,
    V =  <<"V">>,
    Ha = hackney_headers:new([{A,V},{B,V},{C,V}]),
    ?assertEqual(hackney_headers:get_value(A,Ha),V),
    ?assertEqual(hackney_headers:get_value(B,Ha),V),
    ?assertEqual(hackney_headers:get_value(C,Ha),V).

header_multiple_test() ->
  HList = [{<<"a">>, <<"1">>},
           {<<"x-a">>, <<"a, b,c">>},
           {<<"x-a">>, <<"e,f, g">>}],
  Headers = hackney_headers:new(HList),
  Expected = <<"a: 1\r\nx-a: a, b,c, e,f, g\r\n\r\n">>,
  ?assertEqual(Expected, hackney_headers:to_binary(Headers)).

headers_update_with_multiple_test() ->
  HList0 = [{<<"a">>, <<"1">>}],
  HList1 = [{<<"x-a">>, <<"a, b,c">>},
            {<<"x-a">>, <<"e,f, g">>}],
  Headers = hackney_headers:update(
              hackney_headers:new(HList0),
              HList1
             ),
  Expected = <<"a: 1\r\nx-a: a, b,c, e,f, g\r\n\r\n">>,
  ?assertEqual(Expected, hackney_headers:to_binary(Headers)).

authorization_header_test() ->
    Auth = {<<"Aladdin">>,<<"open sesame">>},
    AuthValue = <<"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==">>,
    Headers = hackney_headers:new([{<<"Authorization">>,AuthValue}]),
    ?assertEqual({<<"basic">>, Auth}, hackney_headers:parse(<<"authorization">>, Headers)).
