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

authorization_header_test() ->
    Auth = {<<"Aladdin">>,<<"open sesame">>},
    AuthValue = <<"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==">>,
    Headers = hackney_headers:new([{<<"Authorization">>,AuthValue}]),
    ?assertEqual({<<"basic">>, Auth}, hackney_headers:parse(<<"authorization">>, Headers)).
