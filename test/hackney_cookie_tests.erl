-module(hackney_cookie_tests).
-include_lib("eunit/include/eunit.hrl").

parse_cookie_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"name=value; name2=value2">>, [
                    {<<"name">>, <<"value">>},
                    {<<"name2">>, <<"value2">>}
            ]},
            {<<"$Version=1; Customer=WILE_E_COYOTE; $Path=/acme">>, [
                    {<<"Customer">>, <<"WILE_E_COYOTE">>}
            ]},
            {<<"$Version=1; Customer=WILE_E_COYOTE; $Path=/acme; "
               "Part_Number=Rocket_Launcher_0001; $Path=/acme; "
               "Shipping=FedEx; $Path=/acme">>, [
                    {<<"Customer">>, <<"WILE_E_COYOTE">>},
                    {<<"Part_Number">>, <<"Rocket_Launcher_0001">>},
                    {<<"Shipping">>, <<"FedEx">>}
            ]},
            %% Space in value.
            {<<"foo=Thu Jul 11 2013 15:38:43 GMT+0400 (MSK)">>,
             [{<<"foo">>, <<"Thu Jul 11 2013 15:38:43 GMT+0400 (MSK)">>}]},
            %% Comma in params
            {<<"foo=some:value;Path=/;Expires=Fri, 09-Jun-2017 09:32:51 GMT;Secure;HttpOnly;Priority=HIGH">>,
             [{<<"foo">>, <<"some:value">>},
              {<<"Path">>, <<"/">>},
              {<<"Expires">>, <<"Fri, 09-Jun-2017 09:32:51 GMT">>},
              {<<"Secure">>, true},
              {<<"HttpOnly">>, true},
              {<<"Priority">>, <<"HIGH">>}]},
            %% Potential edge cases (initially from Mochiweb).
            {<<"foo=\\x">>, [{<<"foo">>, <<"\\x">>}]},
            {<<"=">>, {error, badarg}},
            {<<" foo ; bar ">>, {error, badarg}},
            {<<"foo=;bar=">>, [{<<"foo">>, <<>>}, {<<"bar">>, <<>>}]},
            {<<"foo=\\\";;bar ">>, {error, badarg}},
            {<<"foo=\\\";;bar=good ">>,
             [{<<"foo">>, <<"\\\"">>}, {<<"bar">>, <<"good">>}]},
            {<<"foo=\"\\\";bar">>, [{<<"foo">>, <<"\"\\\"">>}, {<<"bar">>, true}]},
            {<<>>, []}
            ],
    [{V, ?_assertEqual(R, hackney_cookie:parse_cookie(V))} || {V, R} <- Tests].

setcookie_test_() ->
    %% {Name, Value, Opts, Result}
    Tests = [
            {<<"Customer">>, <<"WILE_E_COYOTE">>,
             [{http_only, true}, {domain, <<"acme.com">>}],
             <<"Customer=WILE_E_COYOTE; Version=1; "
               "Domain=acme.com; HttpOnly">>},
            {<<"Customer">>, <<"WILE_E_COYOTE">>,
             [{path, <<"/acme">>}],
             <<"Customer=WILE_E_COYOTE; Version=1; Path=/acme">>},
            {<<"Customer">>, <<"WILE_E_COYOTE">>,
             [{path, <<"/acme">>}, {badoption, <<"negatory">>}],
             <<"Customer=WILE_E_COYOTE; Version=1; Path=/acme">>}
            ],
    [{R, fun() -> R = iolist_to_binary(hackney_cookie:setcookie(N, V, O)) end}
     || {N, V, O, R} <- Tests].

setcookie_max_age_test() ->
    F = fun(N, V, O) ->
            binary:split(iolist_to_binary(
                    hackney_cookie:setcookie(N, V, O)), <<";">>, [global])
    end,
    [<<"Customer=WILE_E_COYOTE">>,
     <<" Version=1">>,
     <<" Expires=", _/binary>>,
     <<" Max-Age=111">>,
     <<" Secure">>] = F(<<"Customer">>, <<"WILE_E_COYOTE">>,
                        [{max_age, 111}, {secure, true}]),
    case catch F(<<"Customer">>, <<"WILE_E_COYOTE">>, [{max_age, -111}]) of
        {'EXIT', {{case_clause, {max_age, -111}}, _}} -> ok
    end,
    [<<"Customer=WILE_E_COYOTE">>,
     <<" Version=1">>,
     <<" Expires=", _/binary>>,
     <<" Max-Age=86417">>] = F(<<"Customer">>, <<"WILE_E_COYOTE">>,
                               [{max_age, 86417}]),
    ok.

setcookie_failures_test_() ->
    F = fun(N, V) ->
            try hackney_cookie:setcookie(N, V, []) of
                _ ->
                    false
            catch _:_ ->
                    true
            end
    end,
    Tests = [
            {<<"Na=me">>, <<"Value">>},
            {<<"Name;">>, <<"Value">>},
            {<<"\r\name">>, <<"Value">>},
            {<<"Name">>, <<"Value;">>},
            {<<"Name">>, <<"\value">>}
            ],
    [{iolist_to_binary(io_lib:format("{~p, ~p} failure", [N, V])),
      fun() -> true = F(N, V) end}
     || {N, V} <- Tests].
