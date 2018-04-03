#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/hackney/ebin  -pa ../_build/default/lib/*/ebin  -pa ../_build/default/lib/certifi/ebin  -pa ../_build/default/lib/idna/ebin  -pa ../_build/default/lib/metrics/ebin -pa ../_build/default/lib/mimerl/ebin   -pa ../_build/default/lib/ssl_verify_fun/ebin -pa ../_build/default/lib/unicode_util_compat/ebin

-module(test1).



main(_) ->
    application:ensure_all_started(hackney),

    {ok, _, Headers, Ref} = hackney:request(get,
                                            <<"https://github.com/benoitc">>,
                                         [], <<>>, [{pool, default}]),
    io:format("got header:~n ~p~n~n", [Headers]),
    io:format("cookies:~n ~p~n~n", [hackney:cookies(Headers)]),

    {ok, Body} = hackney:body(Ref),
    io:format("body:~n ~p~n~n", [Body]),

    IsClosed = hackney_manager:get_state(Ref) =:= req_not_found,
    io:format("has been closed: ~p~n", [IsClosed]).
