#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/hackney/ebin  -pa ../_build/default/lib/*/ebin  -pa ../_build/default/lib/certifi/ebin  -pa ../_build/default/lib/idna/ebin  -pa ../_build/default/lib/metrics/ebin -pa ../_build/default/lib/mimerl/ebin   -pa ../_build/default/lib/ssl_verify_fun/ebin -pa ../_build/default/lib/unicode_util_compat/ebin

-module(test_async).


loop(Ref) ->
    receive
        {hackney_response, Ref, {status, StatusInt, Reason}} ->
            io:format("got status: ~p with reason ~p~n", [StatusInt,
                                                          Reason]),
            loop(Ref);
        {hackney_response, Ref, {headers, Headers}} ->
            io:format("got headers: ~p~n", [Headers]),
            loop(Ref);
        {hackney_response, Ref, done} ->
            ok;
        {hackney_response, Ref, Bin} ->
            io:format("got chunk: ~p~n", [Bin]),
            loop(Ref);

        Else ->
            io:format("else ~p~n", [Else]),
            ok
    end.


main(_) ->
    application:ensure_all_started(hackney),

    Url = <<"https://httparrot.herokuapp.com/get">>,
    Opts = [async],
    {ok, Ref} = hackney:get(Url, [], <<>>, Opts),
    loop(Ref).
