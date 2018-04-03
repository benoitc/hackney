#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/hackney/ebin  -pa ../_build/default/lib/*/ebin  -pa ../_build/default/lib/certifi/ebin  -pa ../_build/default/lib/idna/ebin  -pa ../_build/default/lib/metrics/ebin -pa ../_build/default/lib/mimerl/ebin   -pa ../_build/default/lib/ssl_verify_fun/ebin -pa ../_build/default/lib/unicode_util_compat/ebin

-module(test_async).


loop(Ref) ->
    io:format("Received in the loop~n", []),
    ok = hackney:stream_next(Ref),

    receive
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
    Opts = [{async, once}],
    {ok, Ref} = hackney:get(Url, [], <<>>, Opts),
    io:format("received once~n", []),
    receive
        {hackney_response, Ref, {status, StatusInt, Reason}} ->
            io:format("got status: ~p with reason ~p~n~n", [StatusInt,
                                                          Reason]),

            io:format("Wait for 6secs...~n", []),
            timer:sleep(6000),
            io:format("Continue in the loop~n~n", []),
            loop(Ref);
        Else ->
            io:format("else ~p~n", [Else])
    end.
