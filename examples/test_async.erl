#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

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
    hackney:start(),

    Url = <<"https://friendpaste.com/_all_languages">>,
    Opts = [async],
    {ok, Ref} = hackney:get(Url, [], <<>>, Opts),
    loop(Ref).
