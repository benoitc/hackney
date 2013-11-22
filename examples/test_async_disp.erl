#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

-module(test_async).


loop(StreamRef) ->
    receive
        {StreamRef, {status, StatusInt, Reason}} ->
            io:format("got status: ~p with reason ~p~n", [StatusInt,
                                                          Reason]),
            loop(StreamRef);
        {StreamRef, {headers, Headers}} ->
            io:format("got headers: ~p~n", [Headers]),
            loop(StreamRef);
        {StreamRef, done} ->
            ok;
        {StreamRef, Bin} ->
            io:format("got chunk: ~p~n", [Bin]),
            loop(StreamRef);

        Else ->
            io:format("else ~p~n", [Else]),
            ok
    end.


main(_) ->
    hackney:start(hackney_disp),

    Url = <<"https://friendpaste.com/_all_languages">>,
    Opts = [async],
    {ok, {response_stream, StreamRef}} = hackney:get(Url, [], <<>>, Opts),
    loop(StreamRef).
