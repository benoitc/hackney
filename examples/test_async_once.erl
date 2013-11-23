#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

-module(test_async).


loop(StreamRef) ->
    io:format("Received in the loop~n", []),
    hackney:stream_next(StreamRef),

    receive
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
    hackney:start(),

    Url = <<"https://friendpaste.com/_all_languages">>,
    Opts = [{async, once}],
    {ok, {response_stream, StreamRef}} = hackney:get(Url, [], <<>>, Opts),
    io:format("received once~n", []),
    receive
        {StreamRef, {status, StatusInt, Reason}} ->
            io:format("got status: ~p with reason ~p~n~n", [StatusInt,
                                                          Reason]),

            io:format("Wait for 6secs...~n", []),
            timer:sleep(6000),
            io:format("Continue in the loop~n~n", []),
            loop(StreamRef);
        Else ->
            io:format("else ~p~n", [Else])
    end.
