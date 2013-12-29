#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

-module(test_async).

finish_request(Client) ->
    {ok, Body} = hackney:body(Client),
    io:format("got body: ~p~n~n", [Body]).

loop(Ref) ->
    io:format("Received in the loop~n", []),
    hackney:stream_next(Ref),

    receive
        {hackney_response, Ref, {headers, Headers}} ->
            io:format("got headers: ~p~n~n", [Headers]),
            io:format("Stop asynchronous fetching ~n~n", []),
            {ok, Ref} = hackney:stop_async(Ref),
            %% make sure the stream has been unregistered
            IsNotAsync = {error, req_not_async} =:= hackney_manager:async_response_pid(Ref),
            io:format("request is not synchronous: ~p~n",
                      [IsNotAsync]),
            finish_request(Ref);
        Else ->
            io:format("else ~p~n", [Else]),
            ok
    end.

main(_) ->
    hackney:start(),

    Url = <<"https://friendpaste.com/_all_languages">>,
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
