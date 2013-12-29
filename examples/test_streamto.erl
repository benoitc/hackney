#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

-module(test_streamto).


wait_response(Parent) ->
    receive
        {hackney_response, _Ref, {status, StatusInt, Reason}} ->
            io:format("got status: ~p with reason ~p~n", [StatusInt,
                                                          Reason]),
            wait_response(Parent);
        {hackney_response,_Ref, {headers, Headers}} ->
            io:format("got headers: ~p~n", [Headers]),
            wait_response(Parent);
        {hackney_response,Ref, done} ->
            Parent ! {Ref, done},
            ok;
        {hackney_response, _Ref, Bin} ->
            io:format("got chunk: ~p~n", [Bin]),
            wait_response(Parent);

        Else ->
            io:format("else ~p~n", [Else]),
            ok
    end.

main(_) ->
    hackney:start(),
    Self = self(),
    Pid = spawn(fun() -> wait_response(Self) end),
    Url = <<"https://friendpaste.com/_all_languages">>,
    Opts = [async, {stream_to, Pid}],
    {ok, Ref} = hackney:get(Url, [], <<>>, Opts),
    receive
        {Ref, done} -> ok
    end.
