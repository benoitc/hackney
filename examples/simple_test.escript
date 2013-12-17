#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

-module(test1).



main(_) ->
    hackney:start(),

    {ok, _, Headers, Ref} = hackney:request(get,
                                            <<"https://github.com/benoitc">>,
                                         [], <<>>, [{pool, default}]),
    io:format("got header:~n ~p~n~n", [Headers]),
    io:format("cookies:~n ~p~n~n", [hackney:cookies(Headers)]),

    {ok, Body} = hackney:body(Ref),
    io:format("body:~n ~p~n~n", [Body]),

    IsClosed = hackney_manager:get_state(Ref) =:= req_not_found,
    io:format("has been closed: ~p~n", [IsClosed]).
