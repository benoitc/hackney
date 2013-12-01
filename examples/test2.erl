#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

-module(test2).

main(_) ->
    hackney:start(),
    ReqBody = << "{\"snippet\": \"test2.erl\" }" >>,

    io:format("Req body ~p~n", [ReqBody]),

    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>},
                  {<<"Content-Length">>, size(ReqBody)}],
    Path = <<"https://friendpaste.com">>,
    Method = post,
    io:format("start request ~n", []),
    {ok, Ref} = hackney:request(Method, Path, ReqHeaders, stream,
                                   []),

    io:format("~nwant to send body ~n", []),
    ok = hackney:send_body(Ref, ReqBody),

    io:format("fetch response~n", []),
    {ok, _Status, _Headers, Ref} = hackney:start_response(Ref),
    {ok, Body} = hackney:body(Ref),
    io:format(" ** got length ~p~n ** body: ~p~n", [byte_size(Body),
                                                    Body]),
    IsClosed = hackney_manager:get_state(Ref) =:= req_not_found,
    io:format("has been closed: ~p~n", [IsClosed]).
