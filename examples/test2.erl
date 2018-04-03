#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/hackney/ebin  -pa ../_build/default/lib/*/ebin  -pa ../_build/default/lib/certifi/ebin  -pa ../_build/default/lib/idna/ebin  -pa ../_build/default/lib/metrics/ebin -pa ../_build/default/lib/mimerl/ebin   -pa ../_build/default/lib/ssl_verify_fun/ebin -pa ../_build/default/lib/unicode_util_compat/ebin

-module(test2).

main(_) ->
    application:ensure_all_started(hackney),
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
