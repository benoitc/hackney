#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./deps/mimetypes/ebin

-module(test2).

main(_) ->
    hackney:start(),
    ReqBody = << "{
          \"id\": \"some_paste_id2\",
          \"rev\": \"some_revision_id\",
          \"changeset\": \"changeset in unidiff format\"
}" >>,
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    Path = <<"https://friendpaste.com/">>,
    Method = post,
    {ok, Client} = hackney:request(Method, Path, ReqHeaders, stream,
                                   []),

    {ok, Client1} = hackney:stream_request_body(ReqBody, Client),
    {ok, _Status, _Headers, Client2} = hackney:start_response(Client1),
    {ok, Body, Client3} = hackney:body(Client2),
    hackney:close(Client3),
    io:format(" ** got length ~p~n ** body: ~p~n", [byte_size(Body), Body]).
