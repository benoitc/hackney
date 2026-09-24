%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2026 Benoit Chesneau
%%%
%%% @doc Connection-specific headers are dropped from HTTP/3 requests.
%%%
%%% RFC 9114 4.2 bans them, as RFC 9113 8.2.2 does for HTTP/2, and
%%% hackney_conn:normalize_headers/1 is shared by both protocols. The
%%% HTTP/2 side is covered by hackney_http2_connection_headers_tests; these
%%% pin the HTTP/3 side, so narrowing that filter to HTTP/2 cannot pass
%%% unnoticed. The server reports the headers it received on /headers.
-module(hackney_http3_connection_headers_tests).

-include_lib("eunit/include/eunit.hrl").

connection_headers_test_() ->
    {setup,
     fun hackney_h3_test_server:start/0,
     fun hackney_h3_test_server:stop/1,
     fun(Server) ->
         [{"a request carrying Connection: keep-alive still succeeds over h3",
           {timeout, 30, fun() -> keep_alive_header_is_dropped(Server) end}},
          {"every connection-specific header is dropped, whatever the casing",
           {timeout, 30, fun() -> all_connection_headers_are_dropped(Server) end}},
          {"ordinary headers are still delivered",
           {timeout, 30, fun() -> ordinary_headers_survive(Server) end}}]
     end}.

keep_alive_header_is_dropped(Server) ->
    Received = request(Server, [{<<"Connection">>, <<"keep-alive">>}]),
    ?assertEqual([], connection_specific(Received)).

all_connection_headers_are_dropped(Server) ->
    Received = request(Server, [{<<"Connection">>, <<"keep-alive">>},
                                {<<"keep-alive">>, <<"timeout=5">>},
                                {<<"Proxy-Connection">>, <<"keep-alive">>},
                                {<<"Transfer-Encoding">>, <<"chunked">>},
                                {<<"UPGRADE">>, <<"websocket">>}]),
    ?assertEqual([], connection_specific(Received)).

ordinary_headers_survive(Server) ->
    Received = request(Server, [{<<"Connection">>, <<"keep-alive">>},
                                {<<"X-Api-Version">>, <<"4">>},
                                {<<"Accept">>, <<"application/json">>}]),
    ?assertEqual([], connection_specific(Received)),
    ?assertEqual(<<"4">>, proplists:get_value(<<"x-api-version">>, Received)),
    ?assertEqual(<<"application/json">>,
                 proplists:get_value(<<"accept">>, Received)).

%%====================================================================
%% Helpers
%%====================================================================

%% Ask the server which headers reached it, as name/value pairs.
request(Server, Headers) ->
    URL = hackney_h3_test_server:url(Server, <<"/headers">>),
    Opts = [{with_body, true} | hackney_h3_test_server:hackney_opts()],
    {ok, 200, _RespHeaders, Body} = hackney:request(get, URL, Headers, <<>>, Opts),
    parse_headers(Body).

parse_headers(Body) ->
    [split_header(Line) || Line <- binary:split(Body, <<"\n">>, [global]),
                           Line =/= <<>>].

split_header(Line) ->
    [Name, Value] = binary:split(Line, <<": ">>),
    {Name, Value}.

connection_specific(Headers) ->
    Banned = [<<"connection">>, <<"keep-alive">>, <<"proxy-connection">>,
              <<"transfer-encoding">>, <<"upgrade">>],
    [Name || {Name, _} <- Headers, lists:member(Name, Banned)].
