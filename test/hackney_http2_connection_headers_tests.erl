%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Connection-specific request headers must not reach an HTTP/2 request.
%%%
%%% RFC 9113 8.2.2 forbids connection, keep-alive, proxy-connection,
%%% transfer-encoding and upgrade in HTTP/2. The h2 layer validates outbound
%%% header blocks and replies {error, protocol_error} when one is present, so a
%%% caller that sets any of them - which is legal in HTTP/1.1, and common in
%%% clients written before ALPN could pick h2 - had every request fail.
-module(hackney_http2_connection_headers_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BODY, <<"ok">>).

connection_headers_test_() ->
    {setup,
     fun() ->
         _ = application:ensure_all_started(hackney),
         _ = application:ensure_all_started(h2),
         ok
     end,
     fun(_) -> ok end,
     [{"a request carrying Connection: keep-alive still succeeds over h2",
       {timeout, 30, fun keep_alive_header_is_dropped/0}},
      {"every connection-specific header is dropped, whatever the casing",
       {timeout, 30, fun all_connection_headers_are_dropped/0}},
      {"ordinary headers are still delivered",
       {timeout, 30, fun ordinary_headers_survive/0}}]}.

keep_alive_header_is_dropped() ->
    {Server, Port, Seen} = start_server(),
    try
        ?assertMatch({ok, 200, _, ?BODY},
                     request(Port, [{<<"Connection">>, <<"keep-alive">>}])),
        ?assertEqual([], connection_specific(received_headers(Seen)))
    after
        stop_server(Server)
    end.

all_connection_headers_are_dropped() ->
    {Server, Port, Seen} = start_server(),
    Headers = [{<<"Connection">>, <<"keep-alive">>},
               {<<"keep-alive">>, <<"timeout=5">>},
               {<<"Proxy-Connection">>, <<"keep-alive">>},
               {<<"Transfer-Encoding">>, <<"chunked">>},
               {<<"UPGRADE">>, <<"h2c">>}],
    try
        ?assertMatch({ok, 200, _, ?BODY}, request(Port, Headers)),
        ?assertEqual([], connection_specific(received_headers(Seen)))
    after
        stop_server(Server)
    end.

%% The filter must not be over-eager: anything not on the list still goes.
ordinary_headers_survive() ->
    {Server, Port, Seen} = start_server(),
    try
        ?assertMatch({ok, 200, _, ?BODY},
                     request(Port, [{<<"Connection">>, <<"keep-alive">>},
                                    {<<"X-Api-Version">>, <<"4">>},
                                    {<<"Accept">>, <<"application/json">>}])),
        Received = received_headers(Seen),
        ?assertEqual(<<"4">>, proplists:get_value(<<"x-api-version">>, Received)),
        ?assertEqual(<<"application/json">>,
                     proplists:get_value(<<"accept">>, Received))
    after
        stop_server(Server)
    end.

request(Port, Headers) ->
    URL = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    hackney:request(get, URL, Headers, <<>>,
                    [{protocols, [http2]},
                     {pool, false},
                     {with_body, true},
                     {recv_timeout, 5000},
                     {ssl_options, [{insecure, true}, {verify, verify_none}]}]).

connection_specific(Headers) ->
    Banned = [<<"connection">>, <<"keep-alive">>, <<"proxy-connection">>,
              <<"transfer-encoding">>, <<"upgrade">>],
    [N || {N, _} <- Headers, lists:member(N, Banned)].

received_headers(Seen) ->
    receive
        {headers, H} -> H
    after 5000 ->
        exit({no_request_reached_server, Seen})
    end.

%%====================================================================
%% Minimal HTTP/2 server that reports the request headers it saw.
%%====================================================================

start_server() ->
    Self = self(),
    Handler = fun(Conn, Sid, _Method, _Path, Headers) ->
        Self ! {headers, Headers},
        ok = h2:send_response(Conn, Sid, 200,
                              [{<<"content-type">>, <<"text/plain">>}]),
        ok = h2:send_data(Conn, Sid, ?BODY, true)
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    {Server, h2:server_port(Server), Self}.

stop_server(Server) ->
    catch h2:stop_server(Server),
    ok.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
