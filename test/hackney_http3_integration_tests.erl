%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Integration tests for HTTP/3 support in hackney API.

-module(hackney_http3_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    Server = hackney_h3_test_server:start(),
    {ok, _} = application:ensure_all_started(h2),
    Certs = filename:dirname(hackney_h3_test_server:cert_file()),
    {ok, H2} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => fun(Conn, StreamId, _Method, _Path, _Headers) ->
            ok = h2:send_response(Conn, StreamId, 200,
                                  [{<<"content-type">>, <<"text/plain">>}]),
            ok = h2:send_data(Conn, StreamId, <<"h2">>, true)
        end}),
    hackney_altsvc:clear_all(),
    Server#{h2 => H2, tcp_port => h2:server_port(H2)}.

cleanup(#{h2 := H2} = Server) ->
    hackney_conn_sup:stop_all(),
    hackney_altsvc:clear_all(),
    _ = h2:stop_server(H2),
    hackney_h3_test_server:stop(Server).

with_server(Tests) ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Server) ->
         [{Title, {timeout, 30, fun() -> Test(Server) end}} || {Title, Test} <- Tests]
     end}.

connect(Server, Opts) ->
    hackney:connect(hackney_ssl, "127.0.0.1", hackney_h3_test_server:port(Server), Opts).

tcp_opts() ->
    [{protocols, [http3, http2, http1]},
     {connect_timeout, 15000},
     {ssl_options, [{insecure, true}]}].

%%====================================================================
%% HTTP/3 Integration Tests
%%====================================================================

http3_integration_test_() ->
    {"HTTP/3 hackney API integration tests",
     with_server([
         {"connect with explicit http3 protocol", fun test_explicit_h3_connect/1},
         {"connect defaults to http2/http1", fun test_default_protocols/1},
         {"Alt-Svc cached enables H3", fun test_altsvc_enables_h3/1}
     ])}.

test_explicit_h3_connect(Server) ->
    {ok, ConnPid} = connect(Server, hackney_h3_test_server:hackney_opts()),
    ?assertEqual(http3, hackney_conn:get_protocol(ConnPid)),
    hackney:close(ConnPid).

test_default_protocols(#{tcp_port := TcpPort}) ->
    %% Without http3 in the protocol list, hackney negotiates over TCP.
    Opts = [{connect_timeout, 15000}, {ssl_options, [{insecure, true}]}],
    {ok, ConnPid} = hackney:connect(hackney_ssl, "127.0.0.1", TcpPort, Opts),
    ?assertEqual(http2, hackney_conn:get_protocol(ConnPid)),
    hackney:close(ConnPid).

test_altsvc_enables_h3(#{tcp_port := TcpPort} = Server) ->
    %% The origin is the TCP server; Alt-Svc points at the HTTP/3 port.
    H3Port = hackney_h3_test_server:port(Server),
    ok = hackney_altsvc:cache(<<"127.0.0.1">>, TcpPort, H3Port, 3600),
    {ok, ConnPid} = hackney:connect(hackney_ssl, "127.0.0.1", TcpPort, tcp_opts()),
    ?assertEqual(http3, hackney_conn:get_protocol(ConnPid)),
    hackney:close(ConnPid),
    hackney_altsvc:clear(<<"127.0.0.1">>, TcpPort).

%%====================================================================
%% Protocol Selection Tests
%%====================================================================

protocol_selection_test_() ->
    {"Protocol selection tests",
     with_server([
         {"http3 only forces QUIC", fun test_h3_only/1},
         {"blocked h3 falls back", fun test_blocked_fallback/1}
     ])}.

test_h3_only(Server) ->
    {ok, ConnPid} = connect(Server, hackney_h3_test_server:hackney_opts()),
    ?assertEqual(http3, hackney_conn:get_protocol(ConnPid)),
    hackney:close(ConnPid).

test_blocked_fallback(#{tcp_port := TcpPort} = Server) ->
    %% With HTTP/3 marked blocked for the origin, hackney does not try QUIC
    %% even though Alt-Svc advertises it, and falls back to TCP.
    H3Port = hackney_h3_test_server:port(Server),
    ok = hackney_altsvc:cache(<<"127.0.0.1">>, TcpPort, H3Port, 3600),
    hackney_altsvc:mark_h3_blocked(<<"127.0.0.1">>, TcpPort),
    ?assert(hackney_altsvc:is_h3_blocked(<<"127.0.0.1">>, TcpPort)),
    {ok, ConnPid} = hackney:connect(hackney_ssl, "127.0.0.1", TcpPort, tcp_opts()),
    ?assertEqual(http2, hackney_conn:get_protocol(ConnPid)),
    hackney:close(ConnPid),
    hackney_altsvc:clear(<<"127.0.0.1">>, TcpPort).

%%====================================================================
%% hackney:get with HTTP/3 Tests
%%====================================================================

http3_get_test_() ->
    {"HTTP/3 hackney:get tests",
     with_server([
         {"hackney:get over HTTP/3", fun test_h3_get/1},
         {"hackney:get verifies h3 protocol", fun test_h3_get_protocol/1}
     ])}.

test_h3_get(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/cdn-cgi/trace">>),
    Opts = [{with_body, true} | hackney_h3_test_server:hackney_opts()],
    {ok, 200, Headers, Body} = hackney:get(URL, [], <<>>, Opts),
    ?assertEqual(<<"text/plain">>, proplists:get_value(<<"content-type">>, Headers)),
    ?assertEqual(<<"h=127.0.0.1\nhttp=http/3\n">>, Body).

test_h3_get_protocol(Server) ->
    {ok, Conn} = connect(Server, hackney_h3_test_server:hackney_opts()),
    ?assertEqual(http3, hackney_conn:get_protocol(Conn)),
    hackney:close(Conn).
