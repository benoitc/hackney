%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/2 connection support in hackney_conn.
%%%
%%% Note: Integration tests depend on nghttp2.org and gracefully skip
%%% if network is unavailable or connections fail.

-module(hackney_conn_http2_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

cleanup(_) ->
    hackney_conn_sup:stop_all(),
    ok.

%%====================================================================
%% Protocol Detection Tests (Unit tests - no network required)
%%====================================================================

%% Test that TCP connections default to http1 protocol
tcp_connection_defaults_to_http1_test() ->
    setup(),
    {ok, Pid} = hackney_conn:start_link(#{
        host => "localhost",
        port => 8080,
        transport => hackney_tcp
    }),
    ?assertEqual(http1, hackney_conn:get_protocol(Pid)),
    hackney_conn:stop(Pid).

%% Test get_protocol API on idle connection
get_protocol_idle_test() ->
    setup(),
    {ok, Pid} = hackney_conn:start_link(#{
        host => "example.com",
        port => 443,
        transport => hackney_ssl
    }),
    ?assertEqual(http1, hackney_conn:get_protocol(Pid)),
    hackney_conn:stop(Pid).

%%====================================================================
%% HTTP/2 Machine Initialization Tests (Unit tests)
%%====================================================================

h2_machine_init_test() ->
    Opts = #{preface_timeout => infinity, settings_timeout => infinity},
    {ok, Preface, Machine} = hackney_http2_machine:init(client, Opts),
    ?assert(is_binary(iolist_to_binary(Preface))),
    ?assert(is_tuple(Machine)),
    PrefaceBin = iolist_to_binary(Preface),
    ?assertMatch(<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", _/binary>>, PrefaceBin).

%%====================================================================
%% ALPN Options Tests (Unit tests)
%%====================================================================

alpn_opts_default_test() ->
    Opts = hackney_ssl:alpn_opts([]),
    ?assertEqual([{alpn_advertised_protocols, [<<"h2">>, <<"http/1.1">>]}], Opts).

alpn_opts_http2_only_test() ->
    Opts = hackney_ssl:alpn_opts([{protocols, [http2]}]),
    ?assertEqual([{alpn_advertised_protocols, [<<"h2">>]}], Opts).

alpn_opts_http1_only_test() ->
    Opts = hackney_ssl:alpn_opts([{protocols, [http1]}]),
    ?assertEqual([{alpn_advertised_protocols, [<<"http/1.1">>]}], Opts).

%%====================================================================
%% Test Suites
%%====================================================================

protocol_detection_test_() ->
    [
        {"TCP defaults to HTTP/1", fun tcp_connection_defaults_to_http1_test/0},
        {"Get protocol on idle", fun get_protocol_idle_test/0}
    ].

h2_machine_test_() ->
    [
        {"H2 machine init", fun h2_machine_init_test/0}
    ].

alpn_test_() ->
    [
        {"ALPN opts default", fun alpn_opts_default_test/0},
        {"ALPN opts HTTP/2 only", fun alpn_opts_http2_only_test/0},
        {"ALPN opts HTTP/1 only", fun alpn_opts_http1_only_test/0}
    ].
