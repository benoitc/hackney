%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2025 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/2 connection support in hackney_conn.
%%%

-module(hackney_conn_http2_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Protocol Detection Tests
%%====================================================================

%% Test that TCP connections default to http1 protocol
tcp_connection_defaults_to_http1_test() ->
    setup(),
    %% Start a TCP connection (without actually connecting)
    {ok, Pid} = hackney_conn:start_link(#{
        host => "localhost",
        port => 8080,
        transport => hackney_tcp
    }),
    %% Protocol should default to http1
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
    %% Before connecting, protocol defaults to http1
    ?assertEqual(http1, hackney_conn:get_protocol(Pid)),
    hackney_conn:stop(Pid).

%%====================================================================
%% HTTP/2 Machine Initialization Tests
%%====================================================================

%% Test that HTTP/2 machine can be initialized
h2_machine_init_test() ->
    %% Test direct initialization of h2_machine (unit test)
    %% Use infinity timeouts to prevent orphaned timer messages
    Opts = #{preface_timeout => infinity, settings_timeout => infinity},
    {ok, Preface, Machine} = hackney_cow_http2_machine:init(client, Opts),
    ?assert(is_binary(iolist_to_binary(Preface))),
    ?assert(is_tuple(Machine)),
    %% Verify preface contains HTTP/2 magic
    PrefaceBin = iolist_to_binary(Preface),
    ?assertMatch(<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", _/binary>>, PrefaceBin).

%%====================================================================
%% ALPN Options Tests
%%====================================================================

%% Test ALPN options generation
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
%% Integration Tests (require network)
%%====================================================================

%% Test HTTP/2 connection to a real server (nghttp2.org supports HTTP/2)
%% This test is marked as requiring network access
http2_connection_test_() ->
    {
        "HTTP/2 connection tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Connect to HTTP/2 server", fun connect_http2_server/0},
                {"HTTP/2 GET request", fun h2_get_request/0},
                {"HTTP/2 POST request", fun h2_post_request/0}
            ]
        }
    }.

connect_http2_server() ->
    %% nghttp2.org is a test server that supports HTTP/2
    %% Skip if network is not available
    case gen_tcp:connect("nghttp2.org", 443, [], 5000) of
        {ok, TestSock} ->
            gen_tcp:close(TestSock),
            %% Network available, run the test
            {ok, Pid} = hackney_conn:start_link(#{
                host => "nghttp2.org",
                port => 443,
                transport => hackney_ssl,
                connect_timeout => 10000
            }),
            case hackney_conn:connect(Pid, 10000) of
                ok ->
                    %% Check if HTTP/2 was negotiated
                    Protocol = hackney_conn:get_protocol(Pid),
                    %% Protocol should be http2 (nghttp2.org supports it)
                    ?assertEqual(http2, Protocol),
                    hackney_conn:stop(Pid);
                {error, _Reason} ->
                    %% Connection failed, skip test
                    hackney_conn:stop(Pid),
                    ?debugMsg("Skipping HTTP/2 test - connection failed")
            end;
        {error, _} ->
            %% Network not available, skip test
            ?debugMsg("Skipping HTTP/2 test - network not available")
    end.

h2_get_request() ->
    %% Test HTTP/2 GET request
    case gen_tcp:connect("nghttp2.org", 443, [], 5000) of
        {ok, TestSock} ->
            gen_tcp:close(TestSock),
            {ok, Pid} = hackney_conn:start_link(#{
                host => "nghttp2.org",
                port => 443,
                transport => hackney_ssl,
                connect_timeout => 10000,
                recv_timeout => 10000
            }),
            case hackney_conn:connect(Pid, 10000) of
                ok ->
                    ?assertEqual(http2, hackney_conn:get_protocol(Pid)),
                    %% Send GET request
                    case hackney_conn:request(Pid, <<"GET">>, <<"/">>, [], <<>>, 15000) of
                        {ok, Status, Headers} when is_integer(Status) ->
                            ?assert(Status >= 200 andalso Status < 400),
                            ?assert(is_list(Headers)),
                            hackney_conn:stop(Pid);
                        {ok, Status, Headers, _Body} when is_integer(Status) ->
                            ?assert(Status >= 200 andalso Status < 400),
                            ?assert(is_list(Headers)),
                            hackney_conn:stop(Pid);
                        {error, Reason} ->
                            hackney_conn:stop(Pid),
                            ?debugFmt("HTTP/2 GET request failed: ~p", [Reason])
                    end;
                {error, _} ->
                    hackney_conn:stop(Pid),
                    ?debugMsg("Skipping - connection failed")
            end;
        {error, _} ->
            ?debugMsg("Skipping - network not available")
    end.

h2_post_request() ->
    %% Test HTTP/2 POST request with body
    case gen_tcp:connect("nghttp2.org", 443, [], 5000) of
        {ok, TestSock} ->
            gen_tcp:close(TestSock),
            {ok, Pid} = hackney_conn:start_link(#{
                host => "nghttp2.org",
                port => 443,
                transport => hackney_ssl,
                connect_timeout => 10000,
                recv_timeout => 10000
            }),
            case hackney_conn:connect(Pid, 10000) of
                ok ->
                    ?assertEqual(http2, hackney_conn:get_protocol(Pid)),
                    %% Send POST request with body
                    Body = <<"test=data">>,
                    Headers = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
                    case hackney_conn:request(Pid, <<"POST">>, <<"/">>, Headers, Body, 15000) of
                        {ok, Status, RespHeaders} when is_integer(Status) ->
                            ?assert(Status >= 200 andalso Status < 500),
                            ?assert(is_list(RespHeaders)),
                            hackney_conn:stop(Pid);
                        {ok, Status, RespHeaders, _RespBody} when is_integer(Status) ->
                            ?assert(Status >= 200 andalso Status < 500),
                            ?assert(is_list(RespHeaders)),
                            hackney_conn:stop(Pid);
                        {error, Reason} ->
                            hackney_conn:stop(Pid),
                            ?debugFmt("HTTP/2 POST request failed: ~p", [Reason])
                    end;
                {error, _} ->
                    hackney_conn:stop(Pid),
                    ?debugMsg("Skipping - connection failed")
            end;
        {error, _} ->
            ?debugMsg("Skipping - network not available")
    end.

%%====================================================================
%% Multiplexing Tests
%%====================================================================

%% Test that multiple requests on same connection work (sequential)
h2_sequential_requests_test() ->
    case gen_tcp:connect("nghttp2.org", 443, [], 5000) of
        {ok, TestSock} ->
            gen_tcp:close(TestSock),
            %% Create a fresh connection (not from pool) for this test
            {ok, Conn} = hackney_conn:start_link(#{
                host => "nghttp2.org",
                port => 443,
                transport => hackney_ssl,
                connect_timeout => 10000
            }),
            ok = hackney_conn:connect(Conn, 10000),
            ?assertEqual(http2, hackney_conn:get_protocol(Conn)),

            %% Make 3 sequential requests on same connection
            {ok, 200, _, _} = hackney_conn:request(Conn, <<"GET">>, <<"/">>, [], <<>>, 10000),
            {ok, 200, _, _} = hackney_conn:request(Conn, <<"GET">>, <<"/blog/">>, [], <<>>, 10000),
            {ok, 200, _, _} = hackney_conn:request(Conn, <<"GET">>, <<"/">>, [], <<>>, 10000),

            hackney_conn:stop(Conn);
        {error, _} ->
            ?debugMsg("Skipping - network not available")
    end.

%% Test that hackney:connect returns same PID for HTTP/2 (connection reuse)
h2_connection_reuse_test() ->
    case gen_tcp:connect("nghttp2.org", 443, [], 5000) of
        {ok, TestSock} ->
            gen_tcp:close(TestSock),

            %% First connection - creates new HTTP/2 connection
            {ok, Conn1} = hackney:connect(hackney_ssl, "nghttp2.org", 443, []),
            ?assertEqual(http2, hackney_conn:get_protocol(Conn1)),

            %% Second connection - should reuse same connection (multiplexing)
            {ok, Conn2} = hackney:connect(hackney_ssl, "nghttp2.org", 443, []),
            ?assertEqual(http2, hackney_conn:get_protocol(Conn2)),

            %% Third connection - should also reuse
            {ok, Conn3} = hackney:connect(hackney_ssl, "nghttp2.org", 443, []),
            ?assertEqual(http2, hackney_conn:get_protocol(Conn3)),

            %% All should be the same PID (multiplexed)
            ?assertEqual(Conn1, Conn2),
            ?assertEqual(Conn2, Conn3),

            hackney_conn:stop(Conn1);
        {error, _} ->
            ?debugMsg("Skipping - network not available")
    end.

%% Test that pool checkout_h2/register_h2 work correctly
h2_pool_registration_test() ->
    case gen_tcp:connect("nghttp2.org", 443, [], 5000) of
        {ok, TestSock} ->
            gen_tcp:close(TestSock),

            %% Initially no HTTP/2 connection in pool
            ?assertEqual(none, hackney_pool:checkout_h2("nghttp2.org", 443, hackney_ssl, [])),

            %% Connect - this should register the HTTP/2 connection
            {ok, Conn} = hackney:connect(hackney_ssl, "nghttp2.org", 443, []),
            ?assertEqual(http2, hackney_conn:get_protocol(Conn)),

            %% Now checkout_h2 should return the connection
            ?assertEqual({ok, Conn}, hackney_pool:checkout_h2("nghttp2.org", 443, hackney_ssl, [])),

            %% Unregister
            hackney_pool:unregister_h2(Conn, []),

            %% Should be gone from pool
            ?assertEqual(none, hackney_pool:checkout_h2("nghttp2.org", 443, hackney_ssl, [])),

            hackney_conn:stop(Conn);
        {error, _} ->
            ?debugMsg("Skipping - network not available")
    end.

%% Test high-level API connection reuse
h2_high_level_api_reuse_test() ->
    case gen_tcp:connect("nghttp2.org", 443, [], 5000) of
        {ok, TestSock} ->
            gen_tcp:close(TestSock),

            %% Make multiple requests via high-level API
            {ok, 200, H1, _} = hackney:get(<<"https://nghttp2.org/">>, [], <<>>, [with_body]),
            {ok, 200, H2, _} = hackney:get(<<"https://nghttp2.org/blog/">>, [], <<>>, [with_body]),
            {ok, 200, H3, _} = hackney:get(<<"https://nghttp2.org/">>, [], <<>>, [with_body]),

            %% All should have lowercase headers (HTTP/2)
            ?assertMatch({<<"date">>, _}, hd(H1)),
            ?assertMatch({<<"date">>, _}, hd(H2)),
            ?assertMatch({<<"date">>, _}, hd(H3));
        {error, _} ->
            ?debugMsg("Skipping - network not available")
    end.

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

multiplexing_test_() ->
    {
        "HTTP/2 multiplexing tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Sequential requests on same connection", fun h2_sequential_requests_test/0},
                {"Connection reuse (same PID)", fun h2_connection_reuse_test/0},
                {"Pool registration", fun h2_pool_registration_test/0},
                {"High-level API reuse", fun h2_high_level_api_reuse_test/0}
            ]
        }
    }.
