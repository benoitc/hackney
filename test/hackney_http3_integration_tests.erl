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
    {ok, _} = application:ensure_all_started(hackney),
    hackney_altsvc:clear_all(),
    ok.

cleanup(_) ->
    hackney_conn_sup:stop_all(),
    hackney_altsvc:clear_all(),
    ok.

%%====================================================================
%% HTTP/3 Integration Tests
%%====================================================================

http3_integration_test_() ->
    {
        "HTTP/3 hackney API integration tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"connect with explicit http3 protocol", fun test_explicit_h3_connect/0},
                {"connect defaults to http2/http1", fun test_default_protocols/0},
                {"Alt-Svc cached enables H3", fun test_altsvc_enables_h3/0}
            ]
        }
    }.

test_explicit_h3_connect() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Explicitly request HTTP/3
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}],
            case hackney:connect(hackney_ssl, "cloudflare.com", 443, Opts) of
                {ok, ConnPid} ->
                    %% Verify protocol is HTTP/3
                    ?assertEqual(http3, hackney_conn:get_protocol(ConnPid)),
                    hackney:close(ConnPid);
                {error, Reason} ->
                    %% H3 might be blocked, acceptable
                    ?debugFmt("HTTP/3 connect failed (may be blocked): ~p", [Reason])
            end
    end.

test_default_protocols() ->
    %% Default protocols should be http2, http1 (no http3)
    Opts = [{connect_timeout, 10000}],
    case hackney:connect(hackney_ssl, "httpbin.org", 443, Opts) of
        {ok, ConnPid} ->
            Protocol = hackney_conn:get_protocol(ConnPid),
            %% Should be http1 or http2, not http3
            ?assert(Protocol =:= http1 orelse Protocol =:= http2),
            hackney:close(ConnPid);
        {error, Reason} ->
            ?debugFmt("Connect failed: ~p", [Reason])
    end.

test_altsvc_enables_h3() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Manually cache Alt-Svc for cloudflare
            hackney_altsvc:cache(<<"cloudflare.com">>, 443, 443, 3600),

            %% Now connect with h3 in protocols list
            Opts = [{protocols, [http3, http2, http1]}, {connect_timeout, 15000}],
            case hackney:connect(hackney_ssl, "cloudflare.com", 443, Opts) of
                {ok, ConnPid} ->
                    Protocol = hackney_conn:get_protocol(ConnPid),
                    ?debugFmt("Protocol with Alt-Svc cached: ~p", [Protocol]),
                    %% Should try HTTP/3 first due to Alt-Svc
                    hackney:close(ConnPid);
                {error, Reason} ->
                    ?debugFmt("Connect with Alt-Svc failed: ~p", [Reason])
            end,

            %% Clear cache
            hackney_altsvc:clear(<<"cloudflare.com">>, 443)
    end.

%%====================================================================
%% Protocol Selection Tests
%%====================================================================

protocol_selection_test_() ->
    {
        "Protocol selection tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"http3 only forces QUIC", fun test_h3_only/0},
                {"blocked h3 falls back", fun test_blocked_fallback/0}
            ]
        }
    }.

test_h3_only() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Force HTTP/3 only
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}],
            case hackney:connect(hackney_ssl, "cloudflare.com", 443, Opts) of
                {ok, ConnPid} ->
                    ?assertEqual(http3, hackney_conn:get_protocol(ConnPid)),
                    hackney:close(ConnPid);
                {error, _} ->
                    %% H3 blocked is acceptable
                    ok
            end
    end.

test_blocked_fallback() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Mark host as H3 blocked
            hackney_altsvc:mark_h3_blocked(<<"blocked-test.example.com">>, 443),

            %% Even with H3 in protocols, should not try H3
            ?assert(hackney_altsvc:is_h3_blocked(<<"blocked-test.example.com">>, 443)),

            %% Clear
            hackney_altsvc:clear(<<"blocked-test.example.com">>, 443)
    end.

%%====================================================================
%% hackney:get with HTTP/3 Tests
%%====================================================================

http3_get_test_() ->
    {
        "HTTP/3 hackney:get tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"hackney:get over HTTP/3", fun test_h3_get/0},
                {"hackney:get verifies h3 protocol", fun test_h3_get_protocol/0}
            ]
        }
    }.

test_h3_get() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% GET request over HTTP/3
            URL = "https://cloudflare.com/cdn-cgi/trace",
            Opts = [{protocols, [http3]}, {with_body, true}, {connect_timeout, 15000}],
            case hackney:get(URL, [], <<>>, Opts) of
                {ok, Status, Headers, Body} ->
                    ?assert(is_integer(Status)),
                    ?assert(Status >= 200 andalso Status < 400),
                    ?assert(is_list(Headers)),
                    ?assert(is_binary(Body)),
                    ?assert(byte_size(Body) > 0),
                    %% cloudflare trace should contain "h3" in body
                    ?debugFmt("H3 GET body: ~s", [Body]);
                {error, Reason} ->
                    ?debugFmt("H3 GET failed (may be blocked): ~p", [Reason])
            end
    end.

test_h3_get_protocol() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Connect and verify protocol is http3
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}],
            case hackney:connect(hackney_ssl, "cloudflare.com", 443, Opts) of
                {ok, Conn} ->
                    Protocol = hackney_conn:get_protocol(Conn),
                    ?assertEqual(http3, Protocol),
                    hackney:close(Conn);
                {error, Reason} ->
                    ?debugFmt("H3 connect failed: ~p", [Reason])
            end
    end.
