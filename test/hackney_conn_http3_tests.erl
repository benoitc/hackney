%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/3 connection support in hackney_conn.

-module(hackney_conn_http3_tests).

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
%% HTTP/3 Connection Tests
%%====================================================================

%% Test HTTP/3 connection via hackney_conn
http3_conn_test_() ->
    {
        "HTTP/3 connection tests via hackney_conn",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"HTTP/3 connection and request", fun test_h3_connection_request/0},
                {"HTTP/3 get_protocol returns http3", fun test_h3_get_protocol/0}
            ]
        }
    }.

test_h3_connection_request() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Start connection with HTTP/3 enabled
            {ok, Pid} = hackney_conn:start_link(#{
                host => "cloudflare.com",
                port => 443,
                transport => hackney_ssl,
                connect_options => [{protocols, [http3]}],
                connect_timeout => 15000
            }),
            ?assert(is_pid(Pid)),

            %% Connect
            case hackney_conn:connect(Pid, 15000) of
                ok ->
                    %% Verify protocol is HTTP/3
                    ?assertEqual(http3, hackney_conn:get_protocol(Pid)),

                    %% Make a request
                    case hackney_conn:request(Pid, <<"GET">>, <<"/">>, [], <<>>, 15000) of
                        {ok, Status, Headers, Body} ->
                            ?assert(is_integer(Status)),
                            ?assert(Status >= 200 andalso Status < 600),
                            ?assert(is_list(Headers)),
                            ?assert(is_binary(Body));
                        {error, Reason} ->
                            %% Request might fail due to network - acceptable
                            ?debugFmt("HTTP/3 request failed: ~p", [Reason])
                    end,

                    hackney_conn:stop(Pid);
                {error, timeout} ->
                    %% HTTP/3 connection timeout - might be UDP blocked
                    hackney_conn:stop(Pid),
                    ?debugMsg("HTTP/3 connection timed out (UDP may be blocked)");
                {error, Reason} ->
                    hackney_conn:stop(Pid),
                    ?debugFmt("HTTP/3 connection failed: ~p", [Reason])
            end
    end.

test_h3_get_protocol() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Start connection configured for HTTP/3
            {ok, Pid} = hackney_conn:start_link(#{
                host => "cloudflare.com",
                port => 443,
                transport => hackney_ssl,
                connect_options => [{protocols, [http3]}],
                connect_timeout => 15000
            }),

            %% Before connecting, protocol is http1 (default)
            ?assertEqual(http1, hackney_conn:get_protocol(Pid)),

            %% After connecting, should be http3
            case hackney_conn:connect(Pid, 15000) of
                ok ->
                    ?assertEqual(http3, hackney_conn:get_protocol(Pid));
                {error, _} ->
                    %% Connection failed - that's ok for this test
                    ok
            end,
            hackney_conn:stop(Pid)
    end.

%%====================================================================
%% hackney_h3 Module Tests
%%====================================================================

%% Test parse_response_headers with various inputs
parse_headers_test_() ->
    [
        {"Parse valid headers", fun() ->
            Headers = [
                {<<":status">>, <<"200">>},
                {<<"content-type">>, <<"text/html">>},
                {<<"server">>, <<"test">>}
            ],
            {ok, Status, RespHeaders} = hackney_h3:parse_response_headers(Headers),
            ?assertEqual(200, Status),
            ?assertEqual([{<<"content-type">>, <<"text/html">>}, {<<"server">>, <<"test">>}], RespHeaders)
        end},
        {"Parse 301 redirect", fun() ->
            Headers = [
                {<<":status">>, <<"301">>},
                {<<"location">>, <<"https://example.com/">>}
            ],
            {ok, Status, RespHeaders} = hackney_h3:parse_response_headers(Headers),
            ?assertEqual(301, Status),
            ?assertEqual([{<<"location">>, <<"https://example.com/">>}], RespHeaders)
        end},
        {"Missing status returns error", fun() ->
            Headers = [{<<"content-type">>, <<"text/html">>}],
            ?assertEqual({error, no_status}, hackney_h3:parse_response_headers(Headers))
        end}
    ].
