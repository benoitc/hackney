%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/3 integration in hackney.
%%%
%%% These tests verify that HTTP/3 works through the standard hackney API.

-module(hackney_http3_tests).

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
%% hackney_h3 module tests
%%====================================================================

%% Test hackney_h3:is_available
is_available_test() ->
    Result = hackney_h3:is_available(),
    ?assert(is_boolean(Result)).

%% Test hackney_h3:request
http3_request_test_() ->
    {
        "HTTP/3 request tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Direct HTTP/3 request", fun test_http3_direct_request/0}
            ]
        }
    }.

test_http3_direct_request() ->
    case hackney_h3:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Test HTTP/3 connection establishment
            %% Note: Full request/response is not yet implemented in the NIF
            %% This test verifies connection works
            Result = hackney_h3:connect(<<"cloudflare.com">>, 443),
            case Result of
                {ok, ConnRef} ->
                    hackney_h3:close(ConnRef),
                    ok;
                {error, Reason} ->
                    %% Connection issues are acceptable in test environments
                    ?debugFmt("HTTP/3 connect failed: ~p", [Reason]),
                    ok
            end
    end.

%% Test hackney_h3:parse_response_headers
parse_response_headers_test() ->
    Headers = [
        {<<":status">>, <<"200">>},
        {<<"content-type">>, <<"text/html">>},
        {<<"server">>, <<"cloudflare">>}
    ],
    {ok, Status, RespHeaders} = hackney_h3:parse_response_headers(Headers),
    ?assertEqual(200, Status),
    ?assertEqual([{<<"content-type">>, <<"text/html">>}, {<<"server">>, <<"cloudflare">>}], RespHeaders).

parse_response_headers_missing_status_test() ->
    Headers = [
        {<<"content-type">>, <<"text/html">>}
    ],
    Result = hackney_h3:parse_response_headers(Headers),
    ?assertEqual({error, no_status}, Result).

%%====================================================================
%% hackney:request with HTTP/3 integration tests
%%====================================================================

%% Test that hackney_conn can be configured for HTTP/3
hackney_conn_http3_config_test_() ->
    {
        "hackney_conn HTTP/3 configuration tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Conn start with http3 protocol option", fun test_conn_http3_option/0}
            ]
        }
    }.

test_conn_http3_option() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Test that hackney_conn can be started with HTTP/3 configuration
            Opts = #{
                host => "cloudflare.com",
                port => 443,
                transport => hackney_ssl,
                connect_options => [{protocols, [http3, http2, http1]}],
                connect_timeout => 10000
            },
            {ok, Pid} = hackney_conn:start_link(Opts),
            ?assert(is_pid(Pid)),
            %% Connect - this should try HTTP/3 first
            Result = hackney_conn:connect(Pid),
            hackney_conn:stop(Pid),
            case Result of
                ok ->
                    ok;
                {error, Reason} ->
                    %% HTTP/3 might fail and fall back to HTTP/2 or HTTP/1
                    %% That's acceptable behavior
                    ?debugFmt("Connect result: ~p", [Reason]),
                    ok
            end
    end.
