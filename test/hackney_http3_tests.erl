%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/3 integration in hackney.
%%%
%%% These tests verify that HTTP/3 works through the standard hackney API,
%%% against a local HTTP/3 server (hackney_h3_test_server).

-module(hackney_http3_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    hackney_h3_test_server:start().

cleanup(Server) ->
    hackney_conn_sup:stop_all(),
    hackney_h3_test_server:stop(Server).

%%====================================================================
%% hackney_h3 module tests
%%====================================================================

%% Test hackney_h3:request
http3_request_test_() ->
    {
        "HTTP/3 request tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            fun(Server) ->
                [{"Direct HTTP/3 request",
                  {timeout, 30, fun() -> test_http3_direct_request(Server) end}}]
            end
        }
    }.

test_http3_direct_request(Server) ->
    {ok, ConnRef} = hackney_h3:connect(hackney_h3_test_server:host(),
                                       hackney_h3_test_server:port(Server),
                                       hackney_h3_test_server:h3_opts()),
    hackney_h3:close(ConnRef).

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
            fun(Server) ->
                [{"Conn start with http3 protocol option",
                  {timeout, 30, fun() -> test_conn_http3_option(Server) end}}]
            end
        }
    }.

test_conn_http3_option(Server) ->
    %% With http3 first in the protocol list, hackney_conn tries HTTP/3 first.
    Opts0 = hackney_h3_test_server:conn_opts(Server),
    Opts = Opts0#{connect_options => [{protocols, [http3, http2, http1]}]},
    {ok, Pid} = hackney_conn:start_link(Opts),
    ?assertEqual(ok, hackney_conn:connect(Pid)),
    ?assertEqual(http3, hackney_conn:get_protocol(Pid)),
    hackney_conn:stop(Pid).
