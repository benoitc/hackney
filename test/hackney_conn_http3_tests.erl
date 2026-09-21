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
    hackney_h3_test_server:start().

cleanup(Server) ->
    hackney_conn_sup:stop_all(),
    hackney_h3_test_server:stop(Server).

%%====================================================================
%% HTTP/3 Connection Tests
%%====================================================================

%% HTTP/3 connections via hackney_conn, against a local HTTP/3 server.
http3_conn_test_() ->
    {
        "HTTP/3 connection tests via hackney_conn",
        {
            setup,
            fun setup/0, fun cleanup/1,
            fun(Server) ->
                [{Title, {timeout, 30, fun() -> Test(Server) end}} || {Title, Test} <- [
                    {"HTTP/3 connection and request", fun test_h3_connection_request/1},
                    {"HTTP/3 get_protocol returns http3", fun test_h3_get_protocol/1},
                    {"HTTP/3 peername/sockname/peercert", fun test_h3_peer_info/1}
                ]]
            end
        }
    }.

test_h3_connection_request(Server) ->
    {ok, Pid} = hackney_conn:start_link(hackney_h3_test_server:conn_opts(Server)),
    ok = hackney_conn:connect(Pid, 5000),
    ?assertEqual(http3, hackney_conn:get_protocol(Pid)),
    {ok, Status, Headers, Body} =
        hackney_conn:request(Pid, <<"GET">>, <<"/">>, [], <<>>, 5000),
    ?assertEqual(200, Status),
    ?assertEqual(<<"text/html">>, proplists:get_value(<<"content-type">>, Headers)),
    ?assertEqual(<<"<html><body>hackney h3 test server</body></html>">>, Body),
    hackney_conn:stop(Pid).

test_h3_get_protocol(Server) ->
    {ok, Pid} = hackney_conn:start_link(hackney_h3_test_server:conn_opts(Server)),
    %% Before connecting, the protocol is the http1 default.
    ?assertEqual(http1, hackney_conn:get_protocol(Pid)),
    ok = hackney_conn:connect(Pid, 5000),
    ?assertEqual(http3, hackney_conn:get_protocol(Pid)),
    hackney_conn:stop(Pid).

test_h3_peer_info(Server) ->
    Port = hackney_h3_test_server:port(Server),
    {ok, Pid} = hackney_conn:start_link(hackney_h3_test_server:conn_opts(Server)),
    ok = hackney_conn:connect(Pid, 5000),
    ?assertEqual(http3, hackney_conn:get_protocol(Pid)),
    ?assertMatch({ok, {{127, 0, 0, 1}, Port}}, hackney_conn:peername(Pid)),
    ?assertMatch({ok, {_, _}}, hackney_conn:sockname(Pid)),
    case hackney_conn:peercert(Pid) of
        {ok, Cert} -> ?assert(is_binary(Cert));
        {error, no_peercert} -> ok
    end,
    hackney_conn:stop(Pid).

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
