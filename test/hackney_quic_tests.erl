%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2025 Benoit Chesneau
%%%
%%% @doc Tests for QUIC/HTTP3 support in hackney.
%%%

-module(hackney_quic_tests).

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
%% NIF Availability Tests
%%====================================================================

%% Test that we can check if QUIC is available
quic_availability_test() ->
    Result = hackney_quic:is_available(),
    ?assert(is_boolean(Result)).

%%====================================================================
%% Connection Tests (require network and QUIC NIF)
%%====================================================================

%% Test QUIC connection to a real HTTP/3 server
quic_connection_test_() ->
    {
        "QUIC connection tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Connect to cloudflare.com", fun test_cloudflare_connect/0},
                {"Test peername/sockname", fun test_addresses/0},
                {"Test stream opening", fun test_open_stream/0}
            ]
        }
    }.

test_cloudflare_connect() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            Result = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
            ?assertMatch({ok, _}, Result),
            {ok, ConnRef} = Result,
            receive
                {quic, ConnRef, {connected, Info}} ->
                    ?assertMatch(#{resumed := _}, Info);
                {quic, ConnRef, {closed, Reason}} ->
                    ?assertEqual(unexpected_close, Reason)
            after 15000 ->
                hackney_quic:close(ConnRef, normal),
                ?assert(false, "Timeout waiting for connection")
            end,
            hackney_quic:close(ConnRef, normal)
    end.

test_addresses() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
            receive
                {quic, ConnRef, {connected, _}} ->
                    %% Test peername
                    {ok, {PeerIP, PeerPort}} = hackney_quic:peername(ConnRef),
                    ?assert(is_tuple(PeerIP)),
                    ?assertEqual(443, PeerPort),
                    %% Test sockname
                    {ok, {LocalIP, LocalPort}} = hackney_quic:sockname(ConnRef),
                    ?assert(is_tuple(LocalIP)),
                    ?assert(is_integer(LocalPort))
            after 15000 ->
                hackney_quic:close(ConnRef, normal),
                ?assert(false, "Timeout waiting for connection")
            end,
            hackney_quic:close(ConnRef, normal)
    end.

test_open_stream() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
            receive
                {quic, ConnRef, {connected, _}} ->
                    %% Test opening a stream
                    Result = hackney_quic:open_stream(ConnRef),
                    ?assertMatch({ok, _}, Result);
                {quic, ConnRef, {closed, _}} ->
                    ?assert(false, "Connection closed unexpectedly")
            after 15000 ->
                hackney_quic:close(ConnRef, normal),
                ?assert(false, "Timeout waiting for connection")
            end,
            hackney_quic:close(ConnRef, normal)
    end.

%%====================================================================
%% get_fd Tests
%%====================================================================

%% Test get_fd function with a real UDP socket
get_fd_test() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
            Result = hackney_quic:get_fd(Socket),
            ?assertMatch({ok, _}, Result),
            {ok, Fd} = Result,
            ?assert(is_integer(Fd)),
            ?assert(Fd > 0),
            gen_udp:close(Socket)
    end.

%%====================================================================
%% HTTP/3 Request Tests
%%====================================================================

%% Test sending HTTP/3 request headers
test_send_request() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
            receive
                {quic, ConnRef, {connected, _}} ->
                    %% Open a stream
                    {ok, StreamId} = hackney_quic:open_stream(ConnRef),

                    %% Send HTTP/3 request headers
                    Headers = [
                        {<<":method">>, <<"GET">>},
                        {<<":path">>, <<"/">>},
                        {<<":scheme">>, <<"https">>},
                        {<<":authority">>, <<"cloudflare.com">>},
                        {<<"user-agent">>, <<"hackney-quic-test/1.0">>}
                    ],
                    Result = hackney_quic:send_headers(ConnRef, StreamId, Headers, true),
                    ?assertMatch(ok, Result);
                {quic, ConnRef, {closed, _}} ->
                    ?assert(false, "Connection closed unexpectedly")
            after 15000 ->
                hackney_quic:close(ConnRef, normal),
                ?assert(false, "Timeout waiting for connection")
            end,
            hackney_quic:close(ConnRef, normal)
    end.

http3_request_test_() ->
    {
        "HTTP/3 request tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Send HTTP/3 headers", fun test_send_request/0},
                {"Full HTTP/3 request/response", fun test_full_request_response/0}
            ]
        }
    }.

%% Test full HTTP/3 request and response flow
test_full_request_response() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
            receive
                {quic, ConnRef, {connected, _}} ->
                    %% Wait for H3 control streams to be established
                    timer:sleep(100),

                    %% Open a stream
                    {ok, StreamId} = hackney_quic:open_stream(ConnRef),

                    %% Wait for stream_opened notification
                    receive
                        {quic, ConnRef, {stream_opened, StreamId}} -> ok
                    after 1000 -> ok
                    end,

                    %% Send HTTP/3 request headers
                    Headers = [
                        {<<":method">>, <<"GET">>},
                        {<<":scheme">>, <<"https">>},
                        {<<":authority">>, <<"cloudflare.com">>},
                        {<<":path">>, <<"/">>},
                        {<<"user-agent">>, <<"hackney-quic-test/1.0">>}
                    ],
                    ok = hackney_quic:send_headers(ConnRef, StreamId, Headers, true),

                    %% Wait for response headers
                    receive
                        {quic, ConnRef, {stream_headers, StreamId, RespHeaders, _Fin}} ->
                            %% Check we got a valid status
                            ?assert(lists:keymember(<<":status">>, 1, RespHeaders))
                    after 10000 ->
                        hackney_quic:close(ConnRef, normal),
                        ?assert(false, "Timeout waiting for response headers")
                    end,

                    %% Wait for response body
                    receive
                        {quic, ConnRef, {stream_data, StreamId, Body, _BodyFin}} ->
                            ?assert(byte_size(Body) > 0)
                    after 10000 ->
                        ok  %% Body might be empty for redirects
                    end;
                {quic, ConnRef, {closed, _}} ->
                    ?assert(false, "Connection closed unexpectedly")
            after 15000 ->
                hackney_quic:close(ConnRef, normal),
                ?assert(false, "Timeout waiting for connection")
            end,
            hackney_quic:close(ConnRef, normal)
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% Test connection with invalid hostname
invalid_hostname_test() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Invalid port should fail
            Result = hackney_quic:connect(<<"test">>, 0, #{}, self()),
            ?assertMatch({error, badarg}, Result)
    end.

%% Test connect with invalid arguments
invalid_args_test() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Port out of range
            ?assertMatch({error, badarg},
                hackney_quic:connect(<<"test">>, 70000, #{}, self())),
            %% Invalid opts type
            ?assertMatch({error, badarg},
                hackney_quic:connect(<<"test">>, 443, invalid, self())),
            %% Invalid owner type
            ?assertMatch({error, badarg},
                hackney_quic:connect(<<"test">>, 443, #{}, invalid))
    end.
