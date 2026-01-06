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
