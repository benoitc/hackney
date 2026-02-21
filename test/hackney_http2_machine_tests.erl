%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/2 state machine module.
%%%

-module(hackney_http2_machine_tests).

-include_lib("eunit/include/eunit.hrl").

%% Default options that disable timers to prevent orphaned timer messages during tests
-define(TEST_OPTS, #{preface_timeout => infinity, settings_timeout => infinity}).

%%====================================================================
%% Client Mode Tests
%%====================================================================

%% Test client initialization
init_client_test() ->
    {ok, Preface, State} = hackney_http2_machine:init(client, ?TEST_OPTS),
    ?assert(is_binary(iolist_to_binary(Preface))),
    %% Client preface should include "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    PrefaceBin = iolist_to_binary(Preface),
    ?assertMatch(<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", _/binary>>, PrefaceBin),
    ?assert(is_tuple(State)).

%% Test server initialization
init_server_test() ->
    {ok, Preface, State} = hackney_http2_machine:init(server, ?TEST_OPTS),
    ?assert(is_binary(iolist_to_binary(Preface))),
    ?assert(is_tuple(State)).

%% Test client can initialize a stream
init_stream_client_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    %% Client needs to acknowledge server settings first
    %% Let's just test we can call init_stream
    {ok, StreamID, State1} = hackney_http2_machine:init_stream(<<"GET">>, State0),
    ?assertEqual(1, StreamID),
    %% Next stream should be 3
    {ok, StreamID2, _State2} = hackney_http2_machine:init_stream(<<"POST">>, State1),
    ?assertEqual(3, StreamID2).

%% Test stream IDs increment correctly
stream_id_increment_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, 1, State1} = hackney_http2_machine:init_stream(<<"GET">>, State0),
    {ok, 3, State2} = hackney_http2_machine:init_stream(<<"GET">>, State1),
    {ok, 5, State3} = hackney_http2_machine:init_stream(<<"GET">>, State2),
    {ok, 7, _State4} = hackney_http2_machine:init_stream(<<"GET">>, State3).

%%====================================================================
%% Settings Tests
%%====================================================================

%% Test custom initial settings
init_with_custom_settings_test() ->
    Opts = maps:merge(?TEST_OPTS, #{
        initial_stream_window_size => 32768,
        max_concurrent_streams => 50
    }),
    {ok, Preface, _State} = hackney_http2_machine:init(client, Opts),
    PrefaceBin = iolist_to_binary(Preface),
    %% Should contain the connection preface
    ?assertMatch(<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", _/binary>>, PrefaceBin).

%% Test get local settings
get_local_setting_test() ->
    {ok, _Preface, State} = hackney_http2_machine:init(client, ?TEST_OPTS),
    %% Default initial window size is 65535
    ?assertEqual(65535, hackney_http2_machine:get_local_setting(initial_window_size, State)).

%% Test get remote settings (defaults)
get_remote_settings_test() ->
    {ok, _Preface, State} = hackney_http2_machine:init(client, ?TEST_OPTS),
    RemoteSettings = hackney_http2_machine:get_remote_settings(State),
    ?assert(is_map(RemoteSettings)),
    ?assertEqual(65535, maps:get(initial_window_size, RemoteSettings)).

%%====================================================================
%% Headers Preparation Tests
%%====================================================================

%% Test prepare_headers for a GET request
prepare_headers_get_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, StreamID, State1} = hackney_http2_machine:init_stream(<<"GET">>, State0),
    PseudoHeaders = #{
        method => <<"GET">>,
        scheme => <<"https">>,
        authority => <<"example.com">>,
        path => <<"/">>
    },
    Headers = [{<<"user-agent">>, <<"hackney">>}],
    {ok, IsFin, HeaderBlock, _State2} = hackney_http2_machine:prepare_headers(
        StreamID, State1, fin, PseudoHeaders, Headers),
    ?assertEqual(fin, IsFin),
    ?assert(is_binary(iolist_to_binary(HeaderBlock))).

%% Test prepare_headers for a POST request
prepare_headers_post_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, StreamID, State1} = hackney_http2_machine:init_stream(<<"POST">>, State0),
    PseudoHeaders = #{
        method => <<"POST">>,
        scheme => <<"https">>,
        authority => <<"example.com">>,
        path => <<"/api/data">>
    },
    Headers = [
        {<<"content-type">>, <<"application/json">>},
        {<<"content-length">>, <<"100">>}
    ],
    {ok, IsFin, HeaderBlock, _State2} = hackney_http2_machine:prepare_headers(
        StreamID, State1, nofin, PseudoHeaders, Headers),
    ?assertEqual(nofin, IsFin),
    ?assert(is_binary(iolist_to_binary(HeaderBlock))).

%%====================================================================
%% Frame Handling Tests
%%====================================================================

%% Test SETTINGS frame handling
settings_frame_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    %% Simulate receiving a SETTINGS frame from server
    SettingsFrame = {settings, #{initial_window_size => 32768}},
    {ok, State1} = hackney_http2_machine:frame(SettingsFrame, State0),
    ?assert(is_tuple(State1)).

%% Test SETTINGS ACK frame
settings_ack_frame_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    %% First receive settings from server
    SettingsFrame = {settings, #{}},
    {ok, State1} = hackney_http2_machine:frame(SettingsFrame, State0),
    %% Then receive ACK for our settings
    {ok, State2} = hackney_http2_machine:frame(settings_ack, State1),
    ?assert(is_tuple(State2)).

%% Test PING frame
ping_frame_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    %% First need to get past settings state
    {ok, State1} = hackney_http2_machine:frame({settings, #{}}, State0),
    %% Now handle PING
    PingFrame = {ping, 12345},
    {ok, State2} = hackney_http2_machine:frame(PingFrame, State1),
    ?assert(is_tuple(State2)).

%% Test PING ACK frame
ping_ack_frame_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, State1} = hackney_http2_machine:frame({settings, #{}}, State0),
    PingAckFrame = {ping_ack, 12345},
    {ok, State2} = hackney_http2_machine:frame(PingAckFrame, State1),
    ?assert(is_tuple(State2)).

%% Test GOAWAY frame
goaway_frame_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, State1} = hackney_http2_machine:frame({settings, #{}}, State0),
    GoawayFrame = {goaway, 0, no_error, <<>>},
    {ok, {goaway, 0, no_error, <<>>}, _State2} = hackney_http2_machine:frame(GoawayFrame, State1).

%%====================================================================
%% Window Management Tests
%%====================================================================

%% Test update_window for connection
update_window_connection_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    State1 = hackney_http2_machine:update_window(1000, State0),
    ?assert(is_tuple(State1)).

%% Test get connection local buffer size
get_connection_buffer_size_test() ->
    {ok, _Preface, State} = hackney_http2_machine:init(client, ?TEST_OPTS),
    BufferSize = hackney_http2_machine:get_connection_local_buffer_size(State),
    ?assertEqual(0, BufferSize).

%%====================================================================
%% Stream State Tests
%%====================================================================

%% Test get_stream_local_state
get_stream_local_state_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, StreamID, State1} = hackney_http2_machine:init_stream(<<"GET">>, State0),
    {ok, LocalState, BufferState} = hackney_http2_machine:get_stream_local_state(StreamID, State1),
    ?assertEqual(idle, LocalState),
    ?assertEqual(empty, BufferState).

%% Test get_stream_remote_state
get_stream_remote_state_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    {ok, StreamID, State1} = hackney_http2_machine:init_stream(<<"GET">>, State0),
    {ok, RemoteState} = hackney_http2_machine:get_stream_remote_state(StreamID, State1),
    ?assertEqual(idle, RemoteState).

%% Test stream not found
get_stream_state_not_found_test() ->
    {ok, _Preface, State} = hackney_http2_machine:init(client, ?TEST_OPTS),
    ?assertEqual({error, not_found}, hackney_http2_machine:get_stream_local_state(999, State)),
    ?assertEqual({error, not_found}, hackney_http2_machine:get_stream_remote_state(999, State)).

%%====================================================================
%% Last StreamID Tests
%%====================================================================

%% Test get/set last streamid
last_streamid_test() ->
    {ok, _Preface, State0} = hackney_http2_machine:init(client, ?TEST_OPTS),
    %% For a client, last_remote_streamid starts at 0 (no server-initiated streams yet)
    LastStreamID = hackney_http2_machine:get_last_streamid(State0),
    ?assertEqual(0, LastStreamID),
    %% Set last streamid (returns current remote streamid)
    {RemoteStreamID, State1} = hackney_http2_machine:set_last_streamid(State0),
    ?assertEqual(0, RemoteStreamID),
    ?assert(is_tuple(State1)).

%%====================================================================
%% Lingering Stream Tests
%%====================================================================

%% Test is_lingering_stream
is_lingering_stream_test() ->
    {ok, _Preface, State} = hackney_http2_machine:init(client, ?TEST_OPTS),
    ?assertEqual(false, hackney_http2_machine:is_lingering_stream(1, State)).

%%====================================================================
%% Test Suites
%%====================================================================

client_init_test_() ->
    [
        {"Client init", fun init_client_test/0},
        {"Server init", fun init_server_test/0},
        {"Init stream", fun init_stream_client_test/0},
        {"Stream ID increment", fun stream_id_increment_test/0}
    ].

settings_test_() ->
    [
        {"Custom settings", fun init_with_custom_settings_test/0},
        {"Get local setting", fun get_local_setting_test/0},
        {"Get remote settings", fun get_remote_settings_test/0}
    ].

headers_test_() ->
    [
        {"Prepare GET headers", fun prepare_headers_get_test/0},
        {"Prepare POST headers", fun prepare_headers_post_test/0}
    ].

frame_handling_test_() ->
    [
        {"SETTINGS frame", fun settings_frame_test/0},
        {"SETTINGS ACK", fun settings_ack_frame_test/0},
        {"PING frame", fun ping_frame_test/0},
        {"PING ACK frame", fun ping_ack_frame_test/0},
        {"GOAWAY frame", fun goaway_frame_test/0}
    ].

stream_state_test_() ->
    [
        {"Get stream local state", fun get_stream_local_state_test/0},
        {"Get stream remote state", fun get_stream_remote_state_test/0},
        {"Stream not found", fun get_stream_state_not_found_test/0},
        {"Last streamid", fun last_streamid_test/0},
        {"Is lingering stream", fun is_lingering_stream_test/0}
    ].
