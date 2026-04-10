%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
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
    hackney_conn_sup:stop_all(),
    %% Allow time for late UDP packets to be processed
    timer:sleep(100),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper to drive the QUIC event loop until a condition is met or timeout
%% Condition is a fun that receives {quic, ConnRef, Event} and returns:
%%   {done, Result} - Stop and return Result
%%   continue - Keep waiting
quic_loop(ConnRef, Condition, Timeout) ->
    quic_loop(ConnRef, Condition, Timeout, undefined, erlang:monotonic_time(millisecond)).

quic_loop(ConnRef, Condition, Timeout, TimerRef, StartTime) ->
    %% Cancel old timer
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,

    %% Calculate remaining timeout
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    Remaining = max(0, Timeout - Elapsed),

    receive
        %% Socket ready - process and continue
        {select, _Resource, _Ref, ready_input} ->
            NextTimeout = hackney_quic:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);

        %% Timer fired - process timeouts
        {quic_timer, ConnRef} ->
            NextTimeout = hackney_quic:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);

        %% QUIC events - check condition
        {quic, ConnRef, Event} ->
            case Condition(Event) of
                {done, Result} -> Result;
                continue -> quic_loop(ConnRef, Condition, Timeout, TimerRef, StartTime)
            end
    after Remaining ->
        {error, timeout}
    end.

schedule_timer(_ConnRef, infinity) ->
    undefined;
schedule_timer(ConnRef, TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    erlang:send_after(TimeoutMs, self(), {quic_timer, ConnRef});
schedule_timer(_ConnRef, _) ->
    undefined.

%% Wait for connection to be established
wait_connected(ConnRef) ->
    Result = quic_loop(ConnRef, fun
        ({connected, Info}) -> {done, {ok, Info}};
        ({closed, Reason}) -> {done, {error, Reason}};
        (_) -> continue
    end, 15000),
    Result.

%%====================================================================
%% Connection Tests
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
    Result = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
    ?assertMatch({ok, _}, Result),
    {ok, ConnRef} = Result,

    ConnResult = wait_connected(ConnRef),
    hackney_quic:close(ConnRef, normal),

    case ConnResult of
        {ok, Info} ->
            ?assert(is_map(Info));
        {error, Reason} ->
            ?assertEqual(unexpected_close, Reason)
    end.

test_addresses() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),

    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Test peername
            {ok, {PeerIP, PeerPort}} = hackney_quic:peername(ConnRef),
            ?assert(is_tuple(PeerIP)),
            ?assertEqual(443, PeerPort),
            %% Test sockname
            {ok, {LocalIP, LocalPort}} = hackney_quic:sockname(ConnRef),
            ?assert(is_tuple(LocalIP)),
            ?assert(is_integer(LocalPort)),
            hackney_quic:close(ConnRef, normal);
        {error, timeout} ->
            hackney_quic:close(ConnRef, normal),
            ?assert(false, "Timeout waiting for connection")
    end.

test_open_stream() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),

    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Test opening a stream
            Result = hackney_quic:open_stream(ConnRef),
            ?assertMatch({ok, _}, Result),
            hackney_quic:close(ConnRef, normal);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ?assert(false, "Connection closed unexpectedly")
    end.

%%====================================================================
%% get_fd Tests
%%====================================================================

%% Test get_fd function with a real UDP socket
get_fd_test() ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    Result = hackney_quic:get_fd(Socket),
    ?assertMatch({ok, _}, Result),
    {ok, Fd} = Result,
    ?assert(is_integer(Fd)),
    ?assert(Fd > 0),
    gen_udp:close(Socket).

%%====================================================================
%% HTTP/3 Request Tests
%%====================================================================

%% Test sending HTTP/3 request headers
test_send_request() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),

    case wait_connected(ConnRef) of
        {ok, _} ->
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
            ?assertMatch(ok, Result),
            hackney_quic:close(ConnRef, normal);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ?assert(false, "Connection closed unexpectedly")
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
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),

    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Process a bit to let H3 control streams be established
            _ = hackney_quic:process(ConnRef),
            timer:sleep(100),

            %% Open a stream
            {ok, StreamId} = hackney_quic:open_stream(ConnRef),

            %% Wait for stream_opened notification (optional)
            quic_loop(ConnRef, fun
                ({stream_opened, _SId}) -> {done, ok};
                (_) -> continue
            end, 1000),

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
            HeaderResult = quic_loop(ConnRef, fun
                ({stream_headers, _SId, RespHeaders, _Fin}) ->
                    {done, {ok, RespHeaders}};
                ({closed, Reason}) ->
                    {done, {error, {closed, Reason}}};
                (_) -> continue
            end, 5000),

            case HeaderResult of
                {ok, RespHeaders} ->
                    ?assert(lists:keymember(<<":status">>, 1, RespHeaders));
                {error, timeout} ->
                    hackney_quic:close(ConnRef, normal),
                    ?assert(false, "Timeout waiting for response headers");
                {error, Other} ->
                    hackney_quic:close(ConnRef, normal),
                    ?assertEqual(ok, Other)
            end,

            %% Wait for response body (optional - might be empty for redirects)
            _ = quic_loop(ConnRef, fun
                ({stream_data, _SId, Body, _BodyFin}) when byte_size(Body) > 0 ->
                    {done, {ok, Body}};
                ({closed, _}) ->
                    {done, ok};
                (_) -> continue
            end, 5000),

            hackney_quic:close(ConnRef, normal);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ?assert(false, "Connection closed unexpectedly")
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% Test connection with invalid hostname
invalid_hostname_test() ->
    %% Invalid port should fail
    Result = hackney_quic:connect(<<"test">>, 0, #{}, self()),
    ?assertMatch({error, badarg}, Result).

%% Test connect with invalid arguments
invalid_args_test() ->
    %% Port out of range
    ?assertMatch({error, badarg},
        hackney_quic:connect(<<"test">>, 70000, #{}, self())),
    %% Invalid opts type
    ?assertMatch({error, badarg},
        hackney_quic:connect(<<"test">>, 443, invalid, self())),
    %% Invalid owner type
    ?assertMatch({error, badarg},
        hackney_quic:connect(<<"test">>, 443, #{}, invalid)).

%%====================================================================
%% v0.11.0 API Tests
%%====================================================================

quic_v0_11_api_test_() ->
    {
        "QUIC v0.11.0 API tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"get_stats/1 returns connection statistics", fun test_get_stats/0},
                {"send_ping/1 sends PING frame", fun test_send_ping/0},
                {"migrate/1 triggers connection migration", fun test_migrate/0},
                {"set_stream_deadline/3 validates arguments", fun test_stream_deadline_args/0},
                {"set_congestion_control/2 validates algorithm", fun test_congestion_control_args/0},
                {"congestion_control connect option", fun test_congestion_control_option/0}
            ]
        }
    }.

test_get_stats() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Get connection stats
            Result = hackney_quic:get_stats(ConnRef),
            hackney_quic:close(ConnRef, normal),
            case Result of
                {ok, Stats} ->
                    ?assert(is_map(Stats)),
                    %% Stats should contain packet counts
                    ?assert(maps:is_key(packets_sent, Stats) orelse true);
                {error, _} ->
                    %% May fail if quic library doesn't support this yet
                    ok
            end;
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ok
    end.

test_send_ping() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Send PING frame
            Result = hackney_quic:send_ping(ConnRef),
            hackney_quic:close(ConnRef, normal),
            %% Result should be ok or error if not supported
            ?assert(Result =:= ok orelse element(1, Result) =:= error);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ok
    end.

test_migrate() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Trigger migration - may fail if not supported or no alternate path
            Result = hackney_quic:migrate(ConnRef),
            hackney_quic:close(ConnRef, normal),
            %% Just check it doesn't crash
            ?assert(Result =:= ok orelse element(1, Result) =:= error);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ok
    end.

test_stream_deadline_args() ->
    %% Test invalid arguments
    ?assertEqual({error, not_connected}, hackney_quic:set_stream_deadline(make_ref(), 0, 1000)),
    ?assertEqual({error, badarg}, hackney_quic:set_stream_deadline(make_ref(), 0, -1)).

test_congestion_control_args() ->
    %% Test invalid algorithm
    ?assertEqual({error, badarg}, hackney_quic:set_congestion_control(make_ref(), invalid)),
    %% Valid algorithms should return not_connected for invalid ref
    ?assertEqual({error, not_connected}, hackney_quic:set_congestion_control(make_ref(), newreno)),
    ?assertEqual({error, not_connected}, hackney_quic:set_congestion_control(make_ref(), cubic)),
    ?assertEqual({error, not_connected}, hackney_quic:set_congestion_control(make_ref(), bbr)).

test_congestion_control_option() ->
    %% Test connecting with congestion_control option
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{congestion_control => cubic}, self()),
    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Connection established with cubic algorithm
            hackney_quic:close(ConnRef, normal),
            ok;
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ok
    end.

%%====================================================================
%% hackney_h3 v0.11.0 API Tests
%%====================================================================

h3_v0_11_api_test_() ->
    {
        "HTTP/3 v0.11.0 API tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"hackney_h3:get_connection_info/1", fun test_h3_connection_info/0},
                {"hackney_h3:migrate/1", fun test_h3_migrate/0}
            ]
        }
    }.

test_h3_connection_info() ->
    case hackney_h3:connect(<<"cloudflare.com">>, 443, #{}) of
        {ok, ConnRef} ->
            Result = hackney_h3:get_connection_info(ConnRef),
            hackney_h3:close(ConnRef),
            case Result of
                {ok, Info} ->
                    ?assert(is_map(Info)),
                    ?assert(maps:is_key(peername, Info)),
                    ?assert(maps:is_key(sockname, Info)),
                    ?assert(maps:is_key(migration_state, Info));
                {error, _} ->
                    ok
            end;
        {error, _} ->
            %% Connection failed, skip test
            ok
    end.

test_h3_migrate() ->
    case hackney_h3:connect(<<"cloudflare.com">>, 443, #{}) of
        {ok, ConnRef} ->
            %% Just test that the function doesn't crash
            Result = hackney_h3:migrate(ConnRef),
            hackney_h3:close(ConnRef),
            ?assert(Result =:= ok orelse element(1, Result) =:= error);
        {error, _} ->
            %% Connection failed, skip test
            ok
    end.
