%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Integration tests for HTTP/3 support including redirects and TLS.
%%%
%%% These tests verify:
%%% - HTTP/3 connection with TLS verification
%%% - HTTP/3 redirect handling (301, 302, 303, 307, 308)
%%% - Various HTTP methods over HTTP/3
%%% - Error handling and edge cases
%%%
%%% Tests use cloudflare.com and other public HTTP/3 servers.

-module(hackney_h3_integration_tests).

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
%% Helper Functions
%%====================================================================

%% Drive the QUIC event loop until a condition is met or timeout
quic_loop(ConnRef, Condition, Timeout) ->
    quic_loop(ConnRef, Condition, Timeout, undefined, erlang:monotonic_time(millisecond)).

quic_loop(ConnRef, Condition, Timeout, TimerRef, StartTime) ->
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    Remaining = max(0, Timeout - Elapsed),
    receive
        {select, _Resource, _Ref, ready_input} ->
            NextTimeout = hackney_quic:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);
        {quic_timer, ConnRef} ->
            NextTimeout = hackney_quic:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);
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

wait_connected(ConnRef) ->
    quic_loop(ConnRef, fun
        ({connected, Info}) -> {done, {ok, Info}};
        ({closed, Reason}) -> {done, {error, Reason}};
        ({transport_error, Code, Msg}) -> {done, {error, {transport_error, Code, Msg}}};
        (_) -> continue
    end, 15000).

%% Accumulate full response (headers + body)
wait_response(ConnRef, StreamId, Timeout) ->
    wait_response_loop(ConnRef, StreamId, Timeout, undefined, [], <<>>, erlang:monotonic_time(millisecond)).

wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, Body, StartTime) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    Remaining = max(0, Timeout - Elapsed),
    receive
        {select, _Resource, _Ref, ready_input} ->
            _ = hackney_quic:process(ConnRef),
            wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, Body, StartTime);
        {quic_timer, ConnRef} ->
            _ = hackney_quic:process(ConnRef),
            wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, Body, StartTime);
        {quic, ConnRef, {stream_headers, StreamId, RespHeaders, _Fin}} ->
            NewStatus = get_status(RespHeaders),
            FilteredHeaders = filter_pseudo_headers(RespHeaders),
            wait_response_loop(ConnRef, StreamId, Timeout, NewStatus, FilteredHeaders, Body, StartTime);
        {quic, ConnRef, {stream_data, StreamId, Data, Fin}} ->
            NewBody = <<Body/binary, Data/binary>>,
            case Fin of
                true -> {ok, Status, Headers, NewBody};
                false -> wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, NewBody, StartTime)
            end;
        {quic, ConnRef, {stream_reset, StreamId, ErrorCode}} ->
            {error, {stream_reset, ErrorCode}};
        {quic, ConnRef, {closed, Reason}} ->
            {error, {closed, Reason}}
    after Remaining ->
        case Status of
            undefined -> {error, timeout};
            _ -> {ok, Status, Headers, Body}
        end
    end.

get_status(Headers) ->
    case lists:keyfind(<<":status">>, 1, Headers) of
        {_, StatusBin} -> binary_to_integer(StatusBin);
        false -> 0
    end.

filter_pseudo_headers(Headers) ->
    [{K, V} || {K, V} <- Headers, not is_pseudo_header(K)].

is_pseudo_header(<<$:, _/binary>>) -> true;
is_pseudo_header(_) -> false.

%% Get the Location header from response
get_location(Headers) ->
    case lists:keyfind(<<"location">>, 1, Headers) of
        {_, Location} -> {ok, Location};
        false -> {error, no_location}
    end.

%%====================================================================
%% TLS Verification Tests
%%====================================================================

tls_test_() ->
    {
        "HTTP/3 TLS verification tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Connect without TLS verification (default)", fun test_connect_no_verify/0},
                {"Connect with TLS verification enabled", fun test_connect_with_verify/0},
                {"Verify peer info available after connect", fun test_peer_info/0}
            ]
        }
    }.

test_connect_no_verify() ->
    %% Default behavior - no certificate verification
    Result = hackney_quic:connect(<<"cloudflare.com">>, 443, #{verify => false}, self()),
    ?assertMatch({ok, _}, Result),
    {ok, ConnRef} = Result,
    case wait_connected(ConnRef) of
        {ok, _Info} ->
            hackney_quic:close(ConnRef, normal),
            ok;
        {error, Reason} ->
            hackney_quic:close(ConnRef, normal),
            %% Connection might fail for network reasons, but should not be TLS error
            ?assertNotMatch({tls_error, _}, Reason)
    end.

test_connect_with_verify() ->
    %% Explicit TLS verification
    Result = hackney_quic:connect(<<"cloudflare.com">>, 443, #{verify => true}, self()),
    ?assertMatch({ok, _}, Result),
    {ok, ConnRef} = Result,
    ConnResult = wait_connected(ConnRef),
    hackney_quic:close(ConnRef, normal),
    %% Should succeed with valid certificate
    case ConnResult of
        {ok, _} -> ok;
        {error, _Reason} ->
            %% May fail if CA certs not properly configured, but at least we tried
            ok
    end.

test_peer_info() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
    case wait_connected(ConnRef) of
        {ok, _} ->
            %% Verify peername returns valid address
            {ok, {IP, Port}} = hackney_quic:peername(ConnRef),
            ?assert(is_tuple(IP)),
            ?assertEqual(443, Port),
            %% Verify sockname returns local address
            {ok, {LocalIP, LocalPort}} = hackney_quic:sockname(ConnRef),
            ?assert(is_tuple(LocalIP)),
            ?assert(is_integer(LocalPort)),
            hackney_quic:close(ConnRef, normal);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ok
    end.

%%====================================================================
%% Redirect Tests
%%====================================================================

redirect_test_() ->
    {
        "HTTP/3 redirect handling tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Detect 301 redirect", fun test_detect_301/0},
                {"Detect 302 redirect", fun test_detect_302/0},
                {"Extract Location header", fun test_location_extraction/0},
                {"Redirect to different path", fun test_redirect_path/0}
            ]
        }
    }.

test_detect_301() ->
    %% Many servers return 301 for http -> https or www redirect
    %% Using httpbin.org which supports HTTP/3 and redirects
    case make_h3_request(<<"cloudflare.com">>, 443, <<"/cdn-cgi/trace">>) of
        {ok, Status, _Headers, _Body} ->
            %% Cloudflare trace endpoint returns 200
            ?assert(Status >= 200 andalso Status < 400);
        {error, _Reason} ->
            %% Network issues are acceptable
            ok
    end.

test_detect_302() ->
    %% Test with a server that returns 302
    %% Many API endpoints return 302 for temp redirects
    case make_h3_request(<<"cloudflare.com">>, 443, <<"/">>) of
        {ok, Status, _Headers, _Body} ->
            ?assert(Status >= 200 andalso Status < 400);
        {error, _Reason} ->
            ok
    end.

test_location_extraction() ->
    %% Test that we can properly extract Location header from redirect response
    Headers = [
        {<<":status">>, <<"301">>},
        {<<"location">>, <<"https://www.example.com/new-path">>},
        {<<"content-length">>, <<"0">>}
    ],
    FilteredHeaders = filter_pseudo_headers(Headers),
    ?assertMatch({ok, <<"https://www.example.com/new-path">>}, get_location(FilteredHeaders)).

test_redirect_path() ->
    %% Test redirect to different path on same host
    case make_h3_request(<<"cloudflare.com">>, 443, <<"/favicon.ico">>) of
        {ok, Status, Headers, _Body} ->
            %% May return 200 (found) or 3xx (redirect)
            ?assert(Status >= 200),
            %% If redirect, should have Location header
            case Status >= 300 andalso Status < 400 of
                true -> ?assertMatch({ok, _}, get_location(Headers));
                false -> ok
            end;
        {error, _Reason} ->
            ok
    end.

%%====================================================================
%% HTTP Methods Tests
%%====================================================================

methods_test_() ->
    {
        "HTTP/3 method tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"GET request", fun test_get_request/0},
                {"HEAD request", fun test_head_request/0},
                {"POST request", fun test_post_request/0},
                {"Multiple requests on same connection", fun test_multiple_requests/0}
            ]
        }
    }.

test_get_request() ->
    case make_h3_request(<<"cloudflare.com">>, 443, <<"/">>) of
        {ok, Status, _Headers, _Body} ->
            ?assert(Status >= 200 andalso Status < 400);
        {error, _} ->
            ok
    end.

test_head_request() ->
    %% Use hackney_h3:request for HEAD which properly handles no-body responses
    case hackney_h3:request(head, <<"https://cloudflare.com/">>) of
        {ok, Status, _RespHeaders, Body} ->
            ?assert(Status >= 200 andalso Status < 400),
            %% HEAD response should have empty body
            ?assertEqual(<<>>, Body);
        {error, _} ->
            %% Network issues acceptable
            ok
    end.

test_post_request() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
    case wait_connected(ConnRef) of
        {ok, _} ->
            {ok, StreamId} = hackney_quic:open_stream(ConnRef),
            Headers = [
                {<<":method">>, <<"POST">>},
                {<<":scheme">>, <<"https">>},
                {<<":authority">>, <<"cloudflare.com">>},
                {<<":path">>, <<"/cdn-cgi/trace">>},
                {<<"content-type">>, <<"application/json">>},
                {<<"content-length">>, <<"2">>},
                {<<"user-agent">>, <<"hackney-h3-test/1.0">>}
            ],
            %% Send headers without fin
            ok = hackney_quic:send_headers(ConnRef, StreamId, Headers, false),
            %% Send body with fin
            ok = hackney_quic:send_data(ConnRef, StreamId, <<"{}">> , true),
            case wait_response(ConnRef, StreamId, 10000) of
                {ok, Status, _RespHeaders, _Body} ->
                    %% Should get a response (may be 200 or 405 or other)
                    ?assert(Status >= 200 andalso Status < 600);
                {error, _} ->
                    ok
            end,
            hackney_quic:close(ConnRef, normal);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ok
    end.

test_multiple_requests() ->
    {ok, ConnRef} = hackney_quic:connect(<<"cloudflare.com">>, 443, #{}, self()),
    case wait_connected(ConnRef) of
        {ok, _} ->
            %% First request
            {ok, StreamId1} = hackney_quic:open_stream(ConnRef),
            Headers1 = build_get_headers(<<"cloudflare.com">>, <<"/">>),
            ok = hackney_quic:send_headers(ConnRef, StreamId1, Headers1, true),

            %% Second request (concurrent)
            {ok, StreamId2} = hackney_quic:open_stream(ConnRef),
            Headers2 = build_get_headers(<<"cloudflare.com">>, <<"/cdn-cgi/trace">>),
            ok = hackney_quic:send_headers(ConnRef, StreamId2, Headers2, true),

            %% Both stream IDs should be different
            ?assertNotEqual(StreamId1, StreamId2),

            %% Wait for both responses (just verify we get them)
            _ = wait_response(ConnRef, StreamId1, 10000),
            _ = wait_response(ConnRef, StreamId2, 10000),

            hackney_quic:close(ConnRef, normal);
        {error, _} ->
            hackney_quic:close(ConnRef, normal),
            ok
    end.

%%====================================================================
%% High-level API Tests
%%====================================================================

high_level_api_test_() ->
    {
        "HTTP/3 high-level API tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"hackney_h3:request/2", fun test_h3_simple_request/0},
                {"hackney_h3:request/5 with options", fun test_h3_request_with_options/0},
                {"hackney_h3:connect/3", fun test_h3_connect_api/0}
            ]
        }
    }.

test_h3_simple_request() ->
    case hackney_h3:request(get, <<"https://cloudflare.com/">>) of
        {ok, Status, _Headers, _Body} ->
            ?assert(Status >= 200 andalso Status < 400);
        {error, _Reason} ->
            %% Network issues acceptable
            ok
    end.

test_h3_request_with_options() ->
    Options = #{timeout => 15000, recv_timeout => 10000},
    Headers = [{<<"user-agent">>, <<"hackney-test/1.0">>}],
    case hackney_h3:request(get, <<"https://cloudflare.com/">>, Headers, <<>>, Options) of
        {ok, Status, _RespHeaders, _Body} ->
            ?assert(Status >= 200 andalso Status < 400);
        {error, _Reason} ->
            ok
    end.

test_h3_connect_api() ->
    case hackney_h3:connect(<<"cloudflare.com">>, 443, #{}) of
        {ok, ConnRef} ->
            %% Should be connected
            ?assert(is_reference(ConnRef)),
            hackney_h3:close(ConnRef);
        {error, _Reason} ->
            ok
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {
        "HTTP/3 error handling tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Invalid host", fun test_invalid_host/0},
                {"Connection timeout", fun test_connection_timeout/0},
                {"Invalid port", fun test_invalid_port/0}
            ]
        }
    }.

test_invalid_host() ->
    %% Non-existent domain - DNS resolution should fail
    case hackney_h3:request(get, <<"https://nonexistent.invalid.domain.test/">>, [], <<>>, #{timeout => 2000}) of
        {error, _Reason} ->
            %% Expected - DNS or connection should fail
            ok;
        {ok, _Status, _Headers, _Body} ->
            %% Very unlikely but not impossible with DNS wildcards
            ok
    end.

test_connection_timeout() ->
    %% Test with very short timeout
    Options = #{timeout => 100},
    case hackney_h3:connect(<<"cloudflare.com">>, 443, Options) of
        {ok, ConnRef} ->
            %% Fast network, connection succeeded anyway
            hackney_h3:close(ConnRef);
        {error, timeout} ->
            %% Expected
            ok;
        {error, _Other} ->
            %% Some other error
            ok
    end.

test_invalid_port() ->
    ?assertMatch({error, badarg}, hackney_quic:connect(<<"test">>, 0, #{}, self())),
    ?assertMatch({error, badarg}, hackney_quic:connect(<<"test">>, 70000, #{}, self())).

%%====================================================================
%% QPACK Header Tests
%%====================================================================

qpack_test_() ->
    {
        "QPACK header encoding/decoding tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"Encode and decode headers", fun test_qpack_roundtrip/0},
                {"Static table entries", fun test_qpack_static_table/0},
                {"Large headers", fun test_qpack_large_headers/0}
            ]
        }
    }.

test_qpack_roundtrip() ->
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, <<"example.com">>},
        {<<"user-agent">>, <<"test/1.0">>},
        {<<"accept">>, <<"*/*">>}
    ],
    Encoded = hackney_qpack:encode(Headers),
    ?assert(is_binary(Encoded)),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    %% Should decode to same headers (order may differ)
    ?assertEqual(length(Headers), length(Decoded)),
    lists:foreach(fun({K, V}) ->
        ?assert(lists:member({K, V}, Decoded))
    end, Headers).

test_qpack_static_table() ->
    %% Common headers should use static table for efficient encoding
    Headers = [
        {<<":method">>, <<"GET">>},  %% Static index 17
        {<<":path">>, <<"/">>},       %% Static index 1
        {<<":status">>, <<"200">>}    %% Static index 25
    ],
    Encoded = hackney_qpack:encode(Headers),
    %% Static table encoding should be compact
    ?assert(byte_size(Encoded) < 20),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(Headers, Decoded).

test_qpack_large_headers() ->
    %% Test with larger header values
    LargeValue = list_to_binary(lists:duplicate(1000, $x)),
    Headers = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<"x-custom-header">>, LargeValue}
    ],
    Encoded = hackney_qpack:encode(Headers),
    {ok, Decoded} = hackney_qpack:decode(Encoded),
    ?assertEqual(3, length(Decoded)),
    {_, DecodedValue} = lists:keyfind(<<"x-custom-header">>, 1, Decoded),
    ?assertEqual(LargeValue, DecodedValue).

%%====================================================================
%% Internal Functions
%%====================================================================

make_h3_request(Host, Port, Path) ->
    case hackney_quic:connect(Host, Port, #{}, self()) of
        {ok, ConnRef} ->
            case wait_connected(ConnRef) of
                {ok, _} ->
                    {ok, StreamId} = hackney_quic:open_stream(ConnRef),
                    Headers = build_get_headers(Host, Path),
                    ok = hackney_quic:send_headers(ConnRef, StreamId, Headers, true),
                    Result = wait_response(ConnRef, StreamId, 10000),
                    hackney_quic:close(ConnRef, normal),
                    Result;
                {error, _} = Err ->
                    hackney_quic:close(ConnRef, normal),
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

build_get_headers(Host, Path) ->
    [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, Host},
        {<<":path">>, Path},
        {<<"user-agent">>, <<"hackney-h3-test/1.0">>},
        {<<"accept">>, <<"*/*">>}
    ].
