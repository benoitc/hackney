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
%%% Tests run against a local HTTP/3 server (hackney_h3_test_server).

-module(hackney_h3_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    hackney_h3_test_server:start().

cleanup(Server) ->
    hackney_conn_sup:stop_all(),
    hackney_h3_test_server:stop(Server).

with_server(Tests) ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Server) ->
         [{Title, {timeout, 30, fun() -> Test(Server) end}} || {Title, Test} <- Tests]
     end}.

connect(Server, Opts) ->
    hackney_h3:connect(hackney_h3_test_server:host(),
                       hackney_h3_test_server:port(Server), Opts, self()).

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
            NextTimeout = hackney_h3:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);
        {quic_timer, ConnRef} ->
            NextTimeout = hackney_h3:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);
        {h3, ConnRef, Event} ->
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
    end, 5000).

%% Accumulate full response (headers + body)
wait_response(ConnRef, StreamId, Timeout) ->
    wait_response_loop(ConnRef, StreamId, Timeout, undefined, [], <<>>, erlang:monotonic_time(millisecond)).

wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, Body, StartTime) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    Remaining = max(0, Timeout - Elapsed),
    receive
        {select, _Resource, _Ref, ready_input} ->
            _ = hackney_h3:process(ConnRef),
            wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, Body, StartTime);
        {quic_timer, ConnRef} ->
            _ = hackney_h3:process(ConnRef),
            wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, Body, StartTime);
        {h3, ConnRef, {stream_headers, StreamId, RespHeaders, _Fin}} ->
            NewStatus = get_status(RespHeaders),
            FilteredHeaders = filter_pseudo_headers(RespHeaders),
            wait_response_loop(ConnRef, StreamId, Timeout, NewStatus, FilteredHeaders, Body, StartTime);
        {h3, ConnRef, {stream_data, StreamId, Data, Fin}} ->
            NewBody = <<Body/binary, Data/binary>>,
            case Fin of
                true -> {ok, Status, Headers, NewBody};
                false -> wait_response_loop(ConnRef, StreamId, Timeout, Status, Headers, NewBody, StartTime)
            end;
        {h3, ConnRef, {stream_reset, StreamId, ErrorCode}} ->
            {error, {stream_reset, ErrorCode}};
        {h3, ConnRef, {closed, Reason}} ->
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
    {"HTTP/3 TLS verification tests",
     with_server([
         {"Connect without TLS verification", fun test_connect_no_verify/1},
         {"Connect with TLS verification against the test CA", fun test_connect_with_verify/1},
         {"Verification fails without the test CA", fun test_connect_verify_untrusted/1}
     ])}.

test_connect_no_verify(Server) ->
    {ok, ConnRef} = connect(Server, #{verify => false}),
    Result = wait_connected(ConnRef),
    hackney_h3:close(ConnRef, normal),
    ?assertMatch({ok, _}, Result).

test_connect_with_verify(Server) ->
    %% The server certificate is issued by the test CA for 127.0.0.1 and
    %% localhost, so it verifies once that CA is trusted.
    Opts = #{verify => true, cacerts => hackney_h3_test_server:ca_cacerts()},
    {ok, ConnRef} = connect(Server, Opts),
    Result = wait_connected(ConnRef),
    hackney_h3:close(ConnRef, normal),
    ?assertMatch({ok, _}, Result).

test_connect_verify_untrusted(Server) ->
    Result = case connect(Server, #{verify => true}) of
        {ok, ConnRef} ->
            R = wait_connected(ConnRef),
            hackney_h3:close(ConnRef, normal),
            R;
        {error, _} = Error ->
            Error
    end,
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Redirect Tests
%%====================================================================

redirect_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Server) ->
         [{"Detect 301 redirect", {timeout, 30, fun() -> test_detect_301(Server) end}},
          {"Detect 302 redirect", {timeout, 30, fun() -> test_detect_302(Server) end}},
          {"Extract Location header", fun test_location_extraction/0},
          {"Redirect to different path", {timeout, 30, fun() -> test_redirect_path(Server) end}}]
     end}.

test_detect_301(Server) ->
    {ok, Status, Headers, _Body} = make_h3_request(Server, <<"/status/301">>),
    ?assertEqual(301, Status),
    ?assertEqual({ok, <<"/">>}, get_location(Headers)).

test_detect_302(Server) ->
    {ok, Status, Headers, _Body} = make_h3_request(Server, <<"/status/302">>),
    ?assertEqual(302, Status),
    ?assertEqual({ok, <<"/">>}, get_location(Headers)).

test_location_extraction() ->
    %% Test that we can properly extract Location header from redirect response
    Headers = [
        {<<":status">>, <<"301">>},
        {<<"location">>, <<"https://www.example.com/new-path">>},
        {<<"content-length">>, <<"0">>}
    ],
    FilteredHeaders = filter_pseudo_headers(Headers),
    ?assertMatch({ok, <<"https://www.example.com/new-path">>}, get_location(FilteredHeaders)).

test_redirect_path(Server) ->
    %% Redirect to a different path on the same host.
    {ok, Status, Headers, _Body} = make_h3_request(Server, <<"/redirect/1">>),
    ?assertEqual(302, Status),
    ?assertEqual({ok, <<"/redirect/0">>}, get_location(Headers)).

%%====================================================================
%% HTTP Methods Tests
%%====================================================================

methods_test_() ->
    {"HTTP/3 method tests",
     with_server([
         {"GET request", fun test_get_request/1},
         {"HEAD request", fun test_head_request/1},
         {"POST request", fun test_post_request/1},
         {"Multiple requests on same connection", fun test_multiple_requests/1}
     ])}.

test_get_request(Server) ->
    {ok, Status, Headers, Body} = make_h3_request(Server, <<"/">>),
    ?assertEqual(200, Status),
    ?assertEqual(<<"text/html">>, proplists:get_value(<<"content-type">>, Headers)),
    ?assertEqual(<<"<html><body>hackney h3 test server</body></html>">>, Body).

test_head_request(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/">>),
    {ok, Status, _RespHeaders, Body} =
        hackney_h3:request(head, URL, [], <<>>, hackney_h3_test_server:h3_opts()),
    ?assertEqual(200, Status),
    ?assertEqual(<<>>, Body).

test_post_request(Server) ->
    {ok, ConnRef} = connect(Server, hackney_h3_test_server:h3_opts()),
    {ok, _} = wait_connected(ConnRef),
    Headers = [
        {<<":method">>, <<"POST">>},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, hackney_h3_test_server:host()},
        {<<":path">>, <<"/echo">>},
        {<<"content-type">>, <<"application/json">>},
        {<<"content-length">>, <<"2">>},
        {<<"user-agent">>, <<"hackney-h3-test/1.0">>}
    ],
    {ok, StreamId} = hackney_h3:send_request(ConnRef, Headers, false),
    ok = hackney_h3:send_data(ConnRef, StreamId, <<"{}">>, true),
    {ok, Status, RespHeaders, Body} = wait_response(ConnRef, StreamId, 5000),
    hackney_h3:close(ConnRef, normal),
    ?assertEqual(200, Status),
    ?assertEqual(<<"application/json">>,
                 proplists:get_value(<<"content-type">>, RespHeaders)),
    ?assertEqual(<<"{}">>, Body).

test_multiple_requests(Server) ->
    {ok, ConnRef} = connect(Server, hackney_h3_test_server:h3_opts()),
    {ok, _} = wait_connected(ConnRef),
    Host = hackney_h3_test_server:host(),
    %% Two concurrent requests on the same connection.
    {ok, StreamId1} = hackney_h3:send_request(ConnRef, build_get_headers(Host, <<"/">>), true),
    {ok, StreamId2} = hackney_h3:send_request(ConnRef,
                                              build_get_headers(Host, <<"/cdn-cgi/trace">>),
                                              true),
    ?assertNotEqual(StreamId1, StreamId2),
    ?assertMatch({ok, 200, _, _}, wait_response(ConnRef, StreamId1, 5000)),
    ?assertMatch({ok, 200, _, <<"h=127.0.0.1\nhttp=http/3\n">>},
                 wait_response(ConnRef, StreamId2, 5000)),
    hackney_h3:close(ConnRef, normal).

%%====================================================================
%% High-level API Tests
%%====================================================================

high_level_api_test_() ->
    {"HTTP/3 high-level API tests",
     with_server([
         {"hackney_h3:request/5", fun test_h3_simple_request/1},
         {"hackney_h3:request/5 with headers and options", fun test_h3_request_with_options/1},
         {"hackney_h3:connect/3", fun test_h3_connect_api/1}
     ])}.

test_h3_simple_request(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/">>),
    ?assertMatch({ok, 200, _, _},
                 hackney_h3:request(get, URL, [], <<>>, hackney_h3_test_server:h3_opts())).

test_h3_request_with_options(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/cdn-cgi/trace">>),
    Options = (hackney_h3_test_server:h3_opts())#{timeout => 5000, recv_timeout => 5000},
    Headers = [{<<"user-agent">>, <<"hackney-test/1.0">>}],
    ?assertMatch({ok, 200, _, <<"h=127.0.0.1\nhttp=http/3\n">>},
                 hackney_h3:request(get, URL, Headers, <<>>, Options)).

test_h3_connect_api(Server) ->
    {ok, ConnRef} = hackney_h3:connect(hackney_h3_test_server:host(),
                                       hackney_h3_test_server:port(Server),
                                       hackney_h3_test_server:h3_opts()),
    ?assert(is_reference(ConnRef)),
    hackney_h3:close(ConnRef).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {"HTTP/3 error handling tests",
     [{"Nothing listening", {timeout, 30, fun test_invalid_host/0}},
      {"Connection timeout", {timeout, 30, fun test_connection_timeout/0}},
      {"Invalid port", fun test_invalid_port/0}]}.

test_invalid_host() ->
    %% Nothing listens on this port: the request fails instead of hanging.
    {ok, _} = application:ensure_all_started(hackney),
    URL = iolist_to_binary(["https://127.0.0.1:",
                            integer_to_list(hackney_h3_test_server:unused_port()), "/"]),
    Opts = (hackney_h3_test_server:h3_opts())#{timeout => 1000},
    ?assertMatch({error, _}, hackney_h3:request(get, URL, [], <<>>, Opts)).

test_connection_timeout() ->
    {ok, _} = application:ensure_all_started(hackney),
    Opts = (hackney_h3_test_server:h3_opts())#{timeout => 100},
    ?assertMatch({error, _},
                 hackney_h3:connect(hackney_h3_test_server:host(),
                                    hackney_h3_test_server:unused_port(), Opts)).

test_invalid_port() ->
    ?assertMatch({error, badarg}, hackney_h3:connect(<<"test">>, 0, #{}, self())),
    ?assertMatch({error, badarg}, hackney_h3:connect(<<"test">>, 70000, #{}, self())).

%%====================================================================
%% Internal Functions
%%====================================================================

make_h3_request(Server, Path) ->
    {ok, ConnRef} = connect(Server, hackney_h3_test_server:h3_opts()),
    {ok, _} = wait_connected(ConnRef),
    Headers = build_get_headers(hackney_h3_test_server:host(), Path),
    {ok, StreamId} = hackney_h3:send_request(ConnRef, Headers, true),
    Result = wait_response(ConnRef, StreamId, 5000),
    hackney_h3:close(ConnRef, normal),
    Result.

build_get_headers(Host, Path) ->
    [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"https">>},
        {<<":authority">>, Host},
        {<<":path">>, Path},
        {<<"user-agent">>, <<"hackney-h3-test/1.0">>},
        {<<"accept">>, <<"*/*">>}
    ].
