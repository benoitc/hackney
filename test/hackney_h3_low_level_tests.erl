%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Low-level tests for the HTTP/3 adapter in hackney_h3.
%%%

-module(hackney_h3_low_level_tests).

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

connect(Server) ->
    hackney_h3:connect(hackney_h3_test_server:host(),
                       hackney_h3_test_server:port(Server),
                       hackney_h3_test_server:h3_opts(), self()).

get_headers(Path) ->
    [{<<":method">>, <<"GET">>},
     {<<":scheme">>, <<"https">>},
     {<<":authority">>, hackney_h3_test_server:host()},
     {<<":path">>, Path},
     {<<"user-agent">>, <<"hackney-quic-test/1.0">>}].

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper to drive the QUIC event loop until a condition is met or timeout
%% Condition is a fun that receives {h3, ConnRef, Event} and returns:
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
            NextTimeout = hackney_h3:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);

        %% Timer fired - process timeouts
        {quic_timer, ConnRef} ->
            NextTimeout = hackney_h3:process(ConnRef),
            NewTimer = schedule_timer(ConnRef, NextTimeout),
            quic_loop(ConnRef, Condition, Timeout, NewTimer, StartTime);

        %% QUIC events - check condition
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

%% QUIC connection to a local HTTP/3 server.
quic_connection_test_() ->
    {"QUIC connection tests",
     with_server([
         {"Connect to a local server", fun test_local_connect/1},
         {"Test stream opening", fun test_open_stream/1}
     ])}.

test_local_connect(Server) ->
    {ok, ConnRef} = connect(Server),
    ConnResult = wait_connected(ConnRef),
    hackney_h3:close(ConnRef, normal),
    ?assertMatch({ok, #{}}, ConnResult).

test_open_stream(Server) ->
    {ok, ConnRef} = connect(Server),
    {ok, _} = wait_connected(ConnRef),
    %% send_request atomically opens a stream and sends HEADERS
    ?assertMatch({ok, _}, hackney_h3:send_request(ConnRef, get_headers(<<"/">>), true)),
    hackney_h3:close(ConnRef, normal).

%%====================================================================
%% get_fd Tests
%%====================================================================

%% Test get_fd function with a real UDP socket
get_fd_test() ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    Result = hackney_h3:get_fd(Socket),
    ?assertMatch({ok, _}, Result),
    {ok, Fd} = Result,
    ?assert(is_integer(Fd)),
    ?assert(Fd > 0),
    gen_udp:close(Socket).

%%====================================================================
%% HTTP/3 Request Tests
%%====================================================================

%% Test sending HTTP/3 request headers
test_send_request(Server) ->
    {ok, ConnRef} = connect(Server),
    {ok, _} = wait_connected(ConnRef),
    ?assertMatch({ok, _}, hackney_h3:send_request(ConnRef, get_headers(<<"/">>), true)),
    hackney_h3:close(ConnRef, normal).

http3_request_test_() ->
    {"HTTP/3 request tests",
     with_server([
         {"Send HTTP/3 headers", fun test_send_request/1},
         {"Full HTTP/3 request/response", fun test_full_request_response/1}
     ])}.

%% Test full HTTP/3 request and response flow
test_full_request_response(Server) ->
    {ok, ConnRef} = connect(Server),
    {ok, _} = wait_connected(ConnRef),
    {ok, StreamId} = hackney_h3:send_request(ConnRef, get_headers(<<"/cdn-cgi/trace">>), true),
    {ok, RespHeaders} = quic_loop(ConnRef, fun
        ({stream_headers, SId, Hdrs, _Fin}) when SId =:= StreamId -> {done, {ok, Hdrs}};
        ({closed, Reason}) -> {done, {error, {closed, Reason}}};
        (_) -> continue
    end, 15000),
    ?assertEqual({<<":status">>, <<"200">>}, lists:keyfind(<<":status">>, 1, RespHeaders)),
    %% Exactly one :status: two would be a malformed response.
    ?assertEqual(1, length([H || {<<":status">>, _} = H <- RespHeaders])),
    ?assertEqual(<<"h=127.0.0.1\nhttp=http/3\n">>, read_body(ConnRef, StreamId, <<>>)),
    hackney_h3:close(ConnRef, normal).

%% Collect DATA until the frame that carries FIN.
read_body(ConnRef, StreamId, Acc) ->
    case quic_loop(ConnRef, fun
        ({stream_data, SId, Data, Fin}) when SId =:= StreamId -> {done, {Data, Fin}};
        ({closed, Reason}) -> {done, {error, {closed, Reason}}};
        (_) -> continue
    end, 15000) of
        {Data, true} -> <<Acc/binary, Data/binary>>;
        {Data, false} -> read_body(ConnRef, StreamId, <<Acc/binary, Data/binary>>);
        Error -> Error
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% Test connection with invalid hostname
invalid_hostname_test() ->
    %% Invalid port should fail
    Result = hackney_h3:connect(<<"test">>, 0, #{}, self()),
    ?assertMatch({error, badarg}, Result).

%% Test connect with invalid arguments
invalid_args_test() ->
    %% Port out of range
    ?assertMatch({error, badarg},
        hackney_h3:connect(<<"test">>, 70000, #{}, self())),
    %% Invalid opts type
    ?assertMatch({error, badarg},
        hackney_h3:connect(<<"test">>, 443, invalid, self())),
    %% Invalid owner type
    ?assertMatch({error, badarg},
        hackney_h3:connect(<<"test">>, 443, #{}, invalid)).
