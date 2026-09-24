%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for protocol selection in hackney.
%%%

-module(hackney_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

cleanup(_) ->
    hackney_conn_sup:stop_all(),
    %% Reset to default after tests
    application:set_env(hackney, default_protocols, [http2, http1]),
    ok.

%%====================================================================
%% Default Protocol Tests
%%====================================================================

%% Test that default protocols are [http2, http1]
default_protocols_test() ->
    setup(),
    Protocols = hackney_util:default_protocols(),
    ?assertEqual([http2, http1], Protocols).

%% Test that ALPN opts default to h2 and http/1.1
default_alpn_opts_test() ->
    setup(),
    Opts = hackney_ssl:alpn_opts([]),
    ?assertEqual([{alpn_advertised_protocols, [<<"h2">>, <<"http/1.1">>]}], Opts).

%%====================================================================
%% HTTP/3 Opt-In Tests
%%====================================================================

%% Test that http3 can be enabled via protocols option
http3_opt_in_alpn_test() ->
    setup(),
    %% When http3 is in protocols, ALPN still only includes h2/http1.1
    %% (HTTP/3 uses QUIC, not TLS ALPN)
    Opts = hackney_ssl:alpn_opts([{protocols, [http3, http2, http1]}]),
    ?assertEqual([{alpn_advertised_protocols, [<<"h2">>, <<"http/1.1">>]}], Opts).

%% Test that application env can change default protocols
app_env_protocols_test() ->
    setup(),
    %% Set default to include http3
    application:set_env(hackney, default_protocols, [http3, http2, http1]),
    Protocols = hackney_util:default_protocols(),
    ?assertEqual([http3, http2, http1], Protocols),
    %% Reset
    application:set_env(hackney, default_protocols, [http2, http1]).

%% Test that per-request protocols override default
per_request_protocols_test() ->
    setup(),
    %% Default is [http2, http1]
    ?assertEqual([http2, http1], hackney_util:default_protocols()),
    %% Per-request option should work (tested via ALPN opts)
    %% http1 only
    ?assertEqual(
        [{alpn_advertised_protocols, [<<"http/1.1">>]}],
        hackney_ssl:alpn_opts([{protocols, [http1]}])
    ),
    %% http2 only
    ?assertEqual(
        [{alpn_advertised_protocols, [<<"h2">>]}],
        hackney_ssl:alpn_opts([{protocols, [http2]}])
    ).

%%====================================================================
%% HTTP/3 Request Opt-In Test (local HTTP/3 server)
%%====================================================================

http3_opt_in_request_test_() ->
    {
        "HTTP/3 opt-in request tests",
        {
            setup,
            fun() -> setup(), hackney_h3_test_server:start() end,
            fun(Server) -> cleanup(Server), hackney_h3_test_server:stop(Server) end,
            fun(Server) ->
                [{"HTTP/3 opt-in to a local server",
                  {timeout, 30, fun() -> test_http3_opt_in(Server) end}}]
            end
        }
    }.

test_http3_opt_in(Server) ->
    %% hackney_h3 connects over QUIC once HTTP/3 is opted in.
    {ok, ConnRef} = hackney_h3:connect(hackney_h3_test_server:host(),
                                       hackney_h3_test_server:port(Server),
                                       hackney_h3_test_server:h3_opts(), self()),
    Result = quic_loop(ConnRef, fun
        ({connected, _Info}) -> {done, connected};
        ({closed, Reason}) -> {done, {closed, Reason}};
        (_) -> continue
    end, 15000),
    hackney_h3:close(ConnRef, normal),
    ?assertEqual(connected, Result).

%% Helper to drive the QUIC event loop until a condition is met or timeout
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

%%====================================================================
%% Test Suites
%%====================================================================

protocol_selection_test_() ->
    [
        {"Default protocols are [http2, http1]", fun default_protocols_test/0},
        {"Default ALPN opts", fun default_alpn_opts_test/0},
        {"HTTP/3 opt-in ALPN", fun http3_opt_in_alpn_test/0},
        {"App env protocols", fun app_env_protocols_test/0},
        {"Per-request protocols", fun per_request_protocols_test/0}
    ].
