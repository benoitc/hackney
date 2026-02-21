%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc End-to-end tests for HTTP/3 against real servers.
%%%
%%% These tests validate HTTP/3 compliance against production servers:
%%% - cloudflare-quic.com: Cloudflare's QUIC test server
%%% - quic.tech: HTTP/3 test server
%%% - www.google.com: Google with HTTP/3 support
%%%
%%% Tests are skipped if:
%%% - Network is unavailable
%%% - QUIC library is not available
%%%
%%% To run: rebar3 ct --suite=hackney_http3_e2e_SUITE

-module(hackney_http3_e2e_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    cloudflare_simple_get/1,
    cloudflare_concurrent_streams/1,
    google_http3/1,
    quic_tech_test/1
]).

-define(TIMEOUT, 30000).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [{group, e2e_tests}].

groups() ->
    [{e2e_tests, [sequence], [
        cloudflare_simple_get,
        cloudflare_concurrent_streams,
        google_http3,
        quic_tech_test
    ]}].

init_per_suite(Config) ->
    %% Check if QUIC library is available
    case code:which(quic) of
        non_existing ->
            {skip, "QUIC library not available"};
        _ ->
            application:ensure_all_started(hackney),
            Config
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Check network availability and QUIC support
    case check_quic_available() of
        ok -> Config;
        {error, Reason} -> {skip, {quic_unavailable, Reason}}
    end.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Simple GET request to Cloudflare's QUIC test server.
cloudflare_simple_get(_Config) ->
    URL = <<"https://cloudflare-quic.com/">>,
    Opts = [{protocols, [http3]}, {recv_timeout, ?TIMEOUT}],

    case hackney:get(URL, [], <<>>, Opts) of
        {ok, Status, Headers, Body} ->
            ct:log("Status: ~p", [Status]),
            ct:log("Headers: ~p", [Headers]),
            ct:log("Body length: ~p bytes", [byte_size(Body)]),

            %% Verify success
            true = Status >= 200 andalso Status < 400,

            %% Check for alt-svc header indicating HTTP/3 support
            case proplists:get_value(<<"alt-svc">>, Headers) of
                undefined -> ok;
                AltSvc -> ct:log("Alt-Svc: ~s", [AltSvc])
            end,
            ok;
        {error, Reason} ->
            ct:fail({request_failed, Reason})
    end.

%% @doc Test concurrent streams over HTTP/3.
cloudflare_concurrent_streams(_Config) ->
    URL = <<"https://cloudflare-quic.com/">>,
    Opts = [{protocols, [http3]}, {recv_timeout, ?TIMEOUT}],

    %% Launch 5 concurrent requests
    Self = self(),
    Pids = [spawn_link(fun() ->
        Result = hackney:get(URL, [], <<>>, Opts),
        Self ! {done, self(), Result}
    end) || _ <- lists:seq(1, 5)],

    %% Collect results
    Results = [receive
        {done, Pid, Result} -> Result
    after ?TIMEOUT * 2 ->
        {error, timeout}
    end || Pid <- Pids],

    %% All should succeed
    lists:foreach(fun
        ({ok, Status, _Headers, _Body}) when Status >= 200, Status < 400 ->
            ok;
        ({ok, Status, _Headers, _}) ->
            ct:fail({unexpected_status, Status});
        ({error, Reason}) ->
            ct:fail({request_failed, Reason})
    end, Results),
    ok.

%% @doc Test HTTP/3 against Google (supports HTTP/3 via alt-svc).
google_http3(_Config) ->
    URL = <<"https://www.google.com/">>,
    Opts = [{protocols, [http3]}, {recv_timeout, ?TIMEOUT}],

    case hackney:get(URL, [], <<>>, Opts) of
        {ok, Status, Headers, _Body} ->
            ct:log("Google HTTP/3 Status: ~p", [Status]),
            ct:log("Google HTTP/3 Headers: ~p", [Headers]),

            %% Google should return a valid response
            true = Status >= 200 andalso Status < 500,
            ok;
        {error, Reason} ->
            %% Google may not always accept HTTP/3 directly
            ct:log("Google HTTP/3 failed (may require alt-svc upgrade): ~p", [Reason]),
            {skip, {google_http3_not_available, Reason}}
    end.

%% @doc Test against quic.tech test server.
quic_tech_test(_Config) ->
    URL = <<"https://quic.tech:8443/">>,
    Opts = [{protocols, [http3]}, {recv_timeout, ?TIMEOUT}],

    case hackney:get(URL, [], <<>>, Opts) of
        {ok, Status, Headers, _Body} ->
            ct:log("quic.tech Status: ~p", [Status]),
            ct:log("quic.tech Headers: ~p", [Headers]),

            true = Status >= 200 andalso Status < 500,
            ok;
        {error, Reason} ->
            %% Server may be temporarily unavailable
            ct:log("quic.tech failed: ~p", [Reason]),
            {skip, {quic_tech_unavailable, Reason}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check if QUIC/HTTP3 support is available and working.
check_quic_available() ->
    %% Check if the quic application can be started
    case application:ensure_all_started(quic) of
        {ok, _} -> ok;
        {error, Reason} -> {error, {quic_start_failed, Reason}}
    end.
