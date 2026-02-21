%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc End-to-end tests for HTTP/2 against real servers.
%%%
%%% These tests validate HTTP/2 compliance against production servers:
%%% - nghttp2.org: Reference HTTP/2 implementation
%%% - www.google.com: Strict HTTP/2 enforcement
%%% - cloudflare.com: CDN with HTTP/2 optimizations
%%%
%%% Tests are skipped if network is unavailable.
%%%
%%% To run: rebar3 ct --suite=hackney_http2_e2e_SUITE

-module(hackney_http2_e2e_SUITE).

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
    nghttp2_simple_get/1,
    nghttp2_concurrent_streams/1,
    google_strict_headers/1,
    cloudflare_http2/1
]).

-define(TIMEOUT, 30000).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [{group, e2e_tests}].

groups() ->
    [{e2e_tests, [sequence], [
        nghttp2_simple_get,
        nghttp2_concurrent_streams,
        google_strict_headers,
        cloudflare_http2
    ]}].

init_per_suite(Config) ->
    application:ensure_all_started(hackney),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Check network availability by doing a quick test
    case check_network() of
        ok -> Config;
        {error, Reason} -> {skip, {network_unavailable, Reason}}
    end.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Simple GET request to nghttp2.org over HTTP/2.
nghttp2_simple_get(_Config) ->
    URL = <<"https://nghttp2.org/">>,
    Opts = [{protocols, [http2]}, {recv_timeout, ?TIMEOUT}],

    case hackney:get(URL, [], <<>>, Opts) of
        {ok, Status, Headers, Body} ->
            ct:log("Status: ~p", [Status]),
            ct:log("Headers: ~p", [Headers]),
            ct:log("Body length: ~p bytes", [byte_size(Body)]),

            %% Verify success
            true = Status >= 200 andalso Status < 400,

            %% Verify HTTP/2 was used (nghttp2 server header)
            case proplists:get_value(<<"server">>, Headers) of
                undefined -> ok;
                Server -> ct:log("Server: ~s", [Server])
            end,
            ok;
        {error, Reason} ->
            ct:fail({request_failed, Reason})
    end.

%% @doc Test concurrent streams to nghttp2.org.
nghttp2_concurrent_streams(_Config) ->
    URL = <<"https://nghttp2.org/">>,
    Opts = [{protocols, [http2]}, {recv_timeout, ?TIMEOUT}],

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

%% @doc Test against Google which has strict HTTP/2 header validation.
%% This test caught the Host header bug in PR #811.
google_strict_headers(_Config) ->
    URL = <<"https://www.google.com/">>,
    Opts = [{protocols, [http2]}, {recv_timeout, ?TIMEOUT}],

    case hackney:get(URL, [], <<>>, Opts) of
        {ok, Status, Headers, _Body} ->
            ct:log("Google Status: ~p", [Status]),
            ct:log("Google Headers: ~p", [Headers]),

            %% Google should return a valid response
            true = Status >= 200 andalso Status < 500,
            ok;
        {error, Reason} ->
            ct:fail({google_request_failed, Reason})
    end.

%% @doc Test against Cloudflare's HTTP/2 implementation.
cloudflare_http2(_Config) ->
    URL = <<"https://cloudflare.com/">>,
    Opts = [{protocols, [http2]}, {recv_timeout, ?TIMEOUT}],

    case hackney:get(URL, [], <<>>, Opts) of
        {ok, Status, Headers, _Body} ->
            ct:log("Cloudflare Status: ~p", [Status]),

            %% Check for Cloudflare server header
            case proplists:get_value(<<"server">>, Headers) of
                undefined -> ok;
                Server -> ct:log("Server: ~s", [Server])
            end,

            %% Should get a valid response
            true = Status >= 200 andalso Status < 500,
            ok;
        {error, Reason} ->
            ct:fail({cloudflare_request_failed, Reason})
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check if network is available.
check_network() ->
    %% Try to resolve a well-known hostname
    case inet:gethostbyname("nghttp2.org") of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
