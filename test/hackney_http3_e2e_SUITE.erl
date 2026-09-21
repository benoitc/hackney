%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc End-to-end HTTP/3 tests.
%%%
%%% The `local' group runs against an in-process HTTP/3 server
%%% (hackney_h3_test_server) and always runs. The `interop' group checks
%%% compliance against production servers (cloudflare-quic.com, quic.tech,
%%% www.google.com); it needs the network, so it only runs when
%%% HACKNEY_H3_INTEROP is set:
%%%
%%%   HACKNEY_H3_INTEROP=1 rebar3 ct --suite=test/hackney_http3_e2e_SUITE

-module(hackney_http3_e2e_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    local_simple_get/1,
    local_concurrent_streams/1,
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
    Interop = case os:getenv("HACKNEY_H3_INTEROP") of
        false -> [];
        _ -> [{group, interop}]
    end,
    [{group, local} | Interop].

groups() ->
    [{local, [sequence], [
        local_simple_get,
        local_concurrent_streams
    ]},
    {interop, [sequence], [
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

init_per_group(local, Config) ->
    [{server, hackney_h3_test_server:start()} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(local, Config) ->
    hackney_h3_test_server:stop(?config(server, Config));
end_per_group(_Group, _Config) ->
    ok.

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
%% Test Cases: local server
%%====================================================================

%% @doc Simple GET request to the local HTTP/3 server.
local_simple_get(Config) ->
    URL = hackney_h3_test_server:url(?config(server, Config), <<"/">>),
    {ok, 200, Headers, Body} =
        hackney:get(URL, [], <<>>, hackney_h3_test_server:hackney_opts()),
    <<"text/html">> = proplists:get_value(<<"content-type">>, Headers),
    <<"<html><body>hackney h3 test server</body></html>">> = Body,
    ok.

%% @doc Five concurrent requests over HTTP/3 to the local server.
local_concurrent_streams(Config) ->
    URL = hackney_h3_test_server:url(?config(server, Config), <<"/cdn-cgi/trace">>),
    Opts = hackney_h3_test_server:hackney_opts(),
    Self = self(),
    Pids = [spawn_link(fun() -> Self ! {done, self(), hackney:get(URL, [], <<>>, Opts)} end)
            || _ <- lists:seq(1, 5)],
    Results = [receive {done, Pid, Result} -> Result after ?TIMEOUT -> {error, timeout} end
               || Pid <- Pids],
    lists:foreach(fun
        ({ok, 200, _Headers, <<"h=127.0.0.1\nhttp=http/3\n">>}) -> ok;
        (Other) -> ct:fail({unexpected_result, Other})
    end, Results),
    ok.

%%====================================================================
%% Test Cases: interop with production servers (HACKNEY_H3_INTEROP)
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
