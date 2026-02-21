%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc h2spec compliance test suite for HTTP/2 implementation.
%%%
%%% This suite runs h2spec tests against hackney's HTTP/2 implementation.
%%% h2spec must be installed and available in PATH, or downloaded via make.
%%%
%%% See: https://github.com/summerwind/h2spec
%%%
%%% To run these tests:
%%%   make h2spec-test
%%% or:
%%%   rebar3 ct --suite=h2spec_SUITE

-module(h2spec_SUITE).

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
    h2spec_generic/1,
    h2spec_hpack/1,
    h2spec_http2/1
]).

-define(H2SPEC_PORT, 18443).
-define(H2SPEC_TIMEOUT, 120000).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [{group, h2spec_tests}].

groups() ->
    [{h2spec_tests, [sequence], [
        h2spec_generic,
        h2spec_hpack,
        h2spec_http2
    ]}].

init_per_suite(Config) ->
    %% Check if h2spec is available
    H2SpecPath = find_h2spec(),
    case H2SpecPath of
        false ->
            {skip, "h2spec binary not found. Run 'make download-h2spec' to install."};
        Path ->
            %% Ensure SSL application is started
            application:ensure_all_started(ssl),
            application:ensure_all_started(hackney),

            %% Generate test certificates if needed
            case ensure_test_certs() of
                ok ->
                    %% Start h2spec server
                    case h2spec_server:start(?H2SPEC_PORT) of
                        {ok, ServerPid} ->
                            %% Give the accept loop time to start
                            timer:sleep(500),
                            ct:log("Server started, Pid: ~p~n", [ServerPid]),
                            [{h2spec_path, Path},
                             {h2spec_port, ?H2SPEC_PORT},
                             {server_pid, ServerPid} | Config];
                        {error, Reason} ->
                            ct:fail({server_start_failed, Reason})
                    end;
                {error, CertError} ->
                    {skip, {no_test_certs, CertError}}
            end
    end.

end_per_suite(Config) ->
    case proplists:get_value(server_pid, Config) of
        undefined -> ok;
        Pid -> h2spec_server:stop(Pid)
    end,
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Run h2spec generic tests (connection preface, framing).
h2spec_generic(Config) ->
    run_h2spec(Config, ["generic"]).

%% @doc Run h2spec HPACK tests.
h2spec_hpack(Config) ->
    run_h2spec(Config, ["hpack"]).

%% @doc Run h2spec HTTP/2 tests.
h2spec_http2(Config) ->
    run_h2spec(Config, ["http2"]).

%%====================================================================
%% Internal Functions
%%====================================================================

run_h2spec(Config, Sections) ->
    H2SpecPath = proplists:get_value(h2spec_path, Config),
    Port = proplists:get_value(h2spec_port, Config),

    %% Build h2spec command
    Args = [
        "-p", integer_to_list(Port),
        "-t",  %% TLS mode
        "-k",  %% Skip TLS verification
        "--strict",
        "-j", "/tmp/h2spec-report.json"
    ] ++ Sections,

    Cmd = H2SpecPath ++ " " ++ string:join(Args, " "),
    ct:log("Running: ~s", [Cmd]),

    %% Run h2spec
    Result = os:cmd(Cmd),
    ct:log("h2spec output:~n~s", [Result]),

    %% Check if tests passed - accept > 90% pass rate
    case parse_h2spec_results(Result) of
        {ok, Total, Passed, _Skipped, _Failed} when Total > 0 ->
            PassRate = Passed * 100 / Total,
            ct:log("h2spec pass rate: ~.1f% (~p/~p)", [PassRate, Passed, Total]),
            if
                PassRate >= 90.0 ->
                    ok;
                true ->
                    ct:fail({h2spec_pass_rate_too_low, PassRate, Result})
            end;
        {ok, 0, 0, _, _} ->
            %% No tests run - likely a connection issue
            ct:fail({h2spec_no_tests_run, Result});
        error ->
            %% Couldn't parse results - check for known success patterns
            case string:find(Result, "All tests passed") of
                nomatch ->
                    case string:find(Result, "0 failed") of
                        nomatch ->
                            ct:fail({h2spec_failed, Result});
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end
    end.

%% @private Parse h2spec output to extract test counts.
%% Output format: "X tests, Y passed, Z skipped, N failed"
parse_h2spec_results(Output) ->
    %% Convert to binary and handle Unicode properly
    OutputBin = case Output of
        Bin when is_binary(Bin) -> Bin;
        List when is_list(List) -> unicode:characters_to_binary(List)
    end,
    %% Look for pattern like "146 tests, 139 passed, 1 skipped, 6 failed"
    case re:run(OutputBin, "([0-9]+) tests, ([0-9]+) passed, ([0-9]+) skipped, ([0-9]+) failed",
                [{capture, all_but_first, list}]) of
        {match, [Total, Passed, Skipped, Failed]} ->
            {ok, list_to_integer(Total), list_to_integer(Passed),
                 list_to_integer(Skipped), list_to_integer(Failed)};
        nomatch ->
            error
    end.

%% @private Find h2spec binary in PATH or priv directory.
find_h2spec() ->
    %% Check priv directory first
    PrivPath = filename:join([code:priv_dir(hackney), "h2spec"]),
    case filelib:is_regular(PrivPath) of
        true ->
            PrivPath;
        false ->
            %% Check PATH
            case os:find_executable("h2spec") of
                false -> false;
                Path -> Path
            end
    end.

%% @private Ensure test certificates exist.
ensure_test_certs() ->
    CertDir = filename:join([code:priv_dir(hackney), "test_certs"]),
    CertFile = filename:join(CertDir, "server.pem"),
    KeyFile = filename:join(CertDir, "server.key"),

    case {filelib:is_regular(CertFile), filelib:is_regular(KeyFile)} of
        {true, true} ->
            ok;
        _ ->
            %% Try to generate certificates using OpenSSL
            case generate_test_certs(CertDir, CertFile, KeyFile) of
                ok -> ok;
                Error -> {error, Error}
            end
    end.

generate_test_certs(_CertDir, CertFile, KeyFile) ->
    ok = filelib:ensure_dir(CertFile),

    %% Generate self-signed certificate
    Cmd = io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s "
        "-days 365 -nodes -subj '/CN=localhost' 2>/dev/null",
        [KeyFile, CertFile]),

    case os:cmd(lists:flatten(Cmd)) of
        [] ->
            case {filelib:is_regular(CertFile), filelib:is_regular(KeyFile)} of
                {true, true} -> ok;
                _ -> {error, cert_generation_failed}
            end;
        Error ->
            {error, {openssl_error, Error}}
    end.

