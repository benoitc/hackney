%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018-2025, Benoit Chesneau
%%% @doc SSL and ALPN tests for hackney.
%%% @end
%%%-------------------------------------------------------------------
-module(hackney_ssl_tests).
-author("benoitc").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% ALPN Tests (Unit tests - no network needed)
%%====================================================================

alpn_opts_default_test() ->
    %% Default should include both http2 and http1
    Opts = hackney_ssl:alpn_opts([]),
    ?assertEqual([{alpn_advertised_protocols, [<<"h2">>, <<"http/1.1">>]}], Opts).

alpn_opts_http2_only_test() ->
    Opts = hackney_ssl:alpn_opts([{protocols, [http2]}]),
    ?assertEqual([{alpn_advertised_protocols, [<<"h2">>]}], Opts).

alpn_opts_http1_only_test() ->
    Opts = hackney_ssl:alpn_opts([{protocols, [http1]}]),
    ?assertEqual([{alpn_advertised_protocols, [<<"http/1.1">>]}], Opts).

alpn_opts_http11_alias_test() ->
    %% http11 should be an alias for http1
    Opts = hackney_ssl:alpn_opts([{protocols, [http11]}]),
    ?assertEqual([{alpn_advertised_protocols, [<<"http/1.1">>]}], Opts).

alpn_opts_order_preserved_test() ->
    %% Order should be preserved - http1 first, then http2
    Opts = hackney_ssl:alpn_opts([{protocols, [http1, http2]}]),
    ?assertEqual([{alpn_advertised_protocols, [<<"http/1.1">>, <<"h2">>]}], Opts).

alpn_opts_empty_protocols_test() ->
    %% Empty protocols list should return empty opts
    Opts = hackney_ssl:alpn_opts([{protocols, []}]),
    ?assertEqual([], Opts).

%%====================================================================
%% SSL Options Tests (Unit tests)
%%====================================================================

ssl_opts_default_test() ->
    %% Test that ssl_opts returns proper defaults with hostname verification
    Opts = hackney_ssl:ssl_opts("example.com", []),
    %% Should have verify_peer and other defaults
    ?assertEqual(verify_peer, proplists:get_value(verify, Opts)).

ssl_opts_insecure_test() ->
    %% Test insecure mode
    Opts = hackney_ssl:ssl_opts("example.com", [{insecure, true}]),
    ?assertEqual(verify_none, proplists:get_value(verify, Opts)).

ssl_opts_custom_ssl_options_test() ->
    %% Test custom ssl_options merge
    CustomOpts = [{versions, ['tlsv1.3']}],
    Opts = hackney_ssl:ssl_opts("example.com", [{ssl_options, CustomOpts}]),
    ?assertEqual(['tlsv1.3'], proplists:get_value(versions, Opts)).

check_hostname_opts_test() ->
    %% Test hostname verification options
    Opts = hackney_ssl:check_hostname_opts("example.com"),
    ?assertEqual(verify_peer, proplists:get_value(verify, Opts)),
    ?assert(lists:keymember(cacerts, 1, Opts) orelse lists:keymember(cacertfile, 1, Opts)).

verify_fun_rewrites_cert_expired_test() ->
    %% cert_expired must be rewritten to root_cert_expired so OTP's
    %% ssl_certificate:find_cross_sign_root_paths/4 recovery can trigger
    %% (e.g. expired ISRG Root X2 cross-signed anchor in Let's Encrypt chains).
    Opts = hackney_ssl:check_hostname_opts("example.com"),
    {VerifyFun, InitState} = proplists:get_value(verify_fun, Opts),
    ?assertEqual({fail, {bad_cert, root_cert_expired}},
                 VerifyFun(fake_cert, {bad_cert, cert_expired}, InitState)).

verify_fun_passes_through_other_bad_cert_test() ->
    %% Other bad_cert reasons must not be silently rewritten.
    Opts = hackney_ssl:check_hostname_opts("example.com"),
    {VerifyFun, InitState} = proplists:get_value(verify_fun, Opts),
    ?assertEqual({fail, {bad_cert, unknown_ca}},
                 VerifyFun(fake_cert, {bad_cert, unknown_ca}, InitState)).

verify_fun_passes_through_valid_test() ->
    %% Valid and extension events must delegate to ssl_verify_hostname
    %% unchanged, so valid chains (including partial chains whose anchor is
    %% accepted) still verify. Only cert_expired is rewritten.
    Opts = hackney_ssl:check_hostname_opts("example.com"),
    {VerifyFun, InitState} = proplists:get_value(verify_fun, Opts),
    ?assertEqual({valid, InitState}, VerifyFun(fake_cert, valid, InitState)),
    ?assertEqual({unknown, InitState},
                 VerifyFun(fake_cert, {extension, fake_ext}, InitState)).

partial_chain_preserved_test() ->
    %% The cross-sign verify_fun change must not drop the partial_chain
    %% option; partial certificate chains rely on it to pick a trusted anchor.
    Opts = hackney_ssl:check_hostname_opts("example.com"),
    PartialChain = proplists:get_value(partial_chain, Opts),
    ?assert(is_function(PartialChain, 1)).

%%====================================================================
%% effective_opts / options_key Tests (Unit tests)
%%====================================================================

effective_opts_default_test() ->
    Opts = hackney_ssl:effective_opts("example.com", [], []),
    ?assertEqual("example.com", proplists:get_value(server_name_indication, Opts)),
    ?assertEqual(verify_peer, proplists:get_value(verify, Opts)),
    ?assert(lists:keymember(cacerts, 1, Opts) orelse lists:keymember(cacertfile, 1, Opts)).

effective_opts_no_protocols_leak_test() ->
    %% The hackney-level protocols option must not reach ssl:connect,
    %% but it must still drive the advertised ALPN protocols.
    Opts = hackney_ssl:effective_opts("example.com", [{protocols, [http2, http1]}], []),
    ?assertNot(proplists:is_defined(protocols, Opts)),
    ?assertEqual([<<"h2">>, <<"http/1.1">>],
                 proplists:get_value(alpn_advertised_protocols, Opts)).

options_key_deterministic_across_processes_test() ->
    Parent = self(),
    Compute = fun() ->
        Key = hackney_ssl:options_key(
                hackney_ssl:effective_opts("example.com", [], [])),
        Parent ! {key, self(), Key}
    end,
    Pid1 = spawn(Compute),
    Pid2 = spawn(Compute),
    Key1 = receive {key, Pid1, K1} -> K1 after 5000 -> error(timeout) end,
    Key2 = receive {key, Pid2, K2} -> K2 after 5000 -> error(timeout) end,
    ?assertEqual(Key1, Key2),
    ?assertEqual(Key1, hackney_ssl:options_key(
                         hackney_ssl:effective_opts("example.com", [], []))).

options_key_differs_per_host_test() ->
    KeyA = hackney_ssl:options_key(hackney_ssl:effective_opts("a.example.com", [], [])),
    KeyB = hackney_ssl:options_key(hackney_ssl:effective_opts("b.example.com", [], [])),
    ?assertNotEqual(KeyA, KeyB).

options_key_differs_on_verify_test() ->
    Default = hackney_ssl:options_key(
                hackney_ssl:effective_opts("example.com", [], [])),
    NoVerify = hackney_ssl:options_key(
                 hackney_ssl:effective_opts("example.com", [{verify, verify_none}], [])),
    ?assertNotEqual(Default, NoVerify).

options_key_differs_on_alpn_test() ->
    Default = hackney_ssl:options_key(
                hackney_ssl:effective_opts("example.com", [], [])),
    Http1Only = hackney_ssl:options_key(
                  hackney_ssl:effective_opts("example.com", [{protocols, [http1]}], [])),
    ?assertNotEqual(Default, Http1Only).

options_key_duplicate_order_test() ->
    %% ukeysort keeps the first occurrence (proplists semantics), so
    %% conflicting duplicates must hash differently depending on order.
    Base = [{server_name_indication, "example.com"}],
    Key1 = hackney_ssl:options_key([{verify, verify_none}, {verify, verify_peer} | Base]),
    Key2 = hackney_ssl:options_key([{verify, verify_peer}, {verify, verify_none} | Base]),
    ?assertNotEqual(Key1, Key2).

options_key_order_insensitive_test() ->
    %% Reordering distinct options must not change the key.
    Opts1 = [{verify, verify_none}, {server_name_indication, "example.com"}],
    Opts2 = [{server_name_indication, "example.com"}, {verify, verify_none}],
    ?assertEqual(hackney_ssl:options_key(Opts1), hackney_ssl:options_key(Opts2)).
