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
