%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2018 11:42
%%%-------------------------------------------------------------------
-module(hackney_ssl_tests).
-author("benoitc").

-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").


setup() -> ok.

teardown(_) -> ok.

empty_clen_test_() ->
  {
    "test SSL",
    {
      setup,
      fun start/0, fun stop/1,
      {
        foreach,
        fun setup/0, fun teardown/1,
        [
          fun wildcard_cert/1,
          fun custom_ssl_opts_hostname_verification/1,
          fun googleapis_ssl_opts_test/1
        ]
      }
    }
  }.

start() ->
  error_logger:tty(false),
  {ok, _} = application:ensure_all_started(hackney),
  ok.

stop(_) ->
  application:stop(hackney),
  error_logger:tty(true),
  ok.

wildcard_cert(_) ->
    URL = <<"https://friendpaste.com">>,
    Resp = case hackney:get(URL) of
             {ok, _, _, _} -> valid;
             _ -> error
           end,
    ?_assertEqual(Resp, valid).

custom_ssl_opts_hostname_verification(_) ->
    URL = <<"https://www.googleapis.com">>,
    %% Test with custom ssl_options that should not override hostname verification
    CustomSSLOpts = [{versions, ['tlsv1.2', 'tlsv1.3']}, {ciphers, []}],
    Opts = [{ssl_options, CustomSSLOpts}],
    Resp = case hackney:get(URL, [], <<>>, Opts) of
             {ok, _, _, _} -> valid;
             _ -> error
           end,
    ?_assertEqual(Resp, valid).

googleapis_ssl_opts_test(_) ->
    %% Erlang translation of the Elixir test that failed in 1.22.0
    URL = "https://www.googleapis.com/oauth2/v1/certs",
    SSLOpts = [
        {versions, ['tlsv1.2']},
        {verify, verify_peer},
        {cacertfile, certifi:cacertfile()},
        {depth, 10},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ],
    Opts = [
        {ssl_options, SSLOpts},
        {recv_timeout, 500},
        {with_body, true}
    ],
    Resp = case hackney:request(get, URL, [], "", Opts) of
             {ok, 200, _Headers, _Body} -> success;
             _ -> error
           end,
    ?_assertEqual(Resp, success).

%%====================================================================
%% ALPN Tests
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

