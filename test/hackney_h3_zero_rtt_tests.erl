%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Tests for HTTP/3 IPv6 (family) plumbing and 0-RTT / session resumption
%%% wiring: option forwarding into quic_opts and the pool session-ticket cache.

-module(hackney_h3_zero_rtt_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% build_h3_opts/2 - IPv6 family + session_ticket forwarding
%%====================================================================

build_opts_test_() ->
    [
     {"family inet6 is forwarded into quic_opts", fun family_inet6/0},
     {"family inet is forwarded into quic_opts", fun family_inet/0},
     {"no family leaves quic_opts without a family key", fun no_family/0},
     {"happy_eyeballs is forwarded into quic_opts", fun happy_eyeballs/0},
     {"session_ticket is forwarded into quic_opts", fun session_ticket/0},
     {"family and session_ticket can be combined", fun family_and_ticket/0}
    ].

family_inet6() ->
    Opts = hackney_h3:build_h3_opts(<<"example.com">>, #{family => inet6}),
    QuicOpts = maps:get(quic_opts, Opts),
    ?assertEqual(inet6, maps:get(family, QuicOpts)).

family_inet() ->
    Opts = hackney_h3:build_h3_opts(<<"example.com">>, #{family => inet}),
    QuicOpts = maps:get(quic_opts, Opts),
    ?assertEqual(inet, maps:get(family, QuicOpts)).

no_family() ->
    Opts = hackney_h3:build_h3_opts(<<"example.com">>, #{}),
    QuicOpts = maps:get(quic_opts, Opts),
    ?assertNot(maps:is_key(family, QuicOpts)).

happy_eyeballs() ->
    Opts = hackney_h3:build_h3_opts(<<"example.com">>, #{happy_eyeballs => false}),
    QuicOpts = maps:get(quic_opts, Opts),
    ?assertEqual(false, maps:get(happy_eyeballs, QuicOpts)).

session_ticket() ->
    Ticket = {session_ticket, <<"opaque">>},
    Opts = hackney_h3:build_h3_opts(<<"example.com">>, #{session_ticket => Ticket}),
    QuicOpts = maps:get(quic_opts, Opts),
    ?assertEqual(Ticket, maps:get(session_ticket, QuicOpts)).

family_and_ticket() ->
    Ticket = make_ref(),
    Opts = hackney_h3:build_h3_opts(<<"[::1]">>,
                                    #{family => inet6, session_ticket => Ticket}),
    QuicOpts = maps:get(quic_opts, Opts),
    ?assertEqual(inet6, maps:get(family, QuicOpts)),
    ?assertEqual(Ticket, maps:get(session_ticket, QuicOpts)).

%%====================================================================
%% hackney_conn:h3_tls_opts/2 - family/session_ticket from either list
%%====================================================================

tls_opts_test_() ->
    [
     {"family from connect_options", fun tls_family_connect/0},
     {"family from ssl_options", fun tls_family_ssl/0},
     {"session_ticket from connect_options", fun tls_ticket_connect/0},
     {"session_ticket from ssl_options", fun tls_ticket_ssl/0},
     {"insecure maps to verify_none", fun tls_insecure/0}
    ].

tls_family_connect() ->
    M = hackney_conn:h3_tls_opts([{family, inet6}], []),
    ?assertEqual(inet6, maps:get(family, M)).

tls_family_ssl() ->
    M = hackney_conn:h3_tls_opts([], [{family, inet6}]),
    ?assertEqual(inet6, maps:get(family, M)).

tls_ticket_connect() ->
    T = make_ref(),
    M = hackney_conn:h3_tls_opts([{session_ticket, T}], []),
    ?assertEqual(T, maps:get(session_ticket, M)).

tls_ticket_ssl() ->
    T = make_ref(),
    M = hackney_conn:h3_tls_opts([], [{session_ticket, T}]),
    ?assertEqual(T, maps:get(session_ticket, M)).

tls_insecure() ->
    M = hackney_conn:h3_tls_opts([{insecure, true}], []),
    ?assertEqual(verify_none, maps:get(verify, M)).

%%====================================================================
%% hackney_pool - H3 session ticket cache
%%====================================================================

pool_session_test_() ->
    {setup,
     fun() ->
             {ok, _} = application:ensure_all_started(hackney),
             ok
     end,
     fun(_) -> ok end,
     [
      {"get returns none when nothing cached", fun pool_get_none/0},
      {"store then get returns the ticket", fun pool_store_get/0},
      {"delete removes the cached ticket", fun pool_delete/0},
      {"tickets are keyed by host/port/transport", fun pool_keying/0}
     ]}.

pool_get_none() ->
    ?assertEqual(none,
                 hackney_pool:get_h3_session("nope.example", 443, hackney_ssl, [])).

pool_store_get() ->
    T = {ticket, make_ref()},
    ok = hackney_pool:store_h3_session("a.example", 443, hackney_ssl, T, []),
    ?assertEqual({ok, T},
                 hackney_pool:get_h3_session("a.example", 443, hackney_ssl, [])).

pool_delete() ->
    T = {ticket, make_ref()},
    ok = hackney_pool:store_h3_session("b.example", 443, hackney_ssl, T, []),
    ?assertEqual({ok, T},
                 hackney_pool:get_h3_session("b.example", 443, hackney_ssl, [])),
    ok = hackney_pool:delete_h3_session("b.example", 443, hackney_ssl, []),
    ?assertEqual(none,
                 hackney_pool:get_h3_session("b.example", 443, hackney_ssl, [])).

pool_keying() ->
    T = {ticket, make_ref()},
    ok = hackney_pool:store_h3_session("c.example", 443, hackney_ssl, T, []),
    ?assertEqual(none,
                 hackney_pool:get_h3_session("c.example", 8443, hackney_ssl, [])).
