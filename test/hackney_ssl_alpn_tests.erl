%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.

%% Unit tests for the ALPN-across-resumption memo (hackney_ssl). The memo is a
%% shared named ETS table, so each case uses a distinct host to avoid interference.
%% resolve_alpn/6 args: (NegResult, Resumed, Cached, Host, AlpnProtos, Resumable).
-module(hackney_ssl_alpn_tests).

-include_lib("eunit/include/eunit.hrl").

h2() -> [<<"h2">>, <<"http/1.1">>].
h2_reordered() -> [<<"http/1.1">>, <<"h2">>].
h1() -> [<<"http/1.1">>].

setup() ->
    ok = hackney_ssl:init_key_cache().

alpn_test_() ->
    {setup, fun setup/0,
     [
      fun cold_gate/0,
      fun learn_h2_then_recall/0,
      fun resumed_recall/0,
      fun http1_learn_and_recall/0,
      fun full_handshake_refreshes_stale_http2/0,
      fun carried_snapshot_is_race_free/0,
      fun resumed_without_snapshot_defaults_http1/0,
      fun key_preserves_order_and_protocol_set/0,
      fun non_resumable_handshake_does_not_write_memo/0,
      fun only_auto_tickets_are_memo_eligible/0,
      fun options_key_ignores_session_tickets/0
     ]}.

%% A never-seen host+ALPN is not cached, so the gate would not offer resumption.
cold_gate() ->
    ?assertEqual(none, hackney_ssl:recall_alpn("cold.example", h2())).

%% A resumable full handshake reporting h2 caches it.
learn_h2_then_recall() ->
    ?assertEqual(http2,
        hackney_ssl:resolve_alpn({ok, <<"h2">>}, false, none, "learn.example", h2(), true)),
    ?assertEqual(http2, hackney_ssl:recall_alpn("learn.example", h2())).

%% Core: a genuinely resumed session (no ALPN reported) recalls the carried snapshot.
resumed_recall() ->
    ?assertEqual(http2,
        hackney_ssl:resolve_alpn({ok, <<"h2">>}, false, none, "res.example", h2(), true)),
    ?assertEqual(http2,
        hackney_ssl:resolve_alpn({error, protocol_not_negotiated}, true, http2,
                                 "res.example", h2(), true)).

http1_learn_and_recall() ->
    ?assertEqual(http1,
        hackney_ssl:resolve_alpn({ok, <<"http/1.1">>}, false, none, "h1.example", h1(), true)),
    ?assertEqual(http1,
        hackney_ssl:resolve_alpn({error, protocol_not_negotiated}, true, http1,
                                 "h1.example", h1(), true)).

%% Resumption-status guard + memo refresh: a full handshake that reports no ALPN is
%% a real http1 even with http2 carried, and it overwrites the stale http2 (for the
%% resumable source) so a later resumption cannot recall it.
full_handshake_refreshes_stale_http2() ->
    ?assertEqual(http2,
        hackney_ssl:resolve_alpn({ok, <<"h2">>}, false, none, "ref.example", h2(), true)),
    ?assertEqual(http1,
        hackney_ssl:resolve_alpn({error, protocol_not_negotiated}, false, http2,
                                 "ref.example", h2(), true)),
    ?assertEqual(http1, hackney_ssl:recall_alpn("ref.example", h2())).

%% The carried snapshot is authoritative regardless of current memo contents
%% (closes the gate-vs-resolution eviction race).
carried_snapshot_is_race_free() ->
    ?assertEqual(none, hackney_ssl:recall_alpn("race.example", h2())),
    ?assertEqual(http2,
        hackney_ssl:resolve_alpn({error, protocol_not_negotiated}, true, http2,
                                 "race.example", h2(), true)).

%% Resumed but no snapshot (gate should prevent this) defaults to http1, not a guess.
resumed_without_snapshot_defaults_http1() ->
    ?assertEqual(http1,
        hackney_ssl:resolve_alpn({error, protocol_not_negotiated}, true, none,
                                 "ns.example", h2(), true)).

%% The memo key includes the advertised list in order and the exact protocol set.
key_preserves_order_and_protocol_set() ->
    ?assertEqual(http2,
        hackney_ssl:resolve_alpn({ok, <<"h2">>}, false, none, "ord.example", h2(), true)),
    ?assertEqual(none, hackney_ssl:recall_alpn("ord.example", h2_reordered())),
    ?assertEqual(none, hackney_ssl:recall_alpn("ord.example", h1())).

%% A non-resumable (custom ssl_options) handshake must not write the shared memo,
%% so it cannot poison the entry a resumed session reads.
non_resumable_handshake_does_not_write_memo() ->
    %% Non-resumable full handshake: returns the reported protocol but writes nothing.
    ?assertEqual(http1,
        hackney_ssl:resolve_alpn({ok, <<"http/1.1">>}, false, none,
                                 "poison.example", h2(), false)),
    ?assertEqual(none, hackney_ssl:recall_alpn("poison.example", h2())),
    %% The resumable source owns the entry.
    ?assertEqual(http2,
        hackney_ssl:resolve_alpn({ok, <<"h2">>}, false, none,
                                 "poison.example", h2(), true)),
    ?assertEqual(http2, hackney_ssl:recall_alpn("poison.example", h2())),
    %% A later non-resumable http1 handshake does not overwrite it.
    ?assertEqual(http1,
        hackney_ssl:resolve_alpn({error, protocol_not_negotiated}, false, none,
                                 "poison.example", h2(), false)),
    ?assertEqual(http2, hackney_ssl:recall_alpn("poison.example", h2())).

%% Only hackney's automatic tickets make a conn memo-eligible; a caller-supplied
%% disabled/manual session_tickets entry must not (it is not the resumable source).
only_auto_tickets_are_memo_eligible() ->
    ?assert(hackney_ssl:auto_tickets([{verify, verify_none}, {session_tickets, auto}])),
    ?assertNot(hackney_ssl:auto_tickets([{session_tickets, disabled}])),
    ?assertNot(hackney_ssl:auto_tickets([{session_tickets, manual}])),
    ?assertNot(hackney_ssl:auto_tickets([{verify, verify_none}])),
    ?assertNot(hackney_ssl:auto_tickets([])).

%% The pool tls_key must not depend on session_tickets (the handshake varies it).
options_key_ignores_session_tickets() ->
    Base = [{verify, verify_none},
            {alpn_advertised_protocols, [<<"h2">>, <<"http/1.1">>]}],
    WithTickets = [{session_tickets, auto} | Base],
    ?assertEqual(hackney_ssl:options_key(Base),
                 hackney_ssl:options_key(WithTickets)).
