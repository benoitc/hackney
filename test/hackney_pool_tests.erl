%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024 Benoit Chesneau
%%%
%%% @doc Tests for hackney_pool implementation.

-module(hackney_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("hackney.hrl").

-define(PORT, 8123).
-define(SSL_PORT, 8129).

%%====================================================================
%% Test fixtures
%%====================================================================

%% Unit tests - no server required
hackney_pool_unit_test_() ->
    {setup,
     fun setup_unit/0,
     fun teardown_unit/1,
     [
      {"pool starts with default", fun test_default_pool/0},
      {"start custom pool", fun test_custom_pool/0},
      {"pool stats", fun test_pool_stats/0},
      {"max connections setting", fun test_max_connections/0},
      {"timeout setting", fun test_timeout_setting/0}
     ]}.

%% HTTP/2 tls_key bucket tests - no server required
hackney_pool_h2_tls_key_test_() ->
    {setup,
     fun setup_unit/0,
     fun teardown_unit/1,
     [
      {"h2 checkout with a different tls_key returns none", fun test_h2_tls_key_mismatch/0},
      {"h2 checkout with the matching tls_key returns the conn", fun test_h2_tls_key_match/0},
      {"h2 default bucket is isolated from keyed bucket", fun test_h2_tls_key_default_bucket/0}
     ]}.

%% Integration tests - require server
hackney_pool_integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun teardown_integration/1,
     [
      {"checkout creates connection", fun test_checkout_creates/0},
      {"checkin returns connection", fun test_checkin_returns/0},
      {"connection reuse", fun test_connection_reuse/0},
      {"connection death cleanup", fun test_connection_death/0},
      {"owner crash kills connection", fun test_owner_crash/0},
      {"checkin resets owner to pool", fun test_checkin_resets_owner/0},
      {"prewarm creates connections", fun test_prewarm/0},
      {"queue timeout", {timeout, 120, fun test_queue_timeout/0}},
      {"checkout timeout", {timeout, 120, fun test_checkout_timeout/0}},
      {"server close detected when idle (issue #544)", {timeout, 30, fun test_server_close_detected/0}},
      {"checkout survives a connection dying mid-liveness-check (PR #869)",
       {timeout, 30, fun test_checkout_survives_dying_connection/0}},
      {"checkout_ssl without pooled conns returns needs_upgrade",
       fun test_checkout_ssl_needs_upgrade/0},
      {"checkout_ssl overflows past pool_size instead of timing out",
       fun test_checkout_ssl_overflow/0},
      {"checkin of an SSL-keyed conn that was never upgraded closes it",
       fun test_checkout_ssl_unupgraded_closes/0},
      {"checkin pools an upgraded HTTPS/1.1 conn under its SSL key",
       fun test_checkin_ssl_pooled_stub/0},
      {"checkin closes an SSL-keyed no_reuse conn",
       fun test_checkin_ssl_no_reuse_stub/0},
      {"checkin closes an SSL-keyed non-http1 conn",
       fun test_checkin_ssl_wrong_protocol_stub/0},
      {"count/2 aggregates legacy 3-tuples and host_stats spans buckets",
       fun test_count_mixed_buckets/0}
     ]}.

%% HTTPS/1.1 ssl_pooling integration tests - require a TLS server
hackney_pool_ssl_pooling_test_() ->
    {setup,
     fun setup_ssl/0,
     fun teardown_ssl/1,
     [
      {"https conn is pooled and reused with ssl_pooling",
       {timeout, 30, fun test_ssl_pooling_reuse/0}},
      {"https conn closes at checkin without ssl_pooling",
       {timeout, 30, fun test_ssl_pooling_default_off/0}},
      {"different ssl_options never share a pooled conn",
       {timeout, 30, fun test_ssl_pooling_isolation/0}}
     ]}.

setup_unit() ->
    application:ensure_all_started(hackney),
    ok.

teardown_unit(_) ->
    ok.

setup_integration() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Host = '_',
    Routes = [
        {"/pool", pool_resource, []},
        {"/[...]", test_http_resource, []}
    ],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    {ok, _} = cowboy:start_clear(pool_test_server, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}),
    ok.

teardown_integration(_) ->
    cowboy:stop_listener(pool_test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

setup_ssl() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    CertDir = cert_dir(),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_tls(pool_ssl_test_server,
                               [{port, ?SSL_PORT},
                                {certfile, filename:join(CertDir, "server.pem")},
                                {keyfile, filename:join(CertDir, "server.key")}],
                               #{env => #{dispatch => Dispatch}}),
    ok.

teardown_ssl(_) ->
    cowboy:stop_listener(pool_ssl_test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

cert_dir() ->
    filename:join([filename:dirname(code:which(?MODULE)), "..", "test", "certs"]).

%%====================================================================
%% Unit Tests
%%====================================================================

test_default_pool() ->
    ?assertEqual(undefined, hackney_pool:find_pool(nonexistent_pool)),
    ok = hackney_pool:start_pool(test_pool_1, []),
    ?assert(is_pid(hackney_pool:find_pool(test_pool_1))),
    ok = hackney_pool:stop_pool(test_pool_1).

test_custom_pool() ->
    %% Timeout is capped at 2000ms for keepalive
    Options = [{pool_size, 10}, {timeout, 60000}],
    ok = hackney_pool:start_pool(test_pool_2, Options),
    Pool = hackney_pool:find_pool(test_pool_2),
    ?assert(is_pid(Pool)),
    ?assertEqual(10, hackney_pool:max_connections(test_pool_2)),
    ?assertEqual(2000, hackney_pool:timeout(test_pool_2)),  % Capped at 2s
    ok = hackney_pool:stop_pool(test_pool_2).

test_pool_stats() ->
    ok = hackney_pool:start_pool(test_pool_3, []),
    Stats = hackney_pool:get_stats(test_pool_3),
    ?assertEqual(test_pool_3, proplists:get_value(name, Stats)),
    ?assert(is_integer(proplists:get_value(max, Stats))),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(0, proplists:get_value(free_count, Stats)),
    ?assertEqual(0, proplists:get_value(queue_count, Stats)),
    ok = hackney_pool:stop_pool(test_pool_3).

test_max_connections() ->
    ok = hackney_pool:start_pool(test_pool_4, [{pool_size, 5}]),
    ?assertEqual(5, hackney_pool:max_connections(test_pool_4)),
    hackney_pool:set_max_connections(test_pool_4, 10),
    timer:sleep(10),
    ?assertEqual(10, hackney_pool:max_connections(test_pool_4)),
    ok = hackney_pool:stop_pool(test_pool_4).

test_timeout_setting() ->
    %% Keepalive timeout is capped at 2000ms
    ok = hackney_pool:start_pool(test_pool_5, [{timeout, 5000}]),
    ?assertEqual(2000, hackney_pool:timeout(test_pool_5)),  % Capped at 2s
    hackney_pool:set_timeout(test_pool_5, 1000),
    timer:sleep(10),
    ?assertEqual(1000, hackney_pool:timeout(test_pool_5)),
    hackney_pool:set_timeout(test_pool_5, 10000),
    timer:sleep(10),
    ?assertEqual(2000, hackney_pool:timeout(test_pool_5)),  % Capped at 2s
    ok = hackney_pool:stop_pool(test_pool_5).

%%====================================================================
%% HTTP/2 tls_key Bucket Tests
%%====================================================================

%% Dummy connection that answers hackney_conn:get_state/1 (used by the
%% pool's h2_conn_usable liveness check) with {ok, connected}.
dummy_h2_conn() ->
    spawn(fun dummy_h2_loop/0).

dummy_h2_loop() ->
    receive
        {'$gen_call', From, get_state} ->
            gen_statem:reply(From, {ok, connected}),
            dummy_h2_loop();
        stop ->
            ok
    end.

stop_dummy(Conn) ->
    Conn ! stop,
    timer:sleep(50).

test_h2_tls_key_mismatch() ->
    ok = hackney_pool:start_pool(test_pool_h2_key_1, []),
    Conn = dummy_h2_conn(),
    Opts1 = [{pool, test_pool_h2_key_1}, {tls_key, crypto:hash(sha256, <<"opts-one">>)}],
    Opts2 = [{pool, test_pool_h2_key_1}, {tls_key, crypto:hash(sha256, <<"opts-two">>)}],
    ok = hackney_pool:register_h2("h2key.example.com", 443, hackney_ssl, Conn, Opts1),
    timer:sleep(50),
    %% A request carrying different TLS options must not get this conn
    ?assertEqual(none, hackney_pool:checkout_h2("h2key.example.com", 443, hackney_ssl, Opts2)),
    stop_dummy(Conn),
    ok = hackney_pool:stop_pool(test_pool_h2_key_1).

test_h2_tls_key_match() ->
    ok = hackney_pool:start_pool(test_pool_h2_key_2, []),
    Conn = dummy_h2_conn(),
    Opts = [{pool, test_pool_h2_key_2}, {tls_key, crypto:hash(sha256, <<"opts-one">>)}],
    ok = hackney_pool:register_h2("h2key.example.com", 443, hackney_ssl, Conn, Opts),
    timer:sleep(50),
    ?assertEqual({ok, Conn}, hackney_pool:checkout_h2("h2key.example.com", 443, hackney_ssl, Opts)),
    stop_dummy(Conn),
    ok = hackney_pool:stop_pool(test_pool_h2_key_2).

test_h2_tls_key_default_bucket() ->
    %% Callers that pass no tls_key land in the default bucket, isolated
    %% from keyed registrations.
    ok = hackney_pool:start_pool(test_pool_h2_key_3, []),
    KeyedConn = dummy_h2_conn(),
    DefaultConn = dummy_h2_conn(),
    KeyedOpts = [{pool, test_pool_h2_key_3}, {tls_key, crypto:hash(sha256, <<"opts-one">>)}],
    DefaultOpts = [{pool, test_pool_h2_key_3}],
    ok = hackney_pool:register_h2("h2def.example.com", 443, hackney_ssl, KeyedConn, KeyedOpts),
    ok = hackney_pool:register_h2("h2def.example.com", 443, hackney_ssl, DefaultConn, DefaultOpts),
    timer:sleep(50),
    ?assertEqual({ok, DefaultConn},
                 hackney_pool:checkout_h2("h2def.example.com", 443, hackney_ssl, DefaultOpts)),
    ?assertEqual({ok, KeyedConn},
                 hackney_pool:checkout_h2("h2def.example.com", 443, hackney_ssl, KeyedOpts)),
    stop_dummy(KeyedConn),
    stop_dummy(DefaultConn),
    ok = hackney_pool:stop_pool(test_pool_h2_key_3).

%%====================================================================
%% Connection Tests
%%====================================================================

test_checkout_creates() ->
    ok = hackney_pool:start_pool(test_pool_conn_1, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_1}],

    %% Checkout should create a new connection
    {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ?assertMatch({test_pool_conn_1, _, _, _}, PoolInfo),

    %% Stats should show 1 in use
    Stats = hackney_pool:get_stats(test_pool_conn_1),
    ?assertEqual(1, proplists:get_value(in_use_count, Stats)),

    %% Clean up
    hackney_conn:stop(Pid),
    ok = hackney_pool:stop_pool(test_pool_conn_1).

test_checkin_returns() ->
    ok = hackney_pool:start_pool(test_pool_conn_2, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_2}],

    %% Checkout a connection
    {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),

    %% Checkin the connection
    ok = hackney_pool:checkin(PoolInfo, Pid),
    timer:sleep(10),

    %% Stats should show 1 free, 0 in use
    Stats = hackney_pool:get_stats(test_pool_conn_2),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(1, proplists:get_value(free_count, Stats)),

    ok = hackney_pool:stop_pool(test_pool_conn_2).

test_connection_reuse() ->
    ok = hackney_pool:start_pool(test_pool_conn_3, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_3}],

    %% Checkout and checkin
    {ok, PoolInfo, Pid1} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ok = hackney_pool:checkin(PoolInfo, Pid1),
    timer:sleep(10),

    %% Second checkout should reuse the same connection
    {ok, _, Pid2} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ?assertEqual(Pid1, Pid2),

    hackney_conn:stop(Pid2),
    ok = hackney_pool:stop_pool(test_pool_conn_3).

test_connection_death() ->
    ok = hackney_pool:start_pool(test_pool_conn_4, [{pool_size, 5}]),
    Opts = [{pool, test_pool_conn_4}],

    %% Checkout and checkin
    {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ok = hackney_pool:checkin(PoolInfo, Pid),
    timer:sleep(10),

    %% Verify it's in the pool
    Stats1 = hackney_pool:get_stats(test_pool_conn_4),
    ?assertEqual(1, proplists:get_value(free_count, Stats1)),

    %% Kill the connection
    exit(Pid, kill),
    timer:sleep(50),

    %% Should be removed from pool
    Stats2 = hackney_pool:get_stats(test_pool_conn_4),
    ?assertEqual(0, proplists:get_value(free_count, Stats2)),

    ok = hackney_pool:stop_pool(test_pool_conn_4).

test_owner_crash() ->
    %% Test that when the owner process crashes, the connection dies
    ok = hackney_pool:start_pool(test_pool_owner_crash, [{pool_size, 5}]),
    Opts = [{pool, test_pool_owner_crash}],
    Self = self(),

    %% Spawn a process that checks out a connection
    Owner = spawn(fun() ->
        {ok, _PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
        Self ! {conn_pid, Pid},
        %% Wait to be killed
        receive stop -> ok end
    end),

    %% Get the connection pid
    ConnPid = receive {conn_pid, P} -> P after 5000 -> error(timeout) end,
    ?assert(is_process_alive(ConnPid)),

    %% Verify it's in use
    Stats1 = hackney_pool:get_stats(test_pool_owner_crash),
    ?assertEqual(1, proplists:get_value(in_use_count, Stats1)),

    %% Kill the owner process
    exit(Owner, kill),
    timer:sleep(100),

    %% Connection should have died
    ?assertNot(is_process_alive(ConnPid)),

    %% Should be removed from in_use
    Stats2 = hackney_pool:get_stats(test_pool_owner_crash),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats2)),

    ok = hackney_pool:stop_pool(test_pool_owner_crash).

test_checkin_resets_owner() ->
    %% Test that after checkin, the connection survives if the previous owner crashes
    ok = hackney_pool:start_pool(test_pool_checkin_owner, [{pool_size, 5}]),
    Opts = [{pool, test_pool_checkin_owner}],
    Self = self(),

    %% Spawn a process that checks out and checks in a connection
    Owner = spawn(fun() ->
        {ok, PoolInfo, Pid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
        Self ! {conn_pid, Pid},
        %% Check the connection back in
        ok = hackney_pool:checkin(PoolInfo, Pid),
        Self ! checked_in,
        %% Wait to be killed
        receive stop -> ok end
    end),

    %% Get the connection pid
    ConnPid = receive {conn_pid, P} -> P after 5000 -> error(timeout) end,

    %% Wait for checkin
    receive checked_in -> ok after 5000 -> error(timeout) end,
    timer:sleep(50),

    %% Verify connection is in the pool (free)
    Stats1 = hackney_pool:get_stats(test_pool_checkin_owner),
    ?assertEqual(1, proplists:get_value(free_count, Stats1)),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats1)),

    %% Kill the previous owner
    exit(Owner, kill),
    timer:sleep(100),

    %% Connection should still be alive (owner is now the pool)
    ?assert(is_process_alive(ConnPid)),

    %% Should still be in the pool
    Stats2 = hackney_pool:get_stats(test_pool_checkin_owner),
    ?assertEqual(1, proplists:get_value(free_count, Stats2)),

    ok = hackney_pool:stop_pool(test_pool_checkin_owner).

%% Regression test for PR #869.
%%
%% A pooled connection can die in the window between the
%% is_process_alive/1 check and the hackney_conn:is_ready/1 call in
%% find_available/2. is_ready/1 is a gen_statem:call, so the call then
%% exits with {noproc,{gen_statem,call,[_,is_ready,infinity]}}, which
%% used to crash the whole pool. The dead connection must be skipped.
test_checkout_survives_dying_connection() ->
    ok = hackney_pool:start_pool(test_pool_race, [{pool_size, 5}]),
    Pool = hackney_pool:find_pool(test_pool_race),
    Opts = [{pool, test_pool_race}],

    %% Seed the free list with a real connection.
    {ok, PoolInfo, RealPid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ok = hackney_pool:checkin(PoolInfo, RealPid),
    timer:sleep(20),
    ?assertEqual(1, proplists:get_value(free_count, hackney_pool:get_stats(test_pool_race))),

    %% Swap the pooled connection for a process that is alive for
    %% is_process_alive/1 but exits the instant is_ready/1 calls it,
    %% reproducing the race exactly.
    Poison = spawn(fun() -> receive _ -> exit(noproc_race) end end),
    _ = sys:replace_state(Pool, fun(S) -> swap_pid(S, RealPid, Poison) end),

    %% Pre-fix this crashes the pool; post-fix the dead connection is
    %% skipped and a fresh one is started.
    Result = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, Opts),
    ?assert(is_process_alive(Pool)),
    ?assertMatch({ok, _, _}, Result),
    {ok, _, FreshPid} = Result,
    ?assertNotEqual(Poison, FreshPid),

    hackney_conn:stop(FreshPid),
    hackney_conn:stop(RealPid),
    ok = hackney_pool:stop_pool(test_pool_race).

%% Rewrite a pid Old -> New inside list-valued map fields of the pool
%% state record. Only #state.available holds pid lists, so this targets
%% it without depending on the record's field layout.
swap_pid(State, Old, New) ->
    list_to_tuple([swap_field(F, Old, New) || F <- tuple_to_list(State)]).

swap_field(M, Old, New) when is_map(M) ->
    maps:map(fun(_K, V) when is_list(V) -> [swap_one(P, Old, New) || P <- V];
                (_K, V) -> V
             end, M);
swap_field(F, _Old, _New) -> F.

swap_one(Old, Old, New) -> New;
swap_one(P, _Old, _New) -> P.

%%====================================================================
%% SSL Pooling Tests (checkout_ssl / checkin branching)
%%====================================================================

%% Stub conn that answers hackney_conn:checkin_info/1 with canned flags. It
%% also ignores casts (set_owner_async, stop) and cooperates with
%% gen_statem:stop/1 (system terminate) so the pool's close path never
%% blocks on it.
stub_conn(Info) ->
    spawn(fun() -> stub_conn_loop(Info) end).

stub_conn_loop(Info) ->
    receive
        {'$gen_call', From, checkin_info} ->
            gen_statem:reply(From, Info),
            stub_conn_loop(Info);
        {system, From, {terminate, Reason}} ->
            gen_statem:reply(From, ok),
            exit(Reason);
        {'$gen_cast', stop} ->
            ok;
        {'$gen_cast', _} ->
            stub_conn_loop(Info);
        stop ->
            ok
    end.

%% Rename a pid key Old -> New in every pid-keyed map field of the pool
%% state (in_use and pid_monitors), so a stub process can stand in for a
%% checked out conn at checkin time.
rename_pid_key(State, Old, New) ->
    list_to_tuple([rename_key_field(F, Old, New) || F <- tuple_to_list(State)]).

rename_key_field(M, Old, New) when is_map(M) ->
    case maps:take(Old, M) of
        {V, M2} -> maps:put(New, V, M2);
        error -> M
    end;
rename_key_field(F, _Old, _New) -> F.

%% Checkout an SSL-keyed conn and swap a stub in before checkin, so the
%% checkin decision sees the stub's canned checkin_info flags.
checkout_ssl_with_stub(PoolName, TlsKey, Info) ->
    Pool = hackney_pool:find_pool(PoolName),
    Opts = [{pool, PoolName}, {tls_key, TlsKey}],
    {ok, PoolInfo, RealPid, needs_upgrade} =
        hackney_pool:checkout_ssl("127.0.0.1", ?PORT, hackney_ssl, Opts),
    Stub = stub_conn(Info),
    _ = sys:replace_state(Pool, fun(S) -> rename_pid_key(S, RealPid, Stub) end),
    {PoolInfo, RealPid, Stub}.

test_checkout_ssl_needs_upgrade() ->
    ok = hackney_pool:start_pool(test_pool_ssl_co, [{pool_size, 5}, {prewarm_count, 0}]),
    TlsKey = crypto:hash(sha256, <<"ssl-co">>),
    Opts = [{pool, test_pool_ssl_co}, {tls_key, TlsKey}],
    Result = hackney_pool:checkout_ssl("127.0.0.1", ?PORT, hackney_ssl, Opts),
    ?assertMatch({ok, {test_pool_ssl_co, {"127.0.0.1", ?PORT, hackney_ssl, TlsKey}, _, hackney_ssl},
                  _, needs_upgrade}, Result),
    {ok, _, Pid, needs_upgrade} = Result,
    ?assert(is_process_alive(Pid)),
    Stats = hackney_pool:get_stats(test_pool_ssl_co),
    ?assertEqual(1, proplists:get_value(in_use_count, Stats)),
    hackney_conn:stop(Pid),
    ok = hackney_pool:stop_pool(test_pool_ssl_co).

test_checkout_ssl_overflow() ->
    %% pool_size bounds the warm pool, not concurrency. With pool_size=1 a
    %% second concurrent SSL checkout opens an overflow connection instead of
    %% failing with checkout_timeout.
    ok = hackney_pool:start_pool(test_pool_ssl_overflow,
                                 [{pool_size, 1}, {prewarm_count, 0}]),
    TlsKey = crypto:hash(sha256, <<"ssl-overflow">>),
    Opts = [{pool, test_pool_ssl_overflow}, {tls_key, TlsKey}],
    {ok, _, Pid1, needs_upgrade} =
        hackney_pool:checkout_ssl("127.0.0.1", ?PORT, hackney_ssl, Opts),
    {ok, _, Pid2, needs_upgrade} =
        hackney_pool:checkout_ssl("127.0.0.1", ?PORT, hackney_ssl, Opts),
    ?assert(is_process_alive(Pid1)),
    ?assert(is_process_alive(Pid2)),
    ?assertNotEqual(Pid1, Pid2),
    Stats = hackney_pool:get_stats(test_pool_ssl_overflow),
    ?assertEqual(2, proplists:get_value(in_use_count, Stats)),
    hackney_conn:stop(Pid1),
    hackney_conn:stop(Pid2),
    ok = hackney_pool:stop_pool(test_pool_ssl_overflow).

test_checkout_ssl_unupgraded_closes() ->
    %% Anomaly guard: an SSL-keyed conn checked in without being upgraded
    %% must be closed, never pooled.
    ok = hackney_pool:start_pool(test_pool_ssl_anom, [{pool_size, 5}, {prewarm_count, 0}]),
    TlsKey = crypto:hash(sha256, <<"ssl-anom">>),
    Opts = [{pool, test_pool_ssl_anom}, {tls_key, TlsKey}],
    {ok, PoolInfo, Pid, needs_upgrade} =
        hackney_pool:checkout_ssl("127.0.0.1", ?PORT, hackney_ssl, Opts),
    ok = hackney_pool:checkin(PoolInfo, Pid),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)),
    ?assertEqual(0, hackney_pool:count(test_pool_ssl_anom, {"127.0.0.1", ?PORT, hackney_ssl})),
    ?assertEqual(0, proplists:get_value(free_count, hackney_pool:get_stats(test_pool_ssl_anom))),
    ok = hackney_pool:stop_pool(test_pool_ssl_anom).

test_checkin_ssl_pooled_stub() ->
    ok = hackney_pool:start_pool(test_pool_ssl_ci1, [{pool_size, 5}, {prewarm_count, 0}]),
    TlsKey = crypto:hash(sha256, <<"ssl-ci-1">>),
    {PoolInfo, RealPid, Stub} = checkout_ssl_with_stub(test_pool_ssl_ci1, TlsKey,
        #{upgraded_ssl => true, no_reuse => false, pool_ssl => true, protocol => http1}),
    ok = hackney_pool:checkin(PoolInfo, Stub),
    timer:sleep(50),
    ?assert(is_process_alive(Stub)),
    SslKey = {"127.0.0.1", ?PORT, hackney_ssl, TlsKey},
    ?assertEqual(1, hackney_pool:count(test_pool_ssl_ci1, SslKey)),
    ?assertEqual(1, hackney_pool:count(test_pool_ssl_ci1, {"127.0.0.1", ?PORT, hackney_ssl})),
    ?assertEqual(1, proplists:get_value(free_count, hackney_pool:get_stats(test_pool_ssl_ci1))),
    stop_dummy(Stub),
    hackney_conn:stop(RealPid),
    ok = hackney_pool:stop_pool(test_pool_ssl_ci1).

test_checkin_ssl_no_reuse_stub() ->
    ok = hackney_pool:start_pool(test_pool_ssl_ci2, [{pool_size, 5}, {prewarm_count, 0}]),
    TlsKey = crypto:hash(sha256, <<"ssl-ci-2">>),
    {PoolInfo, RealPid, Stub} = checkout_ssl_with_stub(test_pool_ssl_ci2, TlsKey,
        #{upgraded_ssl => true, no_reuse => true, pool_ssl => true, protocol => http1}),
    ok = hackney_pool:checkin(PoolInfo, Stub),
    timer:sleep(50),
    ?assertNot(is_process_alive(Stub)),
    ?assertEqual(0, hackney_pool:count(test_pool_ssl_ci2, {"127.0.0.1", ?PORT, hackney_ssl})),
    hackney_conn:stop(RealPid),
    ok = hackney_pool:stop_pool(test_pool_ssl_ci2).

test_checkin_ssl_wrong_protocol_stub() ->
    %% An ALPN-negotiated h2 conn multiplexes via h2_connections and must
    %% never enter the available map, even under an SSL key.
    ok = hackney_pool:start_pool(test_pool_ssl_ci3, [{pool_size, 5}, {prewarm_count, 0}]),
    TlsKey = crypto:hash(sha256, <<"ssl-ci-3">>),
    {PoolInfo, RealPid, Stub} = checkout_ssl_with_stub(test_pool_ssl_ci3, TlsKey,
        #{upgraded_ssl => true, no_reuse => false, pool_ssl => true, protocol => http2}),
    ok = hackney_pool:checkin(PoolInfo, Stub),
    timer:sleep(50),
    ?assertNot(is_process_alive(Stub)),
    ?assertEqual(0, hackney_pool:count(test_pool_ssl_ci3, {"127.0.0.1", ?PORT, hackney_ssl})),
    hackney_conn:stop(RealPid),
    ok = hackney_pool:stop_pool(test_pool_ssl_ci3).

test_count_mixed_buckets() ->
    ok = hackney_pool:start_pool(test_pool_mixed, [{pool_size, 10}, {prewarm_count, 0}]),
    KeyA = crypto:hash(sha256, <<"bucket-a">>),
    KeyB = crypto:hash(sha256, <<"bucket-b">>),
    Poolable = #{upgraded_ssl => true, no_reuse => false, pool_ssl => true, protocol => http1},
    %% Two SSL buckets with different TLS hashes
    {PoolInfoA, RealA, StubA} = checkout_ssl_with_stub(test_pool_mixed, KeyA, Poolable),
    {PoolInfoB, RealB, StubB} = checkout_ssl_with_stub(test_pool_mixed, KeyB, Poolable),
    ok = hackney_pool:checkin(PoolInfoA, StubA),
    ok = hackney_pool:checkin(PoolInfoB, StubB),
    %% One plain TCP conn
    TcpOpts = [{pool, test_pool_mixed}],
    {ok, PoolInfoT, TcpPid} = hackney_pool:checkout("127.0.0.1", ?PORT, hackney_tcp, TcpOpts),
    ok = hackney_pool:checkin(PoolInfoT, TcpPid),
    timer:sleep(50),
    %% Exact 4-tuple lookups see only their bucket
    ?assertEqual(1, hackney_pool:count(test_pool_mixed, {"127.0.0.1", ?PORT, hackney_ssl, KeyA})),
    ?assertEqual(1, hackney_pool:count(test_pool_mixed, {"127.0.0.1", ?PORT, hackney_ssl, KeyB})),
    %% Legacy 3-tuples aggregate across buckets per transport
    ?assertEqual(2, hackney_pool:count(test_pool_mixed, {"127.0.0.1", ?PORT, hackney_ssl})),
    ?assertEqual(1, hackney_pool:count(test_pool_mixed, {"127.0.0.1", ?PORT, hackney_tcp})),
    %% host_stats spans every bucket of the host
    HostStats = hackney_pool:host_stats(test_pool_mixed, "127.0.0.1", ?PORT),
    ?assertEqual(0, proplists:get_value(in_use, HostStats)),
    ?assertEqual(3, proplists:get_value(free, HostStats)),
    stop_dummy(StubA),
    stop_dummy(StubB),
    hackney_conn:stop(RealA),
    hackney_conn:stop(RealB),
    ok = hackney_pool:stop_pool(test_pool_mixed).

%%====================================================================
%% HTTPS/1.1 ssl_pooling Integration Tests
%%====================================================================

ssl_pool_url() ->
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(?SSL_PORT), <<"/get">>]).

ssl_pool_opts(PoolName, ExtraSslOpts) ->
    [{pool, PoolName},
     {protocols, [http1]},
     {ssl_options, [{insecure, true}, {verify, verify_none} | ExtraSslOpts]}].

ssl_request(Opts) ->
    {ok, 200, _, Body} = hackney:request(get, ssl_pool_url(), [], <<>>, Opts),
    ?assert(is_binary(Body)),
    ok.

test_ssl_pooling_reuse() ->
    ok = hackney_pool:start_pool(test_pool_ssl_reuse, [{pool_size, 5}, {prewarm_count, 0}]),
    Opts = [{ssl_pooling, true} | ssl_pool_opts(test_pool_ssl_reuse, [])],
    SslTriple = {"localhost", ?SSL_PORT, hackney_ssl},
    ok = ssl_request(Opts),
    timer:sleep(50),
    %% The upgraded conn went back to the pool under its SSL key
    ?assertEqual(1, hackney_pool:count(test_pool_ssl_reuse, SslTriple)),
    ok = ssl_request(Opts),
    timer:sleep(50),
    %% Same TLS options: the pooled conn was reused, no second conn appeared
    ?assertEqual(1, hackney_pool:count(test_pool_ssl_reuse, SslTriple)),
    Stats = hackney_pool:get_stats(test_pool_ssl_reuse),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(1, proplists:get_value(free_count, Stats)),
    ok = hackney_pool:stop_pool(test_pool_ssl_reuse).

test_ssl_pooling_default_off() ->
    ok = hackney_pool:start_pool(test_pool_ssl_off, [{pool_size, 5}, {prewarm_count, 0}]),
    Opts = ssl_pool_opts(test_pool_ssl_off, []),
    ok = ssl_request(Opts),
    timer:sleep(50),
    %% Without ssl_pooling the upgraded conn is closed at checkin, as before
    ?assertEqual(0, hackney_pool:count(test_pool_ssl_off, {"localhost", ?SSL_PORT, hackney_ssl})),
    Stats = hackney_pool:get_stats(test_pool_ssl_off),
    ?assertEqual(0, proplists:get_value(in_use_count, Stats)),
    ?assertEqual(0, proplists:get_value(free_count, Stats)),
    ok = hackney_pool:stop_pool(test_pool_ssl_off).

test_ssl_pooling_isolation() ->
    ok = hackney_pool:start_pool(test_pool_ssl_iso, [{pool_size, 5}, {prewarm_count, 0}]),
    OptsX = [{ssl_pooling, true} | ssl_pool_opts(test_pool_ssl_iso, [])],
    OptsY = [{ssl_pooling, true} | ssl_pool_opts(test_pool_ssl_iso, [{depth, 3}])],
    SslTriple = {"localhost", ?SSL_PORT, hackney_ssl},
    ok = ssl_request(OptsX),
    timer:sleep(50),
    ?assertEqual(1, hackney_pool:count(test_pool_ssl_iso, SslTriple)),
    %% Different ssl_options hash to a different bucket: the pooled conn is
    %% not reused and a second conn shows up
    ok = ssl_request(OptsY),
    timer:sleep(50),
    ?assertEqual(2, hackney_pool:count(test_pool_ssl_iso, SslTriple)),
    ok = hackney_pool:stop_pool(test_pool_ssl_iso).

%%====================================================================
%% Prewarm Tests
%%====================================================================

test_prewarm() ->
    %% Test that prewarm creates connections
    ok = hackney_pool:start_pool(test_pool_prewarm, [{pool_size, 10}, {prewarm_count, 3}]),

    %% Initially no connections
    Stats1 = hackney_pool:get_stats(test_pool_prewarm),
    ?assertEqual(0, proplists:get_value(free_count, Stats1)),

    %% Prewarm 3 connections to localhost
    hackney_pool:prewarm(test_pool_prewarm, "127.0.0.1", ?PORT, 3),
    timer:sleep(500),  %% Wait for prewarm connections to be created

    %% Should have 3 free connections
    Stats2 = hackney_pool:get_stats(test_pool_prewarm),
    ?assertEqual(3, proplists:get_value(free_count, Stats2)),

    %% Prewarm with different count
    hackney_pool:prewarm(test_pool_prewarm, "127.0.0.1", ?PORT, 5),
    timer:sleep(500),

    %% Should have 5 free connections now (2 more added)
    Stats3 = hackney_pool:get_stats(test_pool_prewarm),
    ?assertEqual(5, proplists:get_value(free_count, Stats3)),

    ok = hackney_pool:stop_pool(test_pool_prewarm).

%%====================================================================
%% Timeout Tests
%%====================================================================

test_queue_timeout() ->
    %% pool_size bounds the warm (idle) pool, not concurrency. Per-host
    %% concurrency is capped by max_per_host, so with pool_size=1 a second
    %% concurrent request opens an overflow connection instead of failing.
    URL = <<"http://localhost:8123/pool">>,
    Headers = [],
    hackney_pool:start_pool(pool_test, [{pool_size, 1}]),
    Opts = [{pool, pool_test}, {connect_timeout, 100}, {checkout_timeout, 5000}],
    {ok, Ref1} = hackney:request(post, URL, Headers, stream, Opts),
    %% Second concurrent request succeeds via an overflow connection
    {ok, Ref2} = hackney:request(post, URL, Headers, stream, Opts),

    %% Complete both requests
    ok = hackney:finish_send_body(Ref1),
    {ok, _S1, _H1, _B1} = hackney:start_response(Ref1),
    hackney:close(Ref1),
    ok = hackney:finish_send_body(Ref2),
    {ok, _S2, _H2, _B2} = hackney:start_response(Ref2),
    hackney:close(Ref2),

    hackney_pool:stop_pool(pool_test).

test_checkout_timeout() ->
    %% checkout_timeout now comes from load regulation, the hard per-host
    %% concurrency cap. Cap it at 1 so the second concurrent request waits for
    %% a slot and times out.
    URL = <<"http://localhost:8123/pool">>,
    Headers = [],
    hackney_pool:start_pool(pool_test_timeout, [{pool_size, 1}]),
    Opts = [{max_body, 2048}, {pool, pool_test_timeout}, {max_per_host, 1},
            {connect_timeout, 1000}, {checkout_timeout, 100}],
    {ok, Ref} = hackney:request(post, URL, Headers, stream, Opts),
    {error, Error} = hackney:request(post, URL, Headers, stream, Opts),
    hackney:close(Ref),
    ?assertEqual(checkout_timeout, Error),
    hackney_pool:stop_pool(pool_test_timeout).

%% Test for issue #544: Server closes idle connection, pool should detect it
%% This tests that when a server closes a connection while it's idle in the pool,
%% the connection process receives tcp_closed and terminates, removing itself from pool.
test_server_close_detected() ->
    %% Start a simple TCP server that accepts a connection, responds, then closes after delay
    Self = self(),
    {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, ServerPort} = inet:port(ListenSock),

    %% Server process
    ServerPid = spawn_link(fun() ->
        {ok, ClientSock} = gen_tcp:accept(ListenSock, 5000),
        Self ! {server, accepted},
        %% Wait for client to signal ready
        receive ready -> ok end,
        %% Close from server side - this triggers tcp_closed on client
        gen_tcp:close(ClientSock),
        Self ! {server, closed},
        gen_tcp:close(ListenSock)
    end),

    %% Create a pool and checkout a connection to our test server
    ok = hackney_pool:start_pool(test_pool_server_close, [{pool_size, 5}]),
    Opts = [{pool, test_pool_server_close}],

    %% Connect to the test server
    {ok, PoolInfo, ConnPid} = hackney_pool:checkout("127.0.0.1", ServerPort, hackney_tcp, Opts),
    ?assert(is_process_alive(ConnPid)),

    %% Wait for server to accept
    receive {server, accepted} -> ok after 5000 -> error(timeout_accept) end,

    %% Check the connection back into the pool
    ok = hackney_pool:checkin(PoolInfo, ConnPid),
    timer:sleep(50),

    %% Verify connection is in the pool
    Stats1 = hackney_pool:get_stats(test_pool_server_close),
    FreeCount1 = proplists:get_value(free_count, Stats1),
    ?assertEqual(1, FreeCount1),
    ?assert(is_process_alive(ConnPid)),

    %% Tell server to close the connection
    ServerPid ! ready,
    receive {server, closed} -> ok after 5000 -> error(timeout_close) end,

    %% Give time for tcp_closed to be delivered and processed
    timer:sleep(150),

    %% Connection process should have terminated (received tcp_closed, transitioned to closed)
    ?assertNot(is_process_alive(ConnPid)),

    %% Connection should have been removed from pool
    Stats2 = hackney_pool:get_stats(test_pool_server_close),
    FreeCount2 = proplists:get_value(free_count, Stats2),
    ?assertEqual(0, FreeCount2),

    ok = hackney_pool:stop_pool(test_pool_server_close).
