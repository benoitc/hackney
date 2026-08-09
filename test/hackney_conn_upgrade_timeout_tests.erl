%%% Pooled TLS upgrade must not hang on a stalled handshake.
%%%
%%% ssl:connect/2 has no handshake deadline, so a peer that accepts TCP but
%%% never completes the TLS handshake would pin the connection process (and its
%%% pool slot) forever. The upgrade must be bounded by connect_timeout.
-module(hackney_conn_upgrade_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

pooled_tls_upgrade_times_out_test_() ->
    %% Without the bound the upgrade never returns and this test times out.
    {timeout, 10, fun pooled_tls_upgrade_times_out/0}.

pooled_tls_upgrade_times_out() ->
    {ok, _} = application:ensure_all_started(hackney),
    %% Listener that accepts the TCP connection but never speaks TLS.
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    %% Let the conn open (and thus own) its own TCP socket to the stalled peer.
    Opts = #{host => "127.0.0.1", port => Port, transport => hackney_tcp,
             connect_timeout => 500},
    {ok, Pid} = hackney_conn:start_link(Opts),
    ok = hackney_conn:connect(Pid, 1000),
    {ok, _ServerSock} = gen_tcp:accept(LSock, 1000),
    ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),
    %% verify_none so the handshake proceeds and then stalls waiting for the
    %% ServerHello that never arrives; only the timeout can end it.
    T0 = erlang:monotonic_time(millisecond),
    Result = hackney_conn:upgrade_to_ssl(Pid, [{verify, verify_none}], #{final => true}),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    ?assertMatch({error, _}, Result),
    %% 500ms bound with generous slack; a regression hangs instead.
    ?assert(Elapsed < 4000),
    catch hackney_conn:stop(Pid),
    gen_tcp:close(LSock).
