%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Regression for #938: the keepalive timer armed by connected(enter) kept
%%% running across an HTTP/1.1 TLS upgrade. ssl:connect/3 blocks the conn, so a
%%% handshake slower than the keepalive timeout let the timer fire as soon as
%%% the upgrade returned, closing a conn that had not yet carried a request.
-module(hackney_conn_upgrade_idle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(IDLE_MS, 300).
-define(HANDSHAKE_DELAY_MS, 800).

slow_handshake_keeps_conn_open_test_() ->
    {timeout, 30, fun slow_handshake_keeps_conn_open/0}.

slow_handshake_keeps_conn_open() ->
    {ok, _} = application:ensure_all_started(hackney),
    #{server_config := ServerOpts} =
        public_key:pkix_test_data(#{server_chain => #{root => [{key, {rsa, 2048, 17}}],
                                                      peer => [{key, {rsa, 2048, 17}}]},
                                    client_chain => #{root => [{key, {rsa, 2048, 17}}],
                                                      peer => [{key, {rsa, 2048, 17}}]}}),
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    Parent = self(),
    Server = spawn_link(fun() ->
        {ok, Sock} = gen_tcp:accept(LSock, 5000),
        %% Hold the ClientHello unanswered for longer than the idle timeout.
        timer:sleep(?HANDSHAKE_DELAY_MS),
        {ok, SslSock} = ssl:handshake(Sock, ServerOpts, 5000),
        Parent ! {server_ready, self()},
        receive stop -> ssl:close(SslSock) end
    end),
    Opts = #{host => "127.0.0.1", port => Port, transport => hackney_tcp,
             connect_timeout => 5000, idle_timeout => ?IDLE_MS},
    {ok, Pid} = hackney_conn:start_link(Opts),
    try
        ok = hackney_conn:connect(Pid, 2000),
        ?assertEqual({ok, connected}, hackney_conn:get_state(Pid)),
        ?assertEqual(ok, hackney_conn:upgrade_to_ssl(Pid, [{verify, verify_none},
                                                           {versions, ['tlsv1.2']}],
                                                     #{final => true})),
        %% Before the fix the stale timer fired right after the upgrade returned.
        timer:sleep(?IDLE_MS + 200),
        ?assertMatch({connected, _}, sys:get_state(Pid))
    after
        Server ! stop,
        _ = (try hackney_conn:stop(Pid) catch _:_ -> ok end),
        gen_tcp:close(LSock)
    end.
