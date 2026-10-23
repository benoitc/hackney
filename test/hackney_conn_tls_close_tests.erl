%%% A TLS conn stops within the pool's 100 ms stop budget even when the server
%%% does not answer close_notify. ssl:close/1 waited up to 5 s for the server
%%% to close its side, so the pool killed the conn and every pooled TLS conn
%%% closed over a slow link logged a supervisor report (reason killed).
-module(hackney_conn_tls_close_tests).

-include_lib("eunit/include/eunit.hrl").

tls_close_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"stop/2 on a TLS conn exits normal when the server ignores close_notify",
       {timeout, 30, fun t_stop_without_peer_close/0}}]}.

t_stop_without_peer_close() ->
    {Srv, Relay, Port} = start_server(),
    try
        {ok, Conn} = hackney:connect(hackney_ssl, "localhost", Port,
                                     [{pool, false},
                                      {ssl_options, [{insecure, true},
                                                     {verify, verify_none}]}]),
        Relay ! freeze,
        Ref = monitor(process, Conn),
        ok = hackney_conn:stop(Conn, 100),
        receive
            {'DOWN', Ref, process, Conn, Reason} -> ?assertEqual(normal, Reason)
        after 5000 ->
            error(conn_still_up)
        end
    after
        exit(Relay, kill),
        exit(Srv, kill)
    end.

%% A TLS server behind a TCP relay. Once frozen, the relay drops what the
%% client sends: the client's close_notify never reaches the server, so the
%% server never closes its side, as with a peer over a slow link.
start_server() ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, SrvPort}} = ssl:sockname(LSock),
    Srv = spawn(fun() ->
        {ok, TSock} = ssl:transport_accept(LSock, 10000),
        {ok, _Sock} = ssl:handshake(TSock, 5000),
        receive after infinity -> ok end
    end),
    %% exit_on_close false: the client's half-close must not close the relay
    %% socket, which would answer with the FIN the client waits for.
    {ok, RLSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true},
                                      {exit_on_close, false}]),
    {ok, Port} = inet:port(RLSock),
    Relay = spawn(fun() ->
        {ok, Client} = gen_tcp:accept(RLSock, 10000),
        {ok, Upstream} = gen_tcp:connect("localhost", SrvPort, [binary, {active, once}]),
        ok = inet:setopts(Client, [{active, once}]),
        relay(Client, Upstream, false)
    end),
    {Srv, Relay, Port}.

relay(Client, Upstream, Frozen) ->
    receive
        freeze ->
            relay(Client, Upstream, true);
        {tcp, Client, Data} ->
            Frozen orelse gen_tcp:send(Upstream, Data),
            ok = inet:setopts(Client, [{active, once}]),
            relay(Client, Upstream, Frozen);
        {tcp, Upstream, Data} ->
            ok = gen_tcp:send(Client, Data),
            ok = inet:setopts(Upstream, [{active, once}]),
            relay(Client, Upstream, Frozen);
        {tcp_closed, _} ->
            relay(Client, Upstream, Frozen)
    end.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
