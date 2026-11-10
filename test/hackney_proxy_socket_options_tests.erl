%%% #970: socket options set in connect_options must reach the socket on
%%% the proxy transports (HTTP CONNECT, SOCKS5) and on WebSocket, direct or
%%% through a proxy. These transports used to pass only a few options
%%% (linger, nodelay, send_timeout, send_timeout_close, raw, inet6) and
%%% WebSocket ignored connect_options entirely through a proxy.
%%%
%%% {keepalive, false} is the probe: the transports set keepalive by
%%% default and it was not among the options let through.
-module(hackney_proxy_socket_options_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NO_KEEPALIVE, [{connect_options, [{keepalive, false}]}]).

socket_options_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{connect := CProxy, socks5 := SProxy, ws_url := WsUrl} = Ctx) ->
         Url = target_url(Ctx),
         [{"CONNECT tunnel keeps the default socket options",
           fun() -> ?assert(conn_keepalive(Url, connect(CProxy, []))) end},
          {"CONNECT tunnel applies connect_options",
           fun() -> ?assertNot(conn_keepalive(Url, connect(CProxy, ?NO_KEEPALIVE))) end},
          {"SOCKS5 tunnel applies connect_options",
           fun() -> ?assertNot(conn_keepalive(Url, socks5(SProxy, ?NO_KEEPALIVE))) end},
          {"framing options in connect_options do not break a tunnel",
           fun() -> framing_ignored(Url, CProxy) end},
          {"WebSocket applies connect_options",
           fun() -> ?assertNot(ws_keepalive(WsUrl, ?NO_KEEPALIVE)) end},
          {"WebSocket through a CONNECT proxy applies connect_options",
           fun() -> ?assertNot(ws_keepalive(WsUrl, [{proxy, {connect, "localhost", CProxy}}
                                                    | ?NO_KEEPALIVE])) end},
          {"WebSocket through a SOCKS5 proxy applies connect_options",
           fun() -> ?assertNot(ws_keepalive(WsUrl, [{proxy, {socks5, "localhost", SProxy}}
                                                    | ?NO_KEEPALIVE])) end}]
     end}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(cowboy),
    {ok, CPid, CPort} = mock_proxy_server:start_connect_proxy(),
    {ok, SPid, SPort} = mock_proxy_server:start_socks5_proxy(),
    {Target, TargetPort} = start_target(),
    Dispatch = cowboy_router:compile([{'_', [{"/ws", mock_ws_handler, []}]}]),
    Listener = list_to_atom("proxy_sockopts_ws_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {ok, _} = cowboy:start_clear(Listener, [{port, 0}], #{env => #{dispatch => Dispatch}}),
    {_, WsPort} = ranch:get_addr(Listener),
    #{connect => CPort, socks5 => SPort, pids => [CPid, SPid], target => Target,
      target_port => TargetPort, listener => Listener,
      ws_url => "ws://localhost:" ++ integer_to_list(WsPort) ++ "/ws"}.

cleanup(#{pids := Pids, target := Target, listener := Listener}) ->
    [mock_proxy_server:stop(P) || P <- Pids],
    exit(Target, shutdown),
    _ = cowboy:stop_listener(Listener),
    ok.

target_url(#{target_port := Port}) ->
    "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/".

connect(ProxyPort, Extra) -> [{proxy, {connect, "127.0.0.1", ProxyPort}} | Extra].
socks5(ProxyPort, Extra) -> [{proxy, {socks5, "127.0.0.1", ProxyPort}} | Extra].

%% Make a request through the tunnel and report the keepalive option of
%% the socket it used (the socket to the proxy). An async request returns
%% the conn and keeps it open while its response is read.
conn_keepalive(Url, Opts) ->
    {ok, Conn} = hackney:request(get, Url, [], <<>>, [{pool, false}, {async, once} | Opts]),
    receive {hackney_response, Conn, {status, 200, _}} -> ok
    after 5000 -> erlang:error(no_status)
    end,
    try
        {_State, Data} = sys:get_state(Conn),
        keepalive(element(8, Data))  %% #conn_data.socket
    after
        hackney:close(Conn)
    end.

%% The transport reads its socket in binary, passive mode: a caller cannot
%% switch that through connect_options.
framing_ignored(Url, ProxyPort) ->
    Opts = connect(ProxyPort, [{connect_options, [{active, true}, {packet, line},
                                                  list, {mode, list}]}]),
    {ok, 200, _, <<"ok">>} = hackney:request(get, Url, [], <<>>,
                                             [{pool, false}, with_body | Opts]).

ws_keepalive(WsUrl, Opts) ->
    {ok, Ws} = hackney:ws_connect(WsUrl, Opts),
    try
        ok = hackney:ws_send(Ws, {text, <<"hi">>}),
        {ok, {text, <<"hi">>}} = hackney:ws_recv(Ws, 5000),
        {_State, Data} = sys:get_state(Ws),
        keepalive(element(7, Data))  %% #ws_data.socket
    after
        hackney:ws_close(Ws)
    end.

keepalive(Socket) ->
    {ok, [{keepalive, V}]} = inet:getopts(Socket, [keepalive]),
    V.

%% A keep-alive HTTP/1.1 server answering every request with "ok".
start_target() ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(L),
    Pid = spawn(fun() -> accept(L) end),
    {Pid, Port}.

accept(L) ->
    case gen_tcp:accept(L) of
        {ok, S} ->
            Pid = spawn(fun() -> receive go -> serve(S, <<>>) end end),
            ok = gen_tcp:controlling_process(S, Pid),
            Pid ! go,
            accept(L);
        {error, _} ->
            ok
    end.

serve(S, Acc) ->
    case binary:split(Acc, <<"\r\n\r\n">>) of
        [_, Rest] ->
            ok = gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok">>),
            serve(S, Rest);
        [_] ->
            case gen_tcp:recv(S, 0) of
                {ok, D} -> serve(S, <<Acc/binary, D/binary>>);
                {error, _} -> ok
            end
    end.
