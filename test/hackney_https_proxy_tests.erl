%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc HTTPS target through a CONNECT proxy reached over TLS: the target
%%% TLS session runs inside the proxy TLS session. Both servers are local.
-module(hackney_https_proxy_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WAIT, 5000).

https_proxy_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"request to an HTTPS target", {timeout, 30, fun t_request/0}},
      {"set_owner/2 keeps the nested tunnel open", {timeout, 30, fun t_handover/0}}]}.

t_request() ->
    with_servers(fun(Target, ProxyPort) ->
        Url = url(Target),
        Test = self(),
        Caller = spawn(fun() ->
            Test ! {result, hackney:request(get, Url, [], <<>>, opts(ProxyPort))}
        end),
        H = wait_target(Target),
        H ! {send, <<"HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok">>},
        receive
            {result, R} -> ?assertMatch({ok, 200, _, <<"ok">>}, R)
        after ?WAIT ->
            exit(Caller, kill),
            error(no_response)
        end
    end).

%% The opener reads the status, hands the conn over and dies. Both TLS
%% layers must survive it, so the rest of the body still arrives.
t_handover() ->
    with_servers(fun(Target, ProxyPort) ->
        Url = url(Target),
        Test = self(),
        Opener = spawn(fun() ->
            {ok, Conn} = hackney:request(post, Url, [], stream, opts(ProxyPort)),
            ok = hackney:send_body(Conn, <<"body">>),
            ok = hackney:finish_send_body(Conn),
            {ok, 200, _, Conn} = hackney:start_response(Conn),
            ok = hackney_conn:set_owner(Conn, Test),
            Test ! {conn, Conn},
            receive after infinity -> ok end
        end),
        H = wait_target(Target),
        H ! {send, <<"HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n"
                     "2\r\nok\r\n">>},
        Conn = receive {conn, C} -> C after ?WAIT -> error(no_conn) end,
        Mon = monitor(process, Opener),
        exit(Opener, kill),
        receive {'DOWN', Mon, process, Opener, _} -> ok end,
        H ! {send, <<"4\r\nmore\r\n0\r\n\r\n">>},
        ?assertEqual({ok, <<"okmore">>}, hackney:body(Conn))
    end).

%%====================================================================
%% Helpers
%%====================================================================

opts(ProxyPort) ->
    [{pool, false},
     {recv_timeout, infinity},
     {proxy, {connect, "127.0.0.1", ProxyPort}},
     {proxy_transport, ssl},
     {proxy_ssl_options, [{verify, verify_none}]},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

url(#{port := Port}) ->
    iolist_to_binary(["https://localhost:", integer_to_list(Port), "/"]).

wait_target(#{tag := Tag}) ->
    receive {Tag, request_seen, H} -> H after ?WAIT -> error(no_request) end.

with_servers(Fun) ->
    Target = start_target(),
    {Proxy, ProxyPort} = start_proxy(),
    try
        Fun(Target, ProxyPort)
    after
        exit(Proxy, kill),
        exit(maps:get(acceptor, Target), kill)
    end.

server_opts() ->
    Certs = cert_dir(),
    [binary, {active, false}, {ip, {127, 0, 0, 1}},
     {certfile, filename:join(Certs, "server.pem")},
     {keyfile, filename:join(Certs, "server.key")}].

%% TLS server that reports each request and answers when told to
%% ({send, Bytes}).
start_target() ->
    Tag = make_ref(),
    Test = self(),
    {ok, L} = ssl:listen(0, server_opts()),
    {ok, {_, Port}} = ssl:sockname(L),
    Acceptor = spawn(fun() -> target_accept(L, Tag, Test) end),
    ok = ssl:controlling_process(L, Acceptor),
    #{tag => Tag, port => Port, acceptor => Acceptor}.

target_accept(L, Tag, Test) ->
    case ssl:transport_accept(L) of
        {ok, S0} ->
            H = spawn(fun() ->
                receive go -> ok end,
                {ok, S} = ssl:handshake(S0, ?WAIT),
                {ok, _} = ssl:recv(S, 0, ?WAIT),
                Test ! {Tag, request_seen, self()},
                target_loop(S)
            end),
            ok = ssl:controlling_process(S0, H),
            H ! go,
            target_accept(L, Tag, Test);
        {error, _} ->
            ok
    end.

target_loop(S) ->
    receive
        {send, Bytes} -> ok = ssl:send(S, Bytes), target_loop(S)
    after 30000 -> ok
    end.

%% CONNECT proxy listening on TLS. It relays the tunnel to the target over
%% plain TCP, so the target TLS session passes through it untouched.
start_proxy() ->
    {ok, L} = ssl:listen(0, server_opts()),
    {ok, {_, Port}} = ssl:sockname(L),
    Pid = spawn(fun() -> proxy_accept(L) end),
    ok = ssl:controlling_process(L, Pid),
    {Pid, Port}.

proxy_accept(L) ->
    case ssl:transport_accept(L) of
        {ok, S0} ->
            H = spawn(fun() -> receive go -> proxy_handle(S0) end end),
            ok = ssl:controlling_process(S0, H),
            H ! go,
            proxy_accept(L);
        {error, _} ->
            ok
    end.

proxy_handle(S0) ->
    {ok, C} = ssl:handshake(S0, ?WAIT),
    {ok, Req} = recv_headers(C, <<>>),
    [Line | _] = binary:split(Req, <<"\r\n">>),
    [<<"CONNECT">>, HostPort | _] = binary:split(Line, <<" ">>, [global]),
    [_Host, PortBin] = binary:split(HostPort, <<":">>),
    {ok, U} = gen_tcp:connect({127, 0, 0, 1}, binary_to_integer(PortBin),
                              [binary, {active, true}]),
    ok = ssl:send(C, <<"HTTP/1.1 200 Connection Established\r\n\r\n">>),
    ok = ssl:setopts(C, [{active, true}]),
    relay(C, U).

recv_headers(C, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, Data} = ssl:recv(C, 0, ?WAIT),
            recv_headers(C, <<Acc/binary, Data/binary>>);
        _ ->
            {ok, Acc}
    end.

relay(C, U) ->
    receive
        {ssl, C, Data} -> ok = gen_tcp:send(U, Data), relay(C, U);
        {tcp, U, Data} -> ok = ssl:send(C, Data), relay(C, U);
        {ssl_closed, C} -> gen_tcp:close(U);
        {tcp_closed, U} -> ssl:close(C)
    end.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
