%%% -*- erlang -*-
%%%
%%% Mock proxy servers for testing hackney proxy support.
%%% Implements minimal HTTP CONNECT and SOCKS5 proxies.

-module(mock_proxy_server).

-export([start_connect_proxy/0, start_connect_proxy/1]).
-export([start_socks5_proxy/0, start_socks5_proxy/1]).
-export([stop/1]).

%% Start a mock HTTP CONNECT proxy on a random port
start_connect_proxy() ->
    start_connect_proxy(0).

start_connect_proxy(Port) ->
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    {ok, ActualPort} = inet:port(ListenSock),
    Pid = spawn_link(fun() -> connect_proxy_loop(ListenSock) end),
    gen_tcp:controlling_process(ListenSock, Pid),
    {ok, Pid, ActualPort}.

%% Start a mock SOCKS5 proxy on a random port
start_socks5_proxy() ->
    start_socks5_proxy(0).

start_socks5_proxy(Port) ->
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    {ok, ActualPort} = inet:port(ListenSock),
    Pid = spawn_link(fun() -> socks5_proxy_loop(ListenSock) end),
    gen_tcp:controlling_process(ListenSock, Pid),
    {ok, Pid, ActualPort}.

stop(Pid) ->
    Pid ! stop,
    ok.

%% HTTP CONNECT proxy implementation
connect_proxy_loop(ListenSock) ->
    receive
        stop ->
            gen_tcp:close(ListenSock),
            ok
    after 0 ->
        case gen_tcp:accept(ListenSock, 100) of
            {ok, ClientSock} ->
                Pid = spawn(fun() -> handle_connect_client(ClientSock) end),
                gen_tcp:controlling_process(ClientSock, Pid),
                Pid ! socket_ready,
                connect_proxy_loop(ListenSock);
            {error, timeout} ->
                connect_proxy_loop(ListenSock);
            {error, closed} ->
                ok
        end
    end.

handle_connect_client(ClientSock) ->
    receive socket_ready -> ok after 1000 -> ok end,
    case recv_until_headers(ClientSock, <<>>, 5000) of
        {ok, Data} ->
            case parse_connect_request(Data) of
                {ok, Host, Port} ->
                    %% Connect to target
                    case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
                        {ok, TargetSock} ->
                            %% Send 200 Connection Established
                            ok = gen_tcp:send(ClientSock, <<"HTTP/1.1 200 Connection Established\r\n\r\n">>),
                            %% Relay data between client and target
                            relay_data(ClientSock, TargetSock);
                        {error, Reason} ->
                            gen_tcp:send(ClientSock, <<"HTTP/1.1 502 Bad Gateway\r\n\r\n">>),
                            gen_tcp:close(ClientSock),
                            {error, {connect_failed, Reason}}
                    end;
                error ->
                    gen_tcp:send(ClientSock, <<"HTTP/1.1 400 Bad Request\r\n\r\n">>),
                    gen_tcp:close(ClientSock)
            end;
        {error, _} ->
            gen_tcp:close(ClientSock)
    end.

%% Read until we get the full HTTP headers (ending with \r\n\r\n)
recv_until_headers(Socket, Buffer, Timeout) ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        {_, _} ->
            {ok, Buffer};
        nomatch ->
            case gen_tcp:recv(Socket, 0, Timeout) of
                {ok, Data} ->
                    recv_until_headers(Socket, <<Buffer/binary, Data/binary>>, Timeout);
                Error ->
                    Error
            end
    end.

parse_connect_request(Data) ->
    case binary:match(Data, <<"CONNECT ">>) of
        {0, 8} ->
            %% Extract host:port
            Rest = binary:part(Data, 8, byte_size(Data) - 8),
            case binary:match(Rest, <<" ">>) of
                {Pos, 1} ->
                    HostPort = binary:part(Rest, 0, Pos),
                    case binary:match(HostPort, <<":">>) of
                        {ColonPos, 1} ->
                            Host = binary_to_list(binary:part(HostPort, 0, ColonPos)),
                            PortBin = binary:part(HostPort, ColonPos + 1, byte_size(HostPort) - ColonPos - 1),
                            Port = binary_to_integer(PortBin),
                            {ok, Host, Port};
                        nomatch ->
                            error
                    end;
                nomatch ->
                    error
            end;
        _ ->
            error
    end.

%% SOCKS5 proxy implementation
socks5_proxy_loop(ListenSock) ->
    receive
        stop ->
            gen_tcp:close(ListenSock),
            ok
    after 0 ->
        case gen_tcp:accept(ListenSock, 100) of
            {ok, ClientSock} ->
                Pid = spawn(fun() -> handle_socks5_client(ClientSock) end),
                gen_tcp:controlling_process(ClientSock, Pid),
                Pid ! socket_ready,
                socks5_proxy_loop(ListenSock);
            {error, timeout} ->
                socks5_proxy_loop(ListenSock);
            {error, closed} ->
                ok
        end
    end.

handle_socks5_client(ClientSock) ->
    receive socket_ready -> ok after 1000 -> ok end,
    case gen_tcp:recv(ClientSock, 0, 5000) of
        {ok, <<5, NMethods, Methods/binary>>} when byte_size(Methods) >= NMethods ->
            %% Check for no-auth method (0)
            case binary:match(Methods, <<0>>) of
                {_, 1} ->
                    %% Accept no-auth
                    gen_tcp:send(ClientSock, <<5, 0>>),
                    handle_socks5_connect(ClientSock);
                nomatch ->
                    %% No acceptable auth method
                    gen_tcp:send(ClientSock, <<5, 16#FF>>),
                    gen_tcp:close(ClientSock)
            end;
        {ok, _} ->
            gen_tcp:close(ClientSock);
        {error, _} ->
            gen_tcp:close(ClientSock)
    end.

handle_socks5_connect(ClientSock) ->
    case gen_tcp:recv(ClientSock, 0, 5000) of
        {ok, <<5, 1, 0, AType, Rest/binary>>} ->
            %% Connect command
            case parse_socks5_address(AType, Rest) of
                {ok, Host, Port} ->
                    case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
                        {ok, TargetSock} ->
                            %% Send success reply (bound address 0.0.0.0:0)
                            gen_tcp:send(ClientSock, <<5, 0, 0, 1, 0, 0, 0, 0, 0, 0>>),
                            relay_data(ClientSock, TargetSock);
                        {error, _} ->
                            %% Connection refused
                            gen_tcp:send(ClientSock, <<5, 5, 0, 1, 0, 0, 0, 0, 0, 0>>),
                            gen_tcp:close(ClientSock)
                    end;
                error ->
                    gen_tcp:send(ClientSock, <<5, 1, 0, 1, 0, 0, 0, 0, 0, 0>>),
                    gen_tcp:close(ClientSock)
            end;
        {ok, _} ->
            gen_tcp:close(ClientSock);
        {error, _} ->
            gen_tcp:close(ClientSock)
    end.

parse_socks5_address(1, <<A, B, C, D, Port:16>>) ->
    %% IPv4
    Host = inet:ntoa({A, B, C, D}),
    {ok, Host, Port};
parse_socks5_address(3, <<Len, Rest/binary>>) ->
    %% Domain name
    case Rest of
        <<Domain:Len/binary, Port:16>> ->
            {ok, binary_to_list(Domain), Port};
        _ ->
            error
    end;
parse_socks5_address(_, _) ->
    error.

%% Relay data between two sockets
relay_data(Sock1, Sock2) ->
    inet:setopts(Sock1, [{active, once}]),
    inet:setopts(Sock2, [{active, once}]),
    relay_loop(Sock1, Sock2).

relay_loop(Sock1, Sock2) ->
    receive
        {tcp, Sock1, Data} ->
            gen_tcp:send(Sock2, Data),
            inet:setopts(Sock1, [{active, once}]),
            relay_loop(Sock1, Sock2);
        {tcp, Sock2, Data} ->
            gen_tcp:send(Sock1, Data),
            inet:setopts(Sock2, [{active, once}]),
            relay_loop(Sock1, Sock2);
        {tcp_closed, _} ->
            gen_tcp:close(Sock1),
            gen_tcp:close(Sock2);
        {tcp_error, _, _} ->
            gen_tcp:close(Sock1),
            gen_tcp:close(Sock2)
    after 30000 ->
        gen_tcp:close(Sock1),
        gen_tcp:close(Sock2)
    end.
