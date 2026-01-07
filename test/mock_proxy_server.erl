%%% -*- erlang -*-
%%%
%%% Mock proxy servers for testing hackney proxy support.
%%% Implements minimal HTTP CONNECT and SOCKS5 proxies.

-module(mock_proxy_server).

-export([start_connect_proxy/0, start_connect_proxy/1]).
-export([start_socks5_proxy/0, start_socks5_proxy/1]).
-export([start_http_proxy/0, start_http_proxy/1]).
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

%% Simple HTTP proxy implementation (forwards requests with absolute URLs)
start_http_proxy() ->
    start_http_proxy(0).

start_http_proxy(Port) ->
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    {ok, ActualPort} = inet:port(ListenSock),
    Pid = spawn_link(fun() -> http_proxy_loop(ListenSock) end),
    gen_tcp:controlling_process(ListenSock, Pid),
    {ok, Pid, ActualPort}.

http_proxy_loop(ListenSock) ->
    receive
        stop ->
            gen_tcp:close(ListenSock),
            ok
    after 0 ->
        case gen_tcp:accept(ListenSock, 100) of
            {ok, ClientSock} ->
                Pid = spawn(fun() -> handle_http_proxy_client(ClientSock) end),
                gen_tcp:controlling_process(ClientSock, Pid),
                Pid ! socket_ready,
                http_proxy_loop(ListenSock);
            {error, timeout} ->
                http_proxy_loop(ListenSock);
            {error, closed} ->
                ok
        end
    end.

handle_http_proxy_client(ClientSock) ->
    receive socket_ready -> ok after 1000 -> ok end,
    case recv_until_headers(ClientSock, <<>>, 5000) of
        {ok, Data} ->
            case parse_http_request(Data) of
                {ok, Method, Host, Port, Path, Rest} ->
                    %% Connect to target
                    case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
                        {ok, TargetSock} ->
                            %% Rebuild request with relative path
                            Request = rebuild_request(Method, Path, Rest),
                            ok = gen_tcp:send(TargetSock, Request),
                            %% Forward response back
                            forward_response(TargetSock, ClientSock),
                            gen_tcp:close(TargetSock),
                            gen_tcp:close(ClientSock);
                        {error, _} ->
                            gen_tcp:send(ClientSock, <<"HTTP/1.1 502 Bad Gateway\r\n\r\n">>),
                            gen_tcp:close(ClientSock)
                    end;
                error ->
                    gen_tcp:send(ClientSock, <<"HTTP/1.1 400 Bad Request\r\n\r\n">>),
                    gen_tcp:close(ClientSock)
            end;
        {error, _} ->
            gen_tcp:close(ClientSock)
    end.

parse_http_request(Data) ->
    %% Parse request line like: GET http://host:port/path HTTP/1.1
    case binary:split(Data, <<"\r\n">>) of
        [RequestLine, Rest] ->
            case binary:split(RequestLine, <<" ">>, [global]) of
                [Method, AbsoluteUrl, _Version] ->
                    case parse_absolute_url(AbsoluteUrl) of
                        {ok, Host, Port, Path} ->
                            {ok, Method, Host, Port, Path, Rest};
                        error ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.

parse_absolute_url(Url) ->
    %% Parse http://host:port/path or http://host/path
    case Url of
        <<"http://", Rest/binary>> ->
            parse_host_port_path(Rest);
        _ ->
            error
    end.

parse_host_port_path(Rest) ->
    case binary:match(Rest, <<"/">>) of
        {Pos, 1} ->
            HostPort = binary:part(Rest, 0, Pos),
            Path = binary:part(Rest, Pos, byte_size(Rest) - Pos),
            case binary:match(HostPort, <<":">>) of
                {ColonPos, 1} ->
                    Host = binary_to_list(binary:part(HostPort, 0, ColonPos)),
                    PortBin = binary:part(HostPort, ColonPos + 1, byte_size(HostPort) - ColonPos - 1),
                    Port = binary_to_integer(PortBin),
                    {ok, Host, Port, Path};
                nomatch ->
                    {ok, binary_to_list(HostPort), 80, Path}
            end;
        nomatch ->
            %% No path, just host:port
            case binary:match(Rest, <<":">>) of
                {ColonPos, 1} ->
                    Host = binary_to_list(binary:part(Rest, 0, ColonPos)),
                    PortBin = binary:part(Rest, ColonPos + 1, byte_size(Rest) - ColonPos - 1),
                    Port = binary_to_integer(PortBin),
                    {ok, Host, Port, <<"/">>};
                nomatch ->
                    {ok, binary_to_list(Rest), 80, <<"/">>}
            end
    end.

rebuild_request(Method, Path, HeadersAndBody) ->
    [Method, <<" ">>, Path, <<" HTTP/1.1\r\n">>, HeadersAndBody].

forward_response(TargetSock, ClientSock) ->
    case gen_tcp:recv(TargetSock, 0, 10000) of
        {ok, Data} ->
            gen_tcp:send(ClientSock, Data),
            forward_response(TargetSock, ClientSock);
        {error, closed} ->
            ok;
        {error, _} ->
            ok
    end.
