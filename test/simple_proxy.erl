-module(simple_proxy).

-export([handle_client/0]).
-export([proxy_server_init/0]).
-export([start_proxy_server/0]).

start_proxy_server() ->
  {ok, LSock} = gen_tcp:listen(5555, [binary, {packet, http_bin}, {active, false}]),
  Pid = spawn(?MODULE, proxy_server_init, []),
  gen_tcp:controlling_process(LSock, Pid),
  Pid ! {socket, LSock},
  Pid.

proxy_server_init() ->
  LSock = receive
    {socket, Sock} -> Sock
  end,

  io:format(user, "PROXY SERVER RUNNING~n", []),
  proxy_server_loop(LSock).

proxy_server_loop(LSock) ->
  case gen_tcp:accept(LSock) of
    {ok, Sock} ->
      Pid = spawn(?MODULE, handle_client, []),
      ok = gen_tcp:controlling_process(Sock, Pid), 
      Pid ! {socket, Sock},
      proxy_server_loop(LSock);
    Error ->
      io:format(user, "UNEXPECTED ERROR~p~n", [Error])
  end.


skip_headers(Sock) ->
 
  {ok, Header} =  gen_tcp:recv(Sock, 0, 5000),
  case Header of
    {http_header,_,_,_,_} -> skip_headers(Sock);
    http_eoh ->
      ok
  end.

handle_client() ->
  Sock = receive
    {socket, Socket} -> Socket
  end,
  
  {ok, {http_request,<<"CONNECT">>, {scheme, Host, Port}, {1,1}}} = gen_tcp:recv(Sock, 0, 5000),
  skip_headers(Sock),
  IntPort = binary_to_integer(Port),
  ok = inet:setopts(Sock, [{active, true}, {packet, raw}, binary]),
  {ok, ForeignSock} = gen_tcp:connect(binary_to_list(Host), IntPort, [binary, {packet, raw}, {active, true}]),
  io:format(user, "CONNECT TO FOREIGN ADDRESS ~p ~p ~n", [Host, IntPort]),
  ok = gen_tcp:send(Sock, <<"HTTP/1.1 200 OK\r\n\r\n">>),
  proxy_data_loop(Sock, ForeignSock).

proxy_data_loop(Sock, ForeignSock) ->
  receive
    {tcp, Sock, Data} ->
      gen_tcp:send(ForeignSock, Data),
      proxy_data_loop(Sock, ForeignSock);
    {tcp, ForeignSock, Data} ->
      gen_tcp:send(Sock, Data),
      proxy_data_loop(Sock, ForeignSock);
    {tcp_closed, Sock} ->
      io:format(user, "CLIENT SOCK CLOSED~n", []),
      gen_tcp:close(Sock);
    {tcp_closed, ForeignSock} ->
      io:format(user, "FOREIGN SOCK CLOSED~n", []),
      gen_tcp:close(ForeignSock)
    after 30000 ->
      io:format(user, "TIMEOUT~n", []),
      gen_tcp:close(Sock),
      gen_tcp:close(ForeignSock)
  end.
