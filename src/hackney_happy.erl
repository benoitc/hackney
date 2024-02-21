-module(hackney_happy).

-export([connect/3, connect/4]).

-include("hackney_internal.hrl").
-include_lib("kernel/include/inet.hrl").

-define(TIMEOUT, 250).
-define(CONNECT_TIMEOUT, 5000).

connect(Hostname, Port, Opts) ->
  connect(Hostname, Port, Opts, ?CONNECT_TIMEOUT).

connect(Hostname, Port, Opts, Timeout) ->
  case inet_cidr:is_ipv6(Hostname) of
    true ->
      gen_tcp:connect(Hostname, Port, [inet6 | Opts], Timeout);
    false ->
      case inet_cidr:is_ipv4(Hostname) of
        true ->
         gen_tcp:connect(Hostname, Port, [inet | Opts], Timeout);
        false ->
          Self = self(),
          Addrs = getaddrs(Hostname),
          Pid = spawn_link( fun() -> try_connect(Addrs, Port, Opts, Self) end),
          MRef = erlang:monitor(process, Pid),
          receive
            {happy_connect, OK} ->
              erlang:demonitor(MRef, [flush]),
              OK;
            {'DOWN', MRef, _Type, _Pid, _Info} ->
              {'error', 'connect_error'}
          after Timeout -> 
            erlang:demonitor(MRef, [flush]),
            {error, connect_timeout}
          end
      end
  end.
 
getaddrs(Hostname) ->
  IP6Addrs = [{Addr, 'inet6'} || Addr <- getbyname(Hostname, 'aaaa')],
  IP4Addrs = [{Addr, 'inet'} || Addr <- getbyname(Hostname, 'a')],
  IP6Addrs ++ IP4Addrs.

getbyname(Hostname, Type) ->
  case inet_res:getbyname(Hostname, Type) of
    {'ok', #hostent{h_addr_list=AddrList}} -> inet_cidr:usort_cidrs(AddrList);
    {error, _Reason} -> []
  end.

try_connect([], _Port, _Opts, ServerPid) ->
  ServerPid ! {hackney_happy, {error, nxdomain}};
try_connect([{IP, Type} | Rest], Port, Opts, ServerPid) ->
  case gen_tcp:connect(IP, Port, [Type | Opts], ?TIMEOUT) of
    {ok, Socket} = OK ->
      ok = gen_tcp:controlling_process(Socket, ServerPid),
      ServerPid ! {happy_connect, OK};
    _Error ->
      try_connect(Rest, Port, Opts, ServerPid) 
  end.