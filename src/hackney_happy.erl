-module(hackney_happy).

-export([connect/3, connect/4]).

-include("hackney_internal.hrl").
-include_lib("kernel/include/inet.hrl").

-define(TIMEOUT, 250).
-define(CONNECT_TIMEOUT, 5000).

connect(Hostname, Port, Opts) ->
  connect(Hostname, Port, Opts, ?CONNECT_TIMEOUT).

connect(Hostname, Port, Opts, Timeout) ->
  case hackney_cidr:is_ipv6(Hostname) of
    true ->
      ?report_debug("connect using IPv6", [{hostname, Hostname}, {port, Port}]),
      gen_tcp:connect(Hostname, Port, [inet6 | Opts], Timeout);
    false ->
      case hackney_cidr:is_ipv4(Hostname) of
        true ->
          ?report_debug("connect using IPv4", [{hostname, Hostname}, {port, Port}]),
          gen_tcp:connect(Hostname, Port, [inet | Opts], Timeout);
        false ->
          ?report_debug("happy eyeballs, try to connect using IPv6",  [{hostname, Hostname}, {port, Port}]),
          Self = self(),
          Addrs = getaddrs(Hostname),
          Pid = spawn_link( fun() -> try_connect(Addrs, Port, Opts, Self, {error, nxdomain}) end),
          MRef = erlang:monitor(process, Pid),
          receive
            {happy_connect, OK} ->
              erlang:demonitor(MRef, [flush]),
              OK;
            {'DOWN', MRef, _Type, _Pid, Info} ->
              {'error', {'connect_error', Info}}
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
  case (catch inet_res:getbyname(Hostname, Type)) of
    {'ok', #hostent{h_addr_list=AddrList}} -> lists:usort(AddrList);
    {error, _Reason} -> [];
    Else ->
      %% ERLANG 22 has an issue when g matching somee DNS server messages
      ?report_debug("DNS error", [{hostname, Hostname}
                                 ,{type, Type}
                                 ,{error, Else}]),
      []
  end.

try_connect([], _Port, _Opts, ServerPid, LastError) ->
  ?report_trace("happy eyeball: failed to connect", [{error, LastError}]),
  ServerPid ! {hackney_happy, LastError};
try_connect([{IP, Type} | Rest], Port, Opts, ServerPid, _LastError) ->
  ?report_trace("try to connect", [{ip, IP}, {type, Type}]),
  case gen_tcp:connect(IP, Port, [Type | Opts], ?TIMEOUT) of
    {ok, Socket} = OK ->
      ?report_trace("success to connect", [{ip, IP}, {type, Type}]),
      ok = gen_tcp:controlling_process(Socket, ServerPid),
      ServerPid ! {happy_connect, OK};
    Error ->
      try_connect(Rest, Port, Opts, ServerPid, Error)
  end.
