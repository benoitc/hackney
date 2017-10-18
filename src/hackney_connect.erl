%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_connect).

-export([connect/3, connect/4, connect/5,
         create_connection/4, create_connection/5,
         maybe_connect/1,
         reconnect/4,
         set_sockopts/2,
         ssl_opts/2,
         check_or_close/1,
         close/1,
         is_pool/1]).

-export([partial_chain/1]).

-include("hackney.hrl").
-include_lib("hackney_internal.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

connect(Transport, Host, Port) ->
  connect(Transport, Host, Port, []).

connect(Transport, Host, Port, Options) ->
  connect(Transport, Host, Port, Options, false).

connect(Transport, Host, Port, Options, Dynamic) when is_binary(Host) ->
  connect(Transport, binary_to_list(Host), Port, Options, Dynamic);
connect(Transport, Host, Port, Options, Dynamic) ->
  ?report_debug("connect", [{transport, Transport},
    {host, Host},
    {port, Port},
    {dynamic, Dynamic}]),
  case create_connection(Transport, idna:utf8_to_ascii(Host), Port,
    Options, Dynamic) of
    {ok, #client{request_ref=Ref}} ->
      {ok, Ref};
    Error ->
      Error
  end.


%% @doc create a connection and return a client state
create_connection(Transport, Host, Port, Options) ->
  create_connection(Transport, Host, Port, Options, true).

create_connection(Transport, Host, Port, Options, Dynamic)
  when is_list(Options) ->
  Netloc = case {Transport, Port} of
             {hackney_tcp, 80}  -> list_to_binary(Host);
             {hackney_ssl, 443} -> list_to_binary(Host);
             _ ->
               iolist_to_binary([Host, ":", integer_to_list(Port)])
           end,
  %% default timeout
  Timeout = proplists:get_value(recv_timeout, Options, ?RECV_TIMEOUT),
  FollowRedirect = proplists:get_value(follow_redirect, Options, false),
  MaxRedirect = proplists:get_value(max_redirect, Options, 5),
  ForceRedirect = proplists:get_value(force_redirect, Options, false),
  Async =  proplists:get_value(async, Options, false),
  StreamTo = proplists:get_value(stream_to, Options, false),
  WithBody = proplists:get_value(with_body, Options, false),
  MaxBody = proplists:get_value(max_body, Options),

  %% get mod metrics
  Engine = metrics:init(hackney_util:mod_metrics()),

  %% initial state
  InitialState = #client{mod_metrics=Engine,
    transport=Transport,
    host=Host,
    port=Port,
    netloc=Netloc,
    options=Options,
    dynamic=Dynamic,
    recv_timeout=Timeout,
    follow_redirect=FollowRedirect,
    max_redirect=MaxRedirect,
    retries=MaxRedirect,
    force_redirect=ForceRedirect,
    async=Async,
    with_body=WithBody,
    max_body=MaxBody,
    stream_to=StreamTo,
    buffer = <<>>},

  %% if we use a pool then checkout the connection from the pool, else
  %% connect the socket to the remote
  %%
  reconnect(Host, Port, Transport, InitialState).


%% @doc connect a socket and create a client state.
%%
maybe_connect(#client{state=closed, redirect=nil}=Client) ->
  %% the socket has been closed, reconnect it.
  #client{transport=Transport,
    host=Host,
    port=Port} = Client,
  reconnect(Host, Port, Transport, Client);
maybe_connect(#client{state=closed, redirect=Redirect}=Client) ->
  %% connection closed after a redirection, reinit the options and
  %% reconnect it.
  {Transport, Host, Port, Options} = Redirect,
  Client1 = Client#client{options=Options,
    redirect=nil},
  reconnect(Host, Port, Transport, Client1);
maybe_connect(#client{redirect=nil}=Client) ->
  {ok, check_mod_metrics(Client)};
maybe_connect(#client{redirect=Redirect}=Client) ->
  %% reinit the options and reconnect the client
  {Transport, Host, Port, Options} = Redirect,
  reconnect(Host, Port, Transport, Client#client{options=Options,
    redirect=nil}).

check_or_close(#client{socket=nil}=Client) ->
  Client;
check_or_close(Client) ->
  case is_pool(Client) of
    false ->
      close(Client);
    true ->
      #client{socket=Socket, socket_ref=Ref, pool_handler=Handler}=Client,
      _ = Handler:checkin(Ref, Socket),
      Client#client{socket=nil, state=closed}
  end.



%% @doc add set sockets options in the client
set_sockopts(#client{transport=Transport, socket=Skt}, Options) ->
  Transport:setopts(Skt, Options).


%% @doc close the client
%%
%%
close(#client{socket=nil}=Client) ->
  Client#client{state = closed};
close(#client{transport=Transport, socket=Skt}=Client) ->
  Transport:close(Skt),
  Client#client{state = closed, socket=nil};
close(Ref) when is_reference(Ref) ->
  hackney_manager:close_request(Ref).


%% @doc get current pool pid or name used by a client if needed
is_pool(#client{options=Opts}) ->
  UseDefaultPool = use_default_pool(),
  case proplists:get_value(pool, Opts) of
    false ->
      false;
    undefined when UseDefaultPool =:= true ->
      true;
    undefined ->
      false;
    _ ->
      true
  end.

reconnect(Host, Port, Transport, State) ->
  %% if we use a pool then checkout the connection from the pool, else
  %% connect the socket to the remote
  case is_pool(State) of
    false ->
      %% the client won't use any pool
      do_connect(Host, Port, Transport, check_mod_metrics(State));
    true ->
      socket_from_pool(Host, Port, Transport, check_mod_metrics(State))
  end.

%%
%% internal functions
%%

socket_from_pool(Host, Port, Transport, Client0) ->
  PoolHandler = hackney_app:get_app_env(pool_handler, hackney_pool),
  PoolName = proplists:get_value(pool, Client0#client.options, default),
  Metrics = Client0#client.mod_metrics,

  %% new request
  {_RequestRef, Client} = hackney_manager:new_request(Client0),

  case PoolHandler:checkout(Host, Port, Transport, Client) of
    {ok, Ref, Skt} ->
      ?report_debug("reuse a connection", [{pool, PoolName}]),
      _ = metrics:update_meter(Metrics, [hackney_pool, PoolName, take_rate], 1),
      _ = metrics:increment_counter(Metrics, [hackney_pool, Host, reuse_connection]),
      Client1 = Client#client{socket=Skt,
        socket_ref=Ref,
        pool_handler=PoolHandler,
        state = connected},

      hackney_manager:update_state(Client1),
      {ok, Client1};
    {error, no_socket, Ref} ->
      ?report_trace("no socket in the pool", [{pool, PoolName}]),
      _ = metrics:increment_counter(Metrics, [hackney_pool, PoolName, no_socket]),
      Client1 = Client#client{socket_ref=Ref, pool_handler=PoolHandler},

      do_connect(Host, Port, Transport, Client1, pool);
    Error ->
      Error
  end.

do_connect(Host, Port, Transport, Client) ->
  do_connect(Host, Port, Transport, Client, direct).



do_connect(Host, Port, Transport, #client{mod_metrics=Metrics,
  options=Opts}=Client0, Type) ->
  Begin = os:timestamp(),
  {_RequestRef, Client} = case Type of
                            pool ->
                              {Client0#client.request_ref, Client0};
                            direct ->
                              hackney_manager:new_request(Client0)
                          end,

  ConnectOpts0 = proplists:get_value(connect_options, Opts, []),
  ConnectTimeout = proplists:get_value(connect_timeout, Opts, 8000),

  %% handle ipv6
  ConnectOpts1 = case lists:member(inet, ConnectOpts0) orelse
    lists:member(inet6, ConnectOpts0) of
                   true ->
                     ConnectOpts0;
                   false ->
                     case hackney_util:is_ipv6(Host) of
                       true ->
                         [inet6 | ConnectOpts0];
                       false ->
                         ConnectOpts0
                     end
                 end,

  ConnectOpts = case Transport of
                  hackney_ssl ->
                    ConnectOpts1 ++ ssl_opts(Host, Opts);
                  _ ->
                    ConnectOpts1
                end,
  case Transport:connect(Host, Port, ConnectOpts, ConnectTimeout) of
    {ok, Skt} ->
      ?report_trace("new connection", []),
      ConnectTime = timer:now_diff(os:timestamp(), Begin)/1000,
      _ = metrics:update_histogram(Metrics, [hackney, Host, connect_time], ConnectTime),
      _ = metrics:increment_counter(Metrics, [hackney_pool, Host, new_connection]),
      Client1 = Client#client{socket=Skt,
        state = connected},
      hackney_manager:update_state(Client1),
      {ok, Client1};
    {error, timeout} ->
      ?report_trace("connect timeout", []),
      _ = metrics:increment_counter(Metrics, [hackney, Host, connect_timeout]),
      hackney_manager:cancel_request(Client),
      {error, connect_timeout};
    Error ->
      ?report_trace("connect error", []),
      _ = metrics:increment_counter(Metrics, [hackney, Host, connect_error]),
      hackney_manager:cancel_request(Client),
      Error
  end.


use_default_pool() ->
  case application:get_env(hackney, use_default_pool) of
    {ok, Val} ->
      Val;
    _ ->
      true
  end.

check_mod_metrics(#client{mod_metrics=Mod}=State)
  when Mod /= nil, Mod /= undefined ->
  State;
check_mod_metrics(State) ->
  State#client{mod_metrics=metrics:init(hackney_util:mod_metrics())}.

ssl_opts(Host, Options) ->
  case proplists:get_value(ssl_options, Options) of
    undefined ->
      Insecure =  proplists:get_value(insecure, Options, false),
      CACerts = certifi:cacerts(),
      case Insecure of
        true ->
          [{verify, verify_none}];
        false ->
          VerifyFun = {
            fun ssl_verify_hostname:verify_fun/3,
            [{check_hostname, Host}]
          },
          [{verify, verify_peer},
            {depth, 99},
            {cacerts, CACerts},
            {partial_chain, fun partial_chain/1},
            {verify_fun, VerifyFun}]
      end;
    SSLOpts ->
      SSLOpts
  end.

%% code from rebar3 undert BSD license
partial_chain(Certs) ->
  Certs1 = lists:reverse([{Cert, public_key:pkix_decode_cert(Cert, otp)} ||
    Cert <- Certs]),
  CACerts = certifi:cacerts(),
  CACerts1 = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CACerts],

  case find(fun({_, Cert}) ->
    check_cert(CACerts1, Cert)
            end, Certs1) of
    {ok, Trusted} ->
      {trusted_ca, element(1, Trusted)};
    _ ->
      unknown_ca
  end.

extract_public_key_info(Cert) ->
  ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

check_cert(CACerts, Cert) ->
  lists:any(fun(CACert) ->
    extract_public_key_info(CACert) == extract_public_key_info(Cert)
            end, CACerts).

-spec find(fun(), list()) -> {ok, term()} | error.
find(Fun, [Head|Tail]) when is_function(Fun) ->
  case Fun(Head) of
    true ->
      {ok, Head};
    false ->
      find(Fun, Tail)
  end;
find(_Fun, []) ->
  error.
