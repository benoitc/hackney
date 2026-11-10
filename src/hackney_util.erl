%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_util).

-export([filter_options/3]).
-export([socket_opts/1, dial_socket_opts/1]).

%% gen_tcp/inet options a caller may set on a dialled socket.
-define(DIAL_SOCKET_OPTS,
        [keepalive, nodelay, linger, send_timeout, send_timeout_close, raw,
         inet, inet6, ip, ifaddr, recbuf, sndbuf, buffer, tos, tclass,
         priority, reuseaddr, reuseport, reuseport_lb, exclusiveaddruse,
         dontroute, bind_to_device, netns, ipv6_v6only, delay_send,
         high_watermark, low_watermark, high_msgq_watermark,
         low_msgq_watermark, exit_on_close, show_econnreset, nopush]).
-export([set_option_default/3]).
-export([require/1]).
-export([maybe_apply_defaults/2]).
-export([is_ipv6/1]).
-export([privdir/0]).
-export([default_protocols/0]).
-export([to_atom/1]).

-export([merge_opts/2]).
-export([to_int/1]).

-include("hackney.hrl").

%% @doc filter a proplists and only keep allowed keys
-spec filter_options([{atom(), any()} | {raw, any(), any(), any()}],
  [atom()], Acc) -> Acc when Acc :: [any()].
filter_options([], _, Acc) ->
  Acc;
filter_options([Opt = {Key, _}|Tail], AllowedKeys, Acc) ->
  case lists:member(Key, AllowedKeys) of
    true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
    false -> filter_options(Tail, AllowedKeys, Acc)
  end;
filter_options([Opt = {raw, _, _, _}|Tail], AllowedKeys, Acc) ->
  case lists:member(raw, AllowedKeys) of
    true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
    false -> filter_options(Tail, AllowedKeys, Acc)
  end;
filter_options([Opt|Tail], AllowedKeys, Acc) when is_atom(Opt) ->
  case lists:member(Opt, AllowedKeys) of
    true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
    false -> filter_options(Tail, AllowedKeys, Acc)
  end.

%% @doc The socket options in a connect_options list: everything except the
%% entries hackney reads itself. Used to key pooled and shared connections by
%% the socket options they were opened with.
-spec socket_opts(list()) -> list().
socket_opts(ConnectOpts) ->
  [Opt || Opt <- ConnectOpts, not hackney_opt(Opt)].

%% @doc The caller's socket options to merge over a transport's own base
%% options when it dials. Only gen_tcp/inet socket options are kept:
%% connect_options also carries hackney's own entries for the proxy
%% transports (on_connect_response, proxy_auth_fun, ...), and the framing
%% options the transport reads its socket with (binary, active, packet,
%% mode) stay the transport's.
-spec dial_socket_opts(list()) -> list().
dial_socket_opts(ConnectOpts) ->
  lists:reverse(filter_options(ConnectOpts, ?DIAL_SOCKET_OPTS, [])).

hackney_opt({Key, _}) ->
  lists:member(Key, [protocols, insecure, session_ticket, happy_eyeballs]);
hackney_opt(insecure) ->
  true;
hackney_opt(_) ->
  false.

%% @doc set the default options in a proplists if not defined
-spec set_option_default(Opts, atom(), any())
    -> Opts when Opts :: [{atom(), any()}].
set_option_default(Opts, Key, Value) ->
  case lists:keymember(Key, 1, Opts) of
    true -> Opts;
    false -> [{Key, Value}|Opts]
  end.

%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
  ok;
require([App|Rest]) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end,
  require(Rest).

maybe_apply_defaults([], Options) ->
  Options;
maybe_apply_defaults([OptName | Rest], Options) ->
  case proplists:is_defined(OptName, Options) of
    true ->
      maybe_apply_defaults(Rest, Options);
    false ->
      {ok, Default} = application:get_env(hackney, OptName),
      maybe_apply_defaults(Rest, [{OptName, Default} | Options])
  end.

is_ipv6(Host) ->
  case inet_parse:address(Host) of
    {ok, {_, _, _, _, _, _, _, _}} ->
      true;
    {ok, {_, _, _, _}} ->
      false;
    _ ->
      case inet:getaddr(Host, inet) of
        {ok, _} ->
          false;
        _ ->
          case inet:getaddr(Host, inet6) of
            {ok, _} ->
              true;
            _ ->
              false
          end
      end
  end.

privdir() ->
  case code:priv_dir(hackney) of
    {error, _} ->
      %% try to get relative priv dir. useful for tests.
      EbinDir = filename:dirname(code:which(?MODULE)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Dir -> Dir
  end.

%% @doc Get the default protocols for HTTP connections.
%% Returns the value of the `default_protocols` application env,
%% or `[http2, http1]' if not set.
%%
%% The order determines preference: HTTP/2 is preferred, then HTTP/1.1.
%%
%% To enable HTTP/3 (experimental):
%% ```
%% application:set_env(hackney, default_protocols, [http3, http2, http1]).
%% '''
-spec default_protocols() -> [http3 | http2 | http1].
default_protocols() ->
  case application:get_env(hackney, default_protocols) of
    {ok, Protocols} when is_list(Protocols) -> Protocols;
    _ -> [http2, http1]
  end.

%% GHSA-6rmf: never fall back to list_to_atom/1. Minting a fresh atom for
%% every unique caller-supplied string (header field-names, tokens, JSON
%% keys, ...) lets an attacker exhaust the fixed-size, never-GC'd atom table
%% and crash the whole VM. Unknown names raise badarg, the same contract as
%% list_to_existing_atom/1; callers that need new atoms must create them
%% explicitly.
to_atom(V) when is_list(V) ->
  list_to_existing_atom(V);
to_atom(V) when is_binary(V) ->
  to_atom(binary_to_list(V));
to_atom(V) when is_atom(V) ->
  V.

merge_opts([], Options) -> Options;
merge_opts([Opt = {K, _}| Rest], Options) ->
  case lists:keymember(K, 1, Options) of
    true -> merge_opts(Rest, Options);
    false -> merge_opts(Rest, [Opt | Options])
  end;
merge_opts([Opt={raw, _, _, _} | Rest], Options) ->
  merge_opts(Rest, [Opt | Options]);
merge_opts([K | Rest], Options) when is_atom(K) ->
  case lists:member(K, Options) of
    true -> merge_opts(Rest, Options);
    false -> merge_opts(Rest, [K | Options])
  end;
merge_opts([_ | Rest], Options) ->
  merge_opts(Rest, Options).

to_int(S) when is_binary(S) ->
  to_int(binary_to_list(S));
to_int(S) ->
  try
    I = list_to_integer(S),
    {ok, I}
  catch
    error:badarg -> false
  end.