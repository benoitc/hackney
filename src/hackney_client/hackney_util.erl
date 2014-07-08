%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2014 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_util).

-export([filter_options/3]).
-export([set_option_default/3]).
-export([require/1]).
-export([maybe_apply_defaults/2]).
-export([is_ipv6/1]).

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
