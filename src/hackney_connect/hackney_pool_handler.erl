%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_pool_handler).

-include("hackney.hrl").

-type host() :: binary() | string().
-type client() :: #client{}.

-ifdef(no_callback_support).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{start, 0},
     {checkout, 4},
     {checkin, 2}];
behaviour_info(_) ->
    undefined.

-else.

%% start a bool handler
-callback start() -> ok | {error, Reason :: any()}.

-callback checkout(Host::host(), Port::integer(),Transport::atom(),
                   Client::client()) ->
    {ok, {Info::any(), CheckingReference::any(), Owner::pid(),
          Transport::atom()}, Socket::inet:socket()}
    | {error, Reason :: any()}.

-callback checkin({Info::any(), CheckingReference::any(), Owner::pid(),
                   Transport::atom()}, Socket::inet:socket()) ->
    ok
    | {error, Reason :: any()}.

-endif.
