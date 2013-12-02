%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_http_proxy).

-export([connect/5,
         connect/6]).

-include("hackney.hrl").

connect(ProxyUrl, ProxyOpts, Host, Port, Options) ->
    connect(ProxyUrl, ProxyOpts, Host, Port, Options, false).

connect(ProxyUrl, ProxyOpts0, Host, Port, Options, Dynamic) ->
    Host1 = iolist_to_binary([Host, ":", integer_to_list(Port)]),
    Headers = [{<<"Host">>, Host1}],
    Timeout = proplists:get_value(recv_timeout, Options, infinity),
    ProxyOpts = [{recv_timeout, Timeout} | ProxyOpts0],
    case hackney:request(connect, ProxyUrl, Headers, <<>>, ProxyOpts) of
        {ok, 200, _, Ref} ->
            State = hackney_manager:get_state(Ref),
            hackney_manager:update_state(State#client{dynamic=Dynamic,
                                                      recv_timeout=Timeout,
                                                      options=Options,
                                                      response_state=start,
                                                      body_state=waiting}),

            {ok, Ref};
        {ok, S, H, Ref} ->
            Body = hackney:body(Ref),
            %% error, make sure we cancel the request,
            %% we just ignore the result
            hackney_manager:cancel_request(Ref),
            {error, {proxy_connection, S, H, Body}};
        Error ->
            {error, {proxy_connection, Error}}
    end.
