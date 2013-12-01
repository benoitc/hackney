%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_http_proxy).

-export([maybe_proxy/4,
         connect_proxy/5]).

-include("hackney.hrl").

connect_proxy(ProxyUrl, ProxyOpts, Host, Port, Options) ->
    do_connect_proxy(ProxyUrl, ProxyOpts, Host, Port, Options, false).

maybe_proxy(Transport, Host, Port, Options)
        when is_list(Host), is_integer(Port), is_list(Options) ->

    case proplists:get_value(proxy, Options) of
        Url when is_binary(Url) orelse is_list(Url) ->
            ProxyOpts = [{basic_auth, proplists:get_value(proxy_auth,
                                                          Options)}],
            #hackney_url{transport=PTransport} = hackney_url:parse_url(Url),

            if PTransport =/= Transport ->
                    {error, invalid_proxy_transport};
                true ->
                    connect_proxy(Url, ProxyOpts, Host, Port, Options)
            end;
        {ProxyHost, ProxyPort} ->
            Netloc = iolist_to_binary([ProxyHost, ":",
                                       integer_to_list(ProxyPort)]),
            Scheme = hackney_url:transport_scheme(Transport),
            Url = #hackney_url{scheme=Scheme, netloc=Netloc},
            ProxyOpts = [{basic_auth, proplists:get_value(proxy_auth,
                                                          Options)}],
            do_connect_proxy(hackney_url:unparse_url(Url), ProxyOpts, Host,
                             Port, Options, true);

        _ ->
            hackney_connect:connect(Transport, Host, Port, Options)
    end.


%% private
do_connect_proxy(ProxyUrl, ProxyOpts0, Host, Port, Options, Dynamic) ->
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
