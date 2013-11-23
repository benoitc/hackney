%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012, Frederic Trottier-Hebert under the BSD license
%%%
%%% @doc Dispcount worker implementation for TCP socket handling
%%% @end
%%%
-module(hackney_disp_handler).
-behaviour(dispcount).
-behaviour(coffer_pool_handler).

-export([init/1,
         checkout/2,
         checkin/2,
         handle_info/2,
         dead/1,
         terminate/2,
         code_change/3]).

-record(state, {resource = {error,undefined},
                given = false,
                init_arg,
                transport}).

-include("hackney.hrl").

init(Init) ->
    %% Make it lazy to connect. On checkout only!
    {ok, #state{init_arg=Init}}.

checkout(_From, State = #state{given=true}) ->
    {error, busy, State};
checkout(From, State = #state{resource={ok, Socket},
                              transport=Transport}) ->
    Transport:setopts(Socket, [{active,false}]),
    case Transport:controlling_process(Socket, From) of
        ok ->
            {ok, {self(), Socket}, State#state{given=true}};
        {error, badarg} -> % caller died
            Transport:setopts(Socket, [{active, once}]),
            {error, caller_is_dead, State};
        {error, _Reason} -> % socket closed or something
            case reconnect(State) of
                {ok, NewState} ->
                    checkout(From, NewState);
                Return ->
                    Return
            end
    end;
checkout(From, State = #state{resource={error, _Reason}}) ->
    case reconnect(State) of
        {ok, NewState} ->
            checkout(From, NewState);
        Return ->
            Return
    end;
checkout(From, State) ->
    {stop, {invalid_call, From, State}, State}.

checkin(Socket, State = #state{resource={ok, Socket}, given=true,
                               transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    {ok, State#state{given=false}};
checkin(_Socket, State) ->
    %% The socket doesn't match the one we had -- an error happened somewhere
    {ignore, State}.

dead(State) ->
    %% aw shoot, someone lost our resource, we gotta create a new one:
    case reconnect(State#state{given=false}) of
        {ok, NewState} ->
            {ok, NewState};
        {error, _Reason, NewState} -> % stuff might be down!
            {ok, NewState}
    end.

handle_info(_Msg, State) ->
    %% something unexpected with the TCP connection if we set it to active,once???
    {ok, State}.

terminate(_Reason, _State) ->
    %% let the GC clean the socket.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reconnect(State = #state{init_arg={Host, Port, Transport, Opts}}) ->

    ConnectOpts0 = proplists:get_value(connect_options, Opts, []),
    ConnectTimeout = proplists:get_value(connect_timeout, Opts, 8000),

    %% handle ipv6
    ConnectOpts1 = case hackney_util:is_ipv6(Host) of
        true ->
            [inet6 | ConnectOpts0];
        false ->
            ConnectOpts0
    end,

    ConnectOpts = case {Transport, proplists:get_value(ssl_options, Opts)} of
        {hackney_ssl_transport, undefined} ->
            case proplists:get_value(insecure, Opts) of
                true ->
                    ConnectOpts1 ++ [{verify, verify_none},
                             {reuse_sessions, true}];
                _ ->
                    ConnectOpts1
            end;
        {hackney_ssl_transport, SslOpts} ->
            ConnectOpts1 ++ SslOpts;
        {_, _} ->
            ConnectOpts1
    end,

    case Transport:connect(Host, Port, ConnectOpts, ConnectTimeout) of
        {ok, Socket} ->
            {ok, State#state{resource = {ok, Socket}, transport=Transport}};
        {error, Reason} ->
            error_logger:warning_msg("hackney reconnect fail (~p): ~p~n",
                                     [{Transport, Host, Port}, Reason]),
            {error, Reason, State#state{resource = {error, Reason},
                                        transport=Transport}}
    end.
