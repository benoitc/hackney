%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012, Frederic Trottier-Hebert under the BSD license
%%%
%%% @doc Dispatcher for hackney using dispcount. Starts pools and
%%% handles checkin/checkout.  The user shouldn't have to touch this
%%% module.  @end
%%%
%% @todo: find a way to get rid of inactive pools.
%%   idea: each load balancer has an associated series of counters in an ETS
%%         table: one for checkin, one for checkout, and one timestamp.
%%         If the number of checkin is equal to the number of checkouts and the
%%         timestamp is outdated from a given value, it is assumed that the
%%         dispatcher pool has been inactive for that period of time. When
%%         that happens, it is shut down, to be reopened next time.
%%
-module(hackney_disp).

-export([start/0,
         checkout/4,
         checkin/1, checkin/2]).

-define(TABLE, ?MODULE).

-include("hackney.hrl").

start() ->
    hackney_util:require([dispcount]).


checkout(Host, Port, Transport, #client{options=Opts}) ->
    Info = find_disp({Host, Port, Transport}, Opts),
    case dispcount:checkout(Info) of
        {ok, CheckinReference, {Owner, Socket}} ->
            {ok, {Info, CheckinReference, Owner, Transport}, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

checkin({Info, Ref, _Owner, _Transport}) ->
    dispcount:checkin(Info, Ref, dead).

checkin({Info, Ref, Owner, Transport}, Socket) ->
    case Transport:controlling_process(Socket, Owner) of
        ok -> dispcount:checkin(Info, Ref, Socket);
        _ -> dispcount:checkin(Info, Ref, dead)
    end.

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%% This function needs to be revised after the shutdown mechanism
%% for a worker is decided on.
find_disp(Key, Args) ->
    case ets:lookup(hackney_pool, Key) of
        [] ->
            start_disp(Key, Args);
        [{_,Info}] ->
            Info
    end.

%% Wart ahead. Atoms are created dynamically based on the key
%% to ensure uniqueness of pools.
%% TODO: optionally use gproc for registration to avoid this?
start_disp(Key = {Host, Port, Transport}, Opts0) ->
    {ok, Type} = application:get_env(hackney, restart),
    {ok, Timeout} = application:get_env(hackney, shutdown),
    {ok, X} = application:get_env(hackney, maxr),
    {ok, Y} = application:get_env(hackney, maxt),

    %% apply defaults to the options
    Opts = hackney_util:maybe_apply_defaults([max_connections], Opts0),
    MaxConn = proplists:get_value(max_connections, Opts, 25),

    AtomKey = case Transport of
        hackney_ssl_transport ->
            list_to_atom("dispcount_" ++ Host ++ integer_to_list(Port)
                         ++ "_ssl");
        _ ->
            list_to_atom("dispcount_" ++ Host ++ integer_to_list(Port))
    end,
    Res = dispcount:start_dispatch(
            AtomKey,
            {hackney_disp_handler, {Host, Port, Transport, Opts}},
            [{restart,Type},{shutdown,Timeout},
             {maxr,X},{maxt,Y},{resources, MaxConn}]
            ),
    case Res of
        ok ->
            {ok, Info} = dispcount:dispatcher_info(AtomKey),
            ets:insert(hackney_pool, {Key, Info}),
            Info;
        already_started ->
            find_disp(Key, Opts)
    end.
