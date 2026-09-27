%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc connect_timeout on a connection opened without a pool (#945).
%%%
%%% connect_direct/4 waited on the dial with a hardcoded 8000 ms call, so a
%%% larger connect_timeout was cut to 8 s, and a dial outliving the wait
%%% exited the caller instead of returning an error.
-module(hackney_direct_connect_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

direct_connect_timeout_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> hackney_fault_transport:clear() end,
     [{"connect_timeout above 8 s is honoured", {timeout, 30, fun t_above_default/0}},
      {"a dial past the deadline is an error", {timeout, 30, fun t_past_deadline/0}}]}.

%% The dial takes 8.3 s, inside a 9 s connect_timeout: it must succeed.
t_above_default() ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(L),
    try
        ok = hackney_fault_transport:set(connect, {sleep, 8300}),
        Result = hackney:connect(hackney_fault_transport, "127.0.0.1", Port,
                                 [{pool, false}, {connect_timeout, 9000}]),
        ?assertMatch({ok, _}, Result),
        {ok, Conn} = Result,
        hackney:close(Conn)
    after
        hackney_fault_transport:clear(),
        gen_tcp:close(L)
    end.

%% The transport never answers: the caller gets an error on its own deadline,
%% not an exit, and is not held until the transport gives up.
t_past_deadline() ->
    try
        ok = hackney_fault_transport:set(connect, {hang, 1500}),
        {Elapsed, Result} =
            timer:tc(fun() ->
                hackney:connect(hackney_fault_transport, "127.0.0.1", 9,
                                [{pool, false}, {connect_timeout, 50}])
            end),
        ?assertEqual({error, connect_timeout}, Result),
        ?assert(Elapsed div 1000 < 800)
    after
        hackney_fault_transport:clear()
    end.
