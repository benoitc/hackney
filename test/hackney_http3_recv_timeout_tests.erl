%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% recv_timeout holds on HTTP/3 as on HTTP/1.1 and HTTP/2: a server that
%%% stops sending fails the request with {error, timeout}, counted from the
%%% last byte received, and the stalled stream is reset.
-module(hackney_http3_recv_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

h3_recv_timeout_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun(Ctx) -> {timeout, 60, fun() -> stall_before_headers(Ctx) end} end,
      fun(Ctx) -> {timeout, 60, fun() -> stall_mid_body(Ctx) end} end,
      fun(Ctx) -> {timeout, 60, fun() -> conn_still_usable(Ctx) end} end]}.

setup() ->
    hackney_h3_test_server:start().

cleanup(Ctx) ->
    hackney_h3_test_server:stop(Ctx).

%% No response headers: the request fails and the server sees the stream
%% cancelled.
stall_before_headers(Ctx) ->
    true = register(hackney_h3_test_hold, self()),
    try
        {T, Res} = timed(fun() -> get(Ctx, <<"/hold">>, [{pool, false}]) end),
        ?assertEqual({error, timeout}, Res),
        ?assert(T < 5000),
        receive {held, _, _, StreamId} ->
            receive {held_reset, StreamId} -> ok
            after 5000 -> ?assert(false)
            end
        after 0 -> ?assert(false)
        end
    after
        unregister(hackney_h3_test_hold)
    end.

%% Headers and part of the body, then nothing.
stall_mid_body(Ctx) ->
    true = register(hackney_h3_test_reset, self()),
    try
        {T, Res} = timed(fun() -> get(Ctx, <<"/reset">>, [{pool, false}]) end),
        ?assertEqual({error, timeout}, Res),
        ?assert(T < 5000)
    after
        unregister(hackney_h3_test_reset)
    end.

%% After a timed-out stream the pooled connection serves the next request.
conn_still_usable(Ctx) ->
    Pool = list_to_atom("h3_recv_timeout_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = hackney_pool:start_pool(Pool, []),
    try
        true = register(hackney_h3_test_hold, self()),
        try
            ?assertEqual({error, timeout}, get(Ctx, <<"/hold">>, [{pool, Pool}]))
        after
            unregister(hackney_h3_test_hold)
        end,
        ?assertMatch({ok, 200, _, _}, get(Ctx, <<"/">>, [{pool, Pool}]))
    after
        hackney_pool:stop_pool(Pool)
    end.

get(Ctx, Path, Extra) ->
    Opts = lists:keystore(recv_timeout, 1, hackney_h3_test_server:hackney_opts(),
                          {recv_timeout, 500}),
    hackney:request(get, hackney_h3_test_server:url(Ctx, Path), [], <<>>,
                    Extra ++ Opts).

timed(Fun) ->
    T0 = erlang:monotonic_time(millisecond),
    Res = Fun(),
    {erlang:monotonic_time(millisecond) - T0, Res}.
