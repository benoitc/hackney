%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc Structural test: the pool must never let a connection process take it
%%% down.
%%%
%%% The pool talks to connection processes with synchronous calls made from
%%% inside its own gen_server. Every one of those calls can exit: the conn can
%%% be gone (`noproc'), wedged in a transport call (`timeout'), or crash while
%%% answering. An unguarded call turns any of those into a dead pool and a dead
%%% pool takes every caller with it, which is how issue #927 was reported.
%%%
%%% Rather than wait for each of those to be found in production, this walks
%%% the compiled abstract code of `hackney_pool' and fails on any call into
%%% `hackney_conn' that is not lexically inside a `try'. When it fails, the fix
%%% is to route the call through a guarded helper next to the others in
%%% `hackney_pool' (`connect_connection/2', `stop_conn/1', `checkin_info/1'),
%%% not to relax the check.
-module(hackney_pool_safety_tests).

-include_lib("eunit/include/eunit.hrl").

%% Calls that cannot raise: casts are fire and forget.
-define(SAFE_BY_NATURE, [set_owner_async]).

conn_calls_are_guarded_test() ->
    Unguarded = [Call || Call <- conn_calls(hackney_pool), unguarded(Call)],
    ?assertEqual([], Unguarded).

%% The same rule holds for the connection supervisor: nothing there may raise
%% into a caller either.
conn_sup_calls_are_guarded_test() ->
    Unguarded = [Call || Call <- conn_calls(hackney_conn_sup), unguarded(Call)],
    ?assertEqual([], Unguarded).

unguarded({_Where, _Line, Fun, Guarded}) ->
    not (Guarded orelse lists:member(Fun, ?SAFE_BY_NATURE)).

%%====================================================================
%% Abstract code walk
%%====================================================================

%% Returns {Function/Arity, Line, CalledFun, InsideTry} per hackney_conn call.
conn_calls(Module) ->
    lists:flatmap(
      fun({function, _, Name, Arity, Clauses}) ->
              Where = lists:flatten(io_lib:format("~s/~b", [Name, Arity])),
              walk(Clauses, Where, false);
         (_Other) ->
              []
      end,
      abstract_code(Module)).

%% Read the beam through the code server rather than a path: it works the same
%% whether the module is loaded, cover compiled, or only on the code path.
abstract_code(Module) ->
    {Module, Beam, _File} = code:get_object_code(Module),
    {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(Beam, [abstract_code]),
    Forms.

%% A hand walk rather than erl_syntax: all this needs is "is there a try
%% between me and the enclosing function".
walk({'try', _Line, Body, Cases, Catches, After}, Where, _InTry) ->
    walk(Body, Where, true) ++ walk(Cases, Where, true) ++
        walk(Catches, Where, true) ++ walk(After, Where, true);
walk({call, Line, {remote, _, {atom, _, hackney_conn}, {atom, _, Fun}}, Args},
     Where, InTry) ->
    [{Where, Line, Fun, InTry} | walk(Args, Where, InTry)];
walk(Tuple, Where, InTry) when is_tuple(Tuple) ->
    walk(tuple_to_list(Tuple), Where, InTry);
walk(List, Where, InTry) when is_list(List) ->
    lists:flatmap(fun(Item) -> walk(Item, Where, InTry) end, List);
walk(_Other, _Where, _InTry) ->
    [].
