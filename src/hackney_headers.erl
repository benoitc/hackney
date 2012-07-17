%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_headers).

-export([new/0, new/1,
         update/2,
         to_list/1,
         get_value/2, get_value/3,
         insert/3,
         delete/2,
         fold/3]).


new() ->
    dict:new().

new({dict, _}=D) ->
    D;

new(Headers) when is_list(Headers) ->
    lists:foldl(fun({K,_V}=KV, D) ->
                dict:store(hackney_util:to_lower(K), KV, D)
        end, dict:new(), Headers).

update(Headers, KVs) ->
    lists:foldl(fun({K,_V}=KV, D) ->
                dict:store(hackney_util:to_lower(K), KV, D)
        end, Headers, KVs).

to_list(Headers) ->
    dict:fold(fun(_K, KV, Acc) ->
                    [KV | Acc]
            end, [], Headers).

get_value(Key, Headers) ->
    get_value(Key, Headers, undefined).

get_value(Key, Headers, Default) ->
    case dict:find(hackney_util:to_lower(Key), Headers) of
        {ok, {_K, V}} ->
            V;
        _ ->
            Default
    end.

insert(Key, Value, Headers) ->
    dict:store(hackney_util:to_lower(Key), {Key, Value}, Headers).

delete(Key, Headers) ->
    dict:erase(hackney_util:to_lower(Key), Headers).

fold(Fun, Acc0, Headers) ->
    Wrapper = fun(_K, KV, Acc) ->
            Fun(KV, Acc)
    end,
    dict:fold(Wrapper, Acc0, Headers).
