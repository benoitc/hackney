%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_headers).

-export([new/0, new/1,
         update/2,
         to_list/1,
         get_value/2, get_value/3,
         store/3,
         insert/3,
         delete/2,
         fold/3]).

%% @doc initialise an header dict
new() ->
    dict:new().

new({dict, _}=D) ->
    D;

new(Headers) when is_list(Headers) ->
    lists:foldl(fun({K, V}, D) ->
                insert(K, V, D)
        end, dict:new(), Headers).

update(Headers, KVs) ->
    lists:foldl(fun({K,_V}=KV, D) ->
                dict:store(hackney_util:to_lower(K), KV, D)
        end, Headers, KVs).

%% convert the header to a list
to_list(Headers) ->
    dict:fold(fun(_K, KV, Acc) ->
                    [KV | Acc]
            end, [], Headers).

%% @doc get the value of the header
get_value(Key, Headers) ->
    get_value(Key, Headers, undefined).

get_value(Key, Headers, Default) ->
    case dict:find(hackney_util:to_lower(Key), Headers) of
        {ok, {_K, V}} ->
            V;
        _ ->
            Default
    end.

%% @doc store the pair into the headers, replacing any pre-existing key.
store(Key, Value, Headers) ->
    dict:store(hackney_util:to_lower(Key), {Key, Value}, Headers).


%% @doc Insert the pair into the headers, merging with any pre-existing key.
%% A merge is done with Value = V0 ++ ", " ++ V1.
insert(Key, Value, Headers) ->
    Key1 = hackney_util:to_lower(Key),
    Value1 = case dict:find(Key1, Headers) of
        {ok, {_, OldValue}} ->
            << OldValue/binary, ", ", Value/binary >>;
        _ ->
            Value
    end,
    dict:store(Key1, {Key, Value1}, Headers).

%% @doc Delete the header corresponding to key if it is present.
delete(Key, Headers) ->
    dict:erase(hackney_util:to_lower(Key), Headers).

%% @doc fold the list of headers
fold(Fun, Acc0, Headers) ->
    Wrapper = fun(_K, KV, Acc) ->
            Fun(KV, Acc)
    end,
    dict:fold(Wrapper, Acc0, Headers).
