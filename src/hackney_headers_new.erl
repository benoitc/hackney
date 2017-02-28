%%% -*- erlang -*-
%%%
%%% This file is part of hackney_lib released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(hackney_headers_new).
-author("benoitc").

%% API
-export([
  new/0,
  from_list/1,
  append/3,
  store/3,
  store_new/3,
  is_key/2,
  lookup/2,
  size/1,
  fold/3,
  merge/2,
  to_list/1,
  to_iolist/1,
  to_binary/1
]).

-define(kl(Key), hackney_bstr:to_lower(Key)).

new() -> {0, dict:new()}.

from_list(HeadersList) when is_list(HeadersList) ->
  lists:foldl(
    fun({Key, Value}, Headers) -> append(Key, Value, Headers) end,
    {0, dict:new()},
    HeadersList
  );
from_list(_) -> erlang:error(badarg).

append(Key, Value, {N, Headers}) ->
  KL = ?kl(Key),
  {N + 1, dict:append(KL, {N, Key, Value}, Headers)}.


store(Key, Values, {N, Headers}) when is_list(Values) ->
  KL = ?kl(Key),
  lists:foldl(
    fun(V, {I, H}) ->
      {I + 1, dict:append(KL, [{I, Key, V}], H)}
    end,
    {N, dict:store(KL, [], Headers)},
    Values
  );
store(Key, Value, {N, Headers}) ->
  KL = ?kl(Key),
  {N +1, dict:store(KL, [{N, Key, Value}], Headers)}.

store_new(Key, Value, {_N, DictHeaders}=Headers) ->
  KL = ?kl(key),
  case dict:is_key(KL, DictHeaders) of
    true ->
      {false, Headers};
    false ->
      {true, store(Key, Value, Headers)}
  end.

is_key(Key, {_, Headers}) -> dict:is_key(?kl(Key), Headers).

lookup(Key, {_, Headers}) ->
  case dict:find(?kl(Key), Headers) of
    {ok, KVs} -> [{K, V} || {_, K, V} <- KVs];
    error -> []
  end.

size({N, _}) -> N.

fold(Fun, Acc, {_, Headers}) ->
  Lines = dict:fold(
    fun(_, Value, Acc1) -> Value ++ Acc1 end,
    [],
    Headers
  ),
  do_fold(lists:sort(Lines), Fun, Acc).

do_fold([{_, Key, Value} | Rest], Fun, Acc) ->
  do_fold(Rest, Fun, Fun(Key, Value, Acc));
do_fold([], _Fun, Acc) -> Acc.

merge(Headers1, {_, DictHeaders2}) ->
  dict:fold(
    fun(Key, Value, H) ->
      case is_key(Key, H) of
        true -> H;
        false ->
          [{_, K, V} | Rest] = Value,
          do_merge(Rest, store(K, V, H))
      end
    end,
    Headers1,
    DictHeaders2
  ).

do_merge([{_, K, V} | R], H) -> do_merge(R, append(K, V, H));
do_merge([], H) -> H.

to_list(Headers) ->
  Result = fold(
    fun(Key, Value, Acc) -> [{Key, Value} | Acc] end,
    [],
    Headers
  ),
  lists:reverse(Result).

to_iolist(Headers) ->
  L = fold(
    fun(Key, Value0, L1) ->
      case Value0 of
        {Value, Params} ->
          [[
            hackney_bstr:to_binary(Key),": ",  hackney_bstr:to_binary(Value),
            params_to_iolist(Params, []), "\r\n"
          ] | L1];
        _ ->
          [[hackney_bstr:to_binary(Key),": ", hackney_bstr:to_binary(Value0), "\r\n"] | L1]
      end
    end,
    [],
    Headers
  ),
  lists:reverse(["\r\n" | L ]).

params_to_iolist([{K, V} | Rest], List) ->
  List2 = [[";", hackney_bstr:to_binary(K), "=", hackney_bstr:to_binary(V)] | List],
  params_to_iolist(Rest, List2);
params_to_iolist([K | Rest], List) ->
  List2 = [[";", hackney_bstr:to_binary(K)] | List],
  params_to_iolist(Rest, List2);
params_to_iolist([], List) ->
  lists:reverse(List).

to_binary(Headers) ->
  iolist_to_binary(to_iolist(Headers)).