%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2018 11:42
%%%-------------------------------------------------------------------
-module(hackney_noclen_tests).
-author("benoitc").

-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").


setup() -> ok.

teardown(_) -> ok.

empty_clen_test_() ->
  {
    "test empty content length",
    {
      setup,
      fun start/0, fun stop/1,
      {
        foreach,
        fun setup/0, fun teardown/1,
        [
          fun empty_clen/1
        ]
      }
    }
  }.

start() ->
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _} = application:ensure_all_started(hackney),
  Host = '_',
  Resource = {"/empty", empty_clen_resource, []},
  Dispatch = cowboy_router:compile([{Host, [Resource]}]),
  cowboy:start_http(test_empty_clen_server, 10, [{port, 8123}], [{env, [{dispatch, Dispatch}]}]).

stop(_) ->
  ok = cowboy:stop_listener(test_empty_clen_server),
  ok.

empty_clen(_) ->
    URL = <<"http://localhost:8123/empty/">>,
    Headers = [],
    Opts = [with_body],
    {ok, 200, RespHeaders0, Body} = hackney:request(get, URL, Headers, <<>>, Opts),
    RespHeaders1 = hackney_headers_new:from_list(RespHeaders0),
    undefined = hackney_headers_new:get_value(<<"content-length">>, RespHeaders1),
    undefined = hackney_headers_new:get_value(<<"transfer-encoding">>, RespHeaders1),
    ?_assertEqual( <<"this is a body">>, Body).

