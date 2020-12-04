%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2018 11:42
%%%-------------------------------------------------------------------
-module(hackney_ssl_tests).
-author("benoitc").

-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").


setup() -> ok.

teardown(_) -> ok.

empty_clen_test_() ->
  {
    "test SSL",
    {
      setup,
      fun start/0, fun stop/1,
      {
        foreach,
        fun setup/0, fun teardown/1,
        [
          fun wildcard_cert/1
        ]
      }
    }
  }.

start() ->
  error_logger:tty(false),
  {ok, _} = application:ensure_all_started(hackney),
  ok.

stop(_) ->
  application:stop(hackney),
  error_logger:tty(true),
  ok.

wildcard_cert(_) ->
    URL = <<"https://friendpaste.com">>,
    Resp = case hackney:get(URL) of
             {ok, _, _, _} -> valid;
             _ -> error
           end,
    ?_assertEqual(Resp, valid).

