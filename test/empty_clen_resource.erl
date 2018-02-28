%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2018 11:38
%%%-------------------------------------------------------------------
-module(empty_clen_resource).
-author("benoitc").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, _Opts) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Req2 = cowboy_req:set([{resp_state, waiting_stream}], Req),
  {ok, Req3} = cowboy_req:chunked_reply(200, [{<<"connection">>, <<"close">>}], Req2),
  %%Req5 = cowboy_req:delete_resp_header(<<"transfer-encoding">>, Req4),
  cowboy_req:chunk(<<"this is a body">>, Req3),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.
