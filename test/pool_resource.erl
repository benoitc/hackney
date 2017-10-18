%% Feel free to use, reuse and abuse the code in this file.

%% @doc Pool handler.
-module(pool_resource).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req, [{length, infinity}]),
    {ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
