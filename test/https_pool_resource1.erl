-module(https_pool_resource1).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"pool_resource1">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
