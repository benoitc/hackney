%% Feel free to use, reuse and abuse the code in this file.

%% @doc Pool handler.
-module(pool_resource).

-export([init/2]).

init(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Req3 = cowboy_req:reply(200, #{}, Body, Req2),
    {ok, Req3, State}.
