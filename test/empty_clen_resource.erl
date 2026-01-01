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

-export([init/2]).

init(Req, State) ->
    %% Send a streaming response without Content-Length, using connection close
    Headers = #{<<"connection">> => <<"close">>},
    Req2 = cowboy_req:stream_reply(200, Headers, Req),
    ok = cowboy_req:stream_body(<<"this is a body">>, fin, Req2),
    {ok, Req2, State}.
