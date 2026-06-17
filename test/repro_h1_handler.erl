%%% Cowboy handler for the HTTP/1.1 negative control: 200, application/json,
%%% ~14 KB body, NO content-length (chunked), connection kept alive.
-module(repro_h1_handler).
-export([init/2]).

init(Req0, State) ->
    Body = <<"{\"d\":\"", (binary:copy(<<"x">>, 13988))/binary, "\"}">>,
    Req1 = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"application/json">>}, Req0),
    ok = cowboy_req:stream_body(Body, fin, Req1),
    {ok, Req1, State}.
