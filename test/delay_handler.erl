%%% -*- erlang -*-
%%%
%%% Test resource that delays before responding
%%% Used to test timeout behavior

-module(delay_handler).

-export([init/2]).

init(Req0, State) ->
    %% Extract delay seconds from path binding
    Seconds = cowboy_req:binding(seconds, Req0),
    DelayMs = binary_to_integer(Seconds) * 1000,

    %% Sleep to simulate slow response
    timer:sleep(DelayMs),

    %% Return response
    Body = <<"{\"delayed\": true}">>,
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req0),
    {ok, Req, State}.
