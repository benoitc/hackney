-module(http2_demo).
-export([run/0]).

%% HTTP/2 Demo for hackney
%%
%% Run in rebar3 shell:
%%   1> c("examples/http2_demo").
%%   2> http2_demo:run().

run() ->
    io:format("~n"),
    io:format("=== Hackney HTTP/2 Demo ===~n~n"),

    {ok, Status, Headers, Body} = hackney:get(
        <<"https://nghttp2.org/">>,
        [],
        <<>>,
        [with_body]
    ),

    io:format("  Status:   ~p~n", [Status]),
    io:format("  Server:   ~s~n", [proplists:get_value(<<"server">>, Headers, <<>>)]),
    io:format("  Via:      ~s~n", [proplists:get_value(<<"via">>, Headers, <<>>)]),
    io:format("  Body:     ~p bytes~n~n", [byte_size(Body)]),
    ok.
