-module(http2_demo).
-export([run/0]).

%% HTTP/2 Demo for hackney
%%
%% Run in rebar3 shell:
%%   1> c("examples/http2_demo").
%%   2> http2_demo:run().

run() ->
    io:format("~n=== Hackney HTTP/2 Demo ===~n~n"),

    %% HTTP/2 request (default)
    io:format("HTTP/2 request:~n"),
    {ok, S1, H1, _} = hackney:get(<<"https://nghttp2.org/">>, [], <<>>, [with_body]),
    io:format("  Status: ~p~n", [S1]),
    io:format("  Headers: lowercase (HTTP/2)~n"),
    io:format("  Example: ~p~n~n", [element(1, hd(H1))]),

    %% HTTP/1.1 request (forced via protocols option)
    io:format("HTTP/1.1 request (protocols=[http1]):~n"),
    {ok, S2, H2, _} = hackney:get(<<"https://nghttp2.org/">>, [], <<>>,
                                  [with_body, {protocols, [http1]}]),
    io:format("  Status: ~p~n", [S2]),
    io:format("  Headers: mixed case (HTTP/1.1)~n"),
    io:format("  Example: ~p~n", [element(1, hd(H2))]),

    ok.
