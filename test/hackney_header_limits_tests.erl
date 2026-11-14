%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Limits on the HTTP/1.1 response header section: max_header_size and
%%% max_headers, in the parser and through hackney:request/5.

-module(hackney_header_limits_tests).

-include_lib("eunit/include/eunit.hrl").

%% Parser

defaults_accept_normal_headers_test() ->
    Hs = [{<<"X-", (integer_to_binary(N))/binary>>, <<"v">>} || N <- lists:seq(1, 50)],
    ?assertMatch({ok, Hs}, parse([], [header_block(Hs)])).

unterminated_line_too_large_test() ->
    Line = binary:copy(<<"a">>, 2000),
    ?assertEqual({error, headers_too_large},
                 parse([{max_header_size, 1024}], [<<"X-A: ">>, Line])),
    %% same bytes, in small reads
    ?assertEqual({error, headers_too_large},
                 parse([{max_header_size, 1024}], split_every(<<"X-A: ", Line/binary>>, 7))).

complete_lines_add_up_test() ->
    Hs = [{<<"X-", (integer_to_binary(N))/binary>>, binary:copy(<<"v">>, 100)}
          || N <- lists:seq(1, 20)],
    ?assertEqual({error, headers_too_large},
                 parse([{max_header_size, 1024}], [header_block(Hs)])),
    ?assertMatch({ok, _}, parse([{max_header_size, 4096}], [header_block(Hs)])).

too_many_headers_test() ->
    Hs = [{<<"X-Pad">>, <<"v">>} || _ <- lists:seq(1, 11)],
    ?assertEqual({error, too_many_headers},
                 parse([{max_headers, 10}], [header_block(Hs)])),
    ?assertMatch({ok, _}, parse([{max_headers, 11}], [header_block(Hs)])).

default_header_count_test() ->
    Hs = [{<<"X-Pad">>, <<"v">>} || _ <- lists:seq(1, 1001)],
    ?assertEqual({error, too_many_headers}, parse([], [header_block(Hs)])).

infinity_disables_test() ->
    Line = binary:copy(<<"a">>, 2 * 1024 * 1024),
    Hs = [{<<"X-A">>, Line} | [{<<"X-Pad">>, <<"v">>} || _ <- lists:seq(1, 1500)]],
    ?assertMatch({ok, _}, parse([{max_header_size, infinity}, {max_headers, infinity}],
                                [header_block(Hs)])),
    ?assertEqual({error, headers_too_large}, parse([], [header_block(Hs)])).

%% Reading a long header line in many small reads must not rescan the
%% whole buffer each time (that was quadratic).
long_line_small_reads_test_() ->
    {timeout, 60, fun() ->
        Line = binary:copy(<<"a">>, 8 * 1024 * 1024),
        Segs = split_every(<<"X-A: ", Line/binary, "\r\n\r\n">>, 4096),
        T0 = erlang:monotonic_time(millisecond),
        ?assertMatch({ok, [{<<"X-A">>, _}]},
                     parse([{max_header_size, infinity}], Segs)),
        ?assert(erlang:monotonic_time(millisecond) - T0 < 2000)
    end}.

folded_and_lf_only_headers_test() ->
    Block = <<"Content-Type: multipart/related;\r\n\tboundary=x\r\nOther: y\r\n\r\n">>,
    ?assertEqual({ok, [{<<"Content-Type">>, <<"multipart/related; boundary=x">>},
                       {<<"Other">>, <<"y">>}]},
                 parse([], split_every(Block, 3))),
    ?assertEqual({ok, [{<<"A">>, <<"1">>}, {<<"B">>, <<"2">>}]},
                 parse([], split_every(<<"A: 1\nB: 2\n\n">>, 2))).

trailers_count_test() ->
    P0 = hackney_http:parser([response, {max_headers, 3}]),
    {response, _, 200, _, P1} = hackney_http:execute(P0, <<"HTTP/1.1 200 OK\r\n">>),
    {header, _, P2} = hackney_http:execute(P1, <<"Transfer-Encoding: chunked\r\n">>),
    {headers_complete, P3} = hackney_http:execute(P2, <<"\r\n">>),
    ?assertEqual({error, too_many_headers},
                 hackney_http:execute(P3, <<"0\r\nT1: a\r\nT2: b\r\nT3: c\r\n\r\n">>)).

%% hackney:request/5

request_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"unterminated header line", fun t_request_unterminated/0},
      {"too many headers", fun t_request_too_many/0},
      {"per-request override", fun t_request_override/0},
      {"async request", fun t_request_async/0}]}.

t_request_unterminated() ->
    Port = serve(fun(S) ->
        ok = gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nX-A: ">>),
        Chunk = binary:copy(<<"a">>, 65536),
        [ok = gen_tcp:send(S, Chunk) || _ <- lists:seq(1, 32)]
    end),
    ?assertEqual({error, headers_too_large}, get(Port, [])).

t_request_too_many() ->
    Hs = [{<<"X-Pad">>, <<"v">>} || _ <- lists:seq(1, 1200)],
    Port = serve(fun(S) ->
        ok = gen_tcp:send(S, [<<"HTTP/1.1 200 OK\r\n">>, header_block(Hs)])
    end),
    ?assertEqual({error, too_many_headers}, get(Port, [])).

t_request_override() ->
    Hs = [{<<"X-Pad">>, <<"v">>} || _ <- lists:seq(1, 1200)],
    Reply = [<<"HTTP/1.1 200 OK\r\n">>,
             header_block([{<<"Content-Length">>, <<"2">>} | Hs]), <<"ok">>],
    Port = serve(fun(S) -> ok = gen_tcp:send(S, Reply) end),
    ?assertMatch({ok, 200, _, <<"ok">>}, get(Port, [{max_headers, 2000}])),
    Port2 = serve(fun(S) -> ok = gen_tcp:send(S, Reply) end),
    ?assertEqual({error, too_many_headers}, get(Port2, [{max_headers, 100}])).

t_request_async() ->
    Hs = [{<<"X-Pad">>, <<"v">>} || _ <- lists:seq(1, 20)],
    Port = serve(fun(S) ->
        ok = gen_tcp:send(S, [<<"HTTP/1.1 200 OK\r\n">>, header_block(Hs)])
    end),
    {ok, Ref} = get(Port, [async, {max_headers, 10}]),
    receive
        {hackney_response, Ref, Msg} -> ?assertEqual({error, too_many_headers}, Msg)
    after 5000 -> ?assert(false)
    end.

%% helpers

get(Port, Opts) ->
    hackney:request(get, <<"http://127.0.0.1:", (integer_to_binary(Port))/binary, "/">>,
                    [], <<>>, [{pool, false}, {recv_timeout, 5000} | Opts]).

%% One-shot server: reads the request, runs Reply on the socket, then holds
%% it open until the client goes away.
serve(Reply) ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(L),
    spawn(fun() ->
        {ok, S} = gen_tcp:accept(L, 5000),
        {ok, _} = gen_tcp:recv(S, 0, 5000),
        catch Reply(S),
        _ = gen_tcp:recv(S, 0, 10000),
        gen_tcp:close(S),
        gen_tcp:close(L)
    end),
    Port.

header_block(Hs) ->
    iolist_to_binary([[[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- Hs], <<"\r\n">>]).

%% Parse a header section fed as separate reads, the way hackney_conn does:
%% drain with execute/1 until the parser asks for more, then feed the next.
parse(Opts, Segments) ->
    P0 = hackney_http:parser([response | Opts]),
    {response, _, 200, _, P1} = hackney_http:execute(P0, <<"HTTP/1.1 200 OK\r\n">>),
    feed(P1, Segments, []).

feed(P, Segments, Acc) ->
    step(hackney_http:execute(P), Segments, Acc).

step({header, H, P}, Segs, Acc) -> feed(P, Segs, [H | Acc]);
step({headers_complete, _}, _Segs, Acc) -> {ok, lists:reverse(Acc)};
step({error, _} = E, _Segs, _Acc) -> E;
step({more, _}, [], _Acc) -> incomplete;
step({more, P}, [Seg | Rest], Acc) -> step(hackney_http:execute(P, Seg), Rest, Acc).

split_every(Bin, N) when byte_size(Bin) =< N -> [Bin];
split_every(Bin, N) ->
    <<H:N/binary, T/binary>> = Bin,
    [H | split_every(T, N)].
