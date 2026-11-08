%%% max_body_size must hold for a chunked HTTP/1.1 body made of one huge
%%% chunk. The parser used to buffer a chunk until all of it had arrived and
%%% the cap was checked per complete chunk, so a server declaring a 256 MB
%%% chunk made hackney buffer all of it whatever max_body_size said.
-module(hackney_chunk_limit_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MB, (1024 * 1024)).

chunk_limit_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{timeout, 60, fun buffered_body_stops_at_the_cap/0},
      {timeout, 60, fun streamed_body_comes_out_as_it_arrives/0}]}.

%% A 256 MB chunk with max_body_size at 1 MB: the request fails with
%% body_too_large once a little over 1 MB has arrived, and memory stays low.
buffered_body_stops_at_the_cap() ->
    {Port, Server} = huge_chunk_server(64),
    Before = erlang:memory(binary),
    R = hackney:request(get, url(Port), [], <<>>,
                        [{pool, false}, with_body, {max_body_size, ?MB},
                         {recv_timeout, 10000}]),
    Grew = erlang:memory(binary) - Before,
    Server ! stop,
    ?assertEqual({error, body_too_large}, R),
    ?assert(Grew < 16 * ?MB).

%% Read as a stream, the same chunk comes out in pieces before it is
%% complete.
streamed_body_comes_out_as_it_arrives() ->
    {Port, Server} = huge_chunk_server(4),
    {ok, Conn} = hackney:connect(hackney_tcp, "127.0.0.1", Port,
                                 [{pool, false}, {recv_timeout, 10000}]),
    {ok, 200, _Headers, Conn} = hackney:send_request(Conn, {get, <<"/">>, [], <<>>}),
    {ok, Part} = hackney:stream_body(Conn),
    ?assert(byte_size(Part) > 0),
    ?assert(byte_size(Part) =< 4 * ?MB),
    hackney:close(Conn),
    Server ! stop.

%% A server that answers with a chunked body declaring 256 MB, then sends
%% MBs of it and waits.
huge_chunk_server(MBs) ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(L),
    Pid = spawn(fun() ->
        {ok, S} = gen_tcp:accept(L),
        {ok, _Req} = gen_tcp:recv(S, 0),
        ok = gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                               "10000000\r\n">>),
        Chunk = binary:copy(<<"a">>, ?MB),
        _ = [gen_tcp:send(S, Chunk) || _ <- lists:seq(1, MBs)],
        receive stop -> ok after 30000 -> ok end,
        gen_tcp:close(S),
        gen_tcp:close(L)
    end),
    {Port, Pid}.

url(Port) ->
    "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/".
