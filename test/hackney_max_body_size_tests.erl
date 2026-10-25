%%% Tests for the max_body_size cap on bodies hackney buffers whole.
%%%
%%% hackney:request/5 (and body/1 after start_response/1) accumulate the
%%% response body in memory with no bound, and auto_decompress inflated it
%%% through zlib with no bound either, so any server could drive the client
%%% to OOM (a 256 KB gzip bomb became a 256 MiB body). The read now stops
%%% with {error, body_too_large} once the body, after decompression, passes
%%% max_body_size. The standalone HTTP/3 API already had this option.
-module(hackney_max_body_size_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% HTTP/1.1 (raw TCP server)
%%====================================================================

h1_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"chunked body past max_body_size fails",
       {timeout, 30, fun t_h1_chunked_too_large/0}},
      {"content-length body past max_body_size fails",
       {timeout, 30, fun t_h1_clen_too_large/0}},
      {"body within max_body_size is returned",
       {timeout, 30, fun t_h1_within_limit/0}},
      {"max_body is accepted as an alias",
       {timeout, 30, fun t_h1_max_body_alias/0}},
      {"infinity disables the cap",
       {timeout, 30, fun t_h1_infinity/0}},
      {"gzip bomb past max_body_size fails with auto_decompress",
       {timeout, 60, fun t_h1_gzip_bomb/0}},
      {"deflate bomb past max_body_size fails with auto_decompress",
       {timeout, 60, fun t_h1_deflate_bomb/0}},
      {"gzip and deflate within the cap still decompress",
       {timeout, 30, fun t_h1_decompress_within_limit/0}},
      {"body/1 after a streamed request is capped too",
       {timeout, 30, fun t_h1_body_after_start_response/0}}]}.

t_h1_chunked_too_large() ->
    Url = start_h1_server(chunked_response(binary:copy(<<"A">>, 4096), 16)),
    ?assertEqual({error, body_too_large},
                 hackney:request(get, Url, [], <<>>, h1_opts(1024))).

t_h1_clen_too_large() ->
    Url = start_h1_server(clen_response(binary:copy(<<"A">>, 65536), [])),
    ?assertEqual({error, body_too_large},
                 hackney:request(get, Url, [], <<>>, h1_opts(1024))).

t_h1_within_limit() ->
    Body = binary:copy(<<"A">>, 65536),
    Url = start_h1_server(chunked_response(binary:copy(<<"A">>, 4096), 16)),
    ?assertMatch({ok, 200, _, Body},
                 hackney:request(get, Url, [], <<>>, h1_opts(65536))).

t_h1_max_body_alias() ->
    Url = start_h1_server(clen_response(binary:copy(<<"A">>, 65536), [])),
    ?assertEqual({error, body_too_large},
                 hackney:request(get, Url, [], <<>>,
                                 [{with_body, true}, {pool, false}, {max_body, 1024}])).

t_h1_infinity() ->
    Body = binary:copy(<<"A">>, 65536),
    Url = start_h1_server(clen_response(Body, [])),
    ?assertMatch({ok, 200, _, Body},
                 hackney:request(get, Url, [], <<>>, h1_opts(infinity))).

t_h1_gzip_bomb() ->
    %% 64 MiB of zeros is ~64 KB on the wire.
    Bomb = zlib:gzip(binary:copy(<<0>>, 64 bsl 20)),
    ?assert(byte_size(Bomb) < 1 bsl 20),
    Url = start_h1_server(clen_response(Bomb, [{<<"Content-Encoding">>, <<"gzip">>}])),
    ?assertEqual({error, body_too_large},
                 hackney:request(get, Url, [], <<>>,
                                 [{auto_decompress, true} | h1_opts(1 bsl 20)])).

t_h1_deflate_bomb() ->
    Bomb = zlib:compress(binary:copy(<<0>>, 64 bsl 20)),
    ?assert(byte_size(Bomb) < 1 bsl 20),
    Url = start_h1_server(clen_response(Bomb, [{<<"Content-Encoding">>, <<"deflate">>}])),
    ?assertEqual({error, body_too_large},
                 hackney:request(get, Url, [], <<>>,
                                 [{auto_decompress, true} | h1_opts(1 bsl 20)])).

t_h1_decompress_within_limit() ->
    Plain = binary:copy(<<"hello hackney ">>, 1000),
    GzUrl = start_h1_server(clen_response(zlib:gzip(Plain),
                                          [{<<"Content-Encoding">>, <<"gzip">>}])),
    ?assertMatch({ok, 200, _, Plain},
                 hackney:request(get, GzUrl, [], <<>>,
                                 [{auto_decompress, true} | h1_opts(byte_size(Plain))])),
    DfUrl = start_h1_server(clen_response(zlib:compress(Plain),
                                          [{<<"Content-Encoding">>, <<"deflate">>}])),
    ?assertMatch({ok, 200, _, Plain},
                 hackney:request(get, DfUrl, [], <<>>,
                                 [{auto_decompress, true} | h1_opts(byte_size(Plain))])),
    %% Default cap: a small compressed body is still returned.
    GzUrl2 = start_h1_server(clen_response(zlib:gzip(Plain),
                                           [{<<"Content-Encoding">>, <<"gzip">>}])),
    ?assertMatch({ok, 200, _, Plain},
                 hackney:request(get, GzUrl2, [], <<>>,
                                 [{auto_decompress, true}, {with_body, true}, {pool, false}])).

t_h1_body_after_start_response() ->
    Url = start_h1_server(clen_response(binary:copy(<<"A">>, 65536), [])),
    {ok, ConnPid} = hackney:request(get, Url, [], stream, [{pool, false}, {max_body_size, 1024}]),
    ok = hackney:finish_send_body(ConnPid),
    {ok, 200, _Headers, ConnPid} = hackney:start_response(ConnPid),
    ?assertEqual({error, body_too_large}, hackney:body(ConnPid)).

h1_opts(Max) ->
    [{with_body, true}, {pool, false}, {recv_timeout, 10000}, {max_body_size, Max}].

chunked_response(Chunk, N) ->
    Size = io_lib:format("~.16b", [byte_size(Chunk)]),
    ["HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n",
     [[Size, "\r\n", Chunk, "\r\n"] || _ <- lists:seq(1, N)],
     "0\r\n\r\n"].

clen_response(Body, ExtraHeaders) ->
    ["HTTP/1.1 200 OK\r\nContent-Length: ", integer_to_list(byte_size(Body)),
     "\r\nConnection: close\r\n",
     [[Name, ": ", Value, "\r\n"] || {Name, Value} <- ExtraHeaders],
     "\r\n", Body].

%% One-shot server: accept one connection, read the request, write Response.
start_h1_server(Response) ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true},
                                     {packet, raw}]),
    {ok, Port} = inet:port(LSock),
    spawn_link(fun() ->
                   {ok, Sock} = gen_tcp:accept(LSock, 10000),
                   {ok, _Req} = gen_tcp:recv(Sock, 0, 5000),
                   ok = gen_tcp:send(Sock, Response),
                   %% Give the client time to read before closing.
                   timer:sleep(500),
                   gen_tcp:close(Sock),
                   gen_tcp:close(LSock)
               end),
    iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/"]).

%%====================================================================
%% HTTP/2 (h2 test server)
%%====================================================================

h2_test_() ->
    {setup,
     fun h2_setup/0,
     fun h2_cleanup/1,
     fun(Ctx) ->
         [{"HTTP/2 body past max_body_size fails",
           {timeout, 30, fun() -> t_h2_too_large(Ctx) end}},
          {"HTTP/2 body within max_body_size is returned",
           {timeout, 30, fun() -> t_h2_within_limit(Ctx) end}},
          {"HTTP/2 body/1 after start_response/1 is capped",
           {timeout, 30, fun() -> t_h2_body_after_start_response(Ctx) end}},
          {"HTTP/2 connection stays usable after body_too_large",
           {timeout, 30, fun() -> t_h2_conn_reusable(Ctx) end}}]
     end}.

-define(H2_BODY_SIZE, (1 bsl 20)).

h2_setup() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => fun h2_handler/5,
        settings => #{max_concurrent_streams => unlimited}
    }),
    #{server => Server, port => h2:server_port(Server)}.

h2_cleanup(#{server := Server}) ->
    try h2:stop_server(Server) catch _:_ -> ok end,
    ok.

%% /large: 1 MiB in 16 KB DATA frames. other: "ok".
h2_handler(Conn, Sid, _Method, <<"/large">>, _Headers) ->
    ok = h2:send_response(Conn, Sid, 200, [{<<"content-type">>, <<"application/octet-stream">>}]),
    h2_send_chunks(Conn, Sid, binary:copy(<<"x">>, 16384), ?H2_BODY_SIZE div 16384);
h2_handler(Conn, Sid, _Method, _Path, _Headers) ->
    ok = h2:send_response(Conn, Sid, 200, [{<<"content-type">>, <<"text/plain">>}]),
    ok = h2:send_data(Conn, Sid, <<"ok">>, true).

h2_send_chunks(Conn, Sid, Chunk, 1) ->
    _ = h2:send_data(Conn, Sid, Chunk, true, #{block => 10000}),
    ok;
h2_send_chunks(Conn, Sid, Chunk, N) ->
    case h2:send_data(Conn, Sid, Chunk, false, #{block => 10000}) of
        ok -> h2_send_chunks(Conn, Sid, Chunk, N - 1);
        %% The client reset the stream: stop sending.
        _ -> ok
    end.

t_h2_too_large(#{port := Port}) ->
    ?assertEqual({error, body_too_large},
                 hackney:request(get, h2_url(Port, <<"/large">>), [], <<>>,
                                 [{max_body_size, 65536} | h2_opts()])).

t_h2_within_limit(#{port := Port}) ->
    {ok, 200, _, Body} = hackney:request(get, h2_url(Port, <<"/large">>), [], <<>>,
                                         [{max_body_size, ?H2_BODY_SIZE} | h2_opts()]),
    ?assertEqual(?H2_BODY_SIZE, byte_size(Body)).

t_h2_body_after_start_response(#{port := Port}) ->
    {ok, ConnPid} = hackney:request(get, h2_url(Port, <<"/large">>), [], stream,
                                    [{max_body_size, 65536} | h2_opts()]),
    ok = hackney:finish_send_body(ConnPid),
    {ok, 200, _Headers, ConnPid} = hackney:start_response(ConnPid),
    ?assertEqual({error, body_too_large}, hackney:body(ConnPid)),
    hackney:close(ConnPid).

t_h2_conn_reusable(#{port := Port}) ->
    {ok, ConnPid} = hackney:connect(hackney_ssl, "localhost", Port, h2_opts()),
    ?assertEqual({error, body_too_large},
                 hackney_conn:request(ConnPid, <<"GET">>, <<"/large">>, [], <<>>, infinity,
                                      [{max_body_size, 65536}])),
    %% The oversized stream was reset; the connection still serves requests.
    ?assertMatch({ok, 200, _, <<"ok">>},
                 hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>, infinity, [])),
    hackney:close(ConnPid).

h2_opts() ->
    [{pool, false},
     {protocols, [http2]},
     {recv_timeout, 10000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

h2_url(Port, Path) ->
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), Path]).

%% Resolve the test cert dir from the module's beam location so paths work
%% regardless of where eunit is run from.
cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

%%====================================================================
%% HTTP/3 (hackney_h3_test_server)
%%====================================================================

h3_test_() ->
    {setup,
     fun() -> Server = hackney_h3_test_server:start(), hackney_altsvc:clear_all(), Server end,
     fun(Server) ->
         hackney_conn_sup:stop_all(),
         hackney_altsvc:clear_all(),
         hackney_h3_test_server:stop(Server)
     end,
     fun(Server) ->
         [{"HTTP/3 body past max_body_size fails",
           {timeout, 30, fun() -> t_h3_too_large(Server) end}},
          {"HTTP/3 body within max_body_size is returned",
           {timeout, 30, fun() -> t_h3_within_limit(Server) end}}]
     end}.

%% /large on the test server is 65536 bytes.
t_h3_too_large(Server) ->
    Url = hackney_h3_test_server:url(Server, <<"/large">>),
    ?assertEqual({error, body_too_large},
                 hackney:request(get, Url, [], <<>>,
                                 [{max_body_size, 1024}, {pool, false}
                                  | hackney_h3_test_server:hackney_opts()])).

t_h3_within_limit(Server) ->
    Url = hackney_h3_test_server:url(Server, <<"/large">>),
    {ok, 200, _, Body} = hackney:request(get, Url, [], <<>>,
                                         [{max_body_size, 65536}, {pool, false}
                                          | hackney_h3_test_server:hackney_opts()]),
    ?assertEqual(65536, byte_size(Body)).
