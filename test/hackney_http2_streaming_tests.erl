%%% Tests for HTTP/2 streaming request bodies and streaming response reads.
%%%
%%% Brings HTTP/2 to parity with HTTP/1.1 and HTTP/3 on the streaming API:
%%% body = stream -> send_body/2 -> finish_send_body/1 -> start_response/1 ->
%%% body/1 or stream_body/1. The one-shot request/response path is unchanged
%%% and is exercised here too as a regression guard.
-module(hackney_http2_streaming_tests).

-include_lib("eunit/include/eunit.hrl").

%% Resolve the test cert dir from the module's beam location so paths work
%% regardless of where eunit is run from.
cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

%%====================================================================
%% Fixture
%%====================================================================

http2_streaming_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Ctx) ->
         [{"streamed request body received intact",
           {timeout, 30, fun() -> t_stream_request_body(Ctx) end}},
          {"empty streamed request body",
           {timeout, 30, fun() -> t_empty_stream_body(Ctx) end}},
          {"streamed response read via stream_body/1",
           {timeout, 30, fun() -> t_stream_response(Ctx) end}},
          {"one-shot request/response still works",
           {timeout, 30, fun() -> t_oneshot_regression(Ctx) end}}]
     end}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Certs = cert_dir(),
    Handler = fun server_handler/5,
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => Handler,
        settings => #{max_concurrent_streams => unlimited}
    }),
    Port = h2:server_port(Server),
    #{server => Server, port => Port}.

cleanup(#{server := Server}) ->
    try h2:stop_server(Server) catch _:_ -> ok end,
    ok.

%%====================================================================
%% Server handler
%%====================================================================

%% /echo  : reassemble the request body and echo it back in one DATA frame.
%% /chunks: send a fixed multi-chunk response.
%% other  : plain 200 "ok".
server_handler(Conn, Sid, _Method, Path, _Headers) ->
    case Path of
        <<"/echo">> ->
            %% Take ownership of this stream's events so the request-body DATA
            %% frames (buffered until now) replay to us; reassemble and echo.
            ok = h2:set_stream_handler(Conn, Sid, self()),
            Body = recv_request_body(Conn, Sid, <<>>),
            ok = h2:send_response(Conn, Sid, 200,
                                  [{<<"content-type">>, <<"application/octet-stream">>}]),
            ok = h2:send_data(Conn, Sid, Body, true);
        <<"/chunks">> ->
            ok = h2:send_response(Conn, Sid, 200,
                                  [{<<"content-type">>, <<"text/plain">>}]),
            ok = h2:send_data(Conn, Sid, <<"aaaa">>, false),
            ok = h2:send_data(Conn, Sid, <<"bbbb">>, false),
            ok = h2:send_data(Conn, Sid, <<"cccc">>, true);
        _ ->
            ok = h2:send_response(Conn, Sid, 200,
                                  [{<<"content-type">>, <<"text/plain">>}]),
            ok = h2:send_data(Conn, Sid, <<"ok">>, true)
    end.

recv_request_body(Conn, Sid, Acc) ->
    receive
        {h2, Conn, {data, Sid, Data, true}} ->
            <<Acc/binary, Data/binary>>;
        {h2, Conn, {data, Sid, Data, false}} ->
            recv_request_body(Conn, Sid, <<Acc/binary, Data/binary>>)
    after 10000 ->
        Acc
    end.

%%====================================================================
%% Tests
%%====================================================================

%% Stream a body larger than the default flow-control window so the blocking
%% send path is exercised, then assert the server echoed it back byte-for-byte.
t_stream_request_body(#{port := Port}) ->
    Payload = crypto:strong_rand_bytes(150000),
    {ok, ConnPid} = hackney:request(post, url(Port, <<"/echo">>), [], stream, opts()),
    ok = send_in_chunks(ConnPid, Payload, 16384),
    ok = hackney:finish_send_body(ConnPid),
    {ok, 200, _RespHeaders, ConnPid} = hackney:start_response(ConnPid),
    {ok, Echoed} = hackney:body(ConnPid),
    ?assertEqual(Payload, Echoed).

t_empty_stream_body(#{port := Port}) ->
    {ok, ConnPid} = hackney:request(post, url(Port, <<"/echo">>), [], stream, opts()),
    ok = hackney:finish_send_body(ConnPid),
    {ok, 200, _RespHeaders, ConnPid} = hackney:start_response(ConnPid),
    {ok, Echoed} = hackney:body(ConnPid),
    ?assertEqual(<<>>, Echoed).

%% Drive the response pull-read API: stream_body/1 until done.
t_stream_response(#{port := Port}) ->
    {ok, ConnPid} = hackney:request(get, url(Port, <<"/chunks">>), [], stream, opts()),
    ok = hackney:finish_send_body(ConnPid),
    {ok, 200, _RespHeaders, ConnPid} = hackney:start_response(ConnPid),
    Body = drain(ConnPid, <<>>),
    ?assertEqual(<<"aaaabbbbcccc">>, Body).

t_oneshot_regression(#{port := Port}) ->
    {ok, 200, _RespHeaders, Body} =
        hackney:request(get, url(Port, <<"/">>), [], <<>>, opts()),
    ?assertEqual(<<"ok">>, Body).

%%====================================================================
%% Helpers
%%====================================================================

opts() ->
    [{pool, false},
     {protocols, [http2]},
     {recv_timeout, 10000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

url(Port, Path) ->
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), Path]).

send_in_chunks(_ConnPid, <<>>, _Size) ->
    ok;
send_in_chunks(ConnPid, Bin, Size) when byte_size(Bin) =< Size ->
    hackney:send_body(ConnPid, Bin);
send_in_chunks(ConnPid, Bin, Size) ->
    <<Chunk:Size/binary, Rest/binary>> = Bin,
    ok = hackney:send_body(ConnPid, Chunk),
    send_in_chunks(ConnPid, Rest, Size).

drain(ConnPid, Acc) ->
    case hackney:stream_body(ConnPid) of
        {ok, Data} -> drain(ConnPid, <<Acc/binary, Data/binary>>);
        done -> Acc
    end.
