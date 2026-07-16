%%% Tests for HTTP/2 request-body sends under flow control.
%%%
%%% Request bodies larger than the peer's flow-control window used to fail
%%% with {error, send_buffer_full}: hackney called the non-blocking
%%% h2_connection:send_data/4, which caps buffering at 1 MB and never waits
%%% for WINDOW_UPDATE. hackney now uses send_data/5 with #{block => Timeout}
%%% (the send_timeout option), parking the caller until the window opens.
%%%
%%% Uses two servers:
%%% - repro_h2_raw_server: frame-level server with a small initial window and
%%%   a window_update knob, so WINDOW_UPDATE emission is delayed, split, or
%%%   withheld entirely.
%%% - the h2 dep's high-level echo server, to assert byte-for-byte delivery of
%%%   a body larger than the old 1 MB non-blocking buffer cap.
-module(hackney_http2_flow_control_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SMALL_WINDOW, 16384).

%%====================================================================
%% Fixtures
%%====================================================================

raw_server_test_() ->
    [{"large body completes across delayed split WINDOW_UPDATEs",
      {timeout, 120, fun t_large_body_completes/0}},
     {"streamed chunks complete across delayed split WINDOW_UPDATEs",
      {timeout, 120, fun t_streaming_large_small_window/0}},
     {"window never opens: bounded {error, timeout}, no hang",
      {timeout, 60, fun t_window_never_opens_times_out/0}},
     {"send_timeout nonblock keeps the old fail-fast behavior",
      {timeout, 60, fun t_nonblock_opt_out/0}}].

echo_server_test_() ->
    {setup,
     fun setup_echo/0,
     fun cleanup_echo/1,
     fun(Ctx) ->
         [{"body over the 1 MB non-blocking cap is delivered intact",
           {timeout, 120, fun() -> t_body_over_buffer_cap_echoed(Ctx) end}}]
     end}.

%%====================================================================
%% Raw-server tests (controlled flow control)
%%====================================================================

%% The server advertises a 16 KB stream window and never consumes more than
%% one WINDOW_UPDATE increment at a time, 5 ms apart, so a 300 KB upload is
%% forced through many park/flush cycles on both the stream and connection
%% windows.
t_large_body_completes() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, ?SMALL_WINDOW}],
        window_update => {auto, ?SMALL_WINDOW, 5},
        body_size => 128
    }),
    Body = binary:copy(<<"x">>, 300 * 1024),
    try
        Res = hackney:request(post, url(Srv, <<"/">>), [], Body, opts([])),
        ?assertMatch({ok, 200, _, _}, Res)
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% Same constrained window, driven through the streaming API
%% (send_body/finish_send_body) instead of a one-shot body.
t_streaming_large_small_window() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, ?SMALL_WINDOW}],
        window_update => {auto, 8192, 2},
        body_size => 128
    }),
    Body = binary:copy(<<"y">>, 300 * 1024),
    try
        {ok, ConnPid} = hackney:request(post, url(Srv, <<"/">>), [], stream, opts([])),
        ok = send_in_chunks(ConnPid, Body, 32768),
        ok = hackney:finish_send_body(ConnPid),
        {ok, 200, _RespHeaders, ConnPid} = hackney:start_response(ConnPid),
        {ok, _RespBody} = hackney:body(ConnPid)
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% A server that never sends WINDOW_UPDATE must yield {error, timeout} after
%% send_timeout, not a hang and not send_buffer_full.
t_window_never_opens_times_out() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, 1024}],
        window_update => none,
        body_size => 128
    }),
    Body = binary:copy(<<"z">>, 200 * 1024),
    try
        T0 = erlang:monotonic_time(millisecond),
        Res = hackney:request(post, url(Srv, <<"/">>), [], Body,
                              opts([{send_timeout, 500}])),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assertEqual({error, timeout}, Res),
        ?assert(Elapsed < 5000)
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% {send_timeout, nonblock} restores the historical non-blocking send: a body
%% past the h2 per-stream buffer cap fails fast with send_buffer_full.
t_nonblock_opt_out() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, 1024}],
        window_update => none,
        body_size => 128
    }),
    Body = binary:copy(<<"w">>, 1536 * 1024),
    try
        Res = hackney:request(post, url(Srv, <<"/">>), [], Body,
                              opts([{send_timeout, nonblock}])),
        ?assertEqual({error, send_buffer_full}, Res)
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%%====================================================================
%% Echo-server test (well-behaved peer, body over the old cap)
%%====================================================================

setup_echo() ->
    start_apps(),
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

cleanup_echo(#{server := Server}) ->
    try h2:stop_server(Server) catch _:_ -> ok end,
    ok.

%% Reply with size + SHA-256 of the request body rather than echoing it: the
%% server-side h2:send_data is itself non-blocking and would hit its own 1 MB
%% cap on a 1.5 MB response. The digest still proves byte-for-byte delivery.
server_handler(Conn, Sid, _Method, _Path, _Headers) ->
    ok = h2:set_stream_handler(Conn, Sid, self()),
    Body = recv_request_body(Conn, Sid, <<>>),
    Digest = binary:encode_hex(crypto:hash(sha256, Body)),
    Size = integer_to_binary(byte_size(Body)),
    ok = h2:send_response(Conn, Sid, 200,
                          [{<<"content-type">>, <<"text/plain">>}]),
    ok = h2:send_data(Conn, Sid, <<Size/binary, ":", Digest/binary>>, true).

recv_request_body(Conn, Sid, Acc) ->
    receive
        {h2, Conn, {data, Sid, Data, true}} ->
            <<Acc/binary, Data/binary>>;
        {h2, Conn, {data, Sid, Data, false}} ->
            recv_request_body(Conn, Sid, <<Acc/binary, Data/binary>>)
    after 30000 ->
        Acc
    end.

%% 1.5 MB in a single one-shot body: over the h2 non-blocking 1 MB buffer cap,
%% so the old path failed with send_buffer_full even against a well-behaved
%% server. The blocking send must deliver it byte-for-byte.
t_body_over_buffer_cap_echoed(#{port := Port}) ->
    Payload = crypto:strong_rand_bytes(1536 * 1024),
    Expected = <<(integer_to_binary(byte_size(Payload)))/binary, ":",
                 (binary:encode_hex(crypto:hash(sha256, Payload)))/binary>>,
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/echo">>]),
    {ok, 200, _RespHeaders, RespBody} =
        hackney:request(post, Url, [], Payload, opts([{recv_timeout, 30000}])),
    ?assertEqual(Expected, RespBody).

%%====================================================================
%% Helpers
%%====================================================================

start_apps() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    ok.

opts(Extra) ->
    Extra ++
    [{pool, false},
     {protocols, [http2]},
     {recv_timeout, 15000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

url(Srv, Path) ->
    Port = repro_h2_raw_server:port(Srv),
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), Path]).

send_in_chunks(_ConnPid, <<>>, _Size) ->
    ok;
send_in_chunks(ConnPid, Bin, Size) when byte_size(Bin) =< Size ->
    hackney:send_body(ConnPid, Bin);
send_in_chunks(ConnPid, Bin, Size) ->
    <<Chunk:Size/binary, Rest/binary>> = Bin,
    ok = hackney:send_body(ConnPid, Chunk),
    send_in_chunks(ConnPid, Rest, Size).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
