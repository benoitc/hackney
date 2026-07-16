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
     {"body-producer funs complete across delayed split WINDOW_UPDATEs",
      {timeout, 120, fun t_streaming_fun_bodies/0}},
     {"streamed send hits send_timeout when the window never opens",
      {timeout, 60, fun t_streaming_send_times_out/0}},
     {"window never opens: bounded {error, timeout}, no hang",
      {timeout, 60, fun t_window_never_opens_times_out/0}},
     {"async request send hits send_timeout when the window never opens",
      {timeout, 60, fun t_async_send_times_out/0}},
     {"send_timeout nonblock keeps the old fail-fast behavior",
      {timeout, 60, fun t_nonblock_opt_out/0}},
     {"timed-out send RST_STREAMs the abandoned stream",
      {timeout, 60, fun t_timeout_cancels_stream/0}},
     {"per-request send_timeout does not leak into later pooled requests",
      {timeout, 60, fun t_send_timeout_not_sticky/0}},
     {"streamed request send_timeout applies on a reused pooled conn",
      {timeout, 60, fun t_streamed_send_timeout_reused_conn/0}},
     {"direct hackney_conn async API uses the connection default send_timeout",
      {timeout, 60, fun t_async_direct_conn_api/0}},
     {"manual hackney:connect honors {send_timeout, T}",
      {timeout, 60, fun t_manual_connect_send_timeout/0}}].

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

%% Same constrained window, driven through the body-producer fun forms of
%% send_body/2: an arity-0 fun and a {fun/1, State} pair, each emitting more
%% than the window in total.
t_streaming_fun_bodies() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, ?SMALL_WINDOW}],
        window_update => {auto, ?SMALL_WINDOW, 2},
        body_size => 128
    }),
    Chunk = binary:copy(<<"f">>, 32768),
    C = counters:new(1, []),
    Fun0 = fun() ->
        case counters:get(C, 1) of
            N when N < 4 -> counters:add(C, 1, 1), {ok, Chunk};
            _ -> eof
        end
    end,
    Fun1 = {fun(N) when N < 4 -> {ok, Chunk, N + 1};
               (_) -> eof
            end, 0},
    try
        {ok, ConnPid} = hackney:request(post, url(Srv, <<"/">>), [], stream, opts([])),
        ok = hackney:send_body(ConnPid, Fun0),
        ok = hackney:send_body(ConnPid, Fun1),
        ok = hackney:finish_send_body(ConnPid),
        {ok, 200, _RespHeaders, ConnPid} = hackney:start_response(ConnPid),
        {ok, _RespBody} = hackney:body(ConnPid)
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% Streaming picks send_timeout up from the request options at connect time
%% (there is no per-chunk option): a chunk larger than a never-opening window
%% must fail with {error, timeout}, closing the connection.
t_streaming_send_times_out() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, 1024}],
        window_update => none,
        body_size => 128
    }),
    Chunk = binary:copy(<<"t">>, 200 * 1024),
    try
        {ok, ConnPid} = hackney:request(post, url(Srv, <<"/">>), [], stream,
                                        opts([{send_timeout, 500}])),
        T0 = erlang:monotonic_time(millisecond),
        Res = hackney:send_body(ConnPid, Chunk),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assertEqual({error, timeout}, Res),
        ?assert(Elapsed < 5000)
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

%% The async request path sends the body before returning the ref, so a
%% never-opening window surfaces {error, timeout} from the request call.
t_async_send_times_out() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, 1024}],
        window_update => none,
        body_size => 128
    }),
    Body = binary:copy(<<"a">>, 200 * 1024),
    try
        Res = hackney:request(post, url(Srv, <<"/">>), [], Body,
                              opts([async, {send_timeout, 500}])),
        ?assertEqual({error, timeout}, Res)
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

%% A send that times out must not leave the half-sent stream (and its
%% buffered body) behind on the shared connection: the client has to
%% RST_STREAM it. The raw server reports received RST_STREAM frames.
t_timeout_cancels_stream() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, 1024}],
        window_update => none,
        notify => self(),
        body_size => 128
    }),
    Body = binary:copy(<<"c">>, 200 * 1024),
    try
        {error, timeout} = hackney:request(post, url(Srv, <<"/">>), [], Body,
                                           opts([{send_timeout, 300}])),
        receive
            {h2_raw_server, rst_stream, StreamId, _Code} ->
                ?assert(StreamId > 0)
        after 2000 ->
            erlang:error(no_rst_stream_after_send_timeout)
        end
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% A {send_timeout, 25} request must not change the timeout of the next
%% request on the same pooled connection: the follow-up uses the default and
%% completes even though the window drains slower than 25 ms.
t_send_timeout_not_sticky() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, ?SMALL_WINDOW}],
        window_update => {auto, ?SMALL_WINDOW, 30},
        body_size => 128
    }),
    Pool = h2_fc_sticky_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    Body = binary:copy(<<"s">>, 150 * 1024),
    try
        {error, timeout} = hackney:request(post, url(Srv, <<"/">>), [], Body,
                                           pool_opts(Pool, [{send_timeout, 25}])),
        Res = hackney:request(post, url(Srv, <<"/">>), [], Body, pool_opts(Pool, [])),
        ?assertMatch({ok, 200, _, _}, Res)
    after
        catch hackney_pool:stop_pool(Pool),
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% A streamed request must apply its own send_timeout even when it reuses a
%% pooled connection opened by an earlier request without one.
t_streamed_send_timeout_reused_conn() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, 1024}],
        window_update => none,
        body_size => 128
    }),
    Pool = h2_fc_stream_reuse_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    Chunk = binary:copy(<<"r">>, 200 * 1024),
    try
        %% Bodyless request establishes the pooled conn with the default
        %% send_timeout.
        {ok, 200, _, _} = hackney:request(get, url(Srv, <<"/">>), [], <<>>,
                                          pool_opts(Pool, [])),
        {ok, ConnPid} = hackney:request(post, url(Srv, <<"/">>), [], stream,
                                        pool_opts(Pool, [{send_timeout, 25}])),
        T0 = erlang:monotonic_time(millisecond),
        Res = hackney:send_body(ConnPid, Chunk),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assertEqual({error, timeout}, Res),
        ?assert(Elapsed < 1500)
    after
        catch hackney_pool:stop_pool(Pool),
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% The hackney_conn request_async arities without ReqOpts (and the legacy
%% message form without FollowRedirect) fall back to the connection default:
%% a body larger than the window still completes.
t_async_direct_conn_api() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, ?SMALL_WINDOW}],
        window_update => {auto, ?SMALL_WINDOW, 5},
        body_size => 128
    }),
    Body = binary:copy(<<"d">>, 150 * 1024),
    {ok, ConnPid} = hackney_conn_sup:start_conn(#{
        host => "localhost",
        port => repro_h2_raw_server:port(Srv),
        transport => hackney_ssl,
        connect_options => [{protocols, [http2]}],
        ssl_options => [{insecure, true}, {verify, verify_none}]
    }),
    try
        ok = hackney_conn:connect(ConnPid),
        {ok, Ref1} = hackney_conn:request_async(ConnPid, <<"POST">>, <<"/">>, [],
                                                Body, true),
        ok = await_async_status(Ref1, 200),
        %% Legacy message form without the FollowRedirect element.
        {ok, Ref2} = gen_statem:call(ConnPid, {request_async, <<"POST">>, <<"/">>,
                                               [], Body, true, self()}),
        ok = await_async_status(Ref2, 200)
    after
        catch hackney_conn:stop(ConnPid),
        repro_h2_raw_server:stop(element(1, Srv))
    end.

await_async_status(Ref, Status) ->
    receive
        {hackney_response, Ref, {status, Status, _}} -> drain_async(Ref);
        {hackney_response, Ref, {error, Reason}} -> {error, Reason}
    after 15000 ->
        {error, no_async_status}
    end.

drain_async(Ref) ->
    receive
        {hackney_response, Ref, done} -> ok;
        {hackney_response, Ref, {error, Reason}} -> {error, Reason};
        {hackney_response, Ref, _} -> drain_async(Ref)
    after 15000 ->
        {error, no_async_done}
    end.

%% hackney:connect/4 + hackney:send_request/2 has no per-request options
%% channel: the connection-level send_timeout from the connect options must
%% apply (single-owner connection, unlike pooled ones).
t_manual_connect_send_timeout() ->
    start_apps(),
    Srv = repro_h2_raw_server:start(#{
        server_settings => [{initial_window_size, 1024}],
        window_update => none,
        body_size => 128
    }),
    Body = binary:copy(<<"m">>, 200 * 1024),
    %% pool false: hackney:connect defaults to the default pool, but the
    %% conn-level send_timeout seeding is for single-owner direct conns.
    {ok, ConnPid} = hackney:connect(hackney_ssl, "localhost",
                                    repro_h2_raw_server:port(Srv),
                                    [{send_timeout, 25}, {pool, false}, {protocols, [http2]},
                                     {ssl_options, [{insecure, true}, {verify, verify_none}]}]),
    try
        T0 = erlang:monotonic_time(millisecond),
        Res = hackney:send_request(ConnPid, {post, <<"/">>, [], Body}),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assertEqual({error, timeout}, Res),
        ?assert(Elapsed < 1500)
    after
        catch hackney:close(ConnPid),
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

pool_opts(Pool, Extra) ->
    Extra ++
    [{pool, Pool},
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
