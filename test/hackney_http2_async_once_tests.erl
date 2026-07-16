%%% Tests for HTTP/2 {async, once} pull semantics.
%%%
%%% once mode used to be ignored for HTTP/2: every body frame and done were
%%% pushed eagerly, identical to {async, true}. It now follows the HTTP/1.1
%%% contract: status and headers are delivered eagerly, then each
%%% stream_next/1 delivers exactly one message (a body chunk or done). The
%%% stream runs h2 manual flow control, so undelivered bytes keep the peer's
%%% window closed: consume/3 releases them only as the consumer pulls, which
%%% the WINDOW_UPDATE assertions below observe on the wire.
-module(hackney_http2_async_once_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FRAME_COUNT, 3).
-define(BODY_SIZE, 30000).
%% repro_h2_raw_server:make_body/1 wraps the fill in {"d":"..."} and yields
%% body_size - 4 actual bytes.
-define(WIRE_SIZE, (?BODY_SIZE - 4)).

async_once_test_() ->
    [{"once: no body frame is pushed until stream_next/1",
      {timeout, 60, fun t_once_pull_pacing/0}},
     {"once: no stream WINDOW_UPDATE before the first pull (backpressure)",
      {timeout, 60, fun t_once_backpressure_wire/0}},
     {"async true still delivers eagerly",
      {timeout, 60, fun t_async_true_still_eager/0}},
     {"once: a pull before data arrives parks as credit",
      {timeout, 60, fun t_once_pull_before_data/0}},
     {"once: RST_STREAM surfaces as an error message",
      {timeout, 60, fun t_once_stream_reset/0}},
     {"once: connection teardown surfaces as an error message",
      {timeout, 60, fun t_once_conn_teardown/0}},
     {"legacy bare stream_next atom still routes",
      {timeout, 60, fun t_legacy_bare_stream_next/0}}].

%% Status and headers arrive eagerly; body chunks and done arrive strictly
%% one per stream_next/1, never unsolicited.
t_once_pull_pacing() ->
    {Srv, Ref} = start_and_request(once),
    try
        ok = recv_status_headers(Ref),
        %% Nothing may be pushed before the first pull.
        no_message_within(Ref, 300),
        Chunks = pull_all(Ref, []),
        ?assertEqual(?WIRE_SIZE, iolist_size(Chunks)),
        ?assert(length(Chunks) >= 2)
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% Manual flow control on the wire: the client must not open the stream
%% window before the consumer pulls, and must open it (consume/3) after.
t_once_backpressure_wire() ->
    {Srv, Ref} = start_and_request(once),
    try
        ok = recv_status_headers(Ref),
        no_message_within(Ref, 300),
        ?assertEqual([], drain_stream_window_updates(0)),
        _Chunks = pull_all(Ref, []),
        %% consume/3 acknowledged the delivered bytes: at least one
        %% stream-level WINDOW_UPDATE reaches the server.
        WUs = drain_stream_window_updates(2000),
        ?assert(length(WUs) >= 1),
        ?assertEqual(?WIRE_SIZE, lists:sum([Inc || {_Sid, Inc} <- WUs]))
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% {async, true} keeps the eager contract: all frames and done arrive with
%% no stream_next calls.
t_async_true_still_eager() ->
    {Srv, Ref} = start_and_request(true),
    try
        ok = recv_status_headers(Ref),
        Chunks = recv_eager(Ref, []),
        ?assertEqual(?WIRE_SIZE, iolist_size(Chunks))
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% A stream_next issued while the queue is empty parks as a credit: the next
%% DATA frame is delivered immediately, without another pull.
t_once_pull_before_data() ->
    {Srv, Ref} = start_and_request(once, #{
        body_size => ?BODY_SIZE,
        frame_count => 2,
        inter_frame_ms => 400,
        notify => self()
    }),
    try
        ok = recv_status_headers(Ref),
        %% First pull gets the first frame.
        ok = hackney:stream_next(Ref),
        Chunk1 = recv_chunk(Ref),
        %% Second pull arrives while the server still sleeps between frames:
        %% the pull parks and the late frame is delivered on arrival.
        ok = hackney:stream_next(Ref),
        Chunk2 = recv_chunk(Ref),
        ok = hackney:stream_next(Ref),
        done = recv_done_or_chunk(Ref, byte_size(Chunk1) + byte_size(Chunk2))
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% A stream reset by the peer mid-response surfaces as an error message.
t_once_stream_reset() ->
    {Srv, Ref} = start_and_request(once, #{rst_after_headers => cancel}),
    try
        ok = recv_status_headers(Ref),
        receive
            {hackney_response, Ref, {error, {stream_error, _}}} -> ok
        after 5000 ->
            erlang:error(no_stream_reset_error)
        end
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% Connection-wide teardown (GOAWAY + close) aborts an undrained once stream
%% with an error message.
t_once_conn_teardown() ->
    {Srv, Ref} = start_and_request(once, #{
        body_size => ?BODY_SIZE,
        frame_count => ?FRAME_COUNT,
        goaway_after => 1,
        goaway_close => true
    }),
    try
        ok = recv_status_headers(Ref),
        %% Do not pull: the queued frames keep the once stream alive until
        %% the GOAWAY/close arrives and aborts it.
        receive
            {hackney_response, Ref, {error, _}} -> ok
        after 5000 ->
            erlang:error(no_teardown_error)
        end
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%% The pre-upgrade bare stream_next atom (no caller pid) still pulls, and is
%% a no-op once no once-mode stream remains.
t_legacy_bare_stream_next() ->
    {Srv, Ref} = start_and_request(once, #{
        body_size => ?BODY_SIZE,
        frame_count => ?FRAME_COUNT,
        notify => self()
    }),
    try
        ok = recv_status_headers(Ref),
        ok = gen_statem:cast(Ref, stream_next),
        _Chunk = recv_chunk(Ref),
        Rest = pull_all(Ref, []),
        ?assert(iolist_size(Rest) > 0),
        %% Stream is done and removed: a stray legacy pull must not crash.
        ok = gen_statem:cast(Ref, stream_next),
        no_message_within(Ref, 150),
        ?assert(is_process_alive(Ref))
    after
        repro_h2_raw_server:stop(element(1, Srv))
    end.

%%====================================================================
%% Helpers
%%====================================================================

start_and_request(AsyncMode) ->
    start_and_request(AsyncMode, #{
        body_size => ?BODY_SIZE,
        frame_count => ?FRAME_COUNT,
        notify => self()
    }).

start_and_request(AsyncMode, Knobs) ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    %% eunit runs sibling tests in the same process: drop notify leftovers
    %% from a previous test's server.
    flush_server_notifications(),
    Srv = repro_h2_raw_server:start(Knobs),
    Port = repro_h2_raw_server:port(Srv),
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    Opts = [{async, AsyncMode}, {pool, false}, {protocols, [http2]},
            {recv_timeout, 15000},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    {ok, Ref} = hackney:request(get, Url, [], <<>>, Opts),
    {Srv, Ref}.

flush_server_notifications() ->
    receive
        {h2_raw_server, _, _, _} -> flush_server_notifications()
    after 0 ->
        ok
    end.

recv_status_headers(Ref) ->
    receive
        {hackney_response, Ref, {status, 200, _}} ->
            receive
                {hackney_response, Ref, {headers, _}} -> ok
            after 5000 -> erlang:error(no_headers)
            end
    after 5000 ->
        erlang:error(no_status)
    end.

no_message_within(Ref, Ms) ->
    receive
        {hackney_response, Ref, Msg} ->
            erlang:error({unsolicited_message, Msg})
    after Ms ->
        ok
    end.

%% One stream_next = exactly one message. After a chunk, nothing else may
%% arrive until the next pull.
pull_all(Ref, Acc) ->
    ok = hackney:stream_next(Ref),
    receive
        {hackney_response, Ref, done} ->
            lists:reverse(Acc);
        {hackney_response, Ref, Chunk} when is_binary(Chunk) ->
            no_message_within(Ref, 150),
            pull_all(Ref, [Chunk | Acc])
    after 5000 ->
        erlang:error({no_message_after_stream_next, length(Acc)})
    end.

recv_chunk(Ref) ->
    receive
        {hackney_response, Ref, Chunk} when is_binary(Chunk) -> Chunk
    after 5000 ->
        erlang:error(no_chunk)
    end.

%% The tail of a 2-frame body: either done directly (both frames already
%% pulled) or the last small chunk then done on one more pull.
recv_done_or_chunk(Ref, GotSoFar) ->
    receive
        {hackney_response, Ref, done} ->
            ?assertEqual(?WIRE_SIZE, GotSoFar),
            done;
        {hackney_response, Ref, Chunk} when is_binary(Chunk) ->
            ok = hackney:stream_next(Ref),
            recv_done_or_chunk(Ref, GotSoFar + byte_size(Chunk))
    after 5000 ->
        erlang:error(no_tail_message)
    end.

recv_eager(Ref, Acc) ->
    receive
        {hackney_response, Ref, done} ->
            lists:reverse(Acc);
        {hackney_response, Ref, Chunk} when is_binary(Chunk) ->
            recv_eager(Ref, [Chunk | Acc])
    after 5000 ->
        erlang:error(eager_delivery_stalled)
    end.

%% Collect {h2_raw_server, window_update, ...} for non-zero (stream-level)
%% stream ids. Connection-level (stream 0) updates stay in auto mode and are
%% ignored.
drain_stream_window_updates(TimeoutMs) ->
    receive
        {h2_raw_server, window_update, 0, _} ->
            drain_stream_window_updates(TimeoutMs);
        {h2_raw_server, window_update, StreamId, Inc} ->
            [{StreamId, Inc} | drain_stream_window_updates(TimeoutMs)]
    after TimeoutMs ->
        []
    end.
