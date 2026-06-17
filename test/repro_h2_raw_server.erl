%%% Raw, frame-level HTTP/2 TLS server for reproducing ALB-specific behaviour
%%% the high-level h2-dep server cannot emit: a SETTINGS / PING / WINDOW_UPDATE
%%% frame injected after (or mid) a response, exact framing, etc.
%%%
%%% Uses the h2 dep's h2_frame (encode/decode) and h2_hpack (encode) so we only
%%% drive the wire, not reimplement HPACK. Request header blocks are ignored
%%% (we never decode them) - we just respond on the same stream id.
%%%
%%% Knobs (map):
%%%   body_size      :: integer()   (default 14000)
%%%   frame_count    :: integer()   (default 1)   split body into N DATA frames
%%%   inter_frame_ms :: integer()   (default 0)
%%%   server_settings:: [{atom(),int()}] sent in the initial SETTINGS
%%%   quirk          :: none | settings | {settings, [{atom(),int()}]}
%%%                   | ping | window_update
%%%   quirk_when     :: after_each | after_first | mid_first | mid_each
-module(repro_h2_raw_server).

-export([start/1, stop/1, port/1]).

-define(PREFACE, <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>).

start(Knobs) ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {alpn_preferred_protocols, [<<"h2">>]},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn_link(fun() -> accept_loop(LSock, Knobs) end),
    {Pid, Port}.

stop(Pid) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ok.

port({_Pid, Port}) -> Port.

accept_loop(LSock, Knobs) ->
    case ssl:transport_accept(LSock, 5000) of
        {ok, TSock} ->
            Parent = self(),
            spawn(fun() -> handshake(Parent, TSock, Knobs) end),
            accept_loop(LSock, Knobs);
        {error, timeout} ->
            accept_loop(LSock, Knobs);
        {error, closed} ->
            ok
    end.

handshake(_Parent, TSock, Knobs) ->
    case ssl:handshake(TSock, 5000) of
        {ok, Sock} ->
            %% Read the 24-byte client connection preface, then frames.
            case recv_preface(Sock, <<>>) of
                {ok, Rest} ->
                    %% Send our initial SETTINGS immediately.
                    ServerSettings = maps:get(server_settings, Knobs, []),
                    send(Sock, h2_frame:settings(ServerSettings)),
                    EncCtx = h2_hpack:new_context(),
                    St = #{enc => EncCtx, count => 0, goaway => false},
                    conn_loop(Sock, Rest, St, Knobs);
                {error, _} -> ok
            end;
        {error, _} -> ok
    end.

recv_preface(Sock, Acc) when byte_size(Acc) >= 24 ->
    <<Pre:24/binary, Rest/binary>> = Acc,
    case Pre of
        ?PREFACE -> {ok, Rest};
        _        -> {error, bad_preface}
    end;
recv_preface(Sock, Acc) ->
    case ssl:recv(Sock, 0, 5000) of
        {ok, Data} -> recv_preface(Sock, <<Acc/binary, Data/binary>>);
        {error, R} -> {error, R}
    end.

conn_loop(Sock, Buf, St, Knobs) ->
    case h2_frame:decode(Buf) of
        {ok, Frame, Rest} ->
            case handle_frame(Sock, Frame, St, Knobs) of
                {continue, St2} ->
                    conn_loop(Sock, Rest, St2, Knobs);
                stop -> ok
            end;
        {more, _} ->
            case ssl:recv(Sock, 0, 60000) of
                {ok, Data} -> conn_loop(Sock, <<Buf/binary, Data/binary>>, St, Knobs);
                {error, _} -> ok
            end;
        {error, _, Rest} ->
            conn_loop(Sock, Rest, St, Knobs);
        {error, _} ->
            ok
    end.

handle_frame(Sock, {settings, _List}, St, _Knobs) ->
    send(Sock, h2_frame:settings_ack()),
    {continue, St};
handle_frame(_Sock, {settings_ack}, St, _Knobs) ->
    {continue, St};
handle_frame(Sock, {ping, Data}, St, _Knobs) ->
    send(Sock, h2_frame:ping_ack(Data)),
    {continue, St};
handle_frame(_Sock, {ping_ack, _}, St, _Knobs) ->
    {continue, St};
handle_frame(_Sock, {window_update, _, _}, St, _Knobs) ->
    {continue, St};
handle_frame(_Sock, {goaway, _, _, _}, _St, _Knobs) ->
    stop;
handle_frame(_Sock, {rst_stream, _, _}, St, _Knobs) ->
    {continue, St};
%% After we have sent GOAWAY, ignore new streams (the ALB stops answering
%% streams past last_stream_id). The client's read then hangs to recv_timeout.
handle_frame(_Sock, {headers, _StreamId, _B, _E, _H}, #{goaway := true} = St, _Knobs) ->
    {continue, St};
handle_frame(Sock, {headers, StreamId, _Block, _EndStream, _EndHeaders}, St, Knobs) ->
    #{enc := EncCtx, count := Count} = St,
    Count2 = Count + 1,
    EncCtx2 = respond(Sock, StreamId, EncCtx, Count2, Knobs),
    St2 = St#{enc := EncCtx2, count := Count2},
    maybe_goaway(Sock, StreamId, Count2, St2, Knobs);
handle_frame(_Sock, _Other, St, _Knobs) ->
    {continue, St}.

%% Recycle the connection like an ALB: after N responses, send GOAWAY with
%% last_stream_id = the stream we just answered, then either keep the socket
%% open (the dangerous case: hackney may keep reusing a doomed conn) or close it.
maybe_goaway(Sock, LastStreamId, Count, St, Knobs) ->
    case maps:get(goaway_after, Knobs, infinity) of
        N when is_integer(N), Count >= N ->
            send(Sock, h2_frame:goaway(LastStreamId, no_error, <<>>)),
            case maps:get(goaway_close, Knobs, false) of
                true  -> timer:sleep(50), ssl:close(Sock), stop;
                false -> {continue, St#{goaway := true}}
            end;
        _ ->
            {continue, St}
    end.

respond(Sock, StreamId, EncCtx, ReqCount, Knobs) ->
    BodySize   = maps:get(body_size, Knobs, 14000),
    FrameCount = maps:get(frame_count, Knobs, 1),
    DelayMs    = maps:get(inter_frame_ms, Knobs, 0),
    Body = make_body(BodySize),
    %% 200, application/json, NO content-length.
    {HBlock, EncCtx2} = h2_hpack:encode(
        [{<<":status">>, <<"200">>},
         {<<"content-type">>, <<"application/json">>}], EncCtx),
    send(Sock, h2_frame:headers(StreamId, HBlock, false)),
    maybe_quirk(Sock, mid, ReqCount, Knobs),
    send_body(Sock, StreamId, Body, FrameCount, DelayMs),
    maybe_quirk(Sock, after_resp, ReqCount, Knobs),
    EncCtx2.

send_body(Sock, StreamId, Body, FrameCount, DelayMs) ->
    Chunks = chunkify(Body, FrameCount),
    send_chunks(Sock, StreamId, Chunks, DelayMs).

send_chunks(Sock, StreamId, [Last], _DelayMs) ->
    send(Sock, h2_frame:data(StreamId, Last, true));
send_chunks(Sock, StreamId, [C | Rest], DelayMs) ->
    send(Sock, h2_frame:data(StreamId, C, false)),
    case DelayMs of 0 -> ok; _ -> timer:sleep(DelayMs) end,
    send_chunks(Sock, StreamId, Rest, DelayMs).

%% Inject the quirk frame at the requested point.
maybe_quirk(Sock, Point, ReqCount, Knobs) ->
    Quirk = maps:get(quirk, Knobs, none),
    When  = maps:get(quirk_when, Knobs, after_each),
    Active = case {Point, When} of
        {after_resp, after_each}  -> true;
        {after_resp, after_first} -> ReqCount =:= 1;
        {mid, mid_each}           -> true;
        {mid, mid_first}          -> ReqCount =:= 1;
        _ -> false
    end,
    case Active of
        false -> ok;
        true  -> emit_quirk(Sock, Quirk)
    end.

emit_quirk(_Sock, none) -> ok;
emit_quirk(Sock, settings) -> send(Sock, h2_frame:settings([]));
emit_quirk(Sock, {settings, L}) -> send(Sock, h2_frame:settings(L));
emit_quirk(Sock, ping) -> send(Sock, h2_frame:ping(<<0,0,0,0,0,0,0,7>>));
emit_quirk(Sock, window_update) -> send(Sock, h2_frame:window_update(0, 1000)).

send(Sock, FrameData) ->
    ssl:send(Sock, h2_frame:encode(FrameData)).

chunkify(Body, 1) -> [Body];
chunkify(Body, N) when N > 1 ->
    Size = max(1, (byte_size(Body) + N - 1) div N),
    chunkify_1(Body, Size).

chunkify_1(Body, Size) when byte_size(Body) =< Size -> [Body];
chunkify_1(Body, Size) ->
    <<C:Size/binary, Rest/binary>> = Body,
    [C | chunkify_1(Rest, Size)].

make_body(Size) when Size >= 12 ->
    Fill = binary:copy(<<"x">>, Size - 12),
    <<"{\"d\":\"", Fill/binary, "\"}">>;
make_body(Size) ->
    binary:copy(<<"x">>, Size).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
