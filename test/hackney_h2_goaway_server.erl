%%% Frame-level HTTP/2 server for the GOAWAY tests, built on the h2 dep's
%%% h2_frame / h2_hpack.
%%%
%%% Its first connection holds every request until it has sent its one GOAWAY,
%%% then answers only the streams at or below last_stream_id, a request with a
%%% streamed body once that body is complete. Later connections answer every
%%% request as soon as it is complete. Each response body is the stream id.
%%%
%%% The GOAWAY is sent on a trigger:
%%%   {second_stream, Pick}  when the second stream opens, with
%%%                          Pick(FirstStreamId, SecondStreamId) as last_stream_id
%%%   first_data             when a stream's first body chunk arrives, with that
%%%                          stream as last_stream_id
%%% and shaped by options:
%%%   goaway => direct | two_step   two_step first sends GOAWAY(2^31-1), the
%%%                                 graceful shutdown of RFC 9113 6.8 (default direct)
%%%   answer => boolean()           false answers nothing after the GOAWAY, not
%%%                                 even the accepted streams (default true)
%%%   notify => pid()               gets {goaway_server, rst_stream, StreamId} for
%%%                                 each RST_STREAM on the first connection, then
%%%                                 {goaway_server, done} when that connection ends
-module(hackney_h2_goaway_server).

-export([start/1, start/2, stop/1, rst_streams/0]).

-define(PREFACE, <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>).

start(Trigger) ->
    start(Trigger, #{}).

start(Trigger, Opts) ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {alpn_preferred_protocols, [<<"h2">>]},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Options = maps:merge(#{goaway => direct, answer => true}, Opts),
    Pid = spawn(fun() -> accept_loop(LSock, Trigger, Options) end),
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    {Pid, Url}.

stop(Pid) ->
    exit(Pid, kill).

accept_loop(LSock, Trigger, Opts) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() -> serve(TSock, Trigger, Opts) end),
            accept_loop(LSock, none, maps:remove(notify, Opts));
        {error, timeout} -> accept_loop(LSock, Trigger, Opts);
        {error, closed} -> ok
    end.

serve(TSock, Trigger, Opts) ->
    serve_conn(TSock, Trigger, Opts),
    notify(done, Opts).

%% @doc The stream ids the client reset on the first connection, in order,
%% once that connection has ended. Use with the notify option.
rst_streams() ->
    rst_streams([]).

rst_streams(Acc) ->
    receive
        {goaway_server, rst_stream, StreamId} -> rst_streams([StreamId | Acc]);
        {goaway_server, done} -> lists:reverse(Acc)
    after 10000 -> {timeout, lists:reverse(Acc)}
    end.

notify(Msg, #{notify := Pid}) -> Pid ! {goaway_server, Msg};
notify(_Msg, _Opts) -> ok.

serve_conn(TSock, Trigger, Opts) ->
    case ssl:handshake(TSock, 5000) of
        {ok, Sock} ->
            case recv_preface(Sock, <<>>) of
                {ok, Rest} ->
                    send(Sock, h2_frame:settings([])),
                    %% seen: stream ids in order of arrival. complete: streams
                    %% whose request has fully arrived and is not answered yet.
                    %% accepted: after the GOAWAY, the streams it let through.
                    loop(Sock, Rest, Opts#{enc => h2_hpack:new_context(), trigger => Trigger,
                                           seen => [], complete => [], accepted => all});
                _ -> ok
            end;
        _ -> ok
    end.

recv_preface(_Sock, Acc) when byte_size(Acc) >= 24 ->
    <<Pre:24/binary, Rest/binary>> = Acc,
    case Pre of ?PREFACE -> {ok, Rest}; _ -> {error, bad_preface} end;
recv_preface(Sock, Acc) ->
    case ssl:recv(Sock, 0, 5000) of
        {ok, Data} -> recv_preface(Sock, <<Acc/binary, Data/binary>>);
        {error, R} -> {error, R}
    end.

loop(Sock, Buf, St) ->
    case h2_frame:decode(Buf) of
        {ok, Frame, Rest} ->
            case handle(Sock, Frame, St) of
                {continue, St2} -> loop(Sock, Rest, St2);
                stop -> ok
            end;
        {more, _} ->
            case ssl:recv(Sock, 0, 30000) of
                {ok, Data} -> loop(Sock, <<Buf/binary, Data/binary>>, St);
                {error, _} -> ok
            end;
        {error, _, Rest} -> loop(Sock, Rest, St);
        {error, _} -> ok
    end.

handle(Sock, {settings, _}, St) -> send(Sock, h2_frame:settings_ack()), {continue, St};
handle(Sock, {ping, D}, St) -> send(Sock, h2_frame:ping_ack(D)), {continue, St};
handle(_Sock, {goaway, _, _, _}, _St) -> stop;
handle(_Sock, {rst_stream, Sid, _Code}, #{notify := Pid} = St) ->
    Pid ! {goaway_server, rst_stream, Sid},
    {continue, St};
handle(Sock, {headers, Sid, _Block, EndStream, _EndHeaders}, St) ->
    {continue, on_request_frame(Sock, headers, Sid, EndStream, St)};
%% decode/1 adds the flow-controlled size as a fifth element.
handle(Sock, {data, Sid, _Bin, EndStream, _FlowControlled}, St) ->
    {continue, on_request_frame(Sock, data, Sid, EndStream, St)};
handle(_Sock, _Other, St) -> {continue, St}.

on_request_frame(Sock, Kind, Sid, EndStream, #{seen := Seen, complete := Complete} = St) ->
    Seen2 = case lists:member(Sid, Seen) of true -> Seen; false -> Seen ++ [Sid] end,
    Complete2 = case EndStream of true -> [Sid | Complete]; false -> Complete end,
    St1 = St#{seen := Seen2, complete := Complete2},
    St2 = case goaway_for(Kind, Sid, EndStream, St1) of
        none -> St1;
        LastStreamId -> send_goaway(Sock, LastStreamId, St1)
    end,
    answer_ready(Sock, St2).

goaway_for(headers, _Sid, _EndStream, #{trigger := {second_stream, Pick}, seen := [First, Second]}) ->
    Pick(First, Second);
goaway_for(data, Sid, false, #{trigger := first_data}) ->
    Sid;
goaway_for(_Kind, _Sid, _EndStream, _St) ->
    none.

%% One GOAWAY per connection, then a short drain before anything is answered.
send_goaway(Sock, LastStreamId, #{seen := Seen, goaway := Shape, answer := Answer} = St) ->
    case Shape of
        two_step ->
            send(Sock, h2_frame:goaway(16#7fffffff, no_error, <<>>)),
            timer:sleep(50);
        direct ->
            ok
    end,
    send(Sock, h2_frame:goaway(LastStreamId, no_error, <<>>)),
    timer:sleep(200),
    Accepted = case Answer of
        true -> [S || S <- Seen, S =< LastStreamId];
        false -> []
    end,
    St#{trigger := none, accepted := Accepted}.

%% Answer every complete request the connection may still answer. Nothing is
%% answered while a GOAWAY is still to come, so the streams it covers are in
%% flight when it does.
answer_ready(_Sock, #{trigger := Trigger} = St) when Trigger =/= none ->
    St;
answer_ready(Sock, #{complete := Complete, accepted := Accepted} = St) ->
    Ready = [S || S <- lists:reverse(Complete),
                  Accepted =:= all orelse lists:member(S, Accepted)],
    lists:foldl(fun(S, Acc) -> respond(Sock, S, Acc) end,
                St#{complete := Complete -- Ready}, Ready).

respond(Sock, Sid, #{enc := Enc} = St) ->
    {HBlock, Enc2} = h2_hpack:encode([{<<":status">>, <<"200">>}], Enc),
    send(Sock, h2_frame:headers(Sid, HBlock, false)),
    send(Sock, h2_frame:data(Sid, integer_to_binary(Sid), true)),
    St#{enc := Enc2}.

send(Sock, FrameData) -> ssl:send(Sock, h2_frame:encode(FrameData)).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
