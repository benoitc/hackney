%%% GOAWAY must only fail the streams the peer did not accept.
%%%
%%% RFC 9113 6.8: streams up to and including the GOAWAY's last_stream_id may
%%% still be processed, and the peer keeps the connection open to finish them.
%%% hackney used to abort every in-flight stream with {error, {goaway, _}}, so a
%%% request the server went on to complete came back as an error: for a payment
%%% API that is a charge that succeeded and was reported as failed.
%%%
%%% The server below holds the first two streams of its first connection, sends
%%% GOAWAY with a chosen last_stream_id, then answers only the streams at or
%%% below it. Later connections answer immediately.
-module(hackney_http2_goaway_drain_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PREFACE, <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>).
-define(POOL, goaway_drain_test_pool).

goaway_drain_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{timeout, 30, fun accepted_streams_complete/0},
      {timeout, 30, fun unaccepted_stream_fails_fast/0},
      {timeout, 30, fun two_step_shutdown/0},
      {timeout, 30, fun stalled_drain_ends_with_the_stream/0},
      {timeout, 30, fun accepted_upload_completes/0},
      {timeout, 30, fun refused_upload_fails_on_next_send/0},
      {timeout, 30, fun accepted_streamed_response_completes/0}]}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    stop_pool(),
    ok = hackney_pool:start_pool(?POOL, [{max_connections, 5}]),
    ok.

cleanup(_) ->
    stop_pool().

stop_pool() ->
    try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end,
    ok.

%% GOAWAY(last_stream_id = 3) after streams 1 and 3: both were accepted, so both
%% complete. A request made while they drain must not land on the draining
%% connection, where the peer would ignore it, but dial a fresh one.
accepted_streams_complete() ->
    {Server, Url} = start_server(fun([_First, Second]) -> Second end),
    try
        [R1, R2, R3] = concurrent_requests(Url, 3),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({ok, 200, <<"3">>}, R2),
        ?assertEqual({ok, 200, <<"1">>}, R3)
    after
        stop_server(Server)
    end.

%% GOAWAY(last_stream_id = 1) after streams 1 and 3: stream 3 was not accepted
%% and fails at once with the goaway reason, stream 1 still completes.
unaccepted_stream_fails_fast() ->
    {Server, Url} = start_server(fun([First, _Second]) -> First end),
    try
        [R1, R2] = concurrent_requests(Url, 2),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({error, {goaway, no_error}}, R2)
    after
        stop_server(Server)
    end.

%% The graceful shutdown of RFC 9113 6.8: GOAWAY(2^31-1) stops new streams
%% without refusing any, then a second GOAWAY gives the real last_stream_id.
%% The first frame alone must not fail anything.
two_step_shutdown() ->
    {Server, Url} = start_server(fun([First, _Second]) -> {two_step, First} end),
    try
        [R1, R2] = concurrent_requests(Url, 2),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({error, {goaway, no_error}}, R2)
    after
        stop_server(Server)
    end.

%% A server that accepts a stream and then never answers it must not keep the
%% draining connection around: the stream's own recv_timeout ends it, and with
%% it the connection, so the next request gets a fresh one.
stalled_drain_ends_with_the_stream() ->
    {Server, Url} = start_server(fun([First, _Second]) -> {never, First} end),
    try
        [R1, R2] = concurrent_requests(Url, 2, [{recv_timeout, 1000}]),
        ?assertEqual({error, timeout}, R1),
        ?assertEqual({error, {goaway, no_error}}, R2),
        ?assertEqual({ok, 200, <<"1">>}, fetch(Url, []))
    after
        stop_server(Server)
    end.

%% GOAWAY(last_stream_id = 1) while stream 1 still uploads its body: the
%% stream was accepted, so the rest of the body goes out and the response
%% arrives. The drained connection then closes.
accepted_upload_completes() ->
    {Server, Port} = start_server(1, fun([First]) -> First end),
    Conn = direct_conn(Port),
    try
        ok = hackney_conn:send_request_headers(Conn, <<"POST">>, <<"/">>, []),
        ok = hackney_conn:send_body_chunk(Conn, <<"part">>),
        ok = hackney_conn:finish_send_body(Conn),
        {ok, 200, _, _} = hackney_conn:start_response(Conn),
        ?assertEqual({ok, <<"1">>}, hackney_conn:body(Conn)),
        ?assertEqual({ok, closed}, hackney_conn:get_state(Conn))
    after
        stop_conn(Conn),
        stop_server(Server)
    end.

%% A GET on stream 1 and an upload on stream 3, then GOAWAY(last_stream_id = 1):
%% the GET completes, the upload was refused and its next send says so. With
%% nothing left to drain the connection closes.
refused_upload_fails_on_next_send() ->
    {Server, Port} = start_server(2, fun([First, _Second]) -> First end),
    Conn = direct_conn(Port),
    try
        {ok, Ref} = hackney_conn:request_async(Conn, <<"GET">>, <<"/">>, [], <<>>,
                                               false),
        ok = hackney_conn:send_request_headers(Conn, <<"POST">>, <<"/">>, []),
        ?assertEqual({ok, 200, <<"1">>}, await_async(Ref, <<>>)),
        ?assertEqual({error, {goaway, no_error}},
                     hackney_conn:send_body_chunk(Conn, <<"part">>)),
        ?assertEqual({ok, closed}, hackney_conn:get_state(Conn))
    after
        stop_conn(Conn),
        stop_server(Server)
    end.

%% GOAWAY(last_stream_id = 1) while a streamed response waits on stream 1: the
%% stream was accepted, so its headers and body still arrive.
accepted_streamed_response_completes() ->
    {Server, Port} = start_server(1, fun([First]) -> First end),
    Conn = direct_conn(Port),
    try
        {ok, 200, _, _} = hackney_conn:request_streaming(Conn, <<"GET">>, <<"/">>,
                                                          [], <<>>),
        ?assertEqual({ok, <<"1">>}, hackney_conn:stream_body(Conn)),
        ?assertEqual(done, hackney_conn:stream_body(Conn)),
        ?assertEqual({ok, closed}, hackney_conn:get_state(Conn))
    after
        stop_conn(Conn),
        stop_server(Server)
    end.

stop_conn(Conn) ->
    try hackney_conn:stop(Conn) catch _:_ -> ok end.

direct_conn(Port) ->
    {ok, Conn} = hackney_conn_sup:start_conn(#{
        host => "localhost",
        port => Port,
        transport => hackney_ssl,
        connect_options => [{protocols, [http2]}],
        ssl_options => [{insecure, true}, {verify, verify_none}]
    }),
    ok = hackney_conn:connect(Conn),
    Conn.

await_async(Ref, Acc) ->
    receive
        {hackney_response, Ref, {status, Status, _}} ->
            put(async_status, Status),
            await_async(Ref, Acc);
        {hackney_response, Ref, {headers, _}} -> await_async(Ref, Acc);
        {hackney_response, Ref, done} -> {ok, get(async_status), Acc};
        {hackney_response, Ref, {error, E}} -> {error, E};
        {hackney_response, Ref, Bin} when is_binary(Bin) ->
            await_async(Ref, <<Acc/binary, Bin/binary>>)
    after 10000 -> {error, test_timeout}
    end.

%% The first request registers the shared connection before the second checks
%% one out, so both streams share it. The server sends GOAWAY on the second and
%% waits 200ms before answering, so a third lands inside that drain.
concurrent_requests(Url, N) ->
    concurrent_requests(Url, N, []).

concurrent_requests(Url, N, Extra) ->
    Self = self(),
    Pids = [begin
                timer:sleep(Delay),
                spawn_link(fun() -> Self ! {self(), fetch(Url, Extra)} end)
            end || Delay <- lists:sublist([0, 300, 100], N)],
    [receive {P, R} -> R after 10000 -> {error, test_timeout} end || P <- Pids].

fetch(Url, Extra) ->
    Opts = Extra ++ [{pool, ?POOL}, {protocols, [http2]}, {recv_timeout, 5000},
                     {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    case hackney:request(get, Url, [], <<>>, Opts) of
        {ok, S, _H, B} when is_binary(B) -> {ok, S, B};
        {error, E} -> {error, E}
    end.

%%====================================================================
%% Frame-level h2 server. The body of each response is its stream id.
%%====================================================================

start_server(PickLastStreamId) ->
    {Pid, Port} = start_server(2, PickLastStreamId),
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    {Pid, Url}.

%% Hold the first Count streams of the first connection, then send GOAWAY with
%% the last_stream_id PickLastStreamId(HeldIds) returns.
start_server(Count, PickLastStreamId) ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {alpn_preferred_protocols, [<<"h2">>]},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn(fun() -> accept_loop(LSock, {hold, Count, PickLastStreamId}) end),
    {Pid, Port}.

stop_server(Pid) ->
    exit(Pid, kill).

accept_loop(LSock, Mode) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() -> serve(TSock, Mode) end),
            accept_loop(LSock, immediate);
        {error, timeout} -> accept_loop(LSock, Mode);
        {error, closed} -> ok
    end.

serve(TSock, Mode) ->
    case ssl:handshake(TSock, 5000) of
        {ok, Sock} ->
            case recv_preface(Sock, <<>>) of
                {ok, Rest} ->
                    send(Sock, h2_frame:settings([])),
                    loop(Sock, Rest, #{enc => h2_hpack:new_context(), mode => Mode,
                                       held => [], ended => [], waiting => []});
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
handle(Sock, {headers, Sid, _B, true, _H}, #{mode := immediate} = St) ->
    {continue, respond(Sock, Sid, St)};
handle(Sock, {headers, Sid, _B, EndStream, _H},
       #{mode := {hold, Count, Pick}, held := Held, ended := Ended} = St) ->
    Ended2 = case EndStream of true -> [Sid | Ended]; false -> Ended end,
    case Held ++ [Sid] of
        Held2 when length(Held2) =:= Count ->
            LastStreamId = send_goaway(Sock, Pick(Held2)),
            %% The drain: the peer is told, then the accepted streams finish,
            %% an upload once its body has ended.
            timer:sleep(200),
            Accepted = [S || S <- Held2, S =< LastStreamId],
            St2 = lists:foldl(fun(S, Acc) -> respond(Sock, S, Acc) end,
                              St#{held := []},
                              [S || S <- Accepted, lists:member(S, Ended2)]),
            {continue, St2#{mode := draining, waiting := Accepted -- Ended2}};
        Held2 ->
            {continue, St#{held := Held2, ended := Ended2}}
    end;
handle(Sock, Data, #{waiting := Waiting} = St)
  when element(1, Data) =:= data, element(4, Data) =:= true ->
    Sid = element(2, Data),
    case lists:member(Sid, Waiting) of
        true -> {continue, respond(Sock, Sid, St#{waiting := Waiting -- [Sid]})};
        false -> {continue, St#{ended := [Sid | maps:get(ended, St)]}}
    end;
handle(_Sock, _Other, St) -> {continue, St}.

send_goaway(Sock, {never, LastStreamId}) ->
    send(Sock, h2_frame:goaway(LastStreamId, no_error, <<>>)),
    %% Nothing at or below LastStreamId is answered.
    0;
send_goaway(Sock, {two_step, LastStreamId}) ->
    send(Sock, h2_frame:goaway(16#7fffffff, no_error, <<>>)),
    timer:sleep(50),
    send_goaway(Sock, LastStreamId);
send_goaway(Sock, LastStreamId) ->
    send(Sock, h2_frame:goaway(LastStreamId, no_error, <<>>)),
    LastStreamId.

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
