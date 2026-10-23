%%% Tests for HTTP/2 response downloads under receive flow control.
%%%
%%% hackney ran HTTP/2 with the protocol's 65,535 byte initial windows, so
%%% at most 64 KB of a response was in flight per round trip. Over a 120 ms
%%% link a 41 MB body took longer than the server's stream timeout and the
%%% server reset the stream: {error, {stream_error, internal_error}}.
%%% hackney now raises the stream window (SETTINGS_INITIAL_WINDOW_SIZE) and the
%%% connection window (h2 connection_window_size).
%%%
%%% The server below sends DATA only within the windows the client grants,
%%% sleeps ?RTT_MS every time it has to wait for a WINDOW_UPDATE (a round
%%% trip), and resets the stream with INTERNAL_ERROR past ?DEADLINE_MS.
-module(hackney_http2_recv_window_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PREFACE, <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>).
-define(INITIAL_WINDOW, 65535).
-define(FRAME, 16384).
-define(BODY_SIZE, 50 * 1024 * 1024).
-define(RTT_MS, 50).
-define(DEADLINE_MS, 10000).
-define(INTERNAL_ERROR, 16#2).

recv_window_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(hackney),
         {ok, _} = application:ensure_all_started(h2),
         ok
     end,
     fun(_) -> ok end,
     [{"client grants more than 64 KB before the first request",
       {timeout, 60, fun t_windows_granted/0}},
      {"50 MB body over a slow link completes before the server deadline",
       {timeout, 120, fun t_large_body_slow_link/0}}]}.

t_windows_granted() ->
    {Srv, Port} = start_server(self(), 1024),
    try
        ?assertMatch({ok, 200, _, _},
                     hackney:request(get, url(Port), [], <<>>, opts())),
        #{stream_window := StreamWindow, conn_window := ConnWindow} = report(),
        ?assert(StreamWindow >= 1024 * 1024),
        ?assert(ConnWindow >= 1024 * 1024)
    after
        stop_server(Srv)
    end.

t_large_body_slow_link() ->
    {Srv, Port} = start_server(self(), ?BODY_SIZE),
    try
        T0 = erlang:monotonic_time(millisecond),
        Res = hackney:request(get, url(Port), [], <<>>, opts()),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assertMatch({ok, 200, _, _}, Res),
        {ok, 200, _, Body} = Res,
        ?assertEqual(?BODY_SIZE, byte_size(Body)),
        ?assert(Elapsed < ?DEADLINE_MS)
    after
        stop_server(Srv)
    end.

opts() ->
    [{pool, false},
     {protocols, [http2]},
     {recv_timeout, 60000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

url(Port) ->
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]).

report() ->
    receive {recv_window_server, Report} -> Report
    after 5000 -> error(no_report)
    end.

%%====================================================================
%% Server
%%====================================================================

-record(srv, {sock, buf = <<>>, test, body_size,
              stream_initial = ?INITIAL_WINDOW, conn = ?INITIAL_WINDOW,
              conn_granted = ?INITIAL_WINDOW, streams = #{}}).

start_server(TestPid, BodySize) ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {alpn_preferred_protocols, [<<"h2">>]},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn(fun() -> accept(LSock, TestPid, BodySize) end),
    {Pid, Port}.

stop_server(Pid) ->
    exit(Pid, kill),
    ok.

accept(LSock, TestPid, BodySize) ->
    {ok, TSock} = ssl:transport_accept(LSock, 10000),
    {ok, Sock} = ssl:handshake(TSock, 5000),
    Rest = read_preface(Sock, <<>>),
    send(Sock, h2_frame:settings([])),
    idle(#srv{sock = Sock, buf = Rest, test = TestPid, body_size = BodySize}).

read_preface(Sock, Buf) when byte_size(Buf) < byte_size(?PREFACE) ->
    {ok, Data} = ssl:recv(Sock, 0, 5000),
    read_preface(Sock, <<Buf/binary, Data/binary>>);
read_preface(_Sock, Buf) ->
    Size = byte_size(?PREFACE),
    <<Preface:Size/binary, Rest/binary>> = Buf,
    ?PREFACE = Preface,
    Rest.

%% Process frames until a request arrives, then answer it.
idle(S) ->
    case next_frame(S, infinity) of
        {ok, {headers, StreamId, _, _, _}, S1} ->
            S1#srv.test ! {recv_window_server,
                           #{stream_window => S1#srv.stream_initial,
                             conn_window => S1#srv.conn_granted}},
            idle(respond(StreamId, S1));
        {ok, Frame, S1} ->
            idle(handle(Frame, S1))
    end.

respond(StreamId, #srv{sock = Sock, streams = Streams} = S) ->
    {Block, _} = h2_hpack:encode([{<<":status">>, <<"200">>}], h2_hpack:new_context()),
    send(Sock, h2_frame:headers(StreamId, Block, false)),
    Deadline = erlang:monotonic_time(millisecond) + ?DEADLINE_MS,
    S1 = S#srv{streams = Streams#{StreamId => S#srv.stream_initial}},
    send_body(StreamId, S#srv.body_size, Deadline, binary:copy(<<"x">>, ?FRAME), S1).

send_body(_StreamId, 0, _Deadline, _Chunk, S) ->
    S;
send_body(StreamId, Left, Deadline, Chunk, #srv{conn = Conn, streams = Streams} = S) ->
    Size = lists:min([?FRAME, Left, Conn, maps:get(StreamId, Streams)]),
    Now = erlang:monotonic_time(millisecond),
    if
        Now > Deadline ->
            %% What a server does with a stream it cannot make progress on.
            send(S#srv.sock, h2_frame:rst_stream(StreamId, ?INTERNAL_ERROR)),
            S;
        Size > 0 ->
            Left1 = Left - Size,
            <<Data:Size/binary, _/binary>> = Chunk,
            send(S#srv.sock, h2_frame:data(StreamId, Data, Left1 =:= 0)),
            send_body(StreamId, Left1, Deadline, Chunk,
                      S#srv{conn = Conn - Size,
                            streams = Streams#{StreamId => maps:get(StreamId, Streams) - Size}});
        true ->
            %% Window exhausted: one round trip before the update can land.
            timer:sleep(?RTT_MS),
            send_body(StreamId, Left, Deadline, Chunk, wait_window(S))
    end.

wait_window(S) ->
    case next_frame(S, 5000) of
        {ok, {window_update, _, _} = Frame, S1} -> drain(handle(Frame, S1));
        {ok, Frame, S1} -> wait_window(handle(Frame, S1));
        timeout -> error(window_never_opened)
    end.

%% Apply every frame already received, so one round trip lands every update
%% the client sent during it.
drain(#srv{sock = Sock, buf = Buf} = S) ->
    case h2_frame:decode(Buf) of
        {ok, Frame, Rest} ->
            drain(handle(Frame, S#srv{buf = Rest}));
        {more, _} ->
            case ssl:recv(Sock, 0, 0) of
                {ok, Data} -> drain(S#srv{buf = <<Buf/binary, Data/binary>>});
                {error, timeout} -> S
            end
    end.

handle({settings, Settings}, S) ->
    send(S#srv.sock, h2_frame:settings_ack()),
    S#srv{stream_initial = proplists:get_value(16#4, Settings, S#srv.stream_initial)};
handle({window_update, 0, Inc}, S) ->
    S#srv{conn = S#srv.conn + Inc, conn_granted = S#srv.conn_granted + Inc};
handle({window_update, StreamId, Inc}, #srv{streams = Streams} = S) ->
    S#srv{streams = Streams#{StreamId => maps:get(StreamId, Streams, 0) + Inc}};
handle({ping, Data}, S) ->
    send(S#srv.sock, h2_frame:ping_ack(Data)),
    S;
handle(_Frame, S) ->
    S.

next_frame(#srv{sock = Sock, buf = Buf} = S, Timeout) ->
    case h2_frame:decode(Buf) of
        {ok, Frame, Rest} ->
            {ok, Frame, S#srv{buf = Rest}};
        {more, _} ->
            case ssl:recv(Sock, 0, Timeout) of
                {ok, Data} -> next_frame(S#srv{buf = <<Buf/binary, Data/binary>>}, Timeout);
                {error, timeout} -> timeout;
                {error, Reason} -> exit({socket, Reason})
            end
    end.

send(Sock, Frame) ->
    ok = ssl:send(Sock, h2_frame:encode(Frame)).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
