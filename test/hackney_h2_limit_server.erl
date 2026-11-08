%%% Frame-level HTTP/2 server for the stream-limit tests, built on the h2
%%% dep's h2_frame / h2_hpack.
%%%
%%% It advertises SETTINGS_MAX_CONCURRENT_STREAMS and answers nothing on its
%%% own: each complete request is reported to the test process as
%%%   {limit_server, ConnPid, request, StreamId}
%%% and answered (200, body = the stream id) when the test sends ConnPid
%%%   {release, StreamId}
%%% The test can also send ConnPid
%%%   {max_streams, N}   a new SETTINGS frame with that limit
%%%   goaway             GOAWAY(last stream seen, NO_ERROR)
%%% Every connection reports to the process that called start/1.
-module(hackney_h2_limit_server).

-export([start/1, stop/1, next_request/0, next_request/1]).

-define(PREFACE, <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>).

%% @doc Start a server advertising MaxStreams. Returns {Pid, Url}.
start(MaxStreams) ->
    Test = self(),
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {alpn_preferred_protocols, [<<"h2">>]},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn(fun() -> accept_loop(LSock, Test, MaxStreams) end),
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    {Pid, Url}.

stop(Pid) ->
    exit(Pid, kill).

%% @doc Wait for the next complete request: {ConnPid, StreamId}.
next_request() ->
    next_request(10000).

next_request(Timeout) ->
    receive
        {limit_server, ConnPid, request, Sid} -> {ConnPid, Sid}
    after Timeout -> timeout
    end.

accept_loop(LSock, Test, MaxStreams) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            Conn = spawn(fun() -> serve(TSock, Test, MaxStreams) end),
            _ = ssl:controlling_process(TSock, Conn),
            Conn ! go,
            accept_loop(LSock, Test, MaxStreams);
        {error, timeout} -> accept_loop(LSock, Test, MaxStreams);
        {error, closed} -> ok
    end.

serve(TSock, Test, MaxStreams) ->
    receive go -> ok end,
    case ssl:handshake(TSock, 5000) of
        {ok, Sock} ->
            case recv_preface(Sock, <<>>) of
                {ok, Rest} ->
                    send(Sock, h2_frame:settings([{max_concurrent_streams, MaxStreams}])),
                    ok = ssl:setopts(Sock, [{active, once}]),
                    loop(Sock, Rest, #{test => Test, enc => h2_hpack:new_context(),
                                       last => 0});
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
        {error, _, Rest} ->
            loop(Sock, Rest, St);
        {error, _} ->
            ok;
        {more, _} ->
            receive
                {ssl, Sock, Data} ->
                    ok = ssl:setopts(Sock, [{active, once}]),
                    loop(Sock, <<Buf/binary, Data/binary>>, St);
                {ssl_closed, Sock} ->
                    ok;
                {release, Sid} ->
                    loop(Sock, Buf, respond(Sock, Sid, St));
                {max_streams, N} ->
                    send(Sock, h2_frame:settings([{max_concurrent_streams, N}])),
                    loop(Sock, Buf, St);
                goaway ->
                    send(Sock, h2_frame:goaway(maps:get(last, St), no_error, <<>>)),
                    loop(Sock, Buf, St)
            end
    end.

handle(Sock, {settings, _}, St) -> send(Sock, h2_frame:settings_ack()), {continue, St};
handle(Sock, {ping, D}, St) -> send(Sock, h2_frame:ping_ack(D)), {continue, St};
handle(_Sock, {goaway, _, _, _}, _St) -> stop;
handle(_Sock, {headers, Sid, _Block, EndStream, _EndHeaders}, St) ->
    {continue, on_request_frame(Sid, EndStream, St#{last := max(Sid, maps:get(last, St))})};
%% decode/1 adds the flow-controlled size as a fifth element.
handle(_Sock, {data, Sid, _Bin, EndStream, _FlowControlled}, St) ->
    {continue, on_request_frame(Sid, EndStream, St)};
handle(_Sock, _Other, St) -> {continue, St}.

on_request_frame(Sid, true, #{test := Test} = St) ->
    Test ! {limit_server, self(), request, Sid},
    St;
on_request_frame(_Sid, false, St) ->
    St.

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
