%%% Regression for the HTTP/2 GOAWAY pooled-reuse hang.
%%%
%%% An h2 server (like an AWS ALB) recycles a connection by sending GOAWAY and
%%% keeping the socket open for a drain window. hackney used to leave such a
%%% connection `connected` and pooled, so checkout_h2 kept handing it out and
%%% every reused request opened a stream past last_stream_id that the peer
%%% ignored, hanging to recv_timeout. The fix tears the connection down on
%%% GOAWAY so the pool dials a fresh one.
%%%
%%% This test embeds a minimal frame-level h2 server (via the h2 dep's h2_frame
%%% / h2_hpack) that answers N requests then sends GOAWAY and stays open.
-module(hackney_http2_goaway_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PREFACE, <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>).
-define(GOAWAY_AFTER, 5).
-define(BODY, <<"{\"d\":\"", (binary:copy(<<"x">>, 13988))/binary, "\"}">>).

goaway_reuse_test_() ->
    {timeout, 60, fun goaway_does_not_hang_pooled_reuse/0}.

%% Drive 12 sequential requests through a pool against a server that GOAWAYs
%% every 5 responses but keeps the socket open. No request may hang to
%% recv_timeout, and reuse after GOAWAY must dial a fresh connection and succeed.
goaway_does_not_hang_pooled_reuse() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    {ServerPid, Port} = start_server(),
    Pool = goaway_test_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    Opts = [{pool, Pool}, {protocols, [http2]}, {recv_timeout, 2000},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    try
        Results = [ begin R = fetch(Url, Opts), timer:sleep(100), R end
                    || _ <- lists:seq(1, 12) ],
        %% Regression: a GOAWAY'd-but-open connection must never strand a reused
        %% request at recv_timeout.
        Hangs = [ R || {error, timeout} = R <- Results ],
        ?assertEqual([], Hangs),
        %% Reuse after GOAWAY dials fresh and succeeds. The tail requests are well
        %% past the first GOAWAY, on connections opened after it.
        ExpectedBytes = byte_size(?BODY),
        Tail = lists:nthtail(8, Results),
        [ ?assertEqual({ok, 200, ExpectedBytes}, R) || R <- Tail ],
        %% At most the single request that races the GOAWAY in-flight may come
        %% back as a clean (retryable) goaway error rather than a 200.
        Ok = [ R || {ok, 200, _} = R <- Results ],
        ?assert(length(Ok) >= 11)
    after
        catch hackney_pool:stop_pool(Pool),
        unlink(ServerPid),
        exit(ServerPid, shutdown)
    end.

fetch(Url, Opts) ->
    case hackney:request(get, Url, [], <<>>, Opts) of
        {ok, S, _H, B} when is_binary(B) -> {ok, S, byte_size(B)};
        {ok, S, _H, Ref} ->
            case hackney:body(Ref) of
                {ok, B} -> {ok, S, byte_size(B)};
                {error, E} -> {error, E}
            end;
        {ok, S, _H} -> {ok, S, 0};
        {error, E} -> {error, E}
    end.

%%====================================================================
%% Minimal frame-level h2 server: answer ?GOAWAY_AFTER requests, then GOAWAY
%% and keep the socket open (ignore later streams).
%%====================================================================

start_server() ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {alpn_preferred_protocols, [<<"h2">>]},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn_link(fun() -> accept_loop(LSock) end),
    {Pid, Port}.

accept_loop(LSock) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() -> serve(TSock) end),
            accept_loop(LSock);
        {error, timeout} -> accept_loop(LSock);
        {error, closed} -> ok
    end.

serve(TSock) ->
    case ssl:handshake(TSock, 5000) of
        {ok, Sock} ->
            case recv_preface(Sock, <<>>) of
                {ok, Rest} ->
                    send(Sock, h2_frame:settings([])),
                    loop(Sock, Rest, #{enc => h2_hpack:new_context(), count => 0, goaway => false});
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
handle(_Sock, {settings_ack}, St) -> {continue, St};
handle(Sock, {ping, D}, St) -> send(Sock, h2_frame:ping_ack(D)), {continue, St};
handle(_Sock, {window_update, _, _}, St) -> {continue, St};
handle(_Sock, {goaway, _, _, _}, _St) -> stop;
handle(_Sock, {rst_stream, _, _}, St) -> {continue, St};
handle(_Sock, {headers, _Sid, _B, _E, _H}, #{goaway := true} = St) ->
    %% After GOAWAY we no longer answer new streams (the client's read would
    %% hang here without the fix).
    {continue, St};
handle(Sock, {headers, Sid, _B, _E, _H}, #{enc := Enc, count := C} = St) ->
    {HBlock, Enc2} = h2_hpack:encode(
        [{<<":status">>, <<"200">>}, {<<"content-type">>, <<"application/json">>}], Enc),
    send(Sock, h2_frame:headers(Sid, HBlock, false)),
    send(Sock, h2_frame:data(Sid, ?BODY, true)),
    C2 = C + 1,
    St2 = St#{enc := Enc2, count := C2},
    case C2 >= ?GOAWAY_AFTER of
        true  -> send(Sock, h2_frame:goaway(Sid, no_error, <<>>)), {continue, St2#{goaway := true}};
        false -> {continue, St2}
    end;
handle(_Sock, _Other, St) -> {continue, St}.

send(Sock, FrameData) -> ssl:send(Sock, h2_frame:encode(FrameData)).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
