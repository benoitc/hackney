%%% Tests for the full-duplex HTTP/2 bidirectional stream API (h2_*).
%%%
%%% The server handler does a gRPC-shaped bidi echo: respond 200, echo every
%%% client DATA frame back, then send grpc-status trailers when the client
%%% half-closes. This exercises sending and receiving interleaved on one
%%% stream, trailers, active mode, and manual receive flow control.
-module(hackney_h2_bidi_tests).

-include_lib("eunit/include/eunit.hrl").

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

%%====================================================================
%% Fixture
%%====================================================================

h2_bidi_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Ctx) ->
         [{"full duplex: interleaved send/recv + trailers",
           {timeout, 30, fun() -> t_full_duplex(Ctx) end}},
          {"active mode delivers messages in order",
           {timeout, 30, fun() -> t_active(Ctx) end}},
          {"manual flow control round-trips a large payload",
           {timeout, 30, fun() -> t_manual_flow_control(Ctx) end}},
          {"close makes recv return closed",
           {timeout, 30, fun() -> t_close(Ctx) end}}]
     end}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => fun bidi_echo_handler/5,
        settings => #{max_concurrent_streams => unlimited}
    }),
    #{server => Server, port => h2:server_port(Server)}.

cleanup(#{server := Server}) ->
    try h2:stop_server(Server) catch _:_ -> ok end,
    ok.

%%====================================================================
%% Server: gRPC-shaped bidi echo
%%====================================================================

bidi_echo_handler(Conn, Sid, _Method, _Path, _Headers) ->
    ok = h2:set_stream_handler(Conn, Sid, self()),
    ok = h2:send_response(Conn, Sid, 200,
                          [{<<"content-type">>, <<"application/grpc">>}]),
    case echo_loop(Conn, Sid) of
        complete ->
            ok = h2:send_trailers(Conn, Sid, [{<<"grpc-status">>, <<"0">>}]);
        aborted ->
            %% Client cancelled/closed the stream; nothing more to send.
            ok
    end.

echo_loop(Conn, Sid) ->
    receive
        {h2, Conn, {data, Sid, Data, true}} ->
            _ = (Data =/= <<>>) andalso h2:send_data(Conn, Sid, Data, false),
            complete;
        {h2, Conn, {data, Sid, Data, false}} ->
            _ = (Data =/= <<>>) andalso h2:send_data(Conn, Sid, Data, false),
            echo_loop(Conn, Sid);
        {h2, Conn, {trailers, Sid, _T}} ->
            complete;
        {h2, Conn, {stream_reset, Sid, _}} ->
            aborted;
        {h2, Conn, {closed, _}} ->
            aborted
    after 15000 ->
        aborted
    end.

%%====================================================================
%% Tests
%%====================================================================

t_full_duplex(#{port := Port}) ->
    {ok, S} = hackney:h2_open(url(Port, <<"/grpc">>),
                              [{<<"te">>, <<"trailers">>}], opts()),
    %% Server responds 200 before we finish sending: full-duplex.
    {ok, {response, 200, _Hdrs}} = hackney:h2_recv(S),
    ok = hackney:h2_send(S, <<"msg1">>),
    {ok, {data, <<"msg1">>}} = hackney:h2_recv(S),
    %% Send more after having read a response chunk.
    ok = hackney:h2_send(S, <<"msg2">>),
    {ok, {data, <<"msg2">>}} = hackney:h2_recv(S),
    ok = hackney:h2_send(S, <<>>, fin),
    {ok, {trailers, T}} = hackney:h2_recv(S),
    ?assertEqual(<<"0">>, proplists:get_value(<<"grpc-status">>, T)),
    {ok, done} = hackney:h2_recv(S),
    {error, closed} = hackney:h2_recv(S),
    ok = hackney:h2_close(S).

t_active(#{port := Port}) ->
    {ok, S} = hackney:h2_open(url(Port, <<"/grpc">>), [],
                              [{active, true} | opts()]),
    ok = hackney:h2_send(S, <<"ping">>),
    ok = hackney:h2_send(S, <<>>, fin),
    Msgs = collect_active(S, []),
    ?assertMatch({response, 200, _}, hd(Msgs)),
    ?assert(lists:member({data, <<"ping">>}, Msgs)),
    ?assert(lists:any(fun({trailers, _}) -> true; (_) -> false end, Msgs)),
    ?assertEqual(done, lists:last(Msgs)),
    ok = hackney:h2_close(S).

t_manual_flow_control(#{port := Port}) ->
    Payload = crypto:strong_rand_bytes(120000),
    {ok, S} = hackney:h2_open(url(Port, <<"/grpc">>), [],
                              [{flow_control, manual} | opts()]),
    {ok, {response, 200, _}} = hackney:h2_recv(S),
    ok = hackney:h2_send(S, Payload),
    ok = hackney:h2_send(S, <<>>, fin),
    Echoed = drain_consume(S, <<>>),
    ?assertEqual(Payload, Echoed),
    ok = hackney:h2_close(S).

t_close(#{port := Port}) ->
    {ok, S} = hackney:h2_open(url(Port, <<"/grpc">>), [], opts()),
    {ok, {response, 200, _}} = hackney:h2_recv(S),
    ok = hackney:h2_close(S),
    ?assertEqual({error, closed}, hackney:h2_recv(S)).

%%====================================================================
%% Helpers
%%====================================================================

opts() ->
    [{recv_timeout, 10000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

url(Port, Path) ->
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), Path]).

collect_active(S, Acc) ->
    receive
        {hackney_h2, S, done} ->
            lists:reverse([done | Acc]);
        {hackney_h2, S, Msg} ->
            collect_active(S, [Msg | Acc]);
        {hackney_h2_error, S, Reason} ->
            lists:reverse([{error, Reason} | Acc])
    after 10000 ->
        lists:reverse([timeout | Acc])
    end.

%% Drain a manual-flow-control stream, acknowledging each chunk via consume so
%% the peer's send window keeps opening.
drain_consume(S, Acc) ->
    case hackney:h2_recv(S) of
        {ok, {data, Chunk}} ->
            %% Ack the window; once the stream has ended this is a no-op.
            _ = hackney:h2_consume(S, byte_size(Chunk)),
            drain_consume(S, <<Acc/binary, Chunk/binary>>);
        {ok, {trailers, _}} ->
            drain_consume(S, Acc);
        {ok, done} ->
            Acc;
        {ok, {response, _, _}} ->
            drain_consume(S, Acc)
    end.
