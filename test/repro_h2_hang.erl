%%% Reproduction + localization harness for the intermittent HTTP/2 hang on
%%% reused/pooled connections (hackney 4.4.3).
%%%
%%% Run from the test profile shell, e.g.:
%%%   rebar3 as test shell --eval 'repro_h2_hang:scenario(flow_control_reuse), halt().'
%%%
%%% Scenarios (escalation ladder): baseline, flow_control_reuse, framing_small,
%%% framing_big, trailers, negative_control_h1. `all/0` runs the h2 ladder.
-module(repro_h2_hang).
-compile(nowarn_deprecated_catch).

-export([scenario/1, all/0,
         start_h2_server/1, stop_h2_server/1,
         client_loop/3, concurrent_rounds/4, direct_h2/3, dump/1,
         raw_run/2, raw_matrix/0]).

-define(RECV_TIMEOUT, 5000).

%%====================================================================
%% Scenarios
%%====================================================================

scenario(baseline) ->
    run_h2(<<"baseline: 14KB, single DATA + END_STREAM, 50 reused requests">>,
           #{body_size => 14000, frame_count => 1}, 50, ?RECV_TIMEOUT);

scenario(flow_control_reuse) ->
    %% Lead hypothesis: connection-level recv window (65535) depletes across
    %% reused streams if the client fails to send WINDOW_UPDATE(0). 14KB x N
    %% crosses 65535 by request ~5.
    run_h2(<<"flow_control_reuse: 14KB no-content-length, 30 reused requests">>,
           #{body_size => 14000, frame_count => 1}, 30, ?RECV_TIMEOUT);

scenario(framing_small) ->
    run_h2(<<"framing_small: 14KB split into 28 x 500B DATA frames, 30 requests">>,
           #{body_size => 14000, frame_count => 28, inter_frame_ms => 1}, 30, ?RECV_TIMEOUT);

scenario(framing_big) ->
    %% >64KB forces a per-stream WINDOW_UPDATE within a single response.
    run_h2(<<"framing_big: 200KB body, 10 reused requests">>,
           #{body_size => 200000, frame_count => 1}, 10, ?RECV_TIMEOUT);

scenario(trailers) ->
    %% Regression: confirm 4.4.3 still handles trailered END_STREAM.
    run_h2(<<"trailers: 14KB body closed by trailers, 20 reused requests">>,
           #{body_size => 14000, frame_count => 1, trailers => true}, 20, ?RECV_TIMEOUT);

scenario(idle_between) ->
    %% Let the connection go idle between requests, as in prod, so any
    %% control frame the peer sends while idle is processed before reuse.
    run_h2_idle(<<"idle_between: 14KB, 100ms idle between 20 reused requests">>,
                #{body_size => 14000, frame_count => 1}, 20, 5000, 100);

scenario(small_window) ->
    %% Server advertises a tiny initial_window_size (its receive window; gates
    %% client->server DATA, not the GET response, but rules the knob out).
    run_h2(<<"small_window: server initial_window_size=1024, 20 reused requests">>,
           #{body_size => 14000, frame_count => 1,
             settings => #{initial_window_size => 1024, max_concurrent_streams => unlimited}},
           20, 5000);

scenario(concurrency) ->
    concurrency(#{body_size => 14000, frame_count => 1}, 20, 8, 5000);

scenario(negative_control_h1) ->
    negative_control_h1(30).

all() ->
    [ {S, scenario(S)} ||
      S <- [baseline, flow_control_reuse, framing_small, framing_big, trailers] ].

%%====================================================================
%% h2 scenario driver
%%====================================================================

run_h2(Label, Knobs, N, RecvTimeout) ->
    io:format("~n========== ~s ==========~n", [Label]),
    ensure_started(),
    {Server, Port} = start_h2_server(Knobs),
    Pool = repro_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    Opts = [{pool, Pool}, {protocols, [http2]}, {recv_timeout, RecvTimeout},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    try
        Summary = client_loop(Port, N, Opts),
        report(Summary),
        Summary
    after
        catch hackney_pool:stop_pool(Pool),
        stop_h2_server(Server)
    end.

run_h2_idle(Label, Knobs, N, RecvTimeout, IdleMs) ->
    io:format("~n========== ~s ==========~n", [Label]),
    ensure_started(),
    {Server, Port} = start_h2_server(Knobs),
    Pool = repro_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    Opts = [{pool, Pool}, {protocols, [http2]}, {recv_timeout, RecvTimeout},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    try
        Results = [ begin R = do_one(Url, Opts, I), timer:sleep(IdleMs), R end
                    || I <- lists:seq(1, N) ],
        report(summarize_results(N, Results)),
        Results
    after
        catch hackney_pool:stop_pool(Pool),
        stop_h2_server(Server)
    end.

%% Concurrency: Rounds of Conc simultaneous requests over the shared pool (one
%% multiplexed h2 connection), to exercise the multi-stream / reuse races.
concurrency(Knobs, Rounds, Conc, RecvTimeout) ->
    io:format("~n========== concurrency: ~w rounds x ~w concurrent, 14KB ==========~n",
              [Rounds, Conc]),
    ensure_started(),
    {Server, Port} = start_h2_server(Knobs),
    Pool = repro_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 10}]),
    Opts = [{pool, Pool}, {protocols, [http2]}, {recv_timeout, RecvTimeout},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    try
        concurrent_rounds(Url, Opts, Rounds, Conc)
    after
        catch hackney_pool:stop_pool(Pool),
        stop_h2_server(Server)
    end.

concurrent_rounds(Url, Opts, Rounds, Conc) ->
    Parent = self(),
    All = lists:foldl(fun(Round, Acc) ->
        Pids = [ spawn_monitor(fun() ->
                    T0 = erlang:monotonic_time(millisecond),
                    R = fetch(Url, Opts),
                    Ms = erlang:monotonic_time(millisecond) - T0,
                    Parent ! {done, self(), Round, W, Ms, R}
                 end) || W <- lists:seq(1, Conc) ],
        RoundRes = collect(length(Pids), []),
        Hangs = [ {W, Ms} || {_R, W, Ms, {error, timeout}} <- RoundRes ],
        Slow  = [ {W, Ms} || {_R, W, Ms, {ok, _, _}} <- RoundRes, Ms >= 1000 ],
        io:format("  round ~3w: ok=~w hangs=~w slow=~w~n",
                  [Round, length([1 || {_,_,_,{ok,_,_}} <- RoundRes]),
                   length(Hangs), length(Slow)]),
        case Hangs of [] -> ok; _ -> io:format("    HANGS: ~p~n", [Hangs]) end,
        Acc ++ RoundRes
    end, [], lists:seq(1, Rounds)),
    TotalHangs = [ X || {_R, _W, _Ms, {error, timeout}} = X <- All ],
    io:format("  ---- concurrency summary: ~w reqs | total hangs=~w~n",
              [length(All), length(TotalHangs)]),
    case TotalHangs of
        [] -> io:format("  RESULT: no hang reproduced~n");
        _  -> io:format("  RESULT: HANG reproduced (~w)~n", [length(TotalHangs)])
    end,
    All.

collect(0, Acc) -> Acc;
collect(N, Acc) ->
    receive
        {done, Pid, Round, W, Ms, R} ->
            receive {'DOWN', _, process, Pid, _} -> ok after 5000 -> ok end,
            collect(N - 1, [{Round, W, Ms, R} | Acc]);
        {'DOWN', _, process, _Pid, Reason} when Reason =/= normal ->
            collect(N - 1, [{crash, 0, 0, {error, Reason}} | Acc])
    after 30000 ->
        Acc
    end.

summarize_results(N, Results) ->
    #{n => N,
      hangs => [ I || {I, _Ms, {error, timeout}} <- Results ],
      errors => [ {I, E} || {I, _Ms, {error, E}} <- Results, E =/= timeout ],
      slow => [ {I, Ms} || {I, Ms, ok} <- Results, Ms >= 1000 ],
      results => Results}.

%%====================================================================
%% Raw frame-level server runs (ALB-quirk injection)
%%====================================================================

raw_run(Label, Knobs) ->
    io:format("~n========== RAW ~s | ~p ==========~n", [Label, Knobs]),
    ensure_started(),
    Server = repro_h2_raw_server:start(Knobs),
    Port = repro_h2_raw_server:port(Server),
    Pool = repro_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    Opts = [{pool, Pool}, {protocols, [http2]}, {recv_timeout, 5000},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    N = maps:get(n, Knobs, 20),
    try
        Summary = client_loop(Port, N, Opts),
        report(Summary),
        Summary
    after
        catch hackney_pool:stop_pool(Pool),
        catch repro_h2_raw_server:stop(element(1, Server))
    end.

%% Try the ALB-quirk matrix until one hangs.
raw_matrix() ->
    Cases = [
        {<<"no quirk (raw baseline)">>, #{quirk => none, n => 20}},
        {<<"SETTINGS after each response">>, #{quirk => settings, quirk_when => after_each, n => 20}},
        {<<"SETTINGS after first response">>, #{quirk => settings, quirk_when => after_first, n => 20}},
        {<<"SETTINGS mid first response">>, #{quirk => settings, quirk_when => mid_first, frame_count => 4, n => 20}},
        {<<"SETTINGS mid each response">>, #{quirk => settings, quirk_when => mid_each, frame_count => 4, n => 20}},
        {<<"SETTINGS(iw=65535) after each">>, #{quirk => {settings, [{initial_window_size, 65535}]}, quirk_when => after_each, n => 20}},
        {<<"SETTINGS(iw=16384) after each">>, #{quirk => {settings, [{initial_window_size, 16384}]}, quirk_when => after_each, n => 20}},
        {<<"PING after each response">>, #{quirk => ping, quirk_when => after_each, n => 20}},
        {<<"WINDOW_UPDATE after each">>, #{quirk => window_update, quirk_when => after_each, n => 20}}
    ],
    [ {Label, raw_run(Label, Knobs)} || {Label, Knobs} <- Cases ].

%%====================================================================
%% h2 server (h2 dep), with knobs
%%====================================================================

start_h2_server(Knobs) ->
    Certs = cert_dir(),
    Handler = fun(Conn, Sid, M, P, H) -> handle_req(Conn, Sid, M, P, H, Knobs) end,
    ServerSettings = maps:get(settings, Knobs, #{max_concurrent_streams => unlimited}),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => Handler,
        settings => ServerSettings
    }),
    Port = h2:server_port(Server),
    {Server, Port}.

stop_h2_server(Server) ->
    catch h2:stop_server(Server),
    ok.

handle_req(Conn, Sid, _M, _P, _H, Knobs) ->
    BodySize   = maps:get(body_size, Knobs, 14000),
    FrameCount = maps:get(frame_count, Knobs, 1),
    DelayMs    = maps:get(inter_frame_ms, Knobs, 0),
    Trailers   = maps:get(trailers, Knobs, false),
    Body = make_body(BodySize),
    %% 200, application/json, NO content-length, connection kept alive.
    ok = h2:send_response(Conn, Sid, 200, [{<<"content-type">>, <<"application/json">>}]),
    case Trailers of
        true ->
            send_frames(Conn, Sid, Body, FrameCount, DelayMs, false),
            h2:send_trailers(Conn, Sid, [{<<"x-trailer">>, <<"end">>}]);
        false ->
            send_frames(Conn, Sid, Body, FrameCount, DelayMs, true)
    end.

%% Split Body into FrameCount DATA frames. LastFin marks whether the final
%% frame carries END_STREAM (false when trailers will close the stream).
send_frames(Conn, Sid, Body, FrameCount, DelayMs, LastFin) ->
    Chunks = chunkify(Body, FrameCount),
    send_chunks(Conn, Sid, Chunks, DelayMs, LastFin).

send_chunks(Conn, Sid, [Last], _DelayMs, LastFin) ->
    ok = h2:send_data(Conn, Sid, Last, LastFin);
send_chunks(Conn, Sid, [C | Rest], DelayMs, LastFin) ->
    ok = h2:send_data(Conn, Sid, C, false),
    case DelayMs of 0 -> ok; _ -> timer:sleep(DelayMs) end,
    send_chunks(Conn, Sid, Rest, DelayMs, LastFin).

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

%%====================================================================
%% hackney client loop
%%====================================================================

client_loop(Port, N, Opts) ->
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    Results = [ do_one(Url, Opts, I) || I <- lists:seq(1, N) ],
    #{n => N,
      hangs => [ I || {I, _Ms, {error, timeout}} <- Results ],
      errors => [ {I, E} || {I, _Ms, {error, E}} <- Results, E =/= timeout ],
      slow => [ {I, Ms} || {I, Ms, ok} <- Results, Ms >= 1000 ],
      results => Results}.

do_one(Url, Opts, I) ->
    T0 = erlang:monotonic_time(millisecond),
    R = fetch(Url, Opts),
    Ms = erlang:monotonic_time(millisecond) - T0,
    {Tag, Detail} = summarize(R),
    io:format("  [~3w] ~6w ms  ~s~n", [I, Ms, Detail]),
    {I, Ms, Tag}.

fetch(Url, Opts) ->
    %% In this hackney version request/5 returns the full body in the 4th
    %% element (h1 fetches it internally; h2 delivers it in the reply). Only a
    %% streaming ref needs hackney:body/1.
    case hackney:request(get, Url, [], <<>>, Opts) of
        {ok, Status, _H, Body} when is_binary(Body) ->
            {ok, Status, byte_size(Body)};
        {ok, Status, _H, Ref} ->
            case hackney:body(Ref) of
                {ok, Body} -> {ok, Status, byte_size(Body)};
                {error, E} -> {error, E}
            end;
        {ok, Status, _H} -> {ok, Status, 0};
        {error, E} -> {error, E}
    end.

summarize({ok, Status, Bytes}) -> {ok, io_lib:format("ok  status=~w bytes=~w", [Status, Bytes])};
summarize({error, timeout})    -> {{error, timeout}, "HANG (recv_timeout) -> {error, timeout}"};
summarize({error, E})          -> {{error, E}, io_lib:format("error: ~p", [E])}.

report(#{n := N, hangs := Hangs, errors := Errors, slow := Slow}) ->
    io:format("  ---- summary: ~w requests | hangs=~w | other_errors=~w | slow(>=1s)=~w~n",
              [N, length(Hangs), length(Errors), length(Slow)]),
    case Hangs of
        [] -> io:format("  RESULT: no hang reproduced~n");
        _  -> io:format("  RESULT: HANG reproduced at request(s) ~p~n", [Hangs])
    end.

%%====================================================================
%% Negative control: same body over HTTP/1.1 via cowboy
%%====================================================================

negative_control_h1(N) ->
    io:format("~n========== negative_control_h1: 14KB chunked over http1, ~w requests ==========~n", [N]),
    ensure_started(),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/", repro_h1_handler, []}]}]),
    Certs = cert_dir(),
    {ok, _} = cowboy:start_tls(repro_h1_listener,
        [{port, 0},
         {certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")}],
        #{env => #{dispatch => Dispatch}}),
    Port = ranch:get_port(repro_h1_listener),
    Pool = repro_pool_h1,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{max_connections, 5}]),
    Opts = [{pool, Pool}, {protocols, [http1]}, {recv_timeout, ?RECV_TIMEOUT},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    try
        Summary = client_loop(Port, N, Opts),
        report(Summary),
        Summary
    after
        catch hackney_pool:stop_pool(Pool),
        catch cowboy:stop_listener(repro_h1_listener)
    end.

%%====================================================================
%% Part C: drive the h2 dep client directly (bypass hackney)
%%====================================================================

direct_h2(Port, N, ConnSettings) ->
    io:format("~n========== direct_h2: ~w sequential GETs on ONE h2 connection ==========~n", [N]),
    ensure_started(),
    {ok, Conn} = h2:connect("localhost", Port,
                            #{verify => verify_none, settings => ConnSettings}),
    ok = h2:wait_connected(Conn, 5000),
    Hdrs = [{<<":method">>, <<"GET">>}, {<<":scheme">>, <<"https">>},
            {<<":authority">>, <<"localhost">>}, {<<":path">>, <<"/">>}],
    Results = [ direct_one(Conn, Hdrs, I) || I <- lists:seq(1, N) ],
    catch h2:close(Conn),
    Hangs = [ I || {I, timeout} <- Results ],
    io:format("  ---- direct_h2 summary: hangs=~p~n", [Hangs]),
    Results.

direct_one(Conn, Hdrs, I) ->
    T0 = erlang:monotonic_time(millisecond),
    R = case h2:request(Conn, Hdrs) of
        {ok, StreamId} -> direct_read(Conn, StreamId, <<>>);
        {error, E}     -> {error, E}
    end,
    Ms = erlang:monotonic_time(millisecond) - T0,
    case R of
        {ok, Status, Bytes} -> io:format("  [~3w] ~6w ms  ok status=~w bytes=~w~n", [I, Ms, Status, Bytes]), {I, ok};
        timeout             -> io:format("  [~3w] ~6w ms  HANG (no END_STREAM in 5s)~n", [I, Ms]), {I, timeout};
        Other               -> io:format("  [~3w] ~6w ms  ~p~n", [I, Ms, Other]), {I, Other}
    end.

direct_read(Conn, StreamId, Acc) ->
    receive
        {h2, Conn, {response, StreamId, Status, _Hdrs}} ->
            direct_read_body(Conn, StreamId, Status, Acc);
        {h2, Conn, {stream_reset, StreamId, Code}} -> {error, {stream_reset, Code}};
        {h2, Conn, {goaway, _, Code}} -> {error, {goaway, Code}};
        {h2, Conn, closed} -> {error, closed}
    after 5000 -> timeout
    end.

direct_read_body(Conn, StreamId, Status, Acc) ->
    receive
        {h2, Conn, {data, StreamId, Data, true}}  -> {ok, Status, byte_size(<<Acc/binary, Data/binary>>)};
        {h2, Conn, {data, StreamId, Data, false}} -> direct_read_body(Conn, StreamId, Status, <<Acc/binary, Data/binary>>);
        {h2, Conn, {trailers, StreamId, _}}       -> {ok, Status, byte_size(Acc)};
        {h2, Conn, {stream_reset, StreamId, Code}} -> {error, {stream_reset, Code}}
    after 5000 -> timeout
    end.

%%====================================================================
%% Localization dump
%%====================================================================

dump(PoolName) ->
    PoolPid = hackney_pool:find_pool(PoolName),
    io:format("~n==== DUMP pool=~p pid=~p ====~n", [PoolName, PoolPid]),
    St = sys:get_state(PoolPid),
    H2Conns = pool_h2_conns(St),
    io:format("h2_connections: ~p~n", [H2Conns]),
    [ dump_conn(ConnPid) || {_Key, ConnPid} <- H2Conns ],
    ok.

%% The pool #state{} is a record; pull the h2_connections map positionally is
%% fragile, so match on the field via sys:get_state's tuple and grep for a map
%% whose values are pids. Simplest robust path: print the whole state too.
pool_h2_conns(St) when is_tuple(St) ->
    %% Find the map field that looks like #{Key => pid()}.
    Maps = [ M || M <- tuple_to_list(St), is_map(M),
                  lists:all(fun erlang:is_pid/1, maps:values(M)), map_size(M) > 0 ],
    case Maps of
        [H2 | _] -> maps:to_list(H2);
        []       -> []
    end.

dump_conn(ConnPid) ->
    io:format("~n-- hackney_conn ~p --~n", [ConnPid]),
    {State, Data} = sys:get_state(ConnPid),
    io:format("  state=~p~n", [State]),
    print_conn_data(Data),
    print_proc("hackney_conn", ConnPid),
    case conn_data_field(Data, h2_conn) of
        undefined -> io:format("  (no h2_conn)~n");
        H2Conn ->
            io:format("~n-- h2_connection ~p --~n", [H2Conn]),
            case is_pid(H2Conn) andalso is_process_alive(H2Conn) of
                true ->
                    print_proc("h2_connection", H2Conn),
                    io:format("  h2 sys:get_state =~n    ~p~n", [catch sys:get_state(H2Conn)]);
                false -> io:format("  h2_connection dead~n")
            end
    end.

%% #conn_data is a record; print the interesting fields positionally-agnostic by
%% pulling them with element/2 after locating known atoms is overkill - instead
%% rely on the fields we care about being printed by record_info via the shell.
%% Here we just dump the raw tuple plus the h2-relevant slice.
print_conn_data(Data) ->
    io:format("  conn_data (raw)=~n    ~p~n", [Data]).

conn_data_field(Data, h2_conn) ->
    %% Heuristic: the first live-or-dead pid field after the protocol atom.
    L = tuple_to_list(Data),
    case [ P || P <- L, is_pid(P) ] of
        [] -> undefined;
        Pids -> lists:last(Pids)  %% best-effort; verified against the raw dump
    end.

print_proc(Label, Pid) ->
    Info = process_info(Pid, [current_function, current_stacktrace, status,
                              message_queue_len, messages]),
    io:format("  ~s process_info:~n    ~p~n", [Label, Info]).

%%====================================================================
%% Helpers
%%====================================================================

ensure_started() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    ok.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
