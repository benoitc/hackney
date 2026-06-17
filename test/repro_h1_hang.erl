%%% Reproduction harness for the HTTP/1.1 reused-connection hang (active-once
%%% stranding). Raw keep-alive TLS HTTP/1.1 server with a `presend` knob that
%%% delivers the next response's bytes while the client socket is idle in
%%% {active, once}, plus a sequential hackney client loop.
-module(repro_h1_hang).
-compile(nowarn_deprecated_catch).

-export([run/1, start_server/1, stop_server/1, client_loop/3, test_b/0]).

-define(BODY, <<"{\"ok\":true,\"n\":12345}">>).

run(Knobs) ->
    _ = application:ensure_all_started(hackney),
    {Server, Port} = start_server(Knobs),
    Pool = maps:get(pool, Knobs, false),
    PoolOpt = case Pool of
        false -> [{pool, false}];
        _ -> catch hackney_pool:stop_pool(Pool),
             {ok, _} = {hackney_pool:start_pool(Pool, [{pool_size, 5}]), ok},
             [{pool, Pool}]
    end,
    N = maps:get(n, Knobs, 20),
    IdleMs = maps:get(idle_ms, Knobs, 0),
    Opts = PoolOpt ++ [{recv_timeout, 3000},
                       {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    io:format("~n========== h1 reuse ~p, n=~w ==========~n", [Knobs, N]),
    try
        client_loop(Port, N, Opts, IdleMs)
    after
        case Pool of false -> ok; _ -> catch hackney_pool:stop_pool(Pool) end,
        stop_server(Server)
    end.

client_loop(Port, N, Opts) ->
    client_loop(Port, N, Opts, 0).

client_loop(Port, N, Opts, IdleMs) ->
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    Results = [ begin R = do_one(Url, Opts, I),
                      case IdleMs of 0 -> ok; _ -> timer:sleep(IdleMs) end, R end
                || I <- lists:seq(1, N) ],
    Hangs = [ I || {I, _, {error, timeout}} <- Results ],
    Errs  = [ {I, E} || {I, _, {error, E}} <- Results, E =/= timeout ],
    io:format("  ---- hangs=~p other_errors=~p~n", [Hangs, Errs]),
    Results.

do_one(Url, Opts, I) ->
    T0 = erlang:monotonic_time(millisecond),
    R = fetch(Url, Opts),
    Ms = erlang:monotonic_time(millisecond) - T0,
    Detail = case R of
        {ok, S, B} -> io_lib:format("ok status=~w bytes=~w", [S, B]);
        {error, timeout} -> "HANG (recv_timeout)";
        {error, E} -> io_lib:format("error: ~p", [E])
    end,
    io:format("  [~3w] ~6w ms  ~s~n", [I, Ms, Detail]),
    {I, Ms, case R of {ok,_,_} -> ok; Other -> Other end}.

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
%% Test B: deterministic active-once stranding on a reused conn pid
%%====================================================================

%% Reuse ONE conn pid (bypassing the pool's is_ready), strand the next response
%% as an {ssl,Socket,Data} message while the conn is idle in {active,once}, then
%% issue the next request and assert it consumes the stranded bytes.
test_b() ->
    _ = application:ensure_all_started(hackney),
    {Server, Port} = start_server(#{}),
    {ok, ConnPid} = hackney_conn_sup:start_conn(
        #{host => "localhost", port => Port, transport => hackney_ssl,
          ssl_options => [{insecure, true}, {verify, verify_none}],
          recv_timeout => 3000, idle_timeout => infinity}),
    ok = hackney_conn:connect(ConnPid),
    try
        {ok, 200, _H1} = hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>),
        {ok, Body1} = hackney_conn:body(ConnPid),
        io:format("req1 body=~p alive=~p~n", [Body1, is_process_alive(ConnPid)]),
        %% conn is now idle in connected/{active,once}. Grab its socket.
        {StateName, ConnData} = sys:get_state(ConnPid),
        Socket = element(8, ConnData),
        io:format("state=~p~n", [StateName]),
        %% Strand the NEXT response as an active-once mailbox message.
        FakeResp = <<"HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\ncontent-length: 8\r\n\r\nSTRANDED">>,
        ConnPid ! {ssl, Socket, FakeResp},
        timer:sleep(50),
        io:format("after strand: alive=~p state=~p~n",
                  [is_process_alive(ConnPid), (catch element(1, sys:get_state(ConnPid)))]),
        %% Request 2 to /silent (server does not respond) -> must read the
        %% stranded bytes, not hang or fail.
        T0 = erlang:monotonic_time(millisecond),
        R = (catch hackney_conn:request(ConnPid, <<"GET">>, <<"/silent">>, [], <<>>)),
        R2 = case R of
            {ok, 200, _} -> hackney_conn:body(ConnPid);
            Other -> Other
        end,
        Ms = erlang:monotonic_time(millisecond) - T0,
        io:format("req2 (~w ms) -> ~p~n", [Ms, R2])
    after
        catch hackney_conn:close(ConnPid),
        stop_server(Server)
    end.

%%====================================================================
%% Raw keep-alive HTTP/1.1 TLS server
%%====================================================================

start_server(Knobs) ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn(fun() -> accept_loop(LSock, Knobs) end),
    {Pid, Port}.

stop_server(Pid) -> exit(Pid, shutdown), ok.

accept_loop(LSock, Knobs) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() ->
                case ssl:handshake(TSock, 5000) of
                    {ok, Sock} -> serve(Sock, Knobs, 0, false);
                    _ -> ok
                end
            end),
            accept_loop(LSock, Knobs);
        {error, timeout} -> accept_loop(LSock, Knobs);
        {error, _} -> ok
    end.

serve(Sock, Knobs, N, SkipNext) ->
    case read_request(Sock, <<>>) of
        {ok, Path} ->
            Silent = Path =:= <<"/silent">>,
            case SkipNext orelse Silent of
                true ->
                    serve(Sock, Knobs, N + 1, false);
                false ->
                    ok = ssl:send(Sock, response()),
                    case maps:get(presend, Knobs, false) of
                        true ->
                            timer:sleep(maps:get(presend_delay, Knobs, 5)),
                            ok = ssl:send(Sock, response()),   %% strand next response
                            serve(Sock, Knobs, N + 1, true);
                        false ->
                            serve(Sock, Knobs, N + 1, false)
                    end
            end;
        closed -> ok
    end.

read_request(Sock, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        nomatch ->
            case ssl:recv(Sock, 0, 30000) of
                {ok, Data} -> read_request(Sock, <<Acc/binary, Data/binary>>);
                {error, _} -> closed
            end;
        _ ->
            %% Extract the request-line path (GET <path> HTTP/1.1).
            Path = case binary:split(Acc, <<" ">>, [global]) of
                [_Method, P | _] -> P;
                _ -> <<"/">>
            end,
            {ok, Path}
    end.

response() ->
    [<<"HTTP/1.1 200 OK\r\ncontent-type: application/json\r\ncontent-length: ">>,
     integer_to_list(byte_size(?BODY)), <<"\r\n\r\n">>, ?BODY].

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
