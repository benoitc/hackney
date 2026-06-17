%%% Regression for the HTTP/1.1 reused-connection hang (active-once stranding).
%%%
%%% Idle pooled sockets are put in {active, once} for #544 close detection. That
%%% delivers the next inbound bytes (the start of the response on a reused
%%% connection) as a {tcp/ssl,Socket,Data} mailbox message and reverts the
%%% socket to passive; a passive recv then blocked on the empty socket buffer
%%% while the bytes sat stranded. The fix consumes those bytes (feeds them to the
%%% parser) instead of discarding the connection.
-module(hackney_h1_reuse_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BODY, <<"{\"ok\":true,\"n\":12345}">>).

h1_reuse_test_() ->
    {setup,
     fun() -> _ = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"50 reused requests complete promptly with intact bodies",
       {timeout, 60, fun reuse_no_hang/0}},
      {"response bytes stranded by active-once are consumed, not lost",
       {timeout, 30, fun stranded_bytes_consumed/0}}]}.

%% Test A: many sequential reused requests must never block to recv_timeout and
%% must return byte-intact bodies (pool and {pool, false}).
reuse_no_hang() ->
    {Server, Port} = start_server(),
    Pool = h1_reuse_pool,
    catch hackney_pool:stop_pool(Pool),
    ok = hackney_pool:start_pool(Pool, [{pool_size, 1}]),
    Url = iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), <<"/">>]),
    try
        [ run_one(Url, [{pool, Pool}]) || _ <- lists:seq(1, 50) ],
        [ run_one(Url, [{pool, false}]) || _ <- lists:seq(1, 50) ],
        ok
    after
        catch hackney_pool:stop_pool(Pool),
        stop_server(Server)
    end.

run_one(Url, PoolOpt) ->
    Opts = PoolOpt ++ [{recv_timeout, 3000},
                       {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    T0 = erlang:monotonic_time(millisecond),
    R = hackney:request(get, Url, [], <<>>, Opts),
    Ms = erlang:monotonic_time(millisecond) - T0,
    ?assert(Ms < 3000),
    ?assertMatch({ok, 200, _, ?BODY}, R).

%% Test B (deterministic): reuse one conn pid, strand the next response as an
%% {ssl,Socket,Data} message while idle in {active, once}, then issue the next
%% request. Before the fix the conn closed and the request failed with
%% {error, invalid_state}; after the fix the stranded bytes are consumed.
stranded_bytes_consumed() ->
    {Server, Port} = start_server(),
    {ok, ConnPid} = hackney_conn_sup:start_conn(
        #{host => "localhost", port => Port, transport => hackney_ssl,
          ssl_options => [{insecure, true}, {verify, verify_none}],
          recv_timeout => 3000, idle_timeout => infinity}),
    ok = hackney_conn:connect(ConnPid),
    try
        %% Request 1 over the raw conn; leaves it idle in connected/{active,once}.
        {ok, 200, _H1} = hackney_conn:request(ConnPid, <<"GET">>, <<"/">>, [], <<>>),
        {ok, ?BODY} = hackney_conn:body(ConnPid),
        {connected, ConnData} = sys:get_state(ConnPid),
        Socket = element(8, ConnData),  %% #conn_data.socket
        %% Deliver the next response as an active-once mailbox message.
        Stranded = <<"HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\n"
                     "content-length: 8\r\n\r\nSTRANDED">>,
        ConnPid ! {ssl, Socket, Stranded},
        timer:sleep(50),
        %% The conn must NOT have closed on the stranded bytes.
        ?assertMatch({connected, _}, sys:get_state(ConnPid)),
        %% Next request (/silent: server sends nothing) must read the stranded
        %% response, not hang or fail.
        T0 = erlang:monotonic_time(millisecond),
        {ok, 200, _H2} = hackney_conn:request(ConnPid, <<"GET">>, <<"/silent">>, [], <<>>),
        {ok, Body2} = hackney_conn:body(ConnPid),
        Ms = erlang:monotonic_time(millisecond) - T0,
        ?assert(Ms < 3000),
        ?assertEqual(<<"STRANDED">>, Body2)
    after
        catch hackney_conn:close(ConnPid),
        stop_server(Server)
    end.

%%====================================================================
%% Minimal keep-alive HTTP/1.1 TLS server. /silent consumes the request
%% without responding.
%%====================================================================

start_server() ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn(fun() -> accept_loop(LSock) end),
    {Pid, Port}.

stop_server(Pid) -> exit(Pid, shutdown), ok.

accept_loop(LSock) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() ->
                case ssl:handshake(TSock, 5000) of
                    {ok, Sock} -> serve(Sock);
                    _ -> ok
                end
            end),
            accept_loop(LSock);
        {error, timeout} -> accept_loop(LSock);
        {error, _} -> ok
    end.

serve(Sock) ->
    case read_request(Sock, <<>>) of
        {ok, Path} ->
            case Path of
                <<"/silent">> -> ok;
                _ -> ok = ssl:send(Sock, response())
            end,
            serve(Sock);
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
