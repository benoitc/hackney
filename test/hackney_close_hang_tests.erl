%%% #996: closing a connection whose server accepted the request but never
%%% answers must not hang. close/1 used to wait for the passive read in
%%% progress to end, which on a pooled connection (no recv_timeout by
%%% default) was never.
-module(hackney_close_hang_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, hackney_close_hang_pool).

close_hang_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Server, Port}) ->
         Url = iolist_to_binary(["https://localhost:", integer_to_list(Port), "/"]),
         [{"close of a pooled async request returns promptly",
           {timeout, 30, fun() -> close_async(Url, [{pool, ?POOL}]) end}},
          {"close of an unpooled async request returns before recv_timeout",
           {timeout, 30, fun() -> close_async(Url, [{pool, false}]) end}},
          {"close of a pooled sync request returns promptly",
           {timeout, 30, fun() -> close_sync(Url) end}}]
     end}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 2}]),
    start_server(?MODULE).

cleanup({Server, _Port}) ->
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    exit(Server, shutdown),
    ok.

opts(Extra) ->
    [{insecure, true} | Extra].

close_async(Url, PoolOpt) ->
    ok = listen(),
    {ok, Conn} = hackney:get(Url, [], <<>>, opts([{async, once} | PoolOpt])),
    %% The server has read the request: the conn is now blocked reading the
    %% status line of a response that never comes.
    ok = await_request(),
    MRef = erlang:monitor(process, Conn),
    T0 = erlang:monotonic_time(millisecond),
    ok = hackney:close(Conn),
    Ms = erlang:monotonic_time(millisecond) - T0,
    %% recv_timeout is 5000 by default: close must not wait for it.
    ?assert(Ms < 3000),
    receive {'DOWN', MRef, process, Conn, Reason} -> ?assertEqual(normal, Reason)
    after 1000 -> ?assert(false)
    end,
    no_stray_messages(Conn).

%% A sync request blocked on the same server, closed from another process:
%% close returns promptly and the request fails the way a stopped conn
%% always made it fail.
close_sync(Url) ->
    ok = listen(),
    {ok, Conn} = hackney:connect(Url, opts([{pool, ?POOL}])),
    Self = self(),
    Req = spawn(fun() ->
                    Self ! {result, hackney:send_request(Conn, {get, <<"/">>, [], <<>>})}
                end),
    ok = await_request(),
    T0 = erlang:monotonic_time(millisecond),
    ok = hackney:close(Conn),
    ?assert(erlang:monotonic_time(millisecond) - T0 < 3000),
    receive {result, R} -> ?assertMatch({error, _}, R)
    after 3000 -> exit(Req, kill), ?assert(false)
    end,
    ?assertNot(is_process_alive(Conn)).

%% No message for a closed request reaches the caller.
no_stray_messages(Conn) ->
    receive {hackney_response, Conn, _} = M -> ?assertEqual(none, M)
    after 200 -> ok
    end.

%% The server reports to whichever test is running: setup runs in another
%% process, so each test registers itself under ?MODULE.
listen() ->
    true = register(?MODULE, self()),
    ok.

await_request() ->
    Result = receive {server, got_request} -> ok
             after 10000 -> {error, no_request}
             end,
    unregister(?MODULE),
    Result.

%%====================================================================
%% TLS server that reads a request, reports it, and never answers.
%%====================================================================

start_server(Notify) ->
    Certs = cert_dir(),
    {ok, LSock} = ssl:listen(0,
        [{certfile, filename:join(Certs, "server.pem")},
         {keyfile, filename:join(Certs, "server.key")},
         {versions, ['tlsv1.2', 'tlsv1.3']},
         {active, false}, {mode, binary}, {reuseaddr, true}]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Pid = spawn(fun() -> accept_loop(LSock, Notify) end),
    {Pid, Port}.

accept_loop(LSock, Notify) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() ->
                case ssl:handshake(TSock, 5000) of
                    {ok, Sock} -> serve(Sock, Notify, <<>>);
                    _ -> ok
                end
            end),
            accept_loop(LSock, Notify);
        {error, timeout} -> accept_loop(LSock, Notify);
        {error, _} -> ok
    end.

serve(Sock, Notify, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        nomatch ->
            case ssl:recv(Sock, 0, 30000) of
                {ok, Data} -> serve(Sock, Notify, <<Acc/binary, Data/binary>>);
                {error, _} -> ok
            end;
        _ ->
            _ = (try Notify ! {server, got_request} catch error:badarg -> ok end),
            %% Hold the connection open without answering.
            _ = ssl:recv(Sock, 0, 30000),
            ok
    end.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
