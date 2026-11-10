%%% #970: pooled, shared and prewarmed connections must carry the socket
%%% options (connect_options) their requests asked for. The pool used to key
%%% plain connections by host, port and transport only, and prewarm dialled
%%% with no options, so a request asking for {nodelay, true} could be served
%%% a connection opened without it.
-module(hackney_pool_connect_options_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, "127.0.0.1").
-define(NODELAY, [{connect_options, [{nodelay, true}]}]).

connect_options_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Server, Port}) ->
         [{"a pooled conn is reused only with the same socket options",
           fun() -> same_options_only(Port) end},
          {"requests without connect_options still share one bucket",
           fun() -> default_bucket(Port) end},
          {"prewarmed conns are dialled with the bucket's socket options",
           {timeout, 30, fun() -> prewarm_uses_options(Port) end}},
          {"an HTTPS checkout dials with the request's socket options",
           fun() -> ssl_checkout_uses_options(Port) end}]
     end}.

h2_test_() ->
    {timeout, 30, {"a shared HTTP/2 conn is shared only with the same socket options",
                   fun h2_shared_same_options_only/0}}.

setup() ->
    _ = application:ensure_all_started(hackney),
    start_server().

cleanup({Server, _Port}) ->
    exit(Server, shutdown),
    ok.

same_options_only(Port) ->
    with_pool([{prewarm_count, 0}], fun(Pool) ->
        {ok, InfoA, A} = checkout(Pool, Port, ?NODELAY),
        ?assert(nodelay(A)),
        ok = checkin(Pool, InfoA, A),
        {ok, InfoB, B} = checkout(Pool, Port, []),
        ?assertNotEqual(A, B),
        ?assertNot(nodelay(B)),
        ok = checkin(Pool, InfoB, B),
        {ok, InfoC, C} = checkout(Pool, Port, ?NODELAY),
        ?assertEqual(A, C),
        ok = checkin(Pool, InfoC, C)
    end).

default_bucket(Port) ->
    with_pool([{prewarm_count, 0}], fun(Pool) ->
        {ok, Info1, D1} = checkout(Pool, Port, []),
        ok = checkin(Pool, Info1, D1),
        {ok, Info2, D2} = checkout(Pool, Port, []),
        ?assertEqual(D1, D2),
        ok = checkin(Pool, Info2, D2)
    end).

%% With pool_size 1 the second conn is an overflow conn, closed at checkin,
%% which keeps the host's prewarm warm: the pool dials replacements into the
%% same bucket. They must have the options the bucket's requests asked for.
prewarm_uses_options(Port) ->
    with_pool([{pool_size, 1}, {prewarm_count, 2}], fun(Pool) ->
        {ok, Info1, C1} = checkout(Pool, Port, ?NODELAY),
        {ok, Info2, C2} = checkout(Pool, Port, ?NODELAY),
        ok = hackney_pool:checkin(Info1, C1),
        ok = hackney_pool:checkin(Info2, C2),
        wait_until(fun() -> free_count(Pool) >= 2 end),
        {ok, _, P1} = checkout(Pool, Port, ?NODELAY),
        {ok, _, P2} = checkout(Pool, Port, ?NODELAY),
        ?assert(nodelay(P1)),
        ?assert(nodelay(P2))
    end).

%% An HTTPS request is dialled as TCP and upgraded. The TCP conn the pool
%% hands out for the upgrade must come from the bucket of the request's
%% socket options, not be an idle conn opened without them.
ssl_checkout_uses_options(Port) ->
    with_pool([{prewarm_count, 0}], fun(Pool) ->
        {ok, InfoD, Default} = checkout(Pool, Port, []),
        ok = checkin(Pool, InfoD, Default),
        Opts = [{pool, Pool}, {connect_timeout, 2000}, {checkout_timeout, 2000},
                {tls_key, <<"k">>} | ?NODELAY],
        {ok, _Info, Conn, needs_upgrade} =
            hackney_pool:checkout_ssl(?HOST, Port, hackney_ssl, Opts),
        ?assertNotEqual(Default, Conn),
        ?assert(nodelay(Conn)),
        ok = hackney_conn:stop(Conn)
    end).

h2_shared_same_options_only() ->
    _ = application:ensure_all_started(h2),
    Handler = fun(SConn, Sid, _Method, _Path, _Headers) ->
                  ok = h2:send_response(SConn, Sid, 200, []),
                  ok = h2:send_data(SConn, Sid, <<"ok">>, true)
              end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{cert => filename:join(Certs, "server.pem"),
                                        key => filename:join(Certs, "server.key"),
                                        handler => Handler}),
    Port = h2:server_port(Server),
    Url = iolist_to_binary(["https://localhost:", integer_to_list(Port), "/"]),
    try
        with_pool([{prewarm_count, 0}], fun(Pool) ->
            Base = [{pool, Pool}, {protocols, [http2]}, {recv_timeout, 5000},
                    {ssl_options, [{insecure, true}, {verify, verify_none}]}],
            Get = fun(Extra) ->
                      {ok, 200, _, <<"ok">>} = hackney:request(get, Url, [], <<>>,
                                                               Extra ++ Base)
                  end,
            Get([]),
            ?assertEqual(1, length(shared_conns(Pool, Port))),
            Get([]),
            ?assertEqual(1, length(shared_conns(Pool, Port))),
            Get(?NODELAY),
            ?assertEqual(2, length(shared_conns(Pool, Port))),
            Get(?NODELAY),
            ?assertEqual(2, length(shared_conns(Pool, Port)))
        end)
    after
        h2:stop_server(Server),
        hackney_load_regulation:reset("localhost", Port)
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% Shared HTTP/2 conns the pool registered for Port, read from its state.
shared_conns(Pool, Port) ->
    State = sys:get_state(hackney_pool:find_pool(Pool)),
    lists:usort([Pid || Field <- tuple_to_list(State), is_map(Field),
                        {{_Host, P, hackney_ssl, _Key}, Pid} <- maps:to_list(Field),
                        P =:= Port, is_pid(Pid)]).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

with_pool(PoolOpts, Fun) ->
    Pool = list_to_atom("connect_opts_pool_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = hackney_pool:start_pool(Pool, PoolOpts),
    try Fun(Pool)
    after hackney_pool:stop_pool(Pool)
    end.

checkout(Pool, Port, Extra) ->
    hackney_pool:checkout(?HOST, Port, hackney_tcp,
                          [{pool, Pool}, {connect_timeout, 2000},
                           {checkout_timeout, 2000} | Extra]).

%% Check a conn in and wait until the pool has it back.
checkin(Pool, Info, Pid) ->
    Free = free_count(Pool),
    ok = hackney_pool:checkin(Info, Pid),
    wait_until(fun() -> free_count(Pool) > Free end).

free_count(Pool) ->
    proplists:get_value(free_count, hackney_pool:get_stats(Pool)).

%% The conn's socket, read from its state (#conn_data.socket).
nodelay(Conn) ->
    {_StateName, Data} = sys:get_state(Conn),
    Socket = element(8, Data),
    {ok, [{nodelay, V}]} = inet:getopts(Socket, [nodelay]),
    V.

wait_until(Fun) ->
    wait_until(Fun, erlang:monotonic_time(millisecond) + 5000).

wait_until(Fun, Deadline) ->
    case Fun() of
        true -> ok;
        false ->
            case erlang:monotonic_time(millisecond) > Deadline of
                true -> erlang:error(wait_until_timeout);
                false -> timer:sleep(20), wait_until(Fun, Deadline)
            end
    end.

%% A plain HTTP/1.1 keep-alive server; the pool tests only dial it.
start_server() ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}, {backlog, 64}]),
    {ok, Port} = inet:port(L),
    Pid = spawn(fun() -> accept_loop(L) end),
    {Pid, Port}.

accept_loop(L) ->
    case gen_tcp:accept(L) of
        {ok, S} ->
            Pid = spawn(fun() -> receive go -> hold(S) end end),
            ok = gen_tcp:controlling_process(S, Pid),
            Pid ! go,
            accept_loop(L);
        {error, _} ->
            ok
    end.

hold(S) ->
    case gen_tcp:recv(S, 0) of
        {ok, _} -> hold(S);
        {error, _} -> ok
    end.
