%%% A request body given as a fun, `fun() -> {ok, Data} | eof' or
%%% `{fun(State) -> {ok, Data, NewState} | eof, State}', works on HTTP/2 and
%%% HTTP/3 as it does on HTTP/1.1. It used to crash the connection (#1001):
%%% the fun was passed to iolist_to_binary/1.
-module(hackney_fun_body_tests).

-include_lib("eunit/include/eunit.hrl").

h2_fun_body_test_() ->
    {setup,
     fun() ->
         _ = application:ensure_all_started(hackney),
         _ = application:ensure_all_started(h2),
         ok
     end,
     fun(_) -> ok end,
     [{timeout, 30, fun h2_fun0_body_is_sent/0},
      {timeout, 30, fun h2_fun1_body_is_sent/0},
      {timeout, 30, fun h2_failing_fun_resets_the_stream/0}]}.

h3_fun_body_test_() ->
    {setup,
     fun hackney_h3_test_server:start/0,
     fun hackney_h3_test_server:stop/1,
     fun(Ctx) ->
         [{timeout, 60, fun() -> h3_fun0_body_is_sent(Ctx) end},
          {timeout, 60, fun() -> h3_fun1_body_is_sent(Ctx) end},
          {timeout, 60, fun() -> h3_failing_fun_keeps_the_connection(Ctx) end}]
     end}.

%%====================================================================
%% HTTP/2
%%====================================================================

h2_fun0_body_is_sent() ->
    with_h2(fun(Conn) ->
        P = request(Conn, fun0([<<"hello ">>, <<"world">>])),
        {C, 1} = hackney_h2_limit_server:next_request(),
        ?assertEqual(<<"hello world">>, h2_body(C, 1)),
        C ! {release, 1},
        ?assertEqual({ok, 200, <<"1">>}, result(P))
    end).

h2_fun1_body_is_sent() ->
    with_h2(fun(Conn) ->
        P = request(Conn, fun1([<<"a">>, <<"b">>, <<"c">>])),
        {C, 1} = hackney_h2_limit_server:next_request(),
        ?assertEqual(<<"abc">>, h2_body(C, 1)),
        C ! {release, 1},
        ?assertEqual({ok, 200, <<"1">>}, result(P))
    end).

%% A fun that fails part way: the caller gets its error, the stream is reset
%% so the server does not wait for the rest, and the connection still works.
h2_failing_fun_resets_the_stream() ->
    with_h2(fun(Conn) ->
        P = request(Conn, failing_fun()),
        ?assertEqual({error, boom}, result(P)),
        ?assertEqual(ok, receive {limit_server, _, rst_stream, 1} -> ok
                         after 10000 -> no_reset end),
        P2 = request(Conn, <<"next">>),
        {C, 3} = hackney_h2_limit_server:next_request(),
        C ! {release, 3},
        ?assertEqual({ok, 200, <<"3">>}, result(P2))
    end).

%%====================================================================
%% HTTP/3 (the local server echoes a POST body)
%%====================================================================

h3_fun0_body_is_sent(Ctx) ->
    Conn = h3_connect(Ctx),
    ?assertEqual({ok, 200, <<"hello world">>},
                 simplify(hackney_conn:request(Conn, <<"POST">>, <<"/echo">>, [],
                                               fun0([<<"hello ">>, <<"world">>]),
                                               30000, []))).

h3_fun1_body_is_sent(Ctx) ->
    Conn = h3_connect(Ctx),
    ?assertEqual({ok, 200, <<"abc">>},
                 simplify(hackney_conn:request(Conn, <<"POST">>, <<"/echo">>, [],
                                               fun1([<<"a">>, <<"b">>, <<"c">>]),
                                               30000, []))).

h3_failing_fun_keeps_the_connection(Ctx) ->
    Conn = h3_connect(Ctx),
    ?assertEqual({error, boom},
                 hackney_conn:request(Conn, <<"POST">>, <<"/echo">>, [],
                                      failing_fun(), 30000, [])),
    ?assertEqual({ok, 200, <<"next">>},
                 simplify(hackney_conn:request(Conn, <<"POST">>, <<"/echo">>, [],
                                               <<"next">>, 30000, []))).

%%====================================================================
%% Helpers
%%====================================================================

%% The fun runs in the connection process, so its position lives in a
%% holder process.
fun0(Parts) ->
    Holder = spawn(fun() -> holder(Parts) end),
    fun() ->
        Holder ! {next, self()},
        receive {Holder, Next} -> Next after 5000 -> {error, holder_timeout} end
    end.

holder([P | Rest]) ->
    receive {next, From} -> From ! {self(), {ok, P}}, holder(Rest) end;
holder([]) ->
    receive {next, From} -> From ! {self(), eof} end.

fun1(Parts) ->
    {fun([P | Rest]) -> {ok, P, Rest};
        ([]) -> eof
     end, Parts}.

failing_fun() ->
    {fun(0) -> {ok, <<"part">>, 1};
        (1) -> {error, boom}
     end, 0}.

with_h2(Fun) ->
    {Server, Url} = hackney_h2_limit_server:start(100),
    {match, [P]} = re:run(Url, <<":([0-9]+)/">>, [{capture, all_but_first, binary}]),
    {ok, Conn} = hackney:connect(hackney_ssl, "localhost", binary_to_integer(P),
                                 [{protocols, [http2]}, {pool, false},
                                  {ssl_options, [{insecure, true}, {verify, verify_none}]}]),
    try
        ?assertEqual(http2, hackney_conn:get_protocol(Conn)),
        Fun(Conn)
    after
        try hackney:close(Conn) catch _:_ -> ok end,
        hackney_h2_limit_server:stop(Server)
    end.

h3_connect(Ctx) ->
    {ok, Conn} = hackney:connect(hackney_ssl, "127.0.0.1",
                                 hackney_h3_test_server:port(Ctx),
                                 [{pool, false} | hackney_h3_test_server:hackney_opts()]),
    ?assertEqual(http3, hackney_conn:get_protocol(Conn)),
    Conn.

request(Conn, Body) ->
    Self = self(),
    spawn(fun() ->
              R = hackney_conn:request(Conn, <<"POST">>, <<"/">>, [], Body, 20000, []),
              Self ! {self(), simplify(R)}
          end).

h2_body(C, Sid) ->
    receive {limit_server, C, body, Sid, Body} -> Body
    after 10000 -> no_body
    end.

simplify({ok, Status, _Headers, Body}) when is_binary(Body) -> {ok, Status, Body};
simplify(Other) -> Other.

result(Pid) ->
    receive {Pid, R} -> R after 15000 -> {error, test_timeout} end.
