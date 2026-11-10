%%% The documented top-level `insecure' request option and a bare
%%% {verify, verify_none} in ssl_options must both let a request reach a
%%% server with a self-signed certificate, pooled or not.
-module(hackney_insecure_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BODY, <<"ok">>).
-define(POOL, hackney_insecure_pool).

insecure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Server, Port}) ->
         Url = iolist_to_binary(["https://localhost:", integer_to_list(Port), "/"]),
         [{"default options reject the self-signed cert",
           ?_assertMatch({error, _}, req(Url, [{pool, false}]))},
          {"top-level {insecure, true}, pooled",
           ?_assertMatch({ok, 200, _, ?BODY}, req(Url, [{pool, ?POOL}, {insecure, true}]))},
          {"top-level insecure atom, pooled",
           ?_assertMatch({ok, 200, _, ?BODY}, req(Url, [{pool, ?POOL}, insecure]))},
          {"top-level {insecure, true}, unpooled",
           ?_assertMatch({ok, 200, _, ?BODY}, req(Url, [{pool, false}, {insecure, true}]))},
          {"insecure in connect_options, pooled",
           ?_assertMatch({ok, 200, _, ?BODY},
                         req(Url, [{pool, ?POOL}, {connect_options, [{insecure, true}]}]))},
          {"insecure in connect_options, unpooled",
           ?_assertMatch({ok, 200, _, ?BODY},
                         req(Url, [{pool, false}, {connect_options, [{insecure, true}]}]))},
          {"top-level insecure next to other ssl_options",
           ?_assertMatch({ok, 200, _, ?BODY},
                         req(Url, [{pool, ?POOL}, {insecure, true},
                                   {ssl_options, [{versions, ['tlsv1.2', 'tlsv1.3']}]}]))},
          {"top-level {insecure, false} overrides ssl_options",
           ?_assertMatch({error, _},
                         req(Url, [{pool, false}, {insecure, false},
                                   {ssl_options, [{insecure, true}]}]))},
          {"{verify, verify_none} alone in ssl_options",
           ?_assertMatch({ok, 200, _, ?BODY},
                         req(Url, [{pool, ?POOL}, {ssl_options, [{verify, verify_none}]}]))},
          {"hackney:connect/4 with top-level insecure",
           fun() ->
               {ok, Conn} = hackney:connect(hackney_ssl, "localhost", Port,
                                            [{pool, false}, {insecure, true}]),
               try
                   ?assertMatch({ok, 200, _, Conn},
                                hackney:send_request(Conn, {get, <<"/">>, [], <<>>})),
                   ?assertEqual({ok, ?BODY}, hackney:body(Conn))
               after
                   hackney:close(Conn)
               end
           end}]
     end}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 2}]),
    start_server().

cleanup({Server, _Port}) ->
    _ = (try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end),
    exit(Server, shutdown),
    ok.

req(Url, Opts) ->
    case hackney:request(get, Url, [], <<>>, [{recv_timeout, 3000} | Opts]) of
        {ok, Status, Headers, Ref} when is_pid(Ref) ->
            {ok, Body} = hackney:body(Ref),
            {ok, Status, Headers, Body};
        Other ->
            Other
    end.

%%====================================================================
%% Minimal keep-alive HTTP/1.1 TLS server with a self-signed chain.
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

accept_loop(LSock) ->
    case ssl:transport_accept(LSock, 2000) of
        {ok, TSock} ->
            spawn(fun() ->
                case ssl:handshake(TSock, 5000) of
                    {ok, Sock} -> serve(Sock, <<>>);
                    _ -> ok
                end
            end),
            accept_loop(LSock);
        {error, timeout} -> accept_loop(LSock);
        {error, _} -> ok
    end.

serve(Sock, Acc) ->
    case binary:split(Acc, <<"\r\n\r\n">>) of
        [_Req, Rest] ->
            ok = ssl:send(Sock, [<<"HTTP/1.1 200 OK\r\ncontent-length: ">>,
                                 integer_to_list(byte_size(?BODY)),
                                 <<"\r\n\r\n">>, ?BODY]),
            serve(Sock, Rest);
        [_] ->
            case ssl:recv(Sock, 0, 30000) of
                {ok, Data} -> serve(Sock, <<Acc/binary, Data/binary>>);
                {error, _} -> ok
            end
    end.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

%%====================================================================
%% HTTP/3: the same options against the local QUIC server.
%%====================================================================

h3_insecure_test_() ->
    {setup,
     fun hackney_h3_test_server:start/0,
     fun hackney_h3_test_server:stop/1,
     fun(Ctx) ->
         Url = hackney_h3_test_server:url(Ctx, <<"/">>),
         [{timeout, 60,
           {"h3: default options reject the self-signed cert",
            ?_assertMatch({error, _}, h3_req(Url, []))}},
          {timeout, 60,
           {"h3: top-level insecure",
            ?_assertMatch({ok, 200, _, _}, h3_req(Url, [{insecure, true}]))}},
          {timeout, 60,
           {"h3: insecure in connect_options",
            ?_assertMatch({ok, 200, _, _},
                          h3_req(Url, [{connect_options, [{insecure, true}]}]))}},
          {timeout, 60,
           {"h3: {verify, verify_none} in ssl_options",
            ?_assertMatch({ok, 200, _, _},
                          h3_req(Url, [{ssl_options, [{verify, verify_none}]}]))}}]
     end}.

h3_req(Url, Opts) ->
    Base = proplists:delete(ssl_options, hackney_h3_test_server:hackney_opts()),
    req(Url, [{pool, false} | Opts ++ Base]).

h3_verify_none_unit_test() ->
    ?assertEqual(verify_none,
                 maps:get(verify, hackney_conn:h3_tls_opts([], [{verify, verify_none}]))),
    ?assertEqual(hackney_ssl:h3_options_key([{insecure, true}], []),
                 hackney_ssl:h3_options_key([], [{verify, verify_none}])),
    ?assertNotEqual(hackney_ssl:h3_options_key([], []),
                    hackney_ssl:h3_options_key([], [{verify, verify_none}])).
