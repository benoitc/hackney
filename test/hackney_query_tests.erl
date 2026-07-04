%%% Tests for the RFC 10008 QUERY method: helper API, generic request/5, and
%%% QUERY with a request body over HTTP/1.1, HTTP/2, and HTTP/3. Each fixture
%%% runs a local server that echoes the received method, so every assertion
%%% pins the wire method to <<"QUERY">>.
-module(hackney_query_tests).

-include_lib("eunit/include/eunit.hrl").

-define(H1_PORT, 8127).
-define(H3_SERVER, hackney_query_h3_test_server).

%% Resolve the test cert dir from the module's beam location so paths work
%% regardless of where eunit is run from.
cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

%%====================================================================
%% HTTP/1.1 - local cowboy server, test_http_resource echoes the wire
%% method in the JSON "method" field and the body in "data".
%%====================================================================

query_http1_test_() ->
    {setup,
     fun start_h1/0,
     fun stop_h1/1,
     fun({ok, _}) ->
         {inorder,
          [fun query_helper_arity1/0,
           fun query_helper_arity2/0,
           fun query_helper_arity3/0,
           fun query_helper_arity4/0,
           fun generic_request_query/0,
           fun query_json_body/0,
           fun query_streamed_request_body/0,
           fun async_query_request/0,
           fun connection_api_query/0]}
     end}.

start_h1() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Routes = [{"/[...]", test_http_resource, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    cowboy:start_clear(query_test_server, [{port, ?H1_PORT}],
                       #{env => #{dispatch => Dispatch}}).

stop_h1({ok, _Pid}) ->
    cowboy:stop_listener(query_test_server),
    error_logger:tty(true),
    ok.

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?H1_PORT))/binary, Path/binary>>.

query_helper_arity1() ->
    {ok, 200, _, Body} = hackney:query(url(<<"/query">>)),
    Obj = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ?assertEqual(<<>>, maps:get(<<"data">>, Obj)).

query_helper_arity2() ->
    Headers = [{<<"Accept">>, <<"application/json">>}],
    {ok, 200, _, Body} = hackney:query(url(<<"/query">>), Headers),
    Obj = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)).

query_helper_arity3() ->
    Headers = [{<<"Content-Type">>, <<"text/plain">>}],
    {ok, 200, _, Body} = hackney:query(url(<<"/query">>), Headers, <<"a=1&b=2">>),
    Obj = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ?assertEqual(<<"a=1&b=2">>, maps:get(<<"data">>, Obj)).

query_helper_arity4() ->
    Headers = [{<<"Content-Type">>, <<"text/plain">>}],
    {ok, 200, _, Body} =
        hackney:query(url(<<"/query">>), Headers, <<"a=1&b=2">>, [{pool, false}]),
    Obj = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ?assertEqual(<<"a=1&b=2">>, maps:get(<<"data">>, Obj)).

generic_request_query() ->
    Headers = [{<<"Content-Type">>, <<"text/plain">>}],
    {ok, 200, _, Body} =
        hackney:request(query, url(<<"/query">>), Headers, <<"payload">>, []),
    Obj = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ?assertEqual(<<"payload">>, maps:get(<<"data">>, Obj)).

query_json_body() ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    ReqBody = jsx:encode(#{<<"filter">> => <<"name">>, <<"limit">> => 10}),
    {ok, 200, _, RespBody} =
        hackney:query(url(<<"/query">>), Headers, ReqBody, [{pool, false}]),
    Obj = jsx:decode(RespBody, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ReqHeaders = maps:get(<<"headers">>, Obj),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, ReqHeaders)),
    Data = jsx:decode(maps:get(<<"data">>, Obj), [return_maps]),
    ?assertEqual(<<"name">>, maps:get(<<"filter">>, Data)),
    ?assertEqual(10, maps:get(<<"limit">>, Data)).

query_streamed_request_body() ->
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    {ok, ConnPid} =
        hackney:request(query, url(<<"/query">>), Headers, stream, [{pool, false}]),
    ok = hackney:send_body(ConnPid, <<"select ">>),
    ok = hackney:send_body(ConnPid, <<"name">>),
    ok = hackney:finish_send_body(ConnPid),
    {ok, 200, _RespHeaders, ConnPid} = hackney:start_response(ConnPid),
    {ok, RespBody} = hackney:body(ConnPid),
    Obj = jsx:decode(RespBody, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ?assertEqual(<<"select name">>, maps:get(<<"data">>, Obj)).

async_query_request() ->
    {ok, Ref} = hackney:query(url(<<"/query">>), [], <<"q=async">>, [async]),
    Dict = receive_response(Ref, orddict:new()),
    ?assertEqual(200, orddict:fetch(status, Dict)),
    Body = iolist_to_binary(orddict:fetch(body, Dict)),
    Obj = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ?assertEqual(<<"q=async">>, maps:get(<<"data">>, Obj)).

connection_api_query() ->
    {ok, ConnPid} = hackney:connect(hackney_tcp, "localhost", ?H1_PORT, []),
    Headers = [{<<"Host">>, <<"localhost:", (integer_to_binary(?H1_PORT))/binary>>},
               {<<"Content-Type">>, <<"text/plain">>}],
    {ok, 200, _RespHeaders, ConnPid} =
        hackney:send_request(ConnPid, {query, <<"/query">>, Headers, <<"q=conn">>}),
    {ok, Body} = hackney:body(ConnPid),
    Obj = jsx:decode(Body, [return_maps]),
    ?assertEqual(<<"QUERY">>, maps:get(<<"method">>, Obj)),
    ?assertEqual(<<"q=conn">>, maps:get(<<"data">>, Obj)),
    hackney:close(ConnPid).

receive_response(Ref, Dict0) ->
    receive
        {hackney_response, Ref, {status, Status, _Reason}} ->
            receive_response(Ref, orddict:store(status, Status, Dict0));
        {hackney_response, Ref, {headers, Headers}} ->
            receive_response(Ref, orddict:store(headers, Headers, Dict0));
        {hackney_response, Ref, done} -> Dict0;
        {hackney_response, Ref, Bin} ->
            receive_response(Ref, orddict:append(body, Bin, Dict0))
    after 10000 ->
        {error, timeout}
    end.

%%====================================================================
%% HTTP/2 - local h2-over-TLS server. The handler echoes the received
%% :method pseudo-header in the x-echo-method response header and the
%% request body verbatim.
%%====================================================================

query_http2_test_() ->
    {setup,
     fun setup_h2/0,
     fun cleanup_h2/1,
     fun(Ctx) ->
         [{"h2 one-shot QUERY with body",
           {timeout, 30, fun() -> t_h2_query_oneshot(Ctx) end}},
          {"h2 streamed QUERY body",
           {timeout, 30, fun() -> t_h2_query_streamed(Ctx) end}}]
     end}.

setup_h2() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key  => filename:join(Certs, "server.key"),
        handler => fun h2_echo_handler/5,
        settings => #{max_concurrent_streams => unlimited}
    }),
    Port = h2:server_port(Server),
    #{server => Server, port => Port}.

cleanup_h2(#{server := Server}) ->
    try h2:stop_server(Server) catch _:_ -> ok end,
    ok.

h2_echo_handler(Conn, Sid, Method, _Path, _Headers) ->
    ok = h2:set_stream_handler(Conn, Sid, self()),
    Body = h2_recv_body(Conn, Sid, <<>>),
    ok = h2:send_response(Conn, Sid, 200,
                          [{<<"content-type">>, <<"application/octet-stream">>},
                           {<<"x-echo-method">>, Method}]),
    ok = h2:send_data(Conn, Sid, Body, true).

h2_recv_body(Conn, Sid, Acc) ->
    receive
        {h2, Conn, {data, Sid, Data, true}} ->
            <<Acc/binary, Data/binary>>;
        {h2, Conn, {data, Sid, Data, false}} ->
            h2_recv_body(Conn, Sid, <<Acc/binary, Data/binary>>)
    after 10000 ->
        Acc
    end.

t_h2_query_oneshot(#{port := Port}) ->
    {ok, 200, RespHeaders, RespBody} =
        hackney:request(query, h2_url(Port, <<"/query">>), [], <<"select name">>,
                        h2_opts()),
    ?assertEqual(<<"QUERY">>, proplists:get_value(<<"x-echo-method">>, RespHeaders)),
    ?assertEqual(<<"select name">>, RespBody).

t_h2_query_streamed(#{port := Port}) ->
    {ok, ConnPid} =
        hackney:request(query, h2_url(Port, <<"/query">>), [], stream, h2_opts()),
    ok = hackney:send_body(ConnPid, <<"select ">>),
    ok = hackney:send_body(ConnPid, <<"name">>),
    ok = hackney:finish_send_body(ConnPid),
    {ok, 200, RespHeaders, ConnPid} = hackney:start_response(ConnPid),
    ?assertEqual(<<"QUERY">>, proplists:get_value(<<"x-echo-method">>, RespHeaders)),
    {ok, Echoed} = hackney:body(ConnPid),
    ?assertEqual(<<"select name">>, Echoed).

h2_opts() ->
    [{pool, false},
     {protocols, [http2]},
     {recv_timeout, 10000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].

h2_url(Port, Path) ->
    iolist_to_binary([<<"https://localhost:">>, integer_to_list(Port), Path]).

%%====================================================================
%% HTTP/3 - local in-process quic_h3 server. The handler answers 200
%% only when the :method pseudo-header is exactly <<"QUERY">> (404
%% otherwise), so a 200 proves the wire method over real QUIC.
%%====================================================================

query_http3_test_() ->
    {setup,
     fun setup_h3/0,
     fun cleanup_h3/1,
     fun(Ctx) ->
         [{"h3 QUERY with body via hackney_h3",
           {timeout, 30, fun() -> t_h3_module_query(Ctx) end}},
          {"h3 QUERY with body via hackney:request",
           {timeout, 30, fun() -> t_h3_full_stack_query(Ctx) end}}]
     end}.

setup_h3() ->
    _ = application:ensure_all_started(hackney),
    Certs = cert_dir(),
    {ok, CertPem} = file:read_file(filename:join(Certs, "server.pem")),
    {ok, KeyPem} = file:read_file(filename:join(Certs, "server.key")),
    [{'Certificate', CertDer, _} | _] = public_key:pem_decode(CertPem),
    Opts = #{cert => CertDer, key => decode_key(KeyPem),
             handler => fun h3_query_handler/5},
    {ok, _Pid} = quic_h3:start_server(?H3_SERVER, 0, Opts),
    {ok, Port} = quic:get_server_port(?H3_SERVER),
    #{port => Port}.

cleanup_h3(_) ->
    try quic_h3:stop_server(?H3_SERVER) catch _:_ -> ok end,
    ok.

h3_query_handler(Conn, StreamId, <<"QUERY">>, _Path, _Headers) ->
    Body = h3_read_body(Conn, StreamId),
    quic_h3:send_response(Conn, StreamId, 200, [
        {<<"content-type">>, <<"application/octet-stream">>},
        {<<"content-length">>, integer_to_binary(byte_size(Body))}
    ]),
    quic_h3:send_data(Conn, StreamId, Body, true);
h3_query_handler(Conn, StreamId, _Method, _Path, _Headers) ->
    quic_h3:send_response(Conn, StreamId, 404, [{<<"content-length">>, <<"0">>}]),
    quic_h3:send_data(Conn, StreamId, <<>>, true).

%% Body DATA received before the handler registered is returned buffered;
%% the rest arrives as messages until Fin.
h3_read_body(Conn, StreamId) ->
    case quic_h3:set_stream_handler(Conn, StreamId, self()) of
        ok ->
            h3_recv_body(Conn, StreamId, <<>>);
        {ok, Buffered} ->
            case h3_drain_buffered(Buffered, <<>>) of
                {fin, Acc} -> Acc;
                {more, Acc} -> h3_recv_body(Conn, StreamId, Acc)
            end
    end.

h3_drain_buffered([], Acc) ->
    {more, Acc};
h3_drain_buffered([{Data, true} | _], Acc) ->
    {fin, <<Acc/binary, Data/binary>>};
h3_drain_buffered([{Data, false} | Rest], Acc) ->
    h3_drain_buffered(Rest, <<Acc/binary, Data/binary>>).

h3_recv_body(Conn, StreamId, Acc) ->
    receive
        {quic_h3, Conn, {data, StreamId, Data, true}} ->
            <<Acc/binary, Data/binary>>;
        {quic_h3, Conn, {data, StreamId, Data, false}} ->
            h3_recv_body(Conn, StreamId, <<Acc/binary, Data/binary>>)
    after 10000 ->
        Acc
    end.

t_h3_module_query(#{port := Port}) ->
    {ok, Status, _Hdrs, Body} =
        hackney_h3:request(query, h3_url(Port), [], <<"select name">>,
                           #{insecure_skip_verify => true}),
    ?assertEqual(200, Status),
    ?assertEqual(<<"select name">>, Body).

t_h3_full_stack_query(#{port := Port}) ->
    Opts = [{pool, false},
            {protocols, [http3]},
            {recv_timeout, 10000},
            {ssl_options, [{insecure, true}]}],
    {ok, 200, _RespHeaders, Body} =
        hackney:request(query, h3_url(Port), [], <<"select name">>, Opts),
    ?assertEqual(<<"select name">>, Body).

h3_url(Port) ->
    iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), "/query"]).

decode_key(KeyPem) ->
    case public_key:pem_decode(KeyPem) of
        [{'RSAPrivateKey', Der, not_encrypted}] ->
            public_key:der_decode('RSAPrivateKey', Der);
        [{'ECPrivateKey', Der, not_encrypted}] ->
            public_key:der_decode('ECPrivateKey', Der);
        [{'PrivateKeyInfo', Der, not_encrypted}] ->
            public_key:der_decode('PrivateKeyInfo', Der);
        [{_Type, Der, not_encrypted}] ->
            Der
    end.
