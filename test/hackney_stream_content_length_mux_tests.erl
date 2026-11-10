%%% A streamed request body over HTTP/2 and HTTP/3 honors a declared
%%% Content-Length: the DATA must add up to it (RFC 9113 8.1.1, RFC 9114
%%% 4.1.2). hackney used to forward the header unchecked, so a mismatch was a
%%% malformed request the server reset, reported late and vaguely. Now a
%%% write past the declared length, or a short body at finish, resets only
%%% that stream and returns an error; the connection stays usable.
-module(hackney_stream_content_length_mux_tests).

-include_lib("eunit/include/eunit.hrl").

-define(H2_SSL, [{ssl_options, [{insecure, true}, {verify, verify_none}]}]).

%%====================================================================
%% HTTP/2
%%====================================================================

h2_test_() ->
    {setup,
     fun h2_setup/0,
     fun h2_cleanup/1,
     fun(Srv) ->
         [{"h2: Content-Length is sent and the body arrives whole",
           with_h2(Srv, fun h2_matching/1)},
          {"h2: producers count against Content-Length", with_h2(Srv, fun h2_producers/1)},
          {"h2: a write past Content-Length resets only the stream",
           with_h2(Srv, fun h2_overrun/1)},
          {"h2: a short body resets only the stream", with_h2(Srv, fun h2_short/1)},
          {"h2: an invalid Content-Length sends nothing", with_h2(Srv, fun h2_invalid/1)},
          {"h2: Content-Length with Transfer-Encoding is kept and enforced",
           with_h2(Srv, fun h2_with_te/1)},
          {"h2: without Content-Length nothing is checked",
           with_h2(Srv, fun h2_no_length/1)}]
     end}.

h2_setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(h2),
    %% setup runs in another process than the tests: the handler reports to
    %% whichever test registered itself (with_h2/2).
    Handler = fun(Conn, Sid, _Method, _Path, Headers) ->
                  ok = h2:set_stream_handler(Conn, Sid, self()),
                  h2_collect(Conn, Sid, Headers, <<>>, ?MODULE)
              end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{cert => filename:join(Certs, "server.pem"),
                                        key => filename:join(Certs, "server.key"),
                                        handler => Handler}),
    #{server => Server, port => h2:server_port(Server)}.

h2_cleanup(#{server := Server}) ->
    h2:stop_server(Server).

%% Report what the server received for one request: its headers, then the
%% whole body, or that the client reset the stream.
h2_collect(Conn, Sid, Headers, Acc, Test) ->
    receive
        {h2, Conn, {data, Sid, D, true}} ->
            Test ! {h2_body, Headers, <<Acc/binary, D/binary>>},
            ok = h2:send_response(Conn, Sid, 200, []),
            ok = h2:send_data(Conn, Sid, <<"ok">>, true);
        {h2, Conn, {data, Sid, D, false}} ->
            h2_collect(Conn, Sid, Headers, <<Acc/binary, D/binary>>, Test);
        {h2, Conn, {stream_reset, Sid, _}} ->
            Test ! {h2_reset, Headers, Acc};
        {h2, Conn, {closed, _}} ->
            ok
    after 10000 ->
        Test ! {h2_timeout, Headers, Acc}
    end.

with_h2(Srv, Test) ->
    fun() ->
        true = register(?MODULE, self()),
        try Test(Srv)
        after unregister(?MODULE)
        end
    end.

h2_conn(#{port := Port}) ->
    Url = iolist_to_binary(["https://localhost:", integer_to_list(Port), "/"]),
    {ok, C} = hackney:connect(Url, [{pool, false}, {protocols, [http2]} | ?H2_SSL]),
    ?assertEqual(http2, hackney_conn:get_protocol(C)),
    C.

matching(Conn, Headers, Writes) ->
    ok = hackney_conn:send_request_headers(Conn, <<"PUT">>, <<"/k">>, Headers, []),
    [ok = hackney_conn:send_body_chunk(Conn, W) || W <- Writes],
    ok = hackney_conn:finish_send_body(Conn),
    {ok, 200, _, Conn} = hackney_conn:start_response(Conn),
    {ok, _} = hackney_conn:body(Conn),
    ok.

h2_received() ->
    receive
        {h2_body, H, B} -> {body, H, B};
        {h2_reset, H, B} -> {reset, H, B};
        {h2_timeout, H, B} -> {timeout, H, B}
    after 10000 -> erlang:error(nothing_received)
    end.

h2_matching(Srv) ->
    C = h2_conn(Srv),
    ok = matching(C, [{<<"content-length">>, <<"11">>}],
                  [<<"hello">>, [<<" ">>, <<"wor">>], <<"ld">>]),
    {body, H, B} = h2_received(),
    ?assertEqual(<<"11">>, proplists:get_value(<<"content-length">>, H)),
    ?assertEqual(<<"hello world">>, B),
    hackney:close(C).

h2_producers(Srv) ->
    C = h2_conn(Srv),
    Fun = fun([]) -> eof; ([X | R]) -> {ok, X, R} end,
    ok = matching(C, [{<<"content-length">>, <<"5">>}], [{Fun, [<<"he">>, <<"llo">>]}]),
    {body, _, <<"hello">>} = h2_received(),
    T = ets:new(chunks, [public]),
    true = ets:insert(T, {c, [<<"wor">>, <<"ld">>]}),
    Fun0 = fun() -> case ets:lookup(T, c) of
                        [{c, [X | R]}] -> true = ets:insert(T, {c, R}), {ok, X};
                        [{c, []}] -> eof
                    end
           end,
    ok = matching(C, [{<<"content-length">>, <<"5">>}], [Fun0]),
    {body, _, <<"world">>} = h2_received(),
    ets:delete(T),
    hackney:close(C).

h2_overrun(Srv) ->
    C = h2_conn(Srv),
    ok = hackney_conn:send_request_headers(C, <<"PUT">>, <<"/k">>,
                                           [{<<"content-length">>, <<"3">>}], []),
    ?assertEqual({error, {content_length_mismatch, 3, 5}},
                 hackney_conn:send_body_chunk(C, <<"hello">>)),
    %% Nothing of the overrun was sent; the server sees the stream reset.
    {reset, _, <<>>} = h2_received(),
    %% The connection stays usable.
    ok = matching(C, [{<<"content-length">>, <<"2">>}], [<<"ok">>]),
    {body, _, <<"ok">>} = h2_received(),
    hackney:close(C).

h2_short(Srv) ->
    C = h2_conn(Srv),
    ok = hackney_conn:send_request_headers(C, <<"PUT">>, <<"/k">>,
                                           [{<<"content-length">>, <<"5">>}], []),
    ok = hackney_conn:send_body_chunk(C, <<"hel">>),
    ?assertEqual({error, {content_length_mismatch, 5, 3}}, hackney_conn:finish_send_body(C)),
    {reset, _, <<"hel">>} = h2_received(),
    ok = matching(C, [], [<<"again">>]),
    {body, _, <<"again">>} = h2_received(),
    hackney:close(C).

h2_invalid(Srv) ->
    C = h2_conn(Srv),
    ?assertEqual({error, invalid_content_length},
                 hackney_conn:send_request_headers(C, <<"PUT">>, <<"/k">>,
                                                   [{<<"content-length">>, <<"x">>}], [])),
    receive {h2_body, _, _} = M -> erlang:error({unexpected, M});
            {h2_reset, _, _} = M -> erlang:error({unexpected, M})
    after 200 -> ok
    end,
    ok = matching(C, [{<<"content-length">>, <<"1">>}], [<<"y">>]),
    {body, _, <<"y">>} = h2_received(),
    hackney:close(C).

h2_with_te(Srv) ->
    C = h2_conn(Srv),
    ok = matching(C, [{<<"content-length">>, <<"5">>}, {<<"transfer-encoding">>, <<"chunked">>}],
                  [<<"hello">>]),
    {body, H, <<"hello">>} = h2_received(),
    ?assertEqual(<<"5">>, proplists:get_value(<<"content-length">>, H)),
    ?assertEqual(undefined, proplists:get_value(<<"transfer-encoding">>, H)),
    hackney:close(C).

h2_no_length(Srv) ->
    C = h2_conn(Srv),
    ok = matching(C, [], [<<"any">>, <<>>, <<"thing">>]),
    {body, H, <<"anything">>} = h2_received(),
    ?assertEqual(undefined, proplists:get_value(<<"content-length">>, H)),
    hackney:close(C).

%%====================================================================
%% HTTP/3
%%====================================================================

h3_test_() ->
    {setup,
     fun hackney_h3_test_server:start/0,
     fun hackney_h3_test_server:stop/1,
     fun(Ctx) ->
         [{timeout, 60, {"h3: Content-Length is sent and the body arrives whole",
                         fun() -> h3_matching(Ctx) end}},
          {timeout, 60, {"h3: a write past Content-Length resets only the stream",
                         fun() -> h3_overrun(Ctx) end}},
          {timeout, 60, {"h3: a short body resets only the stream",
                         fun() -> h3_short(Ctx) end}},
          {timeout, 60, {"h3: an invalid Content-Length sends nothing",
                         fun() -> h3_invalid(Ctx) end}}]
     end}.

h3_conn(Ctx) ->
    {ok, C} = hackney:connect(hackney_h3_test_server:url(Ctx, <<"/">>),
                              [{pool, false} | hackney_h3_test_server:hackney_opts()]),
    ?assertEqual(http3, hackney_conn:get_protocol(C)),
    C.

%% The test server echoes a POST body.
h3_upload(Conn, Headers, Writes) ->
    ok = hackney_conn:send_request_headers(Conn, <<"POST">>, <<"/echo">>, Headers, []),
    [ok = hackney_conn:send_body_chunk(Conn, W) || W <- Writes],
    ok = hackney_conn:finish_send_body(Conn),
    {ok, 200, _, Conn} = hackney_conn:start_response(Conn),
    hackney_conn:body(Conn).

h3_matching(Ctx) ->
    C = h3_conn(Ctx),
    ?assertEqual({ok, <<"hello world">>},
                 h3_upload(C, [{<<"content-length">>, <<"11">>}],
                           [<<"hello">>, <<" ">>, <<"world">>])),
    hackney:close(C).

h3_overrun(Ctx) ->
    C = h3_conn(Ctx),
    ok = hackney_conn:send_request_headers(C, <<"POST">>, <<"/echo">>,
                                           [{<<"content-length">>, <<"3">>}], []),
    ?assertEqual({error, {content_length_mismatch, 3, 5}},
                 hackney_conn:send_body_chunk(C, <<"hello">>)),
    ?assertEqual({ok, <<"ok">>}, h3_upload(C, [{<<"content-length">>, <<"2">>}], [<<"ok">>])),
    hackney:close(C).

h3_short(Ctx) ->
    C = h3_conn(Ctx),
    ok = hackney_conn:send_request_headers(C, <<"POST">>, <<"/echo">>,
                                           [{<<"content-length">>, <<"5">>}], []),
    ok = hackney_conn:send_body_chunk(C, <<"hel">>),
    ?assertEqual({error, {content_length_mismatch, 5, 3}}, hackney_conn:finish_send_body(C)),
    ?assertEqual({ok, <<"again">>}, h3_upload(C, [], [<<"again">>])),
    hackney:close(C).

h3_invalid(Ctx) ->
    C = h3_conn(Ctx),
    ?assertEqual({error, invalid_content_length},
                 hackney_conn:send_request_headers(C, <<"POST">>, <<"/echo">>,
                                                   [{<<"content-length">>, <<"5, 6">>}], [])),
    ?assertEqual({ok, <<"y">>}, h3_upload(C, [{<<"content-length">>, <<"1">>}], [<<"y">>])),
    hackney:close(C).

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).
