%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2026 Benoit Chesneau
%%%
%%% @doc hackney:send_request/2 on a connection of each protocol.
%%%
%%% It used to crash with a case_clause on HTTP/2 and HTTP/3, which answer
%%% a plain request with {ok, Status, Headers, Body} rather than the
%%% HTTP/1.1 {ok, Status, Headers}. It returns the connection on every
%%% protocol now, so the body is read with body/1 or pulled with
%%% stream_body/1.
-module(hackney_send_request_tests).

-include_lib("eunit/include/eunit.hrl").

-define(H1_PORT, 8127).

%%====================================================================
%% HTTP/1.1
%%====================================================================

h1_test_() ->
    {setup,
     fun start_h1/0,
     fun stop_h1/1,
     [{"body/1 reads the response", {timeout, 30, fun h1_body/0}},
      {"stream_body/1 pulls the response", {timeout, 30, fun h1_stream/0}},
      {"a HEAD response has no body", {timeout, 30, fun h1_head/0}},
      {"the connection serves more requests", {timeout, 30, fun h1_reuse/0}}]}.

start_h1() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    cowboy:start_clear(send_request_test_server, [{port, ?H1_PORT}],
                       #{env => #{dispatch => Dispatch}}).

stop_h1(_) ->
    cowboy:stop_listener(send_request_test_server),
    application:stop(cowboy),
    error_logger:tty(true),
    ok.

h1_connect() ->
    hackney:connect(hackney_tcp, "localhost", ?H1_PORT, []).

h1_body() ->
    {ok, Conn} = h1_connect(),
    ?assertEqual(http1, hackney_conn:get_protocol(Conn)),
    {ok, 200, Headers, Conn} = hackney:send_request(Conn, {get, <<"/get">>, [], <<>>}),
    ?assert(is_list(Headers)),
    {ok, Body} = hackney:body(Conn),
    ?assert(byte_size(Body) > 0),
    hackney:close(Conn).

h1_stream() ->
    {ok, Conn} = h1_connect(),
    {ok, 200, _Headers, Conn} = hackney:send_request(Conn, {get, <<"/get">>, [], <<>>}),
    ?assert(byte_size(read_chunks(Conn)) > 0),
    hackney:close(Conn).

h1_head() ->
    {ok, Conn} = h1_connect(),
    ?assertMatch({ok, 200, _}, hackney:send_request(Conn, {head, <<"/get">>, [], <<>>})),
    hackney:close(Conn).

h1_reuse() ->
    {ok, Conn} = h1_connect(),
    {ok, 200, _, Conn} = hackney:send_request(Conn, {get, <<"/get">>, [], <<>>}),
    {ok, Body1} = hackney:body(Conn),
    {ok, 200, _, Conn} = hackney:send_request(Conn, {get, <<"/get">>, [], <<>>}),
    {ok, Body2} = hackney:body(Conn),
    ?assertEqual(Body1, Body2),
    hackney:close(Conn).

%% Pull a response with stream_body/1 until it ends.
read_chunks(Conn) ->
    read_chunks(Conn, <<>>).

read_chunks(Conn, Acc) ->
    case hackney:stream_body(Conn) of
        {ok, Chunk} -> read_chunks(Conn, <<Acc/binary, Chunk/binary>>);
        done -> Acc
    end.

%%====================================================================
%% HTTP/2
%%====================================================================

h2_test_() ->
    {setup,
     fun start_h2/0,
     fun stop_h2/1,
     fun(Ctx) ->
         [{"body/1 reads the response",
           {timeout, 30, fun() -> h2_body(Ctx) end}},
          {"stream_body/1 pulls the response",
           {timeout, 30, fun() -> h2_stream(Ctx) end}},
          {"a HEAD response has no body",
           {timeout, 30, fun() -> h2_head(Ctx) end}},
          {"concurrent callers read their own response",
           {timeout, 30, fun() -> h2_concurrent(Ctx) end}}]
     end}.

start_h2() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(h2),
    Certs = filename:dirname(hackney_h3_test_server:cert_file()),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => fun(Conn, StreamId, Method, _Path, _Headers) ->
            ok = h2:send_response(Conn, StreamId, 200,
                                  [{<<"content-type">>, <<"text/plain">>}]),
            Body = case Method of
                <<"HEAD">> -> <<>>;
                _ -> <<"h2 body">>
            end,
            %% A HEAD response needs no body and the client may reset the
            %% stream first, so this send is allowed to fail.
            _ = h2:send_data(Conn, StreamId, Body, true),
            ok
        end}),
    #{server => Server, port => h2:server_port(Server)}.

stop_h2(#{server := Server}) ->
    _ = h2:stop_server(Server),
    ok.

h2_connect(#{port := Port}) ->
    hackney:connect(hackney_ssl, "127.0.0.1", Port,
                    [{protocols, [http2]}, {ssl_options, [{insecure, true}]}]).

h2_body(Ctx) ->
    {ok, Conn} = h2_connect(Ctx),
    ?assertEqual(http2, hackney_conn:get_protocol(Conn)),
    {ok, 200, _Headers, Conn} = hackney:send_request(Conn, {get, <<"/">>, [], <<>>}),
    ?assertEqual({ok, <<"h2 body">>}, hackney:body(Conn)),
    hackney:close(Conn).

h2_stream(Ctx) ->
    {ok, Conn} = h2_connect(Ctx),
    {ok, 200, _Headers, Conn} = hackney:send_request(Conn, {get, <<"/">>, [], <<>>}),
    ?assertEqual(<<"h2 body">>, read_chunks(Conn)),
    hackney:close(Conn).

h2_head(Ctx) ->
    {ok, Conn} = h2_connect(Ctx),
    ?assertMatch({ok, 200, _}, hackney:send_request(Conn, {head, <<"/">>, [], <<>>})),
    hackney:close(Conn).

%% Several callers share one connection: each must get its own response,
%% not another caller's.
h2_concurrent(Ctx) ->
    {ok, Conn} = h2_connect(Ctx),
    Paths = [<<"/a">>, <<"/b">>, <<"/c">>],
    ?assertEqual([{P, {ok, <<"h2 body">>}} || P <- Paths],
                 concurrent_get(Conn, Paths)),
    hackney:close(Conn).

%%====================================================================
%% HTTP/3
%%====================================================================

h3_test_() ->
    {setup,
     fun hackney_h3_test_server:start/0,
     fun hackney_h3_test_server:stop/1,
     fun(Server) ->
         [{"body/1 reads the response",
           {timeout, 30, fun() -> h3_body(Server) end}},
          {"stream_body/1 pulls the response",
           {timeout, 30, fun() -> h3_stream(Server) end}},
          {"a HEAD response has no body",
           {timeout, 30, fun() -> h3_head(Server) end}},
          {"the connection serves more requests",
           {timeout, 30, fun() -> h3_reuse(Server) end}},
          {"concurrent callers read their own response",
           {timeout, 30, fun() -> h3_concurrent(Server) end}}]
     end}.

h3_connect(Server) ->
    hackney:connect(hackney_ssl, "127.0.0.1", hackney_h3_test_server:port(Server),
                    hackney_h3_test_server:hackney_opts()).

h3_body(Server) ->
    {ok, Conn} = h3_connect(Server),
    ?assertEqual(http3, hackney_conn:get_protocol(Conn)),
    {ok, 200, _Headers, Conn} =
        hackney:send_request(Conn, {get, <<"/cdn-cgi/trace">>, [], <<>>}),
    ?assertEqual({ok, <<"h=127.0.0.1\nhttp=http/3\n">>}, hackney:body(Conn)),
    hackney:close(Conn).

h3_stream(Server) ->
    {ok, Conn} = h3_connect(Server),
    {ok, 200, _Headers, Conn} =
        hackney:send_request(Conn, {get, <<"/cdn-cgi/trace">>, [], <<>>}),
    ?assertEqual(<<"h=127.0.0.1\nhttp=http/3\n">>, read_chunks(Conn)),
    hackney:close(Conn).

h3_head(Server) ->
    {ok, Conn} = h3_connect(Server),
    ?assertMatch({ok, 200, _}, hackney:send_request(Conn, {head, <<"/">>, [], <<>>})),
    hackney:close(Conn).

h3_reuse(Server) ->
    {ok, Conn} = h3_connect(Server),
    {ok, 200, _, Conn} = hackney:send_request(Conn, {get, <<"/">>, [], <<>>}),
    ?assertEqual({ok, <<"<html><body>hackney h3 test server</body></html>">>},
                 hackney:body(Conn)),
    {ok, 200, _, Conn} = hackney:send_request(Conn, {get, <<"/cdn-cgi/trace">>, [], <<>>}),
    ?assertEqual({ok, <<"h=127.0.0.1\nhttp=http/3\n">>}, hackney:body(Conn)),
    hackney:close(Conn).

h3_concurrent(Server) ->
    {ok, Conn} = h3_connect(Server),
    Index = <<"<html><body>hackney h3 test server</body></html>">>,
    Trace = <<"h=127.0.0.1\nhttp=http/3\n">>,
    ?assertEqual([{<<"/">>, {ok, Index}}, {<<"/cdn-cgi/trace">>, {ok, Trace}}],
                 concurrent_get(Conn, [<<"/">>, <<"/cdn-cgi/trace">>])),
    hackney:close(Conn).

%% Fire one request per path from its own process and collect what each
%% caller read, ordered by path.
concurrent_get(Conn, Paths) ->
    Self = self(),
    [spawn(fun() ->
         Result = case hackney:send_request(Conn, {get, Path, [], <<>>}) of
             {ok, 200, _Headers, Conn} -> hackney:body(Conn);
             Other -> Other
         end,
         Self ! {done, Path, Result}
     end) || Path <- Paths],
    lists:sort([receive {done, Path, Result} -> {Path, Result}
                after 15000 -> {timeout, timeout}
                end || _ <- Paths]).
