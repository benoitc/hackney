%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% A buffered response body (with_body, body/1) is capped by max_body_size
%%% on every protocol, so a server sending an endless body cannot exhaust
%%% memory (GHSA-pph4). Streaming reads are not capped.
-module(hackney_max_body_size_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BODY_SIZE, 32768).

%%====================================================================
%% HTTP/1.1
%%====================================================================

h1_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(cowboy),
         {ok, _} = application:ensure_all_started(hackney),
         Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
         {ok, _} = cowboy:start_clear(max_body_h1, [{port, 0}],
                                      #{env => #{dispatch => Dispatch}}),
         ranch:get_port(max_body_h1)
     end,
     fun(_) -> cowboy:stop_listener(max_body_h1) end,
     fun(Port) ->
         Url = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port),
                                 "/chunked/", integer_to_list(?BODY_SIZE)]),
         [{"with_body over the cap is an error",
           ?_assertEqual({error, body_too_large},
                         h1_get(Url, [{with_body, true}, {max_body_size, 1024}]))},
          {"body/1 after a streamed upload over the cap is an error",
           fun() ->
               {ok, Conn} = hackney:request(get, Url, [], stream,
                                            [{pool, false}, {max_body_size, 1024}]),
               ok = hackney:finish_send_body(Conn),
               {ok, 200, _, Conn} = hackney:start_response(Conn),
               ?assertEqual({error, body_too_large}, hackney:body(Conn))
           end},
          {"a body under the cap is returned",
           fun() ->
               {ok, 200, _, Body} = h1_get(Url, [{with_body, true},
                                                 {max_body_size, ?BODY_SIZE}]),
               ?assertEqual(?BODY_SIZE, byte_size(Body))
           end},
          {"infinity turns the cap off",
           fun() ->
               {ok, 200, _, Body} = h1_get(Url, [{with_body, true},
                                                 {max_body_size, infinity}]),
               ?assertEqual(?BODY_SIZE, byte_size(Body))
           end},
          {"the default cap leaves ordinary bodies alone",
           fun() ->
               {ok, 200, _, Body} = h1_get(Url, [{with_body, true}]),
               ?assertEqual(?BODY_SIZE, byte_size(Body))
           end}]
     end}.

h1_get(Url, Opts) ->
    hackney:request(get, Url, [], <<>>, [{pool, false} | Opts]).

%%====================================================================
%% HTTP/2
%%====================================================================

h2_test_() ->
    {setup,
     fun() ->
         _ = application:ensure_all_started(hackney),
         _ = application:ensure_all_started(h2),
         start_h2_server()
     end,
     fun(Server) -> _ = (try h2:stop_server(Server) catch _:_ -> ok end) end,
     fun(Server) ->
         Url = iolist_to_binary(["https://localhost:",
                                 integer_to_list(h2:server_port(Server)), "/"]),
         [{"with_body over the cap is an error",
           ?_assertEqual({error, body_too_large},
                         h2_get(Url, [{with_body, true}, {max_body_size, 1024}]))},
          {"a body under the cap is returned",
           fun() ->
               {ok, 200, _, Body} = h2_get(Url, [{with_body, true},
                                                 {max_body_size, ?BODY_SIZE}]),
               ?assertEqual(?BODY_SIZE, byte_size(Body))
           end},
          {"the connection keeps serving after a capped stream",
           fun() ->
               Opts = [{pool, max_body_h2_pool}, {with_body, true}],
               ?assertEqual({error, body_too_large},
                            h2_get(Url, [{max_body_size, 1024} | Opts])),
               {ok, 200, _, Body} = h2_get(Url, Opts),
               ?assertEqual(?BODY_SIZE, byte_size(Body)),
               hackney_pool:stop_pool(max_body_h2_pool)
           end}]
     end}.

h2_get(Url, Opts) ->
    Base = [{protocols, [http2]},
            {recv_timeout, 5000},
            {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    Opts1 = case proplists:is_defined(pool, Opts) of
        true -> Opts;
        false -> [{pool, false} | Opts]
    end,
    hackney:request(get, Url, [], <<>>, Opts1 ++ Base).

%% Sends the body in eight DATA frames so the cap trips mid-stream.
start_h2_server() ->
    Chunk = binary:copy(<<"x">>, ?BODY_SIZE div 8),
    Handler = fun(Conn, Sid, _Method, _Path, _Headers) ->
        ok = h2:send_response(Conn, Sid, 200,
                              [{<<"content-type">>, <<"application/octet-stream">>}]),
        %% The client cancels a capped stream mid-body, so later sends
        %% may fail; that is the point.
        _ = [h2:send_data(Conn, Sid, Chunk, false) || _ <- lists:seq(1, 7)],
        _ = h2:send_data(Conn, Sid, Chunk, true),
        ok
    end,
    Certs = cert_dir(),
    {ok, Server} = h2:start_server(0, #{
        cert => filename:join(Certs, "server.pem"),
        key => filename:join(Certs, "server.key"),
        handler => Handler
    }),
    Server.

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

%%====================================================================
%% HTTP/3
%%====================================================================

h3_test_() ->
    {setup,
     fun hackney_h3_test_server:start/0,
     fun hackney_h3_test_server:stop/1,
     fun(Server) ->
         %% /large is 16 x 4 KiB.
         Url = hackney_h3_test_server:url(Server, <<"/large">>),
         [{"with_body over the cap is an error",
           {timeout, 30,
            ?_assertEqual({error, body_too_large},
                          h3_get(Url, [{with_body, true}, {max_body_size, 1024}]))}},
          {"a body under the cap is returned",
           {timeout, 30,
            fun() ->
                {ok, 200, _, Body} = h3_get(Url, [{with_body, true}]),
                ?assertEqual(65536, byte_size(Body))
            end}}]
     end}.

h3_get(Url, Opts) ->
    hackney:request(get, Url, [], <<>>,
                    [{pool, false} | Opts] ++ hackney_h3_test_server:hackney_opts()).
