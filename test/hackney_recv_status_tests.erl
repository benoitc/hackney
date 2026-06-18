%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.

%% Regression for the recv_status no-progress spin: an HTTP/2 frame fed to the
%% HTTP/1 parser (a connection mislabeled as http1) must fail fast instead of
%% looping forever on an unconsumable buffer.
-module(hackney_recv_status_tests).

-include_lib("eunit/include/eunit.hrl").

%% A real HTTP/2 server connection preface SETTINGS frame (length 18, type 4,
%% stream 0) - the bytes observed in the hang trace.
-define(H2_SETTINGS,
        <<0,0,18,4,0,0,0,0,0,0,3,0,0,0,128,0,4,0,1,0,0,0,5,0,
          255,255,255,0,0,4,8,0,0,0,0,0,127,255,0,0>>).

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

teardown(_) ->
    application:stop(hackney),
    ok.

recv_status_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      {"h2 frame on an http1 conn fails fast, no spin",
       {timeout, 10, fun h2_frame_fails_fast/0}},
      {"a normal HTTP/1.1 response still parses",
       {timeout, 10, fun normal_response_ok/0}}
     ]}.

%% The server replies with an HTTP/2 SETTINGS frame and keeps the socket open, so
%% without the guard recv_status spins (and the eunit timeout fails the test);
%% with it, the request returns a fast {error, {bad_response, _}}.
h2_frame_fails_fast() ->
    Port = start_raw_server(?H2_SETTINGS),
    Pid = connect_tcp(Port),
    ?assertMatch({error, {bad_response, _}},
                 hackney_conn:request(Pid, <<"GET">>, <<"/">>, [], <<>>)),
    catch hackney_conn:stop(Pid).

normal_response_ok() ->
    Resp = <<"HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\n"
             "content-length: 2\r\n\r\nhi">>,
    Port = start_raw_server(Resp),
    Pid = connect_tcp(Port),
    ?assertMatch({ok, 200, _}, hackney_conn:request(Pid, <<"GET">>, <<"/">>, [], <<>>)),
    ?assertEqual({ok, <<"hi">>}, hackney_conn:body(Pid)),
    catch hackney_conn:stop(Pid).

%%====================================================================
%% Helpers
%%====================================================================

connect_tcp(Port) ->
    ConnOpts = #{host => "127.0.0.1", port => Port, transport => hackney_tcp,
                 connect_timeout => 5000, recv_timeout => 5000,
                 connect_options => [], ssl_options => []},
    {ok, Pid} = hackney_conn_sup:start_conn(ConnOpts),
    ok = hackney_conn:connect(Pid),
    Pid.

%% Accept one connection, read the request, send Reply, and hold the socket open
%% until told to stop (so the client sees Reply but not a close).
start_raw_server(Reply) ->
    Self = self(),
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    spawn_link(fun() ->
        {ok, Sock} = gen_tcp:accept(LSock, 5000),
        _ = gen_tcp:recv(Sock, 0, 5000),   %% consume the request line/headers
        ok = gen_tcp:send(Sock, Reply),
        Self ! {server_ready, self()},
        receive stop -> ok after 9000 -> ok end,
        gen_tcp:close(Sock),
        gen_tcp:close(LSock)
    end),
    Port.
