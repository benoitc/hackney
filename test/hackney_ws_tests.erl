%%% -*- erlang -*-
%%%
%%% WebSocket tests for hackney

-module(hackney_ws_tests).

-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

-define(PORT, 9876).
-define(WS_URL, "ws://localhost:9876/ws").

%% Start mock WebSocket server
start_ws_server() ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", mock_ws_handler, []},
            {"/ws/[...]", mock_ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(mock_ws_listener,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.

stop_ws_server() ->
    cowboy:stop_listener(mock_ws_listener),
    ok.

%%====================================================================
%% hackney_ws module tests (unit tests)
%%====================================================================

ws_url_parsing_test_() ->
    [
     {"Parse ws:// URL",
      fun() ->
          URL = hackney_url:parse_url(<<"ws://example.com/chat">>),
          ?assertEqual(ws, URL#hackney_url.scheme),
          ?assertEqual(hackney_tcp, URL#hackney_url.transport),
          ?assertEqual(80, URL#hackney_url.port),
          ?assertEqual("example.com", URL#hackney_url.host),
          ?assertEqual(<<"/chat">>, URL#hackney_url.path)
      end},
     {"Parse wss:// URL",
      fun() ->
          URL = hackney_url:parse_url(<<"wss://example.com/chat">>),
          ?assertEqual(wss, URL#hackney_url.scheme),
          ?assertEqual(hackney_ssl, URL#hackney_url.transport),
          ?assertEqual(443, URL#hackney_url.port),
          ?assertEqual("example.com", URL#hackney_url.host)
      end},
     {"Parse ws:// URL with custom port",
      fun() ->
          URL = hackney_url:parse_url(<<"ws://example.com:8080/chat">>),
          ?assertEqual(8080, URL#hackney_url.port)
      end},
     {"Parse ws:// URL with query string",
      fun() ->
          URL = hackney_url:parse_url(<<"ws://example.com/chat?token=abc">>),
          ?assertEqual(<<"/chat">>, URL#hackney_url.path),
          ?assertEqual(<<"token=abc">>, URL#hackney_url.qs)
      end}
    ].

%%====================================================================
%% Integration tests (require mock server)
%%====================================================================

ws_integration_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Connect and disconnect",
       fun test_connect_disconnect/0},
      {"Send and receive text message",
       fun test_text_message/0},
      {"Send and receive binary message",
       fun test_binary_message/0},
      {"Echo multiple messages",
       fun test_multiple_messages/0},
      {"Server close",
       fun test_server_close/0},
      {"Client close",
       fun test_client_close/0},
      {"Ping/pong",
       fun test_ping_pong/0},
      {"Active mode once",
       fun test_active_once/0},
      {"Active mode true",
       fun test_active_true/0}
     ]}.

test_connect_disconnect() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL),
    ?assert(is_pid(Ws)),
    ?assert(is_process_alive(Ws)),
    %% Close returns immediately (async)
    ok = hackney:ws_close(Ws),
    %% Just verify the close was accepted
    ok.

test_text_message() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL),
    ok = hackney:ws_send(Ws, {text, <<"Hello, WebSocket!">>}),
    {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
    ?assertEqual(<<"Hello, WebSocket!">>, Msg),
    hackney:ws_close(Ws).

test_binary_message() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL),
    BinData = <<1, 2, 3, 4, 5>>,
    ok = hackney:ws_send(Ws, {binary, BinData}),
    {ok, {binary, RecvData}} = hackney:ws_recv(Ws, 5000),
    ?assertEqual(BinData, RecvData),
    hackney:ws_close(Ws).

test_multiple_messages() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL),

    %% Send multiple messages
    ok = hackney:ws_send(Ws, {text, <<"msg1">>}),
    ok = hackney:ws_send(Ws, {text, <<"msg2">>}),
    ok = hackney:ws_send(Ws, {text, <<"msg3">>}),

    %% Receive them in order
    {ok, {text, <<"msg1">>}} = hackney:ws_recv(Ws, 5000),
    {ok, {text, <<"msg2">>}} = hackney:ws_recv(Ws, 5000),
    {ok, {text, <<"msg3">>}} = hackney:ws_recv(Ws, 5000),

    hackney:ws_close(Ws).

test_server_close() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL),
    %% Tell server to close
    ok = hackney:ws_send(Ws, {text, <<"close">>}),
    %% Should get close error
    Result = hackney:ws_recv(Ws, 5000),
    ?assertMatch({error, {closed, 1000, _}}, Result).

test_client_close() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL),
    %% Close with code and reason (async)
    ok = hackney:ws_close(Ws, {1000, <<"bye">>}),
    %% Verify close was accepted
    ok.

test_ping_pong() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL),
    %% Send a ping - server will respond with pong
    ok = hackney:ws_send(Ws, ping),
    %% We should receive the pong response
    Result = hackney:ws_recv(Ws, 5000),
    ?assertMatch({ok, pong}, Result),
    hackney:ws_close(Ws).

test_active_once() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL, [{active, once}]),

    %% Send a message
    ok = hackney:ws_send(Ws, {text, <<"active once test">>}),

    %% Should receive via message
    receive
        {hackney_ws, Ws, {text, Msg}} ->
            ?assertEqual(<<"active once test">>, Msg)
    after 5000 ->
        ?assert(false)
    end,

    %% After receiving one message, should be back to passive
    %% Need to request next message
    ok = hackney:ws_setopts(Ws, [{active, once}]),
    ok = hackney:ws_send(Ws, {text, <<"second">>}),
    receive
        {hackney_ws, Ws, {text, <<"second">>}} -> ok
    after 5000 ->
        ?assert(false)
    end,

    hackney:ws_close(Ws).

test_active_true() ->
    {ok, Ws} = hackney:ws_connect(?WS_URL, [{active, true}]),

    %% Send multiple messages
    ok = hackney:ws_send(Ws, {text, <<"a1">>}),
    ok = hackney:ws_send(Ws, {text, <<"a2">>}),

    %% Should receive all via messages
    receive
        {hackney_ws, Ws, {text, <<"a1">>}} -> ok
    after 5000 ->
        ?assert(false)
    end,
    receive
        {hackney_ws, Ws, {text, <<"a2">>}} -> ok
    after 5000 ->
        ?assert(false)
    end,

    hackney:ws_close(Ws).

%%====================================================================
%% Error handling tests
%%====================================================================

ws_error_test_() ->
    [
     {"Connect to non-existent server",
      fun() ->
          Result = hackney:ws_connect("ws://localhost:59999/ws"),
          ?assertMatch({error, _}, Result)
      end},
     {"Invalid scheme",
      fun() ->
          ?assertError({invalid_websocket_scheme, http},
                       hackney:ws_connect("http://localhost/ws"))
      end}
    ].

%%====================================================================
%% Proxy tests (require both mock WS server and mock proxy)
%%====================================================================

ws_proxy_test_() ->
    {setup,
     fun() ->
         start_ws_server(),
         {ok, ProxyPid, ProxyPort} = mock_proxy_server:start_connect_proxy(),
         {ProxyPid, ProxyPort}
     end,
     fun({ProxyPid, _ProxyPort}) ->
         mock_proxy_server:stop(ProxyPid),
         stop_ws_server()
     end,
     fun({_ProxyPid, ProxyPort}) ->
         [
          {"WebSocket through HTTP CONNECT proxy",
           fun() ->
               %% Connect through proxy using tuple config
               ProxyConfig = {connect, "localhost", ProxyPort},
               {ok, Ws} = hackney:ws_connect(?WS_URL, [{proxy, ProxyConfig}]),
               ?assert(is_pid(Ws)),

               %% Send and receive a message
               ok = hackney:ws_send(Ws, {text, <<"proxy test">>}),
               {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<"proxy test">>, Msg),

               hackney:ws_close(Ws)
           end},
          {"WebSocket through HTTP CONNECT proxy with URL config",
           fun() ->
               %% Connect through proxy using URL config
               ProxyUrl = iolist_to_binary(["http://localhost:", integer_to_list(ProxyPort)]),
               {ok, Ws} = hackney:ws_connect(?WS_URL, [{proxy, ProxyUrl}]),
               ?assert(is_pid(Ws)),

               %% Send and receive a message
               ok = hackney:ws_send(Ws, {text, <<"proxy url test">>}),
               {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<"proxy url test">>, Msg),

               hackney:ws_close(Ws)
           end}
         ]
     end}.

%%====================================================================
%% API tests (no server needed)
%%====================================================================

ws_api_test_() ->
    [
     {"ws_send to dead process throws noproc",
      fun() ->
          Pid = spawn(fun() -> ok end),
          timer:sleep(10),
          ?assertException(exit, {noproc, _}, hackney:ws_send(Pid, {text, <<"test">>}))
      end}
    ].

