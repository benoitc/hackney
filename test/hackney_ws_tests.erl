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
      end},
     {"Parse wss:// URL with path and query",
      fun() ->
          URL = hackney_url:parse_url(<<"wss://example.com:8443/api/ws?auth=token&v=2">>),
          ?assertEqual(wss, URL#hackney_url.scheme),
          ?assertEqual(8443, URL#hackney_url.port),
          ?assertEqual(<<"/api/ws">>, URL#hackney_url.path),
          ?assertEqual(<<"auth=token&v=2">>, URL#hackney_url.qs)
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
%% Large message tests
%%====================================================================

ws_large_message_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Send large text message (1KB)",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           LargeText = binary:copy(<<"A">>, 1024),
           ok = hackney:ws_send(Ws, {text, LargeText}),
           {ok, {text, Recv}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(LargeText, Recv),
           hackney:ws_close(Ws)
       end},
      {"Send large text message (64KB)",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           LargeText = binary:copy(<<"B">>, 65536),
           ok = hackney:ws_send(Ws, {text, LargeText}),
           {ok, {text, Recv}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(LargeText, Recv),
           hackney:ws_close(Ws)
       end},
      {"Send large binary message (1KB)",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           LargeBin = crypto:strong_rand_bytes(1024),
           ok = hackney:ws_send(Ws, {binary, LargeBin}),
           {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(LargeBin, Recv),
           hackney:ws_close(Ws)
       end},
      {"Send large binary message (64KB)",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           LargeBin = crypto:strong_rand_bytes(65536),
           ok = hackney:ws_send(Ws, {binary, LargeBin}),
           {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(LargeBin, Recv),
           hackney:ws_close(Ws)
       end},
      {"Receive large message from server",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           %% Ask server to send 32KB
           ok = hackney:ws_send(Ws, {text, <<"large:32768">>}),
           {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(32768, byte_size(Recv)),
           hackney:ws_close(Ws)
       end}
     ]}.

%%====================================================================
%% Query string and headers tests
%%====================================================================

ws_headers_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"URL with query string",
       fun() ->
           URL = "ws://localhost:9876/ws?token=abc123&user=test",
           {ok, Ws} = hackney:ws_connect(URL),
           ok = hackney:ws_send(Ws, {text, <<"get_qs">>}),
           {ok, {text, Qs}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(<<"token=abc123&user=test">>, Qs),
           hackney:ws_close(Ws)
       end},
      {"Custom headers in handshake",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL, [
               {headers, [{<<"X-Custom-Header">>, <<"custom-value">>}]}
           ]),
           ok = hackney:ws_send(Ws, {text, <<"get_headers">>}),
           {ok, {text, HeadersStr}} = hackney:ws_recv(Ws, 5000),
           %% Check that our custom header is present
           ?assertNotEqual(nomatch, binary:match(HeadersStr, <<"x-custom-header">>)),
           ?assertNotEqual(nomatch, binary:match(HeadersStr, <<"custom-value">>)),
           hackney:ws_close(Ws)
       end},
      {"Multiple custom headers",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL, [
               {headers, [
                   {<<"X-Header-1">>, <<"value1">>},
                   {<<"X-Header-2">>, <<"value2">>}
               ]}
           ]),
           ok = hackney:ws_send(Ws, {text, <<"get_headers">>}),
           {ok, {text, HeadersStr}} = hackney:ws_recv(Ws, 5000),
           ?assertNotEqual(nomatch, binary:match(HeadersStr, <<"x-header-1">>)),
           ?assertNotEqual(nomatch, binary:match(HeadersStr, <<"x-header-2">>)),
           hackney:ws_close(Ws)
       end}
     ]}.

%%====================================================================
%% Close code tests
%%====================================================================

ws_close_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Server close with code 1001 (going away)",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           ok = hackney:ws_send(Ws, {text, <<"close:1001">>}),
           Result = hackney:ws_recv(Ws, 5000),
           ?assertMatch({error, {closed, 1001, _}}, Result)
       end},
      {"Server close with code 1008 (policy violation)",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           ok = hackney:ws_send(Ws, {text, <<"close:1008">>}),
           Result = hackney:ws_recv(Ws, 5000),
           ?assertMatch({error, {closed, 1008, _}}, Result)
       end},
      {"Client close with custom code",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           ok = hackney:ws_close(Ws, {1001, <<"going away">>}),
           ok
       end}
     ]}.

%%====================================================================
%% Active mode switching tests
%%====================================================================

ws_mode_switching_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Switch from passive to active",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL, [{active, false}]),

           %% Passive mode - use recv
           ok = hackney:ws_send(Ws, {text, <<"passive1">>}),
           {ok, {text, <<"passive1">>}} = hackney:ws_recv(Ws, 5000),

           %% Switch to active mode
           ok = hackney:ws_setopts(Ws, [{active, true}]),
           ok = hackney:ws_send(Ws, {text, <<"active1">>}),
           receive
               {hackney_ws, Ws, {text, <<"active1">>}} -> ok
           after 5000 ->
               ?assert(false)
           end,

           hackney:ws_close(Ws)
       end},
      {"Switch from active to passive",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL, [{active, true}]),

           %% Active mode
           ok = hackney:ws_send(Ws, {text, <<"active1">>}),
           receive
               {hackney_ws, Ws, {text, <<"active1">>}} -> ok
           after 5000 ->
               ?assert(false)
           end,

           %% Switch to passive mode
           ok = hackney:ws_setopts(Ws, [{active, false}]),
           ok = hackney:ws_send(Ws, {text, <<"passive1">>}),
           {ok, {text, <<"passive1">>}} = hackney:ws_recv(Ws, 5000),

           hackney:ws_close(Ws)
       end},
      {"Recv in active mode returns error",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL, [{active, true}]),
           Result = hackney:ws_recv(Ws, 100),
           ?assertMatch({error, {active_mode, true}}, Result),
           hackney:ws_close(Ws)
       end}
     ]}.

%%====================================================================
%% Ping/pong advanced tests
%%====================================================================

ws_ping_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Ping with data",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           PingData = <<"ping-data-123">>,
           ok = hackney:ws_send(Ws, {ping, PingData}),
           %% Should receive pong with same data
           {ok, {pong, PongData}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(PingData, PongData),
           hackney:ws_close(Ws)
       end},
      {"Multiple pings",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           ok = hackney:ws_send(Ws, ping),
           ok = hackney:ws_send(Ws, ping),
           ok = hackney:ws_send(Ws, ping),
           {ok, pong} = hackney:ws_recv(Ws, 5000),
           {ok, pong} = hackney:ws_recv(Ws, 5000),
           {ok, pong} = hackney:ws_recv(Ws, 5000),
           hackney:ws_close(Ws)
       end},
      {"Send pong frame",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           %% Sending pong is valid (unsolicited pong)
           ok = hackney:ws_send(Ws, pong),
           ok = hackney:ws_send(Ws, {pong, <<"data">>}),
           %% Should still work
           ok = hackney:ws_send(Ws, {text, <<"test">>}),
           {ok, {text, <<"test">>}} = hackney:ws_recv(Ws, 5000),
           hackney:ws_close(Ws)
       end}
     ]}.

%%====================================================================
%% Concurrent connections tests
%%====================================================================

ws_concurrent_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Multiple concurrent connections",
       fun() ->
           %% Open 5 connections
           Conns = [begin
               {ok, Ws} = hackney:ws_connect(?WS_URL),
               Ws
           end || _ <- lists:seq(1, 5)],

           %% Send message on each
           lists:foreach(fun(Ws) ->
               ok = hackney:ws_send(Ws, {text, <<"concurrent">>})
           end, Conns),

           %% Receive from each
           lists:foreach(fun(Ws) ->
               {ok, {text, <<"concurrent">>}} = hackney:ws_recv(Ws, 5000)
           end, Conns),

           %% Close all
           lists:foreach(fun(Ws) ->
               hackney:ws_close(Ws)
           end, Conns)
       end},
      {"Rapid connect/disconnect",
       fun() ->
           lists:foreach(fun(_) ->
               {ok, Ws} = hackney:ws_connect(?WS_URL),
               hackney:ws_close(Ws)
           end, lists:seq(1, 10))
       end}
     ]}.

%%====================================================================
%% Socket info tests
%%====================================================================

ws_socket_info_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Get peername",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           Result = hackney_ws:peername(Ws),
           ?assertMatch({ok, {{127, 0, 0, 1}, ?PORT}}, Result),
           hackney:ws_close(Ws)
       end},
      {"Get sockname",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           Result = hackney_ws:sockname(Ws),
           ?assertMatch({ok, {{127, 0, 0, 1}, _Port}}, Result),
           hackney:ws_close(Ws)
       end}
     ]}.

%%====================================================================
%% Controlling process tests
%%====================================================================

ws_controlling_process_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Transfer controlling process",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL, [{active, true}]),

           %% Spawn a new process to take over
           Parent = self(),
           NewOwner = spawn_link(fun() ->
               receive
                   start ->
                       ok = hackney:ws_send(Ws, {text, <<"from new owner">>}),
                       receive
                           {hackney_ws, Ws, {text, <<"from new owner">>}} ->
                               Parent ! {self(), ok}
                       after 5000 ->
                           Parent ! {self(), timeout}
                       end
               end
           end),

           %% Transfer ownership
           ok = hackney_ws:controlling_process(Ws, NewOwner),

           %% Signal new owner to start
           NewOwner ! start,

           %% Wait for result
           receive
               {NewOwner, ok} -> ok;
               {NewOwner, timeout} -> ?assert(false)
           after 6000 ->
               ?assert(false)
           end,

           hackney:ws_close(Ws)
       end}
     ]}.

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
      end},
     {"Connect timeout",
      fun() ->
          %% Use a non-routable IP to trigger timeout
          Result = hackney:ws_connect("ws://10.255.255.1/ws", [
              {connect_timeout, 100}
          ]),
          ?assertMatch({error, _}, Result)
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
           end},
          {"WebSocket through proxy with large message",
           fun() ->
               ProxyConfig = {connect, "localhost", ProxyPort},
               {ok, Ws} = hackney:ws_connect(?WS_URL, [{proxy, ProxyConfig}]),

               %% Send large message through proxy
               LargeMsg = binary:copy(<<"P">>, 8192),
               ok = hackney:ws_send(Ws, {text, LargeMsg}),
               {ok, {text, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(LargeMsg, Recv),

               hackney:ws_close(Ws)
           end}
         ]
     end}.

ws_socks5_proxy_test_() ->
    {setup,
     fun() ->
         start_ws_server(),
         {ok, ProxyPid, ProxyPort} = mock_proxy_server:start_socks5_proxy(),
         {ProxyPid, ProxyPort}
     end,
     fun({ProxyPid, _ProxyPort}) ->
         mock_proxy_server:stop(ProxyPid),
         stop_ws_server()
     end,
     fun({_ProxyPid, ProxyPort}) ->
         [
          {"WebSocket through SOCKS5 proxy",
           fun() ->
               ProxyConfig = {socks5, "localhost", ProxyPort},
               {ok, Ws} = hackney:ws_connect(?WS_URL, [{proxy, ProxyConfig}]),
               ?assert(is_pid(Ws)),

               ok = hackney:ws_send(Ws, {text, <<"socks5 test">>}),
               {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<"socks5 test">>, Msg),

               hackney:ws_close(Ws)
           end},
          {"WebSocket through SOCKS5 proxy with URL",
           fun() ->
               ProxyUrl = iolist_to_binary(["socks5://localhost:", integer_to_list(ProxyPort)]),
               {ok, Ws} = hackney:ws_connect(?WS_URL, [{proxy, ProxyUrl}]),
               ?assert(is_pid(Ws)),

               ok = hackney:ws_send(Ws, {text, <<"socks5 url test">>}),
               {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<"socks5 url test">>, Msg),

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
      end},
     {"ws_recv to dead process throws noproc",
      fun() ->
          Pid = spawn(fun() -> ok end),
          timer:sleep(10),
          ?assertException(exit, {noproc, _}, hackney:ws_recv(Pid))
      end},
     {"ws_setopts to dead process throws noproc",
      fun() ->
          Pid = spawn(fun() -> ok end),
          timer:sleep(10),
          ?assertException(exit, {noproc, _}, hackney:ws_setopts(Pid, [{active, true}]))
      end}
    ].

%%====================================================================
%% Binary/text frame type tests
%%====================================================================

ws_frame_types_test_() ->
    {setup,
     fun start_ws_server/0,
     fun(_) -> stop_ws_server() end,
     [
      {"Empty text message",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           ok = hackney:ws_send(Ws, {text, <<>>}),
           {ok, {text, <<>>}} = hackney:ws_recv(Ws, 5000),
           hackney:ws_close(Ws)
       end},
      {"Empty binary message",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           ok = hackney:ws_send(Ws, {binary, <<>>}),
           {ok, {binary, <<>>}} = hackney:ws_recv(Ws, 5000),
           hackney:ws_close(Ws)
       end},
      {"Unicode text message",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           Unicode = <<"Hello 世界 🌍"/utf8>>,
           ok = hackney:ws_send(Ws, {text, Unicode}),
           {ok, {text, Recv}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(Unicode, Recv),
           hackney:ws_close(Ws)
       end},
      {"Binary with null bytes",
       fun() ->
           {ok, Ws} = hackney:ws_connect(?WS_URL),
           BinWithNulls = <<0, 1, 0, 2, 0, 3, 0>>,
           ok = hackney:ws_send(Ws, {binary, BinWithNulls}),
           {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
           ?assertEqual(BinWithNulls, Recv),
           hackney:ws_close(Ws)
       end}
     ]}.
