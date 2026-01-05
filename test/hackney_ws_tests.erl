%%% -*- erlang -*-
%%%
%%% WebSocket tests for hackney

-module(hackney_ws_tests).

-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

%% Start mock WebSocket server with dynamic port
start_ws_server() ->
    _ = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", mock_ws_handler, []},
            {"/ws/[...]", mock_ws_handler, []}
        ]}
    ]),
    %% Use unique listener name with unique integer to avoid conflicts
    UniqueId = erlang:unique_integer([positive]),
    ListenerName = list_to_atom("mock_ws_listener_" ++ integer_to_list(UniqueId)),
    %% Use port 0 to let OS assign an available port
    {ok, _} = cowboy:start_clear(ListenerName,
        [{port, 0}],
        #{env => #{dispatch => Dispatch}}
    ),
    %% Get the actual port that was assigned
    {_, Port} = ranch:get_addr(ListenerName),
    WsUrl = "ws://localhost:" ++ integer_to_list(Port) ++ "/ws",
    {ListenerName, Port, WsUrl}.

stop_ws_server({ListenerName, _Port, _WsUrl}) ->
    cowboy:stop_listener(ListenerName),
    ok;
stop_ws_server(_) ->
    %% Handle legacy case (if called with just 'ok')
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
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Connect and disconnect",
           fun() -> test_connect_disconnect(WsUrl) end},
          {"Send and receive text message",
           fun() -> test_text_message(WsUrl) end},
          {"Send and receive binary message",
           fun() -> test_binary_message(WsUrl) end},
          {"Echo multiple messages",
           fun() -> test_multiple_messages(WsUrl) end},
          {"Server close",
           fun() -> test_server_close(WsUrl) end},
          {"Client close",
           fun() -> test_client_close(WsUrl) end},
          {"Ping/pong",
           fun() -> test_ping_pong(WsUrl) end},
          {"Active mode once",
           fun() -> test_active_once(WsUrl) end},
          {"Active mode true",
           fun() -> test_active_true(WsUrl) end}
         ]
     end}.

test_connect_disconnect(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl),
    ?assert(is_pid(Ws)),
    ?assert(is_process_alive(Ws)),
    %% Close returns immediately (async)
    ok = hackney:ws_close(Ws),
    %% Just verify the close was accepted
    ok.

test_text_message(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl),
    ok = hackney:ws_send(Ws, {text, <<"Hello, WebSocket!">>}),
    {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
    ?assertEqual(<<"Hello, WebSocket!">>, Msg),
    hackney:ws_close(Ws).

test_binary_message(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl),
    BinData = <<1, 2, 3, 4, 5>>,
    ok = hackney:ws_send(Ws, {binary, BinData}),
    {ok, {binary, RecvData}} = hackney:ws_recv(Ws, 5000),
    ?assertEqual(BinData, RecvData),
    hackney:ws_close(Ws).

test_multiple_messages(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl),

    %% Send multiple messages
    ok = hackney:ws_send(Ws, {text, <<"msg1">>}),
    ok = hackney:ws_send(Ws, {text, <<"msg2">>}),
    ok = hackney:ws_send(Ws, {text, <<"msg3">>}),

    %% Receive them in order
    {ok, {text, <<"msg1">>}} = hackney:ws_recv(Ws, 5000),
    {ok, {text, <<"msg2">>}} = hackney:ws_recv(Ws, 5000),
    {ok, {text, <<"msg3">>}} = hackney:ws_recv(Ws, 5000),

    hackney:ws_close(Ws).

test_server_close(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl),
    %% Tell server to close
    ok = hackney:ws_send(Ws, {text, <<"close">>}),
    %% Should get close error
    Result = hackney:ws_recv(Ws, 5000),
    ?assertMatch({error, {closed, 1000, _}}, Result).

test_client_close(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl),
    %% Close with code and reason (async)
    ok = hackney:ws_close(Ws, {1000, <<"bye">>}),
    %% Verify close was accepted
    ok.

test_ping_pong(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl),
    %% Send a ping - server will respond with pong
    ok = hackney:ws_send(Ws, ping),
    %% We should receive the pong response
    Result = hackney:ws_recv(Ws, 5000),
    ?assertMatch({ok, pong}, Result),
    hackney:ws_close(Ws).

test_active_once(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl, [{active, once}]),

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

test_active_true(WsUrl) ->
    {ok, Ws} = hackney:ws_connect(WsUrl, [{active, true}]),

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
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Send large text message (1KB)",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               LargeText = binary:copy(<<"A">>, 1024),
               ok = hackney:ws_send(Ws, {text, LargeText}),
               {ok, {text, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(LargeText, Recv),
               hackney:ws_close(Ws)
           end},
          {"Send large text message (64KB)",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               LargeText = binary:copy(<<"B">>, 65536),
               ok = hackney:ws_send(Ws, {text, LargeText}),
               {ok, {text, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(LargeText, Recv),
               hackney:ws_close(Ws)
           end},
          {"Send large binary message (1KB)",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               LargeBin = crypto:strong_rand_bytes(1024),
               ok = hackney:ws_send(Ws, {binary, LargeBin}),
               {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(LargeBin, Recv),
               hackney:ws_close(Ws)
           end},
          {"Send large binary message (64KB)",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               LargeBin = crypto:strong_rand_bytes(65536),
               ok = hackney:ws_send(Ws, {binary, LargeBin}),
               {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(LargeBin, Recv),
               hackney:ws_close(Ws)
           end},
          {"Receive large message from server",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Ask server to send 32KB
               ok = hackney:ws_send(Ws, {text, <<"large:32768">>}),
               {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(32768, byte_size(Recv)),
               hackney:ws_close(Ws)
           end}
         ]
     end}.

%%====================================================================
%% Query string and headers tests
%%====================================================================

ws_headers_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, Port, WsUrl}) ->
         [
          {"URL with query string",
           fun() ->
               URL = "ws://localhost:" ++ integer_to_list(Port) ++ "/ws?token=abc123&user=test",
               {ok, Ws} = hackney:ws_connect(URL),
               ok = hackney:ws_send(Ws, {text, <<"get_qs">>}),
               {ok, {text, Qs}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<"token=abc123&user=test">>, Qs),
               hackney:ws_close(Ws)
           end},
          {"Custom headers in handshake",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl, [
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
               {ok, Ws} = hackney:ws_connect(WsUrl, [
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
         ]
     end}.

%%====================================================================
%% Close code tests
%%====================================================================

ws_close_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Server close with code 1001 (going away)",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws, {text, <<"close:1001">>}),
               Result = hackney:ws_recv(Ws, 5000),
               ?assertMatch({error, {closed, 1001, _}}, Result)
           end},
          {"Server close with code 1008 (policy violation)",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws, {text, <<"close:1008">>}),
               Result = hackney:ws_recv(Ws, 5000),
               ?assertMatch({error, {closed, 1008, _}}, Result)
           end},
          {"Client close with custom code",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_close(Ws, {1001, <<"going away">>}),
               ok
           end}
         ]
     end}.

%%====================================================================
%% Active mode switching tests
%%====================================================================

ws_mode_switching_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Switch from passive to active",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl, [{active, false}]),

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
               {ok, Ws} = hackney:ws_connect(WsUrl, [{active, true}]),

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
               {ok, Ws} = hackney:ws_connect(WsUrl, [{active, true}]),
               Result = hackney:ws_recv(Ws, 100),
               ?assertMatch({error, {active_mode, true}}, Result),
               hackney:ws_close(Ws)
           end}
         ]
     end}.

%%====================================================================
%% Ping/pong advanced tests
%%====================================================================

ws_ping_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Ping with data",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               PingData = <<"ping-data-123">>,
               ok = hackney:ws_send(Ws, {ping, PingData}),
               %% Should receive pong with same data
               {ok, {pong, PongData}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(PingData, PongData),
               hackney:ws_close(Ws)
           end},
          {"Multiple pings",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
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
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Sending pong is valid (unsolicited pong)
               ok = hackney:ws_send(Ws, pong),
               ok = hackney:ws_send(Ws, {pong, <<"data">>}),
               %% Should still work
               ok = hackney:ws_send(Ws, {text, <<"test">>}),
               {ok, {text, <<"test">>}} = hackney:ws_recv(Ws, 5000),
               hackney:ws_close(Ws)
           end}
         ]
     end}.

%%====================================================================
%% Concurrent connections tests
%%====================================================================

ws_concurrent_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Multiple concurrent connections",
           fun() ->
               %% Open 5 connections
               Conns = [begin
                   {ok, Ws} = hackney:ws_connect(WsUrl),
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
                   {ok, Ws} = hackney:ws_connect(WsUrl),
                   hackney:ws_close(Ws)
               end, lists:seq(1, 10))
           end}
         ]
     end}.

%%====================================================================
%% Socket info tests
%%====================================================================

ws_socket_info_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, Port, WsUrl}) ->
         [
          {"Get peername",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               Result = hackney_ws:peername(Ws),
               ?assertMatch({ok, {{127, 0, 0, 1}, Port}}, Result),
               hackney:ws_close(Ws)
           end},
          {"Get sockname",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               Result = hackney_ws:sockname(Ws),
               ?assertMatch({ok, {{127, 0, 0, 1}, _ClientPort}}, Result),
               hackney:ws_close(Ws)
           end}
         ]
     end}.

%%====================================================================
%% Controlling process tests
%%====================================================================

ws_controlling_process_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Transfer controlling process",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl, [{active, true}]),

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
         ]
     end}.

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
         {_ListenerName, _WsPort, WsUrl} = start_ws_server(),
         {ok, ProxyPid, ProxyPort} = mock_proxy_server:start_connect_proxy(),
         {ProxyPid, ProxyPort, WsUrl}
     end,
     fun({ProxyPid, _ProxyPort, _WsUrl}) ->
         mock_proxy_server:stop(ProxyPid),
         %% Note: WS server cleanup is handled by the unique listener name
         ok
     end,
     fun({_ProxyPid, ProxyPort, WsUrl}) ->
         [
          {"WebSocket through HTTP CONNECT proxy",
           fun() ->
               %% Connect through proxy using tuple config
               ProxyConfig = {connect, "localhost", ProxyPort},
               {ok, Ws} = hackney:ws_connect(WsUrl, [{proxy, ProxyConfig}]),
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
               {ok, Ws} = hackney:ws_connect(WsUrl, [{proxy, ProxyUrl}]),
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
               {ok, Ws} = hackney:ws_connect(WsUrl, [{proxy, ProxyConfig}]),

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
         {_ListenerName, _WsPort, WsUrl} = start_ws_server(),
         {ok, ProxyPid, ProxyPort} = mock_proxy_server:start_socks5_proxy(),
         {ProxyPid, ProxyPort, WsUrl}
     end,
     fun({ProxyPid, _ProxyPort, _WsUrl}) ->
         mock_proxy_server:stop(ProxyPid),
         %% Note: WS server cleanup is handled by the unique listener name
         ok
     end,
     fun({_ProxyPid, ProxyPort, WsUrl}) ->
         [
          {"WebSocket through SOCKS5 proxy",
           fun() ->
               ProxyConfig = {socks5, "localhost", ProxyPort},
               {ok, Ws} = hackney:ws_connect(WsUrl, [{proxy, ProxyConfig}]),
               ?assert(is_pid(Ws)),

               ok = hackney:ws_send(Ws, {text, <<"socks5 test">>}),
               {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<"socks5 test">>, Msg),

               hackney:ws_close(Ws)
           end},
          {"WebSocket through SOCKS5 proxy with URL",
           fun() ->
               ProxyUrl = iolist_to_binary(["socks5://localhost:", integer_to_list(ProxyPort)]),
               {ok, Ws} = hackney:ws_connect(WsUrl, [{proxy, ProxyUrl}]),
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
%% Scripted interaction tests (fake messages/scenarios)
%%====================================================================

ws_scripted_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Delayed response from server",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Ask server to wait 100ms then respond
               ok = hackney:ws_send(Ws, {text, <<"delay:100:delayed_msg">>}),
               T1 = erlang:monotonic_time(millisecond),
               {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
               T2 = erlang:monotonic_time(millisecond),
               ?assertEqual(<<"delayed_msg">>, Msg),
               %% Verify delay occurred (at least 50ms to account for timing variance)
               ?assert((T2 - T1) >= 50),
               hackney:ws_close(Ws)
           end},
          {"Multiple messages from single command",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Ask server to send 5 messages
               ok = hackney:ws_send(Ws, {text, <<"multi:5">>}),
               %% Receive all 5
               {ok, {text, <<"1">>}} = hackney:ws_recv(Ws, 5000),
               {ok, {text, <<"2">>}} = hackney:ws_recv(Ws, 5000),
               {ok, {text, <<"3">>}} = hackney:ws_recv(Ws, 5000),
               {ok, {text, <<"4">>}} = hackney:ws_recv(Ws, 5000),
               {ok, {text, <<"5">>}} = hackney:ws_recv(Ws, 5000),
               hackney:ws_close(Ws)
           end},
          {"Multiple messages in active mode",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl, [{active, true}]),
               ok = hackney:ws_send(Ws, {text, <<"multi:3">>}),
               %% Receive all 3 via messages
               Received = receive_n_messages(Ws, 3, 5000),
               ?assertEqual([{text, <<"1">>}, {text, <<"2">>}, {text, <<"3">>}], Received),
               hackney:ws_close(Ws)
           end},
          {"Server-initiated ping in active mode",
           fun() ->
               %% In passive mode, pings are auto-handled (pong sent) but not delivered
               %% to the caller. Use active mode to receive ping frames.
               {ok, Ws} = hackney:ws_connect(WsUrl, [{active, true}]),
               %% Tell server to send us a ping
               ok = hackney:ws_send(Ws, {text, <<"ping">>}),
               %% We should receive the ping (pong is auto-sent)
               receive
                   {hackney_ws, Ws, ping} -> ok
               after 5000 ->
                   ?assert(false)
               end,
               hackney:ws_close(Ws)
           end},
          {"Server-initiated ping with data in active mode",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl, [{active, true}]),
               %% Tell server to ping us with specific data
               ok = hackney:ws_send(Ws, {text, <<"ping:heartbeat-123">>}),
               receive
                   {hackney_ws, Ws, {ping, Data}} ->
                       ?assertEqual(<<"heartbeat-123">>, Data)
               after 5000 ->
                   ?assert(false)
               end,
               hackney:ws_close(Ws)
           end},
          {"Echo command",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws, {text, <<"echo:specific message">>}),
               {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<"specific message">>, Msg),
               hackney:ws_close(Ws)
           end},
          {"Conversation sequence - request/response pattern",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Simulate a typical conversation
               ok = hackney:ws_send(Ws, {text, <<"echo:login:user1">>}),
               {ok, {text, <<"login:user1">>}} = hackney:ws_recv(Ws, 5000),

               ok = hackney:ws_send(Ws, {text, <<"echo:subscribe:channel1">>}),
               {ok, {text, <<"subscribe:channel1">>}} = hackney:ws_recv(Ws, 5000),

               ok = hackney:ws_send(Ws, {text, <<"echo:message:hello">>}),
               {ok, {text, <<"message:hello">>}} = hackney:ws_recv(Ws, 5000),

               ok = hackney:ws_send(Ws, {text, <<"echo:unsubscribe:channel1">>}),
               {ok, {text, <<"unsubscribe:channel1">>}} = hackney:ws_recv(Ws, 5000),

               hackney:ws_close(Ws)
           end},
          {"Rapid message exchange",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Send 100 messages rapidly
               Messages = [iolist_to_binary([<<"msg">>, integer_to_binary(N)]) || N <- lists:seq(1, 100)],
               lists:foreach(fun(Msg) ->
                   ok = hackney:ws_send(Ws, {text, Msg})
               end, Messages),
               %% Receive all 100
               lists:foreach(fun(Expected) ->
                   {ok, {text, Received}} = hackney:ws_recv(Ws, 5000),
                   ?assertEqual(Expected, Received)
               end, Messages),
               hackney:ws_close(Ws)
           end},
          {"Interleaved text and binary messages",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Send alternating text and binary
               ok = hackney:ws_send(Ws, {text, <<"text1">>}),
               ok = hackney:ws_send(Ws, {binary, <<1,2,3>>}),
               ok = hackney:ws_send(Ws, {text, <<"text2">>}),
               ok = hackney:ws_send(Ws, {binary, <<4,5,6>>}),
               %% Receive in order
               {ok, {text, <<"text1">>}} = hackney:ws_recv(Ws, 5000),
               {ok, {binary, <<1,2,3>>}} = hackney:ws_recv(Ws, 5000),
               {ok, {text, <<"text2">>}} = hackney:ws_recv(Ws, 5000),
               {ok, {binary, <<4,5,6>>}} = hackney:ws_recv(Ws, 5000),
               hackney:ws_close(Ws)
           end},
          {"Ping between messages",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws, {text, <<"msg1">>}),
               {ok, {text, <<"msg1">>}} = hackney:ws_recv(Ws, 5000),
               %% Send a ping mid-conversation
               ok = hackney:ws_send(Ws, ping),
               {ok, pong} = hackney:ws_recv(Ws, 5000),
               %% Continue conversation
               ok = hackney:ws_send(Ws, {text, <<"msg2">>}),
               {ok, {text, <<"msg2">>}} = hackney:ws_recv(Ws, 5000),
               hackney:ws_close(Ws)
           end}
         ]
     end}.

%% Helper to receive N messages in active mode
receive_n_messages(Ws, N, Timeout) ->
    receive_n_messages(Ws, N, Timeout, []).

receive_n_messages(_Ws, 0, _Timeout, Acc) ->
    lists:reverse(Acc);
receive_n_messages(Ws, N, Timeout, Acc) ->
    receive
        {hackney_ws, Ws, Frame} ->
            receive_n_messages(Ws, N - 1, Timeout, [Frame | Acc])
    after Timeout ->
        {error, {timeout, N, lists:reverse(Acc)}}
    end.

%%====================================================================
%% Reconnection and recovery tests
%%====================================================================

ws_recovery_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Reconnect after server close",
           fun() ->
               {ok, Ws1} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws1, {text, <<"test1">>}),
               {ok, {text, <<"test1">>}} = hackney:ws_recv(Ws1, 5000),

               %% Server closes the connection
               ok = hackney:ws_send(Ws1, {text, <<"close">>}),
               {error, {closed, 1000, _}} = hackney:ws_recv(Ws1, 5000),

               %% Reconnect (new connection)
               {ok, Ws2} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws2, {text, <<"test2">>}),
               {ok, {text, <<"test2">>}} = hackney:ws_recv(Ws2, 5000),
               hackney:ws_close(Ws2)
           end},
          {"Handle multiple server closes",
           fun() ->
               lists:foreach(fun(N) ->
                   {ok, Ws} = hackney:ws_connect(WsUrl),
                   Msg = iolist_to_binary([<<"iter">>, integer_to_binary(N)]),
                   ok = hackney:ws_send(Ws, {text, Msg}),
                   {ok, {text, Msg}} = hackney:ws_recv(Ws, 5000),
                   ok = hackney:ws_send(Ws, {text, <<"close">>}),
                   {error, {closed, 1000, _}} = hackney:ws_recv(Ws, 5000)
               end, lists:seq(1, 5))
           end}
         ]
     end}.

%%====================================================================
%% JSON-like messaging tests (simulating real protocols)
%%====================================================================

ws_protocol_simulation_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Simulate JSON-RPC style messaging",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Simulate JSON-RPC request
               Req1 = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
               ok = hackney:ws_send(Ws, {text, Req1}),
               {ok, {text, Req1}} = hackney:ws_recv(Ws, 5000),

               Req2 = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notify\"}">>,
               ok = hackney:ws_send(Ws, {text, Req2}),
               {ok, {text, Req2}} = hackney:ws_recv(Ws, 5000),

               hackney:ws_close(Ws)
           end},
          {"Simulate pub/sub messaging",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Subscribe
               ok = hackney:ws_send(Ws, {text, <<"{\"type\":\"subscribe\",\"channel\":\"news\"}">>}),
               {ok, {text, _}} = hackney:ws_recv(Ws, 5000),

               %% Publish
               ok = hackney:ws_send(Ws, {text, <<"{\"type\":\"publish\",\"channel\":\"news\",\"data\":\"hello\"}">>}),
               {ok, {text, _}} = hackney:ws_recv(Ws, 5000),

               %% Unsubscribe
               ok = hackney:ws_send(Ws, {text, <<"{\"type\":\"unsubscribe\",\"channel\":\"news\"}">>}),
               {ok, {text, _}} = hackney:ws_recv(Ws, 5000),

               hackney:ws_close(Ws)
           end},
          {"Binary protocol simulation (length-prefixed)",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               %% Simulate length-prefixed binary protocol
               Payload1 = <<"command1">>,
               Len1 = byte_size(Payload1),
               ok = hackney:ws_send(Ws, {binary, <<Len1:32, Payload1/binary>>}),
               {ok, {binary, <<Len1:32, Payload1/binary>>}} = hackney:ws_recv(Ws, 5000),

               Payload2 = crypto:strong_rand_bytes(256),
               Len2 = byte_size(Payload2),
               ok = hackney:ws_send(Ws, {binary, <<Len2:32, Payload2/binary>>}),
               {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(<<Len2:32, Payload2/binary>>, Recv),

               hackney:ws_close(Ws)
           end}
         ]
     end}.

%%====================================================================
%% Binary/text frame type tests
%%====================================================================

ws_frame_types_test_() ->
    {setup,
     fun start_ws_server/0,
     fun stop_ws_server/1,
     fun({_ListenerName, _Port, WsUrl}) ->
         [
          {"Empty text message",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws, {text, <<>>}),
               {ok, {text, <<>>}} = hackney:ws_recv(Ws, 5000),
               hackney:ws_close(Ws)
           end},
          {"Empty binary message",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               ok = hackney:ws_send(Ws, {binary, <<>>}),
               {ok, {binary, <<>>}} = hackney:ws_recv(Ws, 5000),
               hackney:ws_close(Ws)
           end},
          {"Unicode text message",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               Unicode = <<"Hello 世界 🌍"/utf8>>,
               ok = hackney:ws_send(Ws, {text, Unicode}),
               {ok, {text, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(Unicode, Recv),
               hackney:ws_close(Ws)
           end},
          {"Binary with null bytes",
           fun() ->
               {ok, Ws} = hackney:ws_connect(WsUrl),
               BinWithNulls = <<0, 1, 0, 2, 0, 3, 0>>,
               ok = hackney:ws_send(Ws, {binary, BinWithNulls}),
               {ok, {binary, Recv}} = hackney:ws_recv(Ws, 5000),
               ?assertEqual(BinWithNulls, Recv),
               hackney:ws_close(Ws)
           end}
         ]
     end}.
