%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025 Benoît Chesneau <bchesneau@gmail.com>
%%%

-module(hackney_proxy_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test empty proxy environment variables handling
empty_proxy_env_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
         [
          {"Empty http_proxy", fun() -> test_empty_proxy_env("http_proxy", State) end},
          {"Empty HTTP_PROXY", fun() -> test_empty_proxy_env("HTTP_PROXY", State) end},
          {"Empty https_proxy", fun() -> test_empty_proxy_env("https_proxy", State) end},
          {"Empty HTTPS_PROXY", fun() -> test_empty_proxy_env("HTTPS_PROXY", State) end},
          {"Whitespace http_proxy", fun() -> test_whitespace_proxy_env("http_proxy", State) end},
          {"Whitespace HTTPS_PROXY", fun() -> test_whitespace_proxy_env("HTTPS_PROXY", State) end},
          {"Multiple spaces proxy", fun() -> test_multiple_spaces_proxy_env("http_proxy", State) end},
          {"Valid proxy URL", fun() -> test_valid_proxy_env("http_proxy", State) end},
          {"Valid proxy with spaces", fun() -> test_valid_proxy_with_spaces("HTTP_PROXY", State) end}
         ]
     end}.

setup() ->
    %% Save current environment variables
    SavedEnv = [{Var, os:getenv(Var)} || Var <- ["http_proxy", "HTTP_PROXY", 
                                                  "https_proxy", "HTTPS_PROXY",
                                                  "all_proxy", "ALL_PROXY"]],
    %% Clear all proxy environment variables
    [os:unsetenv(Var) || {Var, _} <- SavedEnv],
    %% Clear hackney's cached proxy settings
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    SavedEnv.

teardown(SavedEnv) ->
    %% Restore original environment variables
    lists:foreach(fun
        ({Var, false}) -> os:unsetenv(Var);
        ({Var, Value}) -> os:putenv(Var, Value)
    end, SavedEnv),
    %% Clear hackney's cached proxy settings
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy).

test_empty_proxy_env(Var, _State) ->
    os:putenv(Var, ""),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should not crash and should return false (no proxy)
    Result = hackney:get_proxy_env(http),
    ?assertEqual(false, Result),
    os:unsetenv(Var).

test_whitespace_proxy_env(Var, _State) ->
    os:putenv(Var, "   "),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should not crash and should return false (no proxy)
    Result = hackney:get_proxy_env(http),
    ?assertEqual(false, Result),
    os:unsetenv(Var).

test_multiple_spaces_proxy_env(Var, _State) ->
    os:putenv(Var, "     \t   \n   "),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should not crash and should return false (no proxy)
    Result = hackney:get_proxy_env(http),
    ?assertEqual(false, Result),
    os:unsetenv(Var).

test_valid_proxy_env(Var, _State) ->
    ProxyUrl = "http://proxy.example.com:8080",
    os:putenv(Var, ProxyUrl),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should return the proxy URL
    Result = hackney:get_proxy_env(http),
    ?assertEqual({ok, ProxyUrl}, Result),
    os:unsetenv(Var).

test_valid_proxy_with_spaces(Var, _State) ->
    ProxyUrl = "  http://proxy.example.com:8080  ",
    ExpectedUrl = "http://proxy.example.com:8080",
    os:putenv(Var, ProxyUrl),
    %% Clear cached values first
    application:unset_env(hackney, http_proxy),
    application:unset_env(hackney, https_proxy),
    %% This should return the proxy URL with stripped spaces
    Result = hackney:get_proxy_env(http),
    ?assertEqual({ok, ExpectedUrl}, Result),
    os:unsetenv(Var).

%% Direct tests for do_get_proxy_env function
do_get_proxy_env_test_() ->
    [
     {"Empty list returns false",
      fun() -> ?assertEqual(false, hackney:do_get_proxy_env([])) end},
     {"All empty values returns false",
      fun() ->
          os:putenv("TEST_PROXY1", ""),
          os:putenv("TEST_PROXY2", "   "),
          os:putenv("TEST_PROXY3", "\t\n"),
          Result = hackney:do_get_proxy_env(["TEST_PROXY1", "TEST_PROXY2", "TEST_PROXY3"]),
          ?assertEqual(false, Result),
          os:unsetenv("TEST_PROXY1"),
          os:unsetenv("TEST_PROXY2"),
          os:unsetenv("TEST_PROXY3")
      end},
     {"First valid proxy is returned",
      fun() ->
          os:putenv("TEST_PROXY1", ""),
          os:putenv("TEST_PROXY2", "  http://proxy.example.com  "),
          os:putenv("TEST_PROXY3", "http://proxy2.example.com"),
          Result = hackney:do_get_proxy_env(["TEST_PROXY1", "TEST_PROXY2", "TEST_PROXY3"]),
          ?assertEqual({ok, "http://proxy.example.com"}, Result),
          os:unsetenv("TEST_PROXY1"),
          os:unsetenv("TEST_PROXY2"),
          os:unsetenv("TEST_PROXY3")
      end}
    ].

%% Tests for parse_proxy_url/1 (issue #741)
parse_proxy_url_test_() ->
    [
     {"Parse simple HTTP proxy URL",
      fun() ->
          Result = hackney:parse_proxy_url("http://proxy.example.com:8080"),
          ?assertEqual({ok, #{scheme => http,
                              host => "proxy.example.com",
                              port => 8080,
                              user => undefined,
                              password => undefined}}, Result)
      end},
     {"Parse HTTP proxy URL with credentials",
      fun() ->
          Result = hackney:parse_proxy_url("http://user:pass@proxy.example.com:8080"),
          ?assertEqual({ok, #{scheme => http,
                              host => "proxy.example.com",
                              port => 8080,
                              user => <<"user">>,
                              password => <<"pass">>}}, Result)
      end},
     {"Parse HTTPS proxy URL with credentials",
      fun() ->
          Result = hackney:parse_proxy_url("https://admin:secret@secure-proxy.example.com:443"),
          ?assertEqual({ok, #{scheme => https,
                              host => "secure-proxy.example.com",
                              port => 443,
                              user => <<"admin">>,
                              password => <<"secret">>}}, Result)
      end},
     {"Parse SOCKS5 proxy URL",
      fun() ->
          Result = hackney:parse_proxy_url("socks5://socks.example.com:1080"),
          ?assertEqual({ok, #{scheme => socks5,
                              host => "socks.example.com",
                              port => 1080,
                              user => undefined,
                              password => undefined}}, Result)
      end},
     {"Parse SOCKS5 proxy URL with credentials",
      fun() ->
          Result = hackney:parse_proxy_url("socks5://user:pass@socks.example.com:1080"),
          ?assertEqual({ok, #{scheme => socks5,
                              host => "socks.example.com",
                              port => 1080,
                              user => <<"user">>,
                              password => <<"pass">>}}, Result)
      end},
     {"Parse proxy URL with URL-encoded credentials",
      fun() ->
          Result = hackney:parse_proxy_url("http://user%40domain:p%40ss@proxy.example.com:8080"),
          ?assertEqual({ok, #{scheme => http,
                              host => "proxy.example.com",
                              port => 8080,
                              user => <<"user@domain">>,
                              password => <<"p@ss">>}}, Result)
      end},
     {"Parse proxy URL with binary input",
      fun() ->
          Result = hackney:parse_proxy_url(<<"http://proxy.example.com:8080">>),
          ?assertEqual({ok, #{scheme => http,
                              host => "proxy.example.com",
                              port => 8080,
                              user => undefined,
                              password => undefined}}, Result)
      end},
     {"Parse proxy URL with default HTTP port",
      fun() ->
          Result = hackney:parse_proxy_url("http://proxy.example.com"),
          ?assertEqual({ok, #{scheme => http,
                              host => "proxy.example.com",
                              port => 80,
                              user => undefined,
                              password => undefined}}, Result)
      end},
     {"Parse proxy URL with default HTTPS port",
      fun() ->
          Result = hackney:parse_proxy_url("https://proxy.example.com"),
          ?assertEqual({ok, #{scheme => https,
                              host => "proxy.example.com",
                              port => 443,
                              user => undefined,
                              password => undefined}}, Result)
      end}
    ].

%% Tests for get_proxy_config/2
get_proxy_config_test_() ->
    [
     {"No proxy option returns false",
      fun() ->
          ?assertEqual(false, hackney:get_proxy_config(http, []))
      end},
     {"Proxy option set to false returns false",
      fun() ->
          ?assertEqual(false, hackney:get_proxy_config(http, [{proxy, false}]))
      end},
     {"Simple tuple proxy for HTTP scheme returns http type",
      fun() ->
          Result = hackney:get_proxy_config(http, [{proxy, {"proxy.local", 8080}}]),
          ?assertEqual({http, "proxy.local", 8080, undefined}, Result)
      end},
     {"Simple tuple proxy for HTTPS scheme returns connect type",
      fun() ->
          Result = hackney:get_proxy_config(https, [{proxy, {"proxy.local", 8080}}]),
          ?assertEqual({connect, "proxy.local", 8080, undefined}, Result)
      end},
     {"Explicit connect tuple returns connect type",
      fun() ->
          Result = hackney:get_proxy_config(http, [{proxy, {connect, "proxy.local", 8080}}]),
          ?assertEqual({connect, "proxy.local", 8080, undefined}, Result)
      end},
     {"SOCKS5 tuple returns socks5 type",
      fun() ->
          Result = hackney:get_proxy_config(http, [{proxy, {socks5, "socks.local", 1080}}]),
          ?assertEqual({socks5, "socks.local", 1080, undefined}, Result)
      end},
     {"SOCKS5 tuple with auth",
      fun() ->
          Result = hackney:get_proxy_config(http, [
              {proxy, {socks5, "socks.local", 1080}},
              {socks5_user, <<"user">>},
              {socks5_pass, <<"pass">>}
          ]),
          ?assertEqual({socks5, "socks.local", 1080, {<<"user">>, <<"pass">>}}, Result)
      end},
     {"HTTP URL proxy for HTTP scheme returns http type",
      fun() ->
          Result = hackney:get_proxy_config(http, [{proxy, "http://proxy.local:8080"}]),
          ?assertEqual({http, "proxy.local", 8080, undefined}, Result)
      end},
     {"HTTP URL proxy for HTTPS scheme returns connect type",
      fun() ->
          Result = hackney:get_proxy_config(https, [{proxy, "http://proxy.local:8080"}]),
          ?assertEqual({connect, "proxy.local", 8080, undefined}, Result)
      end},
     {"SOCKS5 URL proxy returns socks5 type",
      fun() ->
          Result = hackney:get_proxy_config(http, [{proxy, "socks5://socks.local:1080"}]),
          ?assertEqual({socks5, "socks.local", 1080, undefined}, Result)
      end},
     {"Proxy URL with credentials",
      fun() ->
          Result = hackney:get_proxy_config(http, [{proxy, "http://user:pass@proxy.local:8080"}]),
          ?assertEqual({http, "proxy.local", 8080, {<<"user">>, <<"pass">>}}, Result)
      end},
     {"Proxy auth option used when URL has no credentials",
      fun() ->
          Result = hackney:get_proxy_config(http, [
              {proxy, "http://proxy.local:8080"},
              {proxy_auth, {<<"admin">>, <<"secret">>}}
          ]),
          ?assertEqual({http, "proxy.local", 8080, {<<"admin">>, <<"secret">>}}, Result)
      end},
     {"URL credentials override proxy_auth option",
      fun() ->
          Result = hackney:get_proxy_config(http, [
              {proxy, "http://urluser:urlpass@proxy.local:8080"},
              {proxy_auth, {<<"admin">>, <<"secret">>}}
          ]),
          ?assertEqual({http, "proxy.local", 8080, {<<"urluser">>, <<"urlpass">>}}, Result)
      end},
     {"Binary proxy URL works",
      fun() ->
          Result = hackney:get_proxy_config(http, [{proxy, <<"http://proxy.local:8080">>}]),
          ?assertEqual({http, "proxy.local", 8080, undefined}, Result)
      end}
    ].

%% Tests for start_conn_with_socket/5
start_conn_with_socket_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(hackney),
         ok
     end,
     fun(_) -> ok end,
     [
      {"Start connection with raw socket",
       fun() ->
           %% Create a mock socket (we just need any port for the test)
           {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
           {ok, Port} = inet:port(ListenSock),

           %% Connect to ourselves to get a real socket
           {ok, ClientSock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
           {ok, _ServerSock} = gen_tcp:accept(ListenSock, 1000),

           %% Start hackney with pre-established socket
           {ok, ConnPid} = hackney:start_conn_with_socket("127.0.0.1", Port, hackney_tcp, ClientSock, []),

           ?assert(is_pid(ConnPid)),
           ?assertEqual({ok, connected}, hackney_conn:get_state(ConnPid)),

           hackney_conn:stop(ConnPid),
           gen_tcp:close(ListenSock)
       end},
      {"Start connection with {Transport, Socket} tuple",
       fun() ->
           %% Create a mock socket
           {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
           {ok, Port} = inet:port(ListenSock),

           %% Connect to ourselves
           {ok, ClientSock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
           {ok, _ServerSock} = gen_tcp:accept(ListenSock, 1000),

           %% Wrap in tuple like proxy modules return
           WrappedSocket = {hackney_tcp, ClientSock},

           {ok, ConnPid} = hackney:start_conn_with_socket("127.0.0.1", Port, hackney_tcp, WrappedSocket, []),

           ?assert(is_pid(ConnPid)),
           ?assertEqual({ok, connected}, hackney_conn:get_state(ConnPid)),

           hackney_conn:stop(ConnPid),
           gen_tcp:close(ListenSock)
       end},
      {"Normalize gen_tcp to hackney_tcp",
       fun() ->
           %% Create a mock socket
           {ok, ListenSock} = gen_tcp:listen(0, [binary, {active, false}]),
           {ok, Port} = inet:port(ListenSock),

           {ok, ClientSock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
           {ok, _ServerSock} = gen_tcp:accept(ListenSock, 1000),

           %% Use gen_tcp as transport - should be normalized to hackney_tcp
           {ok, ConnPid} = hackney:start_conn_with_socket("127.0.0.1", Port, gen_tcp, ClientSock, []),

           ?assert(is_pid(ConnPid)),
           ?assertEqual({ok, connected}, hackney_conn:get_state(ConnPid)),

           hackney_conn:stop(ConnPid),
           gen_tcp:close(ListenSock)
       end}
     ]}.

%% Tests for HTTP CONNECT proxy
connect_proxy_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(hackney),
         ok
     end,
     fun(_) -> ok end,
     [
      {"Connect proxy with unreachable proxy returns error",
       fun() ->
           %% Try to connect through a proxy that doesn't exist
           %% This verifies the connect proxy code path is exercised
           Options = [{proxy, {connect, "127.0.0.1", 19999}}],
           Result = hackney:request(get, <<"https://example.com">>, [], <<>>, Options),
           ?assertMatch({error, _}, Result)
       end},
      {"Connect proxy via URL for HTTPS target",
       fun() ->
           %% When target is HTTPS and proxy is HTTP URL, should use connect type
           %% Verify by checking that connection fails to unreachable proxy
           Options = [{proxy, "http://127.0.0.1:19998"}],
           Result = hackney:request(get, <<"https://example.com">>, [], <<>>, Options),
           ?assertMatch({error, _}, Result)
       end}
     ]}.

%% Tests for SOCKS5 proxy
socks5_proxy_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(hackney),
         ok
     end,
     fun(_) -> ok end,
     [
      {"SOCKS5 proxy with tuple config returns error for unreachable proxy",
       fun() ->
           %% Try to connect through a SOCKS5 proxy that doesn't exist
           Options = [{proxy, {socks5, "127.0.0.1", 19997}}],
           Result = hackney:request(get, <<"http://example.com">>, [], <<>>, Options),
           ?assertMatch({error, _}, Result)
       end},
      {"SOCKS5 proxy via URL",
       fun() ->
           %% Verify socks5:// URL is parsed and used
           Options = [{proxy, "socks5://127.0.0.1:19996"}],
           Result = hackney:request(get, <<"http://example.com">>, [], <<>>, Options),
           ?assertMatch({error, _}, Result)
       end},
      {"SOCKS5 proxy with auth",
       fun() ->
           %% Verify socks5 with authentication
           Options = [
               {proxy, {socks5, "127.0.0.1", 19995}},
               {socks5_user, <<"testuser">>},
               {socks5_pass, <<"testpass">>}
           ],
           Result = hackney:request(get, <<"http://example.com">>, [], <<>>, Options),
           ?assertMatch({error, _}, Result)
       end}
     ]}.

%% Integration tests with mock proxy servers
-define(TEST_PORT, 8126).

proxy_integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun teardown_integration/1,
     fun(State) ->
         [
          {"HTTP CONNECT proxy to HTTP target", {timeout, 30, fun() -> test_connect_proxy_http(State) end}},
          {"SOCKS5 proxy to HTTP target", {timeout, 30, fun() -> test_socks5_proxy_http(State) end}},
          {"Simple HTTP proxy to HTTP target", {timeout, 30, fun() -> test_http_proxy(State) end}}
         ]
     end}.

setup_integration() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    %% Start test HTTP server
    Host = '_',
    Routes = [{"/[...]", test_http_resource, []}],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    {ok, _} = cowboy:start_clear(proxy_test_server, [{port, ?TEST_PORT}],
                                  #{env => #{dispatch => Dispatch}}),
    %% Start mock proxies
    {ok, ConnectPid, ConnectPort} = mock_proxy_server:start_connect_proxy(),
    {ok, Socks5Pid, Socks5Port} = mock_proxy_server:start_socks5_proxy(),
    {ok, HttpPid, HttpPort} = mock_proxy_server:start_http_proxy(),
    #{connect_proxy => {ConnectPid, ConnectPort},
      socks5_proxy => {Socks5Pid, Socks5Port},
      http_proxy => {HttpPid, HttpPort}}.

teardown_integration(#{connect_proxy := {ConnectPid, _}, socks5_proxy := {Socks5Pid, _}, http_proxy := {HttpPid, _}}) ->
    mock_proxy_server:stop(ConnectPid),
    mock_proxy_server:stop(Socks5Pid),
    mock_proxy_server:stop(HttpPid),
    cowboy:stop_listener(proxy_test_server),
    error_logger:tty(true),
    ok.

test_connect_proxy_http(#{connect_proxy := {_, ProxyPort}}) ->
    %% Use explicit connect proxy to reach our test server
    Url = iolist_to_binary([<<"http://127.0.0.1:">>, integer_to_binary(?TEST_PORT), <<"/get">>]),
    Options = [{proxy, {connect, "127.0.0.1", ProxyPort}}, {recv_timeout, 10000}],
    case hackney:request(get, Url, [], <<>>, Options) of
        {ok, Status, _Headers, ConnPid} ->
            {ok, Body} = hackney:body(ConnPid),
            hackney:close(ConnPid),
            ?assert(Status >= 200 andalso Status < 400),
            ?assert(byte_size(Body) > 0);
        {error, Reason} ->
            ct:pal("Connect proxy failed: ~p~n", [Reason]),
            error({connect_proxy_failed, Reason})
    end.

test_socks5_proxy_http(#{socks5_proxy := {_, ProxyPort}}) ->
    %% Use SOCKS5 proxy to reach our test server
    Url = iolist_to_binary([<<"http://127.0.0.1:">>, integer_to_binary(?TEST_PORT), <<"/get">>]),
    Options = [{proxy, {socks5, "127.0.0.1", ProxyPort}}, {recv_timeout, 10000}],
    case hackney:request(get, Url, [], <<>>, Options) of
        {ok, Status, _Headers, ConnPid} ->
            {ok, Body} = hackney:body(ConnPid),
            hackney:close(ConnPid),
            ?assert(Status >= 200 andalso Status < 400),
            ?assert(byte_size(Body) > 0);
        {error, Reason} ->
            ct:pal("SOCKS5 proxy failed: ~p~n", [Reason]),
            error({socks5_proxy_failed, Reason})
    end.

test_http_proxy(#{http_proxy := {_, ProxyPort}}) ->
    %% Use simple HTTP proxy (with absolute URLs) to reach our test server
    Url = iolist_to_binary([<<"http://127.0.0.1:">>, integer_to_binary(?TEST_PORT), <<"/get">>]),
    %% Use tuple format for simple HTTP proxy
    Options = [{proxy, {"127.0.0.1", ProxyPort}}, {recv_timeout, 10000}],
    case hackney:request(get, Url, [], <<>>, Options) of
        {ok, Status, _Headers, ConnPid} ->
            {ok, Body} = hackney:body(ConnPid),
            hackney:close(ConnPid),
            ?assert(Status >= 200 andalso Status < 400),
            ?assert(byte_size(Body) > 0);
        {error, Reason} ->
            ct:pal("HTTP proxy failed: ~p~n", [Reason]),
            error({http_proxy_failed, Reason})
    end.

