-module(hackney_https_pool_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

%% This seems necessary to list the tests including the generator
dummy_test() ->
    ?assertEqual(ok, ok).

multipart_test_() ->
    {setup, fun start/0, fun stop/1,
      [{timeout, 120, requests_to_different_servers_go_to_different_servers()}]}.

start() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    ProxyPid = simple_proxy:start_proxy_server(),
    register(proxy_server, ProxyPid),

    hackney_pool:start_pool(pool_test, [{pool_size, 1}]),
    Host = '_',
    Resource = {"/pool", https_pool_resource1, []},
    Dispatch = cowboy_router:compile([{Host, [Resource]}]),
    Resource2 = {"/pool", https_pool_resource2, []},
    Dispatch2 = cowboy_router:compile([{Host, [Resource2]}]),

    CaCertFile = "test/cowboy-ca.crt",
    CertFile = "test/server.crt",
    KeyFile = "test/server.key",
     
    cowboy:start_https(test_server, 10, [{port, 8443}, {cacertfile, CaCertFile}, {certfile, CertFile}, {keyfile, KeyFile}], [{env, [{dispatch, Dispatch}]}]),
    cowboy:start_https(test_server2, 10, [{port, 8444}, {cacertfile, CaCertFile}, {certfile, CertFile}, {keyfile, KeyFile}], [{env, [{dispatch, Dispatch2}]}]).

stop({ok, _Pid}) ->
    cowboy:stop_listener(test_server),
    application:stop(cowboy),
    hackney_pool:stop_pool(pool_test),
    application:stop(hackney),
    error_logger:tty(true),
    exit(whereis(proxy_server), kill),
    ok.

requests_to_different_servers_go_to_different_servers() ->
    fun() ->
        URL = <<"https://localhost:8443/pool">>,
        OtherURL = <<"https://127.0.0.1:8444/pool">>,

        Headers = [],
        SSLOptions = [{verify, verify_none}],
        Opts = [{pool, pool_test}, {connect_timeout, 100}, {ssl_options, SSLOptions}, {proxy, "http://localhost:5555"}],
        {ok, _, _, Ref1} = hackney:request(get, URL, Headers, <<>>, Opts),
        {ok, Body1} = hackney:body(Ref1),
        ?assertEqual(<<"pool_resource1">>, Body1),

        {ok, _, _, Ref2} = hackney:request(get, OtherURL, Headers, <<>>, Opts),
        {ok, Body2} = hackney:body(Ref2),
        ?assertEqual(<<"pool_resource2">>, Body2)
    end.

