%%% #970: socket options set in connect_options must reach the UDP socket of
%%% an HTTP/3 connection. hackney used to forward only `family' to quic, and
%%% the shared HTTP/3 connection key ignored connect_options.
-module(hackney_h3_socket_options_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BIND, [{connect_options, [{ip, {127,0,0,1}}]}]).

%%====================================================================
%% Option mapping
%%====================================================================

udp_options_forwarded_test() ->
    M = hackney_conn:h3_tls_opts([{ip, {127,0,0,1}}, {recbuf, 65536}], []),
    ?assertEqual([{ip, {127,0,0,1}}, {recbuf, 65536}], maps:get(extra_socket_opts, M)).

tcp_only_options_left_out_test() ->
    M = hackney_conn:h3_tls_opts([{nodelay, true}, {keepalive, false}, {linger, {true, 1}},
                                  {send_timeout, 1000}, {protocols, [http3]}], []),
    ?assertNot(maps:is_key(extra_socket_opts, M)).

no_options_no_change_test() ->
    ?assertEqual(hackney_conn:h3_tls_opts([], []),
                 hackney_conn:h3_tls_opts([{protocols, [http3]}], [])),
    ?assertNot(maps:is_key(extra_socket_opts, hackney_conn:h3_tls_opts([], []))).

quic_opts_carry_socket_options_test() ->
    #{quic_opts := QuicOpts} =
        hackney_h3:build_h3_opts(<<"example.com">>,
                                 #{extra_socket_opts => [{ip, {127,0,0,1}}]}),
    ?assertEqual([{ip, {127,0,0,1}}], maps:get(extra_socket_opts, QuicOpts)).

%%====================================================================
%% Against the local HTTP/3 server
%%====================================================================

h3_test_() ->
    {setup,
     fun hackney_h3_test_server:start/0,
     fun hackney_h3_test_server:stop/1,
     fun(Ctx) ->
         [{timeout, 60, {"the UDP socket is bound to the requested address",
                         fun() -> bound_address(Ctx) end}},
          {timeout, 60, {"TCP-only options do not break an HTTP/3 request",
                         fun() -> tcp_only_options(Ctx) end}},
          {timeout, 60, {"a shared HTTP/3 conn is shared only with the same socket options",
                         fun() -> shared_conn_per_options(Ctx) end}}]
     end}.

bound_address(Ctx) ->
    {ok, Conn} = hackney:connect(hackney_h3_test_server:url(Ctx, <<"/">>),
                                 [{pool, false} | ?BIND ++ base_opts()]),
    try
        ?assertMatch({ok, {{127,0,0,1}, _}}, hackney:sockname(Conn))
    after
        hackney:close(Conn)
    end.

tcp_only_options(Ctx) ->
    Opts = [{pool, false}, {connect_options, [{nodelay, true}, {keepalive, true}]}
            | base_opts()],
    ?assertMatch({ok, 200, _, _},
                 hackney:request(get, hackney_h3_test_server:url(Ctx, <<"/">>), [], <<>>,
                                 [with_body | Opts])).

shared_conn_per_options(Ctx) ->
    Pool = h3_sockopts_pool,
    ok = hackney_pool:start_pool(Pool, [{prewarm_count, 0}]),
    Url = hackney_h3_test_server:url(Ctx, <<"/">>),
    Get = fun(Extra) ->
              {ok, 200, _, _} = hackney:request(get, Url, [], <<>>,
                                                [with_body, {pool, Pool} | Extra ++ base_opts()])
          end,
    try
        Get([]),
        Get([]),
        ?assertEqual(1, length(h3_conns(Pool))),
        Get(?BIND),
        Get(?BIND),
        ?assertEqual(2, length(h3_conns(Pool)))
    after
        hackney_pool:stop_pool(Pool)
    end.

base_opts() ->
    hackney_h3_test_server:hackney_opts().

%% Shared HTTP/3 conns the pool registered, read from its state.
h3_conns(Pool) ->
    State = sys:get_state(hackney_pool:find_pool(Pool)),
    lists:usort([Pid || Field <- tuple_to_list(State), is_map(Field),
                        {{_Host, _Port, hackney_ssl, _Key}, Pid} <- maps:to_list(Field),
                        is_pid(Pid)]).
