%%% CRLF injection guard for the CONNECT proxy handshake.
%%%
%%% The target host is concatenated into the CONNECT request line and Host
%%% header, so a host carrying CR/LF/NUL (e.g. from a percent-decoded URL) must
%%% be rejected before any payload is built and sent to the proxy.
-module(hackney_http_connect_tests).

-include_lib("eunit/include/eunit.hrl").

crlf_host_rejected_test() ->
    assert_rejected("victim.example\r\nX-Injected: yes").

lf_host_rejected_test() ->
    assert_rejected("victim.example\nX-Injected: yes").

nul_host_rejected_test() ->
    assert_rejected("victim.example\0").

%% Point the handshake at a local listener acting as the proxy: the guard must
%% return {error, invalid_connect_host} and no CONNECT bytes may reach it.
assert_rejected(Host) ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false},
                                     {reuseaddr, true}, {ip, {127, 0, 0, 1}}]),
    {ok, LPort} = inet:port(LSock),
    Opts = [{connect_host, Host},
            {connect_port, 443},
            {connect_transport, hackney_tcp},
            {proxy_transport, tcp}],
    Result = hackney_http_connect:connect("127.0.0.1", LPort, Opts, 1000),
    ?assertEqual({error, invalid_connect_host}, Result),
    Received = case gen_tcp:accept(LSock, 300) of
                   {ok, S} ->
                       R = gen_tcp:recv(S, 0, 100),
                       gen_tcp:close(S),
                       R;
                   {error, timeout} ->
                       no_connection
               end,
    gen_tcp:close(LSock),
    %% Whatever happened, the proxy must not have seen request bytes.
    ?assert(Received =:= no_connection orelse Received =:= {error, closed}).
