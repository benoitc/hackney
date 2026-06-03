%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% @doc HTTP/3 0-RTT / resumption and IPv6 tests.
%%%
%%% All cases run deterministically against a local, in-process pure-Erlang
%%% `quic_h3' server on 127.0.0.1 / [::1]. The server issues NewSessionTickets
%%% and (with quic >= 1.6.1) accepts 0-RTT early data, so the full round trip is
%%% exercised without any external server: ticket emission, real request-in-0-RTT
%%% acceptance, the request/5 ticket-consume path, and IPv6.
%%%
%%% Cases skip gracefully when openssl or an IPv6 loopback is unavailable.

-module(hackney_http3_zero_rtt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([basic_get/1,
         session_ticket_emitted/1,
         real_0rtt_one_shot/1,
         request5_resumption/1,
         ipv6_loopback/1]).

-define(SERVER, hackney_h3_0rtt_test_server).

all() ->
    [basic_get,
     session_ticket_emitted,
     real_0rtt_one_shot,
     request5_resumption,
     ipv6_loopback].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(hackney),
    case make_cert() of
        {ok, CertDer, KeyDer} ->
            [{cert, CertDer}, {key, KeyDer} | Config];
        {error, Reason} ->
            {skip, {no_cert, Reason}}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(ipv6_loopback, Config) ->
    case ipv6_loopback_available() of
        true -> start_server(Config, [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 1}}]);
        false -> {skip, no_ipv6_loopback}
    end;
init_per_testcase(_Case, Config) ->
    start_server(Config, []).

end_per_testcase(_Case, _Config) ->
    catch quic_h3:stop_server(?SERVER),
    ok.

%%====================================================================
%% Local, deterministic cases
%%====================================================================

%% Plain HTTP/3 GET against the local server (sanity, no ticket).
basic_get(Config) ->
    Port = ?config(port, Config),
    {ok, Status, _Hdrs, Body} =
        hackney_h3:request(get, url("127.0.0.1", Port), [], <<>>,
                           #{insecure_skip_verify => true}),
    ?assertEqual(200, Status),
    ?assertEqual(<<"ok">>, Body).

%% A first connection yields a session ticket forwarded to the owner.
session_ticket_emitted(Config) ->
    Port = ?config(port, Config),
    {ok, Ref} = hackney_h3:connect(<<"127.0.0.1">>, Port,
                                   #{verify => verify_none}, self()),
    {ok, _Ticket} = hackney_h3:wait_session_ticket(Ref, 5000),
    ok = hackney_h3:close(Ref).

%% True request-in-0-RTT: capture a ticket, then on a fresh no-wait connection
%% send a bodyless request before `connected'. The server accepts the early
%% data (quic >= 1.6.1 echoes the early_data extension).
real_0rtt_one_shot(Config) ->
    Port = ?config(port, Config),
    Ticket = capture_ticket(<<"127.0.0.1">>, Port),
    {ok, Ref} = hackney_h3:connect(<<"127.0.0.1">>, Port,
                                   #{verify => verify_none,
                                     session_ticket => Ticket}, self()),
    {ok, StreamId, _Streams} =
        hackney_h3:send_request(Ref, <<"GET">>, <<"127.0.0.1">>, <<"/">>, [], <<>>),
    {ok, Status, _Hdrs, Body} = hackney_h3:await_response(Ref, StreamId),
    ?assertEqual(200, Status),
    ?assertEqual(<<"ok">>, Body),
    ?assertEqual(true, hackney_h3:early_data_accepted(Ref)),
    ok = hackney_h3:close(Ref).

%% request/5 consumes a captured ticket for a bodyless request (0-RTT).
request5_resumption(Config) ->
    Port = ?config(port, Config),
    Ticket = capture_ticket(<<"127.0.0.1">>, Port),
    {ok, Status, _Hdrs, _Body} =
        hackney_h3:request(get, url("127.0.0.1", Port), [], <<>>,
                           #{insecure_skip_verify => true,
                             session_ticket => Ticket}),
    ?assertEqual(200, Status).

%% Connect over an IPv6 loopback literal, forcing the inet6 family.
ipv6_loopback(Config) ->
    Port = ?config(port, Config),
    {ok, Ref} = hackney_h3:connect(<<"[::1]">>, Port,
                                   #{verify => verify_none, family => inet6}, self()),
    receive
        {h3, Ref, {connected, _}} -> ok
    after 5000 ->
        ct:fail(ipv6_connect_timeout)
    end,
    {ok, StreamId, _Streams} =
        hackney_h3:send_request(Ref, <<"GET">>, <<"[::1]">>, <<"/">>, [], <<>>),
    {ok, Status, _Hdrs, _Body} = hackney_h3:await_response(Ref, StreamId),
    ?assertEqual(200, Status),
    ok = hackney_h3:close(Ref).

%%====================================================================
%% Helpers
%%====================================================================

start_server(Config, ExtraSocketOpts) ->
    Cert = ?config(cert, Config),
    Key = ?config(key, Config),
    QuicOpts0 = #{max_data => 16 * 1024 * 1024,
                  max_stream_data_bidi_local => 4 * 1024 * 1024,
                  max_stream_data_bidi_remote => 4 * 1024 * 1024,
                  max_stream_data_uni => 4 * 1024 * 1024},
    QuicOpts = case ExtraSocketOpts of
        [] -> QuicOpts0;
        _ -> QuicOpts0#{extra_socket_opts => ExtraSocketOpts}
    end,
    Opts = #{cert => Cert, key => Key, quic_opts => QuicOpts,
             handler => fun handle/5},
    case quic_h3:start_server(?SERVER, 0, Opts) of
        {ok, _Pid} ->
            {ok, Port} = quic:get_server_port(?SERVER),
            [{port, Port} | Config];
        {error, Reason} ->
            {skip, {server_start_failed, Reason}}
    end.

%% First connection: capture and return the session ticket, then close.
capture_ticket(Host, Port) ->
    {ok, Ref} = hackney_h3:connect(Host, Port, #{verify => verify_none}, self()),
    {ok, Ticket} = hackney_h3:wait_session_ticket(Ref, 5000),
    ok = hackney_h3:close(Ref),
    Ticket.

url(Host, Port) ->
    "https://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/".

%% Bodyless GET handler.
handle(Conn, StreamId, <<"GET">>, _Path, _Headers) ->
    Body = <<"ok">>,
    quic_h3:send_response(Conn, StreamId, 200, [
        {<<"content-type">>, <<"text/plain">>},
        {<<"content-length">>, integer_to_binary(byte_size(Body))}
    ]),
    quic_h3:send_data(Conn, StreamId, Body, true);
handle(Conn, StreamId, _Method, _Path, _Headers) ->
    quic_h3:send_response(Conn, StreamId, 404, [{<<"content-length">>, <<"0">>}]),
    quic_h3:send_data(Conn, StreamId, <<>>, true).

ipv6_loopback_available() ->
    case gen_udp:open(0, [inet6, {ip, {0, 0, 0, 0, 0, 0, 0, 1}}]) of
        {ok, S} -> gen_udp:close(S), true;
        {error, _} -> false
    end.

%% Self-signed cert via openssl; {error, _} -> suite skips.
make_cert() ->
    Dir = test_tmp_dir(),
    KeyFile = filename:join(Dir, "h3_0rtt_key.pem"),
    CertFile = filename:join(Dir, "h3_0rtt_cert.pem"),
    Cmd = lists:flatten(io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s "
        "-days 1 -nodes -subj '/CN=localhost' 2>/dev/null",
        [KeyFile, CertFile])),
    _ = os:cmd(Cmd),
    case {filelib:is_regular(KeyFile), filelib:is_regular(CertFile)} of
        {true, true} ->
            {ok, CertPem} = file:read_file(CertFile),
            {ok, KeyPem} = file:read_file(KeyFile),
            [{'Certificate', CertDer, _}] = public_key:pem_decode(CertPem),
            {ok, CertDer, decode_key(KeyPem)};
        _ ->
            {error, openssl_unavailable}
    end.

decode_key(KeyPem) ->
    case public_key:pem_decode(KeyPem) of
        [{'RSAPrivateKey', Der, not_encrypted}] ->
            public_key:der_decode('RSAPrivateKey', Der);
        [{'ECPrivateKey', Der, not_encrypted}] ->
            public_key:der_decode('ECPrivateKey', Der);
        [{'PrivateKeyInfo', Der, not_encrypted}] ->
            public_key:der_decode('PrivateKeyInfo', Der);
        [{_Type, Der, not_encrypted}] ->
            Der
    end.

test_tmp_dir() ->
    Base = case os:getenv("TMPDIR") of
        false -> "/tmp";
        "" -> "/tmp";
        T -> T
    end,
    Dir = filename:join(Base, "hackney_h3_0rtt_test"),
    _ = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.
