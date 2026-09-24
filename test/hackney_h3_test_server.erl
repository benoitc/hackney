%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2026 Benoit Chesneau
%%%
%%% @doc Local HTTP/3 server for tests.
%%%
%%% Starts an in-process quic_h3 server on 127.0.0.1 with the test
%%% certificate, so HTTP/3 tests do not depend on public servers. Routes:
%%%
%%%   GET|HEAD /             200 text/html
%%%   GET /cdn-cgi/trace     200 text/plain, a small key=value body
%%%   GET /large             200, a 64 KiB body sent in 16 DATA frames
%%%   GET /redirect/N        302 to /redirect/N-1; /redirect/0 is 200
%%%   GET /status/N          status N; a 3xx carries location: /
%%%   GET /headers           200 text/plain, the request headers it received,
%%%                          one "name: value" per line, sorted
%%%   GET /reset             200 and part of a body, then resets the stream
%%%                          when the process registered as
%%%                          hackney_h3_test_reset sends `reset'
%%%   POST any path          200, echoes the request body
%%%   anything else          404
-module(hackney_h3_test_server).

-export([start/0, stop/1]).
-export([port/1, url/2, host/0]).
-export([hackney_opts/0, h3_opts/0, conn_opts/1]).
-export([cert_file/0, ca_file/0, ca_cacerts/0, unused_port/0]).

-define(LARGE_CHUNK, 4096).
-define(LARGE_CHUNKS, 16).

%% @doc Start a server on a free UDP port. Returns the context the other
%% functions take.
start() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, CertPem} = file:read_file(cert_file()),
    {ok, KeyPem} = file:read_file(filename:join(cert_dir(), "server.key")),
    [{'Certificate', CertDer, _} | _] = public_key:pem_decode(CertPem),
    Name = list_to_atom("hackney_h3_test_server_" ++
                        integer_to_list(erlang:unique_integer([positive]))),
    Opts = #{cert => CertDer, key => decode_key(KeyPem),
             handler => fun handle/5},
    {ok, _Pid} = quic_h3:start_server(Name, 0, Opts),
    {ok, Port} = quic:get_server_port(Name),
    #{name => Name, port => Port}.

stop(#{name := Name}) ->
    try quic_h3:stop_server(Name) catch _:_ -> ok end,
    ok.

port(#{port := Port}) -> Port.

host() -> <<"127.0.0.1">>.

url(#{port := Port}, Path) ->
    iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port), Path]).

%% Budget for a QUIC handshake or a response. Generous: the handshake runs
%% in pure Erlang on both ends, and slow CI runners are why these tests
%% stopped using public servers.
-define(TIMEOUT, 15000).

%% @doc hackney:request/connect options for an HTTP/3 request to the server.
%% Session resumption is off: these tests are not about it, and resuming
%% from a cached ticket can stall the handshake (reproduced on one
%% scheduler), which would make unrelated tests flaky.
hackney_opts() ->
    [{protocols, [http3]},
     {connect_timeout, ?TIMEOUT},
     {recv_timeout, ?TIMEOUT},
     {ssl_options, [{insecure, true}]}].

%% @doc hackney_h3 options: the certificate is self-signed.
h3_opts() ->
    #{insecure_skip_verify => true, timeout => ?TIMEOUT, recv_timeout => ?TIMEOUT}.

%% @doc hackney_conn:start_link/1 options for an HTTP/3 connection.
conn_opts(Ctx) ->
    #{host => "127.0.0.1",
      port => port(Ctx),
      transport => hackney_ssl,
      connect_options => [{protocols, [http3]}],
      ssl_options => [{insecure, true}],
      connect_timeout => ?TIMEOUT,
      recv_timeout => ?TIMEOUT}.

cert_file() ->
    filename:join(cert_dir(), "server.pem").

%% @doc The PEM file of the CA that issued the server certificate.
ca_file() ->
    filename:join(cert_dir(), "ca.pem").

%% @doc The CA that issued the server certificate, as DER trust anchors.
ca_cacerts() ->
    {ok, Pem} = file:read_file(ca_file()),
    [Der || {'Certificate', Der, not_encrypted} <- public_key:pem_decode(Pem)].

%% @doc A UDP port with nothing listening on it.
unused_port() ->
    {ok, Sock} = gen_udp:open(0, [{ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.

%%====================================================================
%% Handler
%%====================================================================

handle(Conn, StreamId, <<"POST">>, _Path, Headers) ->
    Body = read_body(Conn, StreamId),
    ContentType = proplists:get_value(<<"content-type">>, Headers,
                                      <<"application/octet-stream">>),
    respond(Conn, StreamId, 200, [{<<"content-type">>, ContentType}], Body);
handle(Conn, StreamId, <<"HEAD">>, <<"/">>, _Headers) ->
    quic_h3:send_response(Conn, StreamId, 200,
                          [{<<"content-type">>, <<"text/html">>},
                           {<<"content-length">>, integer_to_binary(byte_size(index()))}]),
    quic_h3:send_data(Conn, StreamId, <<>>, true);
handle(Conn, StreamId, <<"GET">>, <<"/">>, _Headers) ->
    respond(Conn, StreamId, 200, [{<<"content-type">>, <<"text/html">>}], index());
handle(Conn, StreamId, <<"GET">>, <<"/cdn-cgi/trace">>, _Headers) ->
    respond(Conn, StreamId, 200, [{<<"content-type">>, <<"text/plain">>}],
            <<"h=127.0.0.1\nhttp=http/3\n">>);
handle(Conn, StreamId, <<"GET">>, <<"/large">>, _Headers) ->
    Chunk = binary:copy(<<"x">>, ?LARGE_CHUNK),
    quic_h3:send_response(Conn, StreamId, 200,
                          [{<<"content-type">>, <<"application/octet-stream">>},
                           {<<"content-length">>,
                            integer_to_binary(?LARGE_CHUNK * ?LARGE_CHUNKS)}]),
    [quic_h3:send_data(Conn, StreamId, Chunk, false)
     || _ <- lists:seq(1, ?LARGE_CHUNKS - 1)],
    quic_h3:send_data(Conn, StreamId, Chunk, true);
handle(Conn, StreamId, <<"GET">>, <<"/redirect/", N/binary>>, _Headers) ->
    case binary_to_integer(N) of
        0 ->
            respond(Conn, StreamId, 200, [{<<"content-type">>, <<"text/plain">>}],
                    <<"redirected">>);
        Count ->
            Location = <<"/redirect/", (integer_to_binary(Count - 1))/binary>>,
            respond(Conn, StreamId, 302, [{<<"location">>, Location}], <<>>)
    end;
handle(Conn, StreamId, <<"GET">>, <<"/status/", N/binary>>, _Headers) ->
    Status = binary_to_integer(N),
    Headers = case Status >= 300 andalso Status < 400 of
        true -> [{<<"location">>, <<"/">>}];
        false -> []
    end,
    respond(Conn, StreamId, Status, Headers, <<>>);
handle(Conn, StreamId, <<"GET">>, <<"/headers">>, Headers) ->
    %% Report the headers that reached the server, so a test can assert on
    %% what hackney put on the wire.
    Lines = [[Name, <<": ">>, Value, <<"\n">>]
             || {Name, Value} <- lists:sort(Headers)],
    respond(Conn, StreamId, 200, [{<<"content-type">>, <<"text/plain">>}],
            iolist_to_binary(Lines));
handle(Conn, StreamId, <<"GET">>, <<"/reset">>, _Headers) ->
    quic_h3:send_response(Conn, StreamId, 200, [{<<"content-type">>, <<"text/plain">>}]),
    quic_h3:send_data(Conn, StreamId, <<"part">>, false),
    hackney_h3_test_reset ! {reset_ready, self()},
    receive reset -> quic_h3:cancel(Conn, StreamId, 16#010c)
    after 15000 -> ok
    end;
handle(Conn, StreamId, _Method, _Path, _Headers) ->
    respond(Conn, StreamId, 404, [], <<>>).

index() ->
    <<"<html><body>hackney h3 test server</body></html>">>.

respond(Conn, StreamId, Status, Headers, Body) ->
    quic_h3:send_response(Conn, StreamId, Status,
                          [{<<"content-length">>, integer_to_binary(byte_size(Body))}
                           | Headers]),
    quic_h3:send_data(Conn, StreamId, Body, true).

%% Body DATA received before the handler registered comes back buffered;
%% the rest arrives as messages until Fin.
read_body(Conn, StreamId) ->
    case quic_h3:set_stream_handler(Conn, StreamId, self()) of
        ok ->
            recv_body(Conn, StreamId, <<>>);
        {ok, Buffered} ->
            case drain_buffered(Buffered, <<>>) of
                {fin, Acc} -> Acc;
                {more, Acc} -> recv_body(Conn, StreamId, Acc)
            end
    end.

drain_buffered([], Acc) ->
    {more, Acc};
drain_buffered([{Data, true} | _], Acc) ->
    {fin, <<Acc/binary, Data/binary>>};
drain_buffered([{Data, false} | Rest], Acc) ->
    drain_buffered(Rest, <<Acc/binary, Data/binary>>).

recv_body(Conn, StreamId, Acc) ->
    receive
        {quic_h3, Conn, {data, StreamId, Data, true}} ->
            <<Acc/binary, Data/binary>>;
        {quic_h3, Conn, {data, StreamId, Data, false}} ->
            recv_body(Conn, StreamId, <<Acc/binary, Data/binary>>)
    after 5000 ->
        Acc
    end.

%%====================================================================
%% Certificates
%%====================================================================

cert_dir() ->
    BeamDir = filename:dirname(code:which(?MODULE)),
    Root = filename:join([BeamDir, "..", "..", "..", "..", ".."]),
    filename:join([filename:absname(Root), "test", "certs"]).

decode_key(KeyPem) ->
    case public_key:pem_decode(KeyPem) of
        [{'RSAPrivateKey', Der, not_encrypted}] ->
            public_key:der_decode('RSAPrivateKey', Der);
        [{'ECPrivateKey', Der, not_encrypted}] ->
            public_key:der_decode('ECPrivateKey', Der);
        [{'PrivateKeyInfo', Der, not_encrypted}] ->
            public_key:der_decode('PrivateKeyInfo', Der)
    end.
