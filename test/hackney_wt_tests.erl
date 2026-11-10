%%% -*- erlang -*-
%%%
%%% WebTransport client tests for hackney.
%%%
%%% Unit tests (URL parsing, scheme validation, header-injection guard) run
%%% without a server. Integration tests spin up a local HTTP/3 WebTransport
%%% echo server (erlang-webtransport) and exercise the wt_* API against it;
%%% they skip gracefully if a test certificate or the listener cannot be
%%% created.
-module(hackney_wt_tests).

-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

-define(LISTENER, hackney_wt_test_listener).

%%====================================================================
%% Unit tests (no server)
%%====================================================================

wt_url_parsing_test_() ->
    [
     {"Parse https:// URL",
      fun() ->
          URL = hackney_url:parse_url(<<"https://example.com/wt">>),
          ?assertEqual(https, URL#hackney_url.scheme),
          ?assertEqual(443, URL#hackney_url.port),
          ?assertEqual("example.com", URL#hackney_url.host),
          ?assertEqual(<<"/wt">>, URL#hackney_url.path)
      end},
     {"Parse https:// URL with custom port and query",
      fun() ->
          URL = hackney_url:parse_url(<<"https://example.com:8443/wt?room=1">>),
          ?assertEqual(8443, URL#hackney_url.port),
          ?assertEqual(<<"/wt">>, URL#hackney_url.path),
          ?assertEqual(<<"room=1">>, URL#hackney_url.qs)
      end}
    ].

wt_scheme_validation_test_() ->
    [
     {"Reject ws:// scheme",
      fun() ->
          ?assertError({invalid_webtransport_scheme, ws},
                       hackney:wt_connect("ws://localhost/wt"))
      end},
     {"Reject http:// scheme",
      fun() ->
          ?assertError({invalid_webtransport_scheme, http},
                       hackney:wt_connect("http://localhost/wt"))
      end}
    ].

%% GHSA-f9vr analog: a header carrying CR/LF must be refused before any
%% connection attempt.
wt_header_injection_test_() ->
    [
     {"CRLF in header value rejected",
      fun() ->
          Result = hackney:wt_connect("https://localhost:1/wt",
                                      [{headers, [{<<"x-test">>, <<"bad\r\nInjected: 1">>}]}]),
          ?assertEqual({error, invalid_handshake_header}, Result)
      end},
     {"NUL in header name rejected",
      fun() ->
          Result = hackney:wt_connect("https://localhost:1/wt",
                                      [{headers, [{<<"x\0evil">>, <<"v">>}]}]),
          ?assertEqual({error, invalid_handshake_header}, Result)
      end}
    ].

%% A close the peer would refuse is rejected before reaching the session.
wt_close_validation_test_() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    {setup,
     fun() -> Pid end,
     fun(P) -> P ! stop end,
     [
      {"Reason over 1024 bytes rejected",
       fun() ->
           ?assertEqual({error, reason_too_long},
                        hackney:wt_close(Pid, {0, binary:copy(<<"a">>, 1025)}))
       end},
      {"Error code over 32 bits rejected",
       fun() ->
           ?assertEqual({error, invalid_error_code},
                        hackney:wt_close(Pid, {16#100000000, <<>>}))
       end},
      {"Reason that is not UTF-8 rejected",
       fun() ->
           ?assertEqual({error, invalid_reason},
                        hackney:wt_close(Pid, {0, <<16#ff, 16#fe>>}))
       end}
     ]}.

%%====================================================================
%% Integration tests (local HTTP/3 WebTransport echo server)
%%====================================================================

%% #970: connect_options reach webtransport:connect/4 as `socket_opts' and
%% `family'; webtransport keeps the UDP options for HTTP/3 and the TCP ones
%% for HTTP/2. Traced on a dial to a closed port, so no server is needed.
wt_socket_options_test_() ->
    [{"socket options and family are passed to webtransport",
      {timeout, 30, fun() ->
          Opts = traced_wt_opts([{connect_options, [{ip, {127,0,0,1}}, {nodelay, true},
                                                    {protocols, [http3]}, {family, inet}]}]),
          ?assertEqual([{ip, {127,0,0,1}}, {nodelay, true}], maps:get(socket_opts, Opts)),
          ?assertEqual(inet, maps:get(family, Opts))
      end}},
     {"without connect_options nothing is added",
      {timeout, 30, fun() ->
          Opts = traced_wt_opts([]),
          ?assertNot(maps:is_key(socket_opts, Opts)),
          ?assertNot(maps:is_key(family, Opts))
      end}}].

%% The options map hackney hands to webtransport:connect/4.
traced_wt_opts(Extra) ->
    {ok, _} = application:ensure_all_started(hackney),
    Self = self(),
    Tracer = spawn(fun() -> receive {trace, _, call, {webtransport, connect, Args}} ->
                                Self ! {wt_args, Args}
                            after 10000 -> ok
                            end
                   end),
    {module, webtransport} = code:ensure_loaded(webtransport),
    1 = erlang:trace_pattern({webtransport, connect, 4}, true, [global]),
    _ = erlang:trace(new_processes, true, [call, {tracer, Tracer}]),
    try
        _ = hackney:wt_connect("https://localhost:1/wt",
                               [{verify, verify_none}, {connect_timeout, 500} | Extra]),
        receive {wt_args, [_Host, _Port, _Path, Opts]} -> Opts
        after 10000 -> erlang:error(webtransport_connect_not_called)
        end
    after
        _ = erlang:trace(new_processes, false, [call]),
        _ = erlang:trace_pattern({webtransport, connect, 4}, false, [global])
    end.

wt_integration_test_() ->
    {setup,
     fun start_wt_server/0,
     fun stop_wt_server/1,
     fun integration_tests/1}.

integration_tests(#{ok := false, reason := Reason}) ->
    [{"WebTransport integration skipped",
      fun() -> ?debugFmt("WebTransport integration skipped: ~p", [Reason]) end}];
integration_tests(#{ok := true, url := URL}) ->
    [
     {"Connect and close",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          ?assert(is_process_alive(C)),
          ok = hackney:wt_close(C)
      end}},
     {"Echo with socket options in connect_options (#970)",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL, [{connect_options, [{ip, {127,0,0,1}}, {nodelay, true},
                                                     {family, inet}]}]),
          ok = hackney:wt_send(C, {binary, <<"opts">>}),
          ?assertEqual({ok, {binary, <<"opts">>}}, hackney:wt_recv(C, 10000)),
          ok = hackney:wt_close(C)
      end}},
     {"Default-stream echo (binary)",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          ok = hackney:wt_send(C, {binary, <<"hello">>}),
          ?assertEqual({ok, {binary, <<"hello">>}}, hackney:wt_recv(C, 10000)),
          ok = hackney:wt_close(C)
      end}},
     {"Default-stream echo (text alias)",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          ok = hackney:wt_send(C, {text, <<"hi there">>}),
          ?assertEqual({ok, {binary, <<"hi there">>}}, hackney:wt_recv(C, 10000)),
          ok = hackney:wt_close(C)
      end}},
     {"Datagram echo",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          ok = hackney:wt_send_datagram(C, <<"ping">>),
          ?assertEqual({ok, {datagram, <<"ping">>}}, recv_datagram(C, 10000)),
          ok = hackney:wt_close(C)
      end}},
     {"Explicit multiplexed stream",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          {ok, Sid} = hackney:wt_open_stream(C, bidi),
          ?assert(is_integer(Sid)),
          ok = hackney:wt_stream_send(C, Sid, <<"abc">>),
          ?assertEqual(<<"abc">>, stream_bytes(hackney:wt_stream_recv(C, Sid, 10000))),
          ok = hackney:wt_close(C)
      end}},
     {"Two streams multiplexed over one session",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          {ok, S1} = hackney:wt_open_stream(C, bidi),
          {ok, S2} = hackney:wt_open_stream(C, bidi),
          ?assertNotEqual(S1, S2),
          ok = hackney:wt_stream_send(C, S1, <<"one">>),
          ok = hackney:wt_stream_send(C, S2, <<"two">>),
          ?assertEqual(<<"one">>, stream_bytes(hackney:wt_stream_recv(C, S1, 10000))),
          ?assertEqual(<<"two">>, stream_bytes(hackney:wt_stream_recv(C, S2, 10000))),
          ok = hackney:wt_close(C)
      end}},
     {"Active mode delivers default-stream data",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL, [{active, true}]),
          ok = hackney:wt_send(C, {binary, <<"active">>}),
          receive
              {hackney_wt, C, {binary, <<"active">>}} -> ok
          after 10000 ->
              ?assert(false)
          end,
          ok = hackney:wt_close(C)
      end}},
     {"recv in active mode is rejected",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL, [{active, true}]),
          ?assertEqual({error, {active_mode, true}}, hackney:wt_recv(C, 100)),
          ok = hackney:wt_close(C)
      end}},
     {"session_info",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          {ok, Info} = hackney:wt_session_info(C),
          ?assert(is_map(Info)),
          ?assertEqual(h3, maps:get(transport, Info)),
          ?assert(maps:is_key(stream_count, Info)),
          ok = hackney:wt_close(C)
      end}},
     {"Rejected close keeps the session open",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL),
          ?assertEqual({error, invalid_reason},
                       hackney:wt_close(C, {0, <<16#ff>>})),
          ok = hackney:wt_send(C, {binary, <<"still open">>}),
          ?assertEqual({ok, {binary, <<"still open">>}}, hackney:wt_recv(C, 10000)),
          ok = hackney:wt_close(C, {7, <<"bye">>})
      end}},
     {"Subprotocol negotiated",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL, [{protocols, [<<"chat-v2">>, <<"echo-v1">>]}]),
          {ok, Info} = hackney:wt_session_info(C),
          ?assertEqual(<<"echo-v1">>, maps:get(protocol, Info)),
          ok = hackney:wt_close(C)
      end}},
     {"require_protocol fails when the server picks none",
      {timeout, 30, fun() ->
          ?assertEqual({error, {alpn_error, no_protocol}},
                       connect(URL, [{protocols, [<<"chat-v2">>]},
                                     {require_protocol, true}]))
      end}}
    ].

%% The server allows two bidi streams: the default stream and one more. A
%% third open fails on the limit; once the extra stream is finished on both
%% sides the server grants more and an active client is told.
wt_stream_credit_test_() ->
    {setup,
     fun() -> start_wt_server(hackney_wt_credit_listener, #{max_streams_bidi => 2}) end,
     fun stop_wt_server/1,
     fun stream_credit_tests/1}.

stream_credit_tests(#{ok := false, reason := Reason}) ->
    [{"WebTransport stream credit skipped",
      fun() -> ?debugFmt("WebTransport stream credit skipped: ~p", [Reason]) end}];
stream_credit_tests(#{ok := true, url := URL}) ->
    [{"stream_credit is delivered in active mode",
      {timeout, 30, fun() ->
          {ok, C} = connect(URL, [{active, true}]),
          {ok, Sid} = hackney:wt_open_stream(C, bidi),
          ?assertEqual({error, stream_limit_reached}, hackney:wt_open_stream(C, bidi)),
          ok = hackney:wt_close_stream(C, Sid),
          receive
              {hackney_wt, C, {stream_credit, bidi}} -> ok
          after 10000 ->
              ?assert(false)
          end,
          ?assertMatch({ok, _}, hackney:wt_open_stream(C, bidi)),
          ok = hackney:wt_close(C)
      end}}].

%%====================================================================
%% Helpers
%%====================================================================

connect(URL) ->
    connect(URL, []).

connect(URL, Extra) ->
    connect(URL, Extra, 3).

%% The in-process WebTransport handshake over UDP loopback occasionally stalls
%% under load and times out; retry a few times so the integration tests don't
%% flake on a transient connect timeout.
connect(_URL, _Extra, 0) ->
    {error, connect_timeout};
connect(URL, Extra, Retries) ->
    case hackney:wt_connect(URL, [{verify, verify_none}, {connect_timeout, 10000} | Extra]) of
        {error, connect_timeout} ->
            connect(URL, Extra, Retries - 1);
        Other ->
            Other
    end.

%% Read until a datagram is seen (a stray default-stream chunk could in
%% principle arrive first; datagrams on loopback normally arrive directly).
recv_datagram(C, Timeout) ->
    case hackney:wt_recv(C, Timeout) of
        {ok, {datagram, _}} = R -> R;
        {ok, _Other} -> recv_datagram(C, Timeout);
        {error, _} = E -> E
    end.

stream_bytes({ok, Data}) when is_binary(Data) -> Data;
stream_bytes({ok, {fin, Data}}) -> Data;
stream_bytes(Other) -> Other.

start_wt_server() ->
    start_wt_server(?LISTENER, #{}).

start_wt_server(Name, Extra) ->
    {ok, _} = application:ensure_all_started(hackney),
    case make_cert() of
        {ok, CertFile, KeyFile} ->
            Port = free_port(),
            Opts = Extra#{
                transport => h3,
                port => Port,
                certfile => CertFile,
                keyfile => KeyFile,
                handler => mock_wt_handler
            },
            try webtransport:start_listener(Name, Opts) of
                {ok, _Pid} ->
                    timer:sleep(200),
                    URL = "https://localhost:" ++ integer_to_list(Port) ++ "/wt",
                    #{ok => true, url => URL, started => true, name => Name};
                {error, Reason} ->
                    #{ok => false, reason => {listener, Reason}}
            catch
                Class:Err ->
                    #{ok => false, reason => {listener_crash, Class, Err}}
            end;
        {error, Reason} ->
            #{ok => false, reason => {cert, Reason}}
    end.

stop_wt_server(#{started := true, name := Name}) ->
    _ = (try webtransport:stop_listener(Name) catch _:_ -> ok end),
    ok;
stop_wt_server(_) ->
    ok.

free_port() ->
    {ok, S} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(S),
    gen_tcp:close(S),
    Port.

%% Generate a short-lived self-signed cert with openssl; returns an error
%% tuple (which causes the integration tests to skip) if anything fails.
make_cert() ->
    Dir = test_tmp_dir(),
    KeyFile = filename:join(Dir, "wt_test_key.pem"),
    CertFile = filename:join(Dir, "wt_test_cert.pem"),
    KeyCmd = lists:flatten(io_lib:format(
        "openssl genrsa -out ~s 2048 2>/dev/null", [KeyFile])),
    _ = os:cmd(KeyCmd),
    CertCmd = lists:flatten(io_lib:format(
        "openssl req -new -x509 -key ~s -out ~s -days 1 -subj \"/CN=localhost\" 2>/dev/null",
        [KeyFile, CertFile])),
    _ = os:cmd(CertCmd),
    case {filelib:is_regular(KeyFile), filelib:is_regular(CertFile)} of
        {true, true} -> {ok, CertFile, KeyFile};
        _ -> {error, openssl_unavailable}
    end.

test_tmp_dir() ->
    Base = case os:getenv("TMPDIR") of
        false -> "/tmp";
        "" -> "/tmp";
        T -> T
    end,
    Dir = filename:join(Base, "hackney_wt_test"),
    _ = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.
