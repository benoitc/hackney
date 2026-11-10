%%% A streamed HTTP/1.1 request body (request/5 with `stream', then
%%% send_body/2 and finish_send_body/1) honors a Content-Length the caller
%%% set: the body goes out raw, with no Transfer-Encoding and no chunk
%%% framing. Before, hackney always added Transfer-Encoding: chunked and
%%% chunk-framed every write, so the request carried both headers (RFC 9112
%%% 6.2) and a server honoring Content-Length read the framing as body bytes.
%%%
%%% Every test asserts the raw bytes a local listener receives.
-module(hackney_stream_content_length_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OK, <<"HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n">>).

stream_content_length_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(hackney), ok end,
     fun(_) -> ok end,
     [{"Content-Length, one write", fun cl_one_write/0},
      {"Content-Length in another case", fun cl_header_case/0},
      {"Content-Length, several writes", fun cl_several_writes/0},
      {"Content-Length, fun/0 producer", fun cl_fun0/0},
      {"Content-Length, {Fun, State} producer", fun cl_fun_state/0},
      {"no Content-Length: chunked as before", fun chunked_unchanged/0},
      {"both headers: Transfer-Encoding only, chunked", fun both_headers/0},
      {"a pooled conn reused for a raw then a chunked body",
       {timeout, 30, fun keepalive_reuse/0}},
      {"a short body is an error and the conn is not reused",
       {timeout, 30, fun short_body/0}},
      {"a long body is an error and the conn is not reused",
       {timeout, 30, fun long_body/0}},
      {"an invalid Content-Length is refused", fun invalid_content_length/0},
      {"an empty write does not end a chunked body", fun empty_write_chunked/0},
      {"an empty write is nothing on a raw body", fun empty_write_raw/0},
      {"a Transfer-Encoding without chunked last gets chunked appended",
       fun te_without_chunked/0},
      {"chunked not last in Transfer-Encoding is refused", fun te_chunked_not_last/0},
      {"equal duplicate Content-Length values are sent once", fun duplicate_equal/0},
      {"different duplicate Content-Length values are refused",
       fun duplicate_different/0}]}.

%%====================================================================
%% Tests
%%====================================================================

cl_one_write() ->
    Raw = upload([{<<"content-length">>, <<"5">>}], [<<"hello">>]),
    assert_raw(Raw, <<"hello">>).

cl_header_case() ->
    Raw = upload([{<<"Content-Length">>, <<"5">>}], [<<"hello">>]),
    assert_raw(Raw, <<"hello">>).

cl_several_writes() ->
    Raw = upload([{<<"content-length">>, <<"11">>}],
                 [<<"hel">>, [<<"lo">>, <<" ">>], <<"world">>]),
    assert_raw(Raw, <<"hello world">>).

%% The producer runs in the connection process: it keeps its position in a
%% public table.
cl_fun0() ->
    T = ets:new(chunks, [public]),
    true = ets:insert(T, {chunks, [<<"hel">>, <<"lo">>]}),
    Fun = fun() ->
              case ets:lookup(T, chunks) of
                  [{chunks, [H | Rest]}] -> true = ets:insert(T, {chunks, Rest}), {ok, H};
                  [{chunks, []}] -> eof
              end
          end,
    Raw = upload([{<<"content-length">>, <<"5">>}], [Fun]),
    ets:delete(T),
    assert_raw(Raw, <<"hello">>).

cl_fun_state() ->
    Fun = fun([]) -> eof;
             ([H | Rest]) -> {ok, H, Rest}
          end,
    Raw = upload([{<<"content-length">>, <<"5">>}], [{Fun, [<<"he">>, <<"llo">>]}]),
    assert_raw(Raw, <<"hello">>).

chunked_unchanged() ->
    Raw = upload([], [<<"hello">>]),
    {Head, Body} = split(Raw),
    ?assertMatch({_, _}, binary:match(Head, <<"\r\nTransfer-Encoding: chunked">>)),
    ?assertEqual(nomatch, binary:match(lower(Head), <<"content-length">>)),
    ?assertEqual(<<"5\r\nhello\r\n0\r\n\r\n">>, Body).

both_headers() ->
    Raw = upload([{<<"content-length">>, <<"5">>}, {<<"transfer-encoding">>, <<"chunked">>}],
                 [<<"hello">>]),
    {Head, Body} = split(Raw),
    ?assertEqual(nomatch, binary:match(lower(Head), <<"content-length">>)),
    ?assertMatch({_, _}, binary:match(lower(Head), <<"transfer-encoding: chunked">>)),
    ?assertEqual(<<"5\r\nhello\r\n0\r\n\r\n">>, Body).

keepalive_reuse() ->
    with_pool(fun(Pool, Server) ->
        C1 = stream(Server, Pool, [{<<"content-length">>, <<"5">>}], [<<"hello">>]),
        C2 = stream(Server, Pool, [], [<<"world">>]),
        ?assertEqual(C1, C2),
        [{1, R1}, {1, R2}] = requests(Server, 2),
        assert_raw(R1, <<"hello">>),
        {_, Body2} = split(R2),
        ?assertEqual(<<"5\r\nworld\r\n0\r\n\r\n">>, Body2)
    end).

short_body() ->
    with_pool(fun(Pool, Server) ->
        {ok, C} = request(Server, Pool, [{<<"content-length">>, <<"5">>}]),
        ok = hackney:send_body(C, <<"hel">>),
        ?assertEqual({error, {content_length_mismatch, 5, 3}}, hackney:finish_send_body(C)),
        assert_not_reused(C, Server, Pool)
    end).

long_body() ->
    with_pool(fun(Pool, Server) ->
        {ok, C} = request(Server, Pool, [{<<"content-length">>, <<"3">>}]),
        ?assertEqual({error, {content_length_mismatch, 3, 5}},
                     hackney:send_body(C, <<"hello">>)),
        assert_not_reused(C, Server, Pool)
    end).

invalid_content_length() ->
    Server = start_server(),
    [?assertEqual({error, invalid_content_length},
                  hackney:request(put, url(Server), [{<<"content-length">>, V}], stream,
                                  [{pool, false}]))
     || V <- [<<"abc">>, <<"-1">>, <<"+5">>, <<"5, 5">>, <<>>]],
    stop_server(Server).

%% A chunk of size 0 is the last chunk (RFC 9112 7.1): an empty write used
%% to end the body, and the next write became a stray request.
empty_write_chunked() ->
    Raw = upload([], [<<"he">>, <<>>, [], <<"llo">>]),
    {_, Body} = split(Raw),
    ?assertEqual(<<"2\r\nhe\r\n3\r\nllo\r\n0\r\n\r\n">>, Body).

empty_write_raw() ->
    Raw = upload([{<<"content-length">>, <<"5">>}], [<<"he">>, <<>>, <<"llo">>]),
    assert_raw(Raw, <<"hello">>).

%% hackney frames the body as chunked, so chunked must be the final coding
%% of the request (RFC 9112 6.1).
te_without_chunked() ->
    Raw = upload([{<<"Transfer-Encoding">>, <<"gzip">>}], [<<"hello">>]),
    {Head, Body} = split(Raw),
    ?assertMatch({_, _}, binary:match(Head, <<"\r\nTransfer-Encoding: gzip, chunked">>)),
    ?assertEqual(<<"5\r\nhello\r\n0\r\n\r\n">>, Body).

te_chunked_not_last() ->
    Server = start_server(),
    ?assertEqual({error, invalid_transfer_encoding},
                 hackney:request(put, url(Server), [{<<"transfer-encoding">>, <<"chunked, gzip">>}],
                                 stream, [{pool, false}])),
    stop_server(Server).

duplicate_equal() ->
    Raw = upload([{<<"content-length">>, <<"5">>}, {<<"Content-Length">>, <<"5">>}],
                 [<<"hello">>]),
    {Head, _} = split(Raw),
    ?assertEqual(1, length(binary:matches(lower(Head), <<"content-length">>))),
    assert_raw(Raw, <<"hello">>).

duplicate_different() ->
    Server = start_server(),
    ?assertEqual({error, invalid_content_length},
                 hackney:request(put, url(Server), [{<<"content-length">>, <<"5">>},
                                                    {<<"content-length">>, <<"6">>}],
                                 stream, [{pool, false}])),
    stop_server(Server).

%%====================================================================
%% Helpers
%%====================================================================

%% One streamed upload on its own connection; returns the raw request.
upload(Headers, Writes) ->
    Server = start_server(),
    {ok, C} = hackney:request(put, url(Server), Headers, stream, [{pool, false}]),
    [ok = hackney:send_body(C, W) || W <- Writes],
    ok = hackney:finish_send_body(C),
    {ok, 200, _, C} = hackney:start_response(C),
    [{1, Raw}] = requests(Server, 1),
    hackney:close(C),
    stop_server(Server),
    Raw.

%% A streamed upload through Pool, read to the end so the conn goes back.
stream(Server, Pool, Headers, Writes) ->
    {ok, C} = request(Server, Pool, Headers),
    [ok = hackney:send_body(C, W) || W <- Writes],
    ok = hackney:finish_send_body(C),
    {ok, 200, _, C} = hackney:start_response(C),
    {ok, <<>>} = hackney:body(C),
    C.

request(Server, Pool, Headers) ->
    hackney:request(put, url(Server), Headers, stream, [{pool, Pool}]).

%% A conn whose body did not match its Content-Length is closed: it is not
%% alive in a reusable state, and the next request opens a new connection.
assert_not_reused(C, Server, Pool) ->
    ?assert(not is_process_alive(C) orelse element(1, sys:get_state(C)) =:= closed),
    C2 = stream(Server, Pool, [], [<<"x">>]),
    ?assertNotEqual(C, C2),
    %% The new request came in on the listener's second connection.
    ?assertMatch([{2, _}], requests(Server, 1)).

assert_raw(Raw, Expected) ->
    {Head, Body} = split(Raw),
    ?assertEqual(nomatch, binary:match(lower(Head), <<"transfer-encoding">>)),
    ?assertMatch({_, _}, binary:match(lower(Head), <<"content-length: ",
                                                     (integer_to_binary(byte_size(Expected)))/binary>>)),
    ?assertEqual(Expected, Body).

split(Raw) ->
    [Head, Body] = binary:split(Raw, <<"\r\n\r\n">>),
    {Head, Body}.

lower(Bin) -> string:lowercase(Bin).

with_pool(Fun) ->
    Server = start_server(),
    Pool = list_to_atom("stream_cl_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = hackney_pool:start_pool(Pool, [{pool_size, 1}, {prewarm_count, 0}]),
    try Fun(Pool, Server)
    after
        hackney_pool:stop_pool(Pool),
        stop_server(Server)
    end.

%% Requests seen by this server only: every message carries its reference.
requests(#{ref := Ref}, N) ->
    [receive {request, Ref, Conn, Raw} -> {Conn, Raw} after 5000 -> erlang:error(no_request) end
     || _ <- lists:seq(1, N)].

url(#{port := Port}) ->
    iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/k"]).

%% A keep-alive HTTP/1.1 listener. It reports each complete request, tagged
%% with the number of the connection it came in on, and answers 200.
start_server() ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(L),
    Test = {self(), make_ref()},
    Pid = spawn(fun() -> accept(L, Test, 1) end),
    #{pid => Pid, port => Port, ref => element(2, Test)}.

stop_server(#{pid := Pid}) ->
    exit(Pid, shutdown),
    ok.

accept(L, Test, N) ->
    case gen_tcp:accept(L) of
        {ok, S} ->
            Pid = spawn(fun() -> receive go -> serve(S, Test, N, <<>>) end end),
            ok = gen_tcp:controlling_process(S, Pid),
            Pid ! go,
            accept(L, Test, N + 1);
        {error, _} ->
            ok
    end.

serve(S, Test, N, Acc) ->
    case complete(Acc) of
        {ok, Req, Rest} ->
            {TestPid, Ref} = Test,
            TestPid ! {request, Ref, N, Req},
            ok = gen_tcp:send(S, ?OK),
            serve(S, Test, N, Rest);
        more ->
            case gen_tcp:recv(S, 0, 10000) of
                {ok, Data} -> serve(S, Test, N, <<Acc/binary, Data/binary>>);
                {error, _} -> ok
            end
    end.

%% A complete request in Acc, framed by its own headers.
complete(Acc) ->
    case binary:split(Acc, <<"\r\n\r\n">>) of
        [Head, Body] ->
            HeadL = lower(Head),
            case re:run(HeadL, <<"transfer-encoding:[^\\r]*chunked">>) of
                nomatch ->
                    Len = case re:run(HeadL, <<"content-length: *([0-9]+)">>,
                                      [{capture, all_but_first, binary}]) of
                              {match, [L]} -> binary_to_integer(L);
                              nomatch -> 0
                          end,
                    case Body of
                        <<B:Len/binary, Rest/binary>> ->
                            {ok, <<Head/binary, "\r\n\r\n", B/binary>>, Rest};
                        _ -> more
                    end;
                _ ->
                    case binary:match(<<"\r\n", Body/binary>>, <<"\r\n0\r\n\r\n">>) of
                        {Pos, 7} ->
                            End = Pos + 7 - 2,
                            <<B:End/binary, Rest/binary>> = Body,
                            {ok, <<Head/binary, "\r\n\r\n", B/binary>>, Rest};
                        nomatch -> more
                    end
            end;
        [_] ->
            more
    end.
