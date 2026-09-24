%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/3 async streaming support, against a local
%%% HTTP/3 server (hackney_h3_test_server).

-module(hackney_http3_streaming_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    Server = hackney_h3_test_server:start(),
    hackney_altsvc:clear_all(),
    Server.

cleanup(Server) ->
    hackney_conn_sup:stop_all(),
    hackney_altsvc:clear_all(),
    hackney_h3_test_server:stop(Server).

with_server(Tests) ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Server) ->
         [{Title, {timeout, 30, fun() -> Test(Server) end}} || {Title, Test} <- Tests]
     end}.

%%====================================================================
%% HTTP/3 Async Streaming Tests
%%====================================================================

h3_async_streaming_test_() ->
    with_server([
        {"async=true streams continuously", fun test_h3_async_true/1},
        {"async=once streams on demand", fun test_h3_async_once/1},
        {"async streaming receives status", fun test_h3_async_status/1},
        {"async streaming receives headers", fun test_h3_async_headers/1}
    ]).

%%====================================================================
%% HTTP/3 Pull-based Streaming Tests (stream_body)
%%====================================================================

h3_stream_body_test_() ->
    with_server([
        {"stream_body returns chunks", fun test_h3_stream_body/1},
        {"stream_body returns done", fun test_h3_stream_body_done/1}
    ]).

test_h3_stream_body(Server) ->
    {ok, ConnPid} = connect(Server),
    {ok, Status, Headers} =
        hackney_conn:request_streaming(ConnPid, <<"GET">>, <<"/large">>, [], <<>>),
    ?assertEqual(200, Status),
    ?assert(is_list(Headers)),
    Body = read_all_chunks(ConnPid),
    ?assertEqual(binary:copy(<<"x">>, 65536), Body),
    hackney:close(ConnPid).

test_h3_stream_body_done(Server) ->
    {ok, ConnPid} = connect(Server),
    {ok, 200, _Headers} =
        hackney_conn:request_streaming(ConnPid, <<"GET">>, <<"/cdn-cgi/trace">>, [], <<>>),
    ?assertEqual(<<"h=127.0.0.1\nhttp=http/3\n">>, read_all_chunks(ConnPid)),
    %% Once the stream is done, stream_body has no stream to read.
    ?assertEqual({error, no_stream}, hackney_conn:stream_body(ConnPid)),
    hackney:close(ConnPid).

read_all_chunks(ConnPid) ->
    read_all_chunks(ConnPid, <<>>).

read_all_chunks(ConnPid, Acc) ->
    case hackney_conn:stream_body(ConnPid) of
        {ok, Chunk} -> read_all_chunks(ConnPid, <<Acc/binary, Chunk/binary>>);
        done -> Acc
    end.

%%====================================================================
%% HTTP/3 send_body Tests
%%====================================================================

h3_send_body_test_() ->
    with_server([
        {"send body in chunks", fun test_h3_send_body_chunks/1},
        {"stream_body after an upload", fun test_h3_upload_stream_body/1},
        {"upload through hackney:request", fun test_h3_upload_public_api/1},
        {"start_response after the response arrived", fun test_h3_upload_response_first/1},
        {"body/1 on a reset stream returns an error", fun test_h3_body_stream_reset/1}
    ]).

test_h3_send_body_chunks(Server) ->
    {ok, ConnPid} = connect(Server),
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    ok = hackney_conn:send_request_headers(ConnPid, <<"POST">>, <<"/echo">>, Headers),
    ok = hackney_conn:send_body_chunk(ConnPid, <<"Hello ">>),
    ok = hackney_conn:send_body_chunk(ConnPid, <<"World!">>),
    ok = hackney_conn:finish_send_body(ConnPid),
    {ok, 200, RespHeaders, _} = hackney_conn:start_response(ConnPid),
    ?assertEqual(<<"text/plain">>,
                 proplists:get_value(<<"content-type">>, RespHeaders)),
    ?assertEqual({ok, <<"Hello World!">>}, hackney_conn:body(ConnPid)),
    hackney:close(ConnPid).

test_h3_upload_stream_body(Server) ->
    {ok, ConnPid} = connect(Server),
    ok = upload(ConnPid, [<<"chunk 1, ">>, <<"chunk 2">>]),
    {ok, 200, _RespHeaders, _} = hackney_conn:start_response(ConnPid),
    ?assertEqual(<<"chunk 1, chunk 2">>, read_all_chunks(ConnPid)),
    hackney:close(ConnPid).

test_h3_upload_public_api(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/echo">>),
    {ok, Ref} = hackney:request(post, URL, [{<<"content-type">>, <<"text/plain">>}],
                                stream, [{pool, false} | opts()]),
    ok = hackney:send_body(Ref, <<"Hello ">>),
    ok = hackney:send_body(Ref, <<"World!">>),
    ok = hackney:finish_send_body(Ref),
    {ok, 200, _RespHeaders, Ref} = hackney:start_response(Ref),
    ?assertEqual({ok, <<"Hello World!">>}, hackney:body(Ref)),
    hackney:close(Ref).

%% The response headers are in before start_response/1 is called: it must
%% answer at once instead of waiting for headers that already came.
test_h3_upload_response_first(Server) ->
    {ok, ConnPid} = connect(Server),
    ok = upload(ConnPid, [<<"early">>]),
    ok = wait_until(fun() -> hackney_conn:response_headers(ConnPid) =/= undefined end),
    {ok, 200, _RespHeaders, _} = hackney_conn:start_response(ConnPid),
    ?assertEqual({ok, <<"early">>}, hackney_conn:body(ConnPid)),
    hackney:close(ConnPid).

%% A body/1 caller parked on a stream the server resets gets an error
%% instead of waiting forever. Needs a quic that reports a peer reset.
test_h3_body_stream_reset(Server) ->
    true = register(hackney_h3_test_reset, self()),
    try
        {ok, ConnPid} = connect(Server),
        {ok, 200, _Headers} =
            hackney_conn:request_streaming(ConnPid, <<"GET">>, <<"/reset">>, [], <<>>),
        Handler = receive {reset_ready, H} -> H after 15000 -> error(no_reset_handler) end,
        Parent = self(),
        spawn_link(fun() -> Parent ! {body, hackney_conn:body(ConnPid)} end),
        ok = wait_until(fun() -> body_parked(ConnPid) end),
        Handler ! reset,
        receive
            {body, Result} -> ?assertMatch({error, {stream_reset, _}}, Result)
        after 15000 ->
            error(body_not_answered)
        end,
        hackney:close(ConnPid)
    after
        unregister(hackney_h3_test_reset)
    end.

%% True once a body/1 call is parked on an HTTP/3 stream of the connection.
body_parked(ConnPid) ->
    {_StateName, Data} = sys:get_state(ConnPid),
    lists:any(fun(Field) ->
                  is_map(Field) andalso
                  lists:any(fun({_, State}) when is_tuple(State), tuple_size(State) > 0 ->
                                    element(1, State) =:= streaming_body_full;
                               (_) -> false
                            end, maps:values(Field))
              end, tuple_to_list(Data)).

%%====================================================================
%% Async tests
%%====================================================================

test_h3_async_true(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/large">>),
    {ok, Ref} = hackney:get(URL, [], <<>>, [{async, true} | opts()]),
    Messages = collect_async_messages(Ref, 15000),
    ?assert(has_status_message(Messages)),
    ?assert(has_headers_message(Messages)),
    ?assert(has_done_message(Messages)),
    ?assertEqual(binary:copy(<<"x">>, 65536), body_of(Messages)).

test_h3_async_once(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/cdn-cgi/trace">>),
    {ok, Ref} = hackney:get(URL, [], <<>>, [{async, once} | opts()]),
    Messages = collect_once_messages(Ref, 15000),
    ?assert(has_status_message(Messages)),
    ?assert(has_headers_message(Messages)),
    ?assert(has_done_message(Messages)),
    ?assertEqual(<<"h=127.0.0.1\nhttp=http/3\n">>, body_of(Messages)).

test_h3_async_status(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/">>),
    {ok, Ref} = hackney:get(URL, [], <<>>, [{async, true} | opts()]),
    Messages = collect_async_messages(Ref, 15000),
    ?assertMatch({hackney_response, _, {status, 200, _}},
                 find_message(fun({hackney_response, _, {status, _, _}}) -> true;
                                 (_) -> false
                              end, Messages)).

test_h3_async_headers(Server) ->
    URL = hackney_h3_test_server:url(Server, <<"/">>),
    {ok, Ref} = hackney:get(URL, [], <<>>, [{async, true} | opts()]),
    Messages = collect_async_messages(Ref, 15000),
    {hackney_response, _, {headers, Headers}} =
        find_message(fun({hackney_response, _, {headers, _}}) -> true;
                        (_) -> false
                     end, Messages),
    ?assertEqual(<<"text/html">>, proplists:get_value(<<"content-type">>, Headers)).

%%====================================================================
%% Helper Functions
%%====================================================================

opts() ->
    hackney_h3_test_server:hackney_opts().

upload(ConnPid, Chunks) ->
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    ok = hackney_conn:send_request_headers(ConnPid, <<"POST">>, <<"/echo">>, Headers),
    lists:foreach(fun(Chunk) -> ok = hackney_conn:send_body_chunk(ConnPid, Chunk) end,
                  Chunks),
    hackney_conn:finish_send_body(ConnPid).

wait_until(Fun) ->
    wait_until(Fun, erlang:monotonic_time(millisecond) + 15000).

wait_until(Fun, Deadline) ->
    case Fun() of
        true -> ok;
        false ->
            case erlang:monotonic_time(millisecond) > Deadline of
                true -> error(wait_until_timeout);
                false -> receive after 5 -> ok end, wait_until(Fun, Deadline)
            end
    end.

connect(Server) ->
    hackney:connect(hackney_ssl, "127.0.0.1", hackney_h3_test_server:port(Server),
                    opts()).

collect_async_messages(Ref, Timeout) ->
    collect_async_messages(Ref, Timeout, []).

collect_async_messages(Ref, Timeout, Acc) ->
    receive
        {hackney_response, Ref, done} ->
            lists:reverse([{hackney_response, Ref, done} | Acc]);
        {hackney_response, Ref, {error, _Reason} = Error} ->
            lists:reverse([{hackney_response, Ref, Error} | Acc]);
        {hackney_response, Ref, Msg} ->
            collect_async_messages(Ref, Timeout, [{hackney_response, Ref, Msg} | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

%% async once delivers one message per stream_next/1.
collect_once_messages(Ref, Timeout) ->
    collect_once_messages(Ref, Timeout, []).

collect_once_messages(Ref, Timeout, Acc) ->
    receive
        {hackney_response, Ref, done} ->
            lists:reverse([{hackney_response, Ref, done} | Acc]);
        {hackney_response, Ref, {error, _Reason} = Error} ->
            lists:reverse([{hackney_response, Ref, Error} | Acc]);
        {hackney_response, Ref, Msg} ->
            ok = hackney:stream_next(Ref),
            collect_once_messages(Ref, Timeout, [{hackney_response, Ref, Msg} | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

has_status_message(Messages) ->
    lists:any(fun({hackney_response, _, {status, _, _}}) -> true;
                 (_) -> false
              end, Messages).

has_headers_message(Messages) ->
    lists:any(fun({hackney_response, _, {headers, _}}) -> true;
                 (_) -> false
              end, Messages).

has_done_message(Messages) ->
    lists:any(fun({hackney_response, _, done}) -> true;
                 (_) -> false
              end, Messages).

body_of(Messages) ->
    iolist_to_binary([Data || {hackney_response, _, Data} <- Messages,
                              is_binary(Data)]).

find_message(Pred, Messages) ->
    case lists:filter(Pred, Messages) of
        [First | _] -> First;
        [] -> undefined
    end.
