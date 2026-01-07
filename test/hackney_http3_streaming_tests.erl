%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc Tests for HTTP/3 async streaming support.

-module(hackney_http3_streaming_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    hackney_altsvc:clear_all(),
    ok.

cleanup(_) ->
    hackney_conn_sup:stop_all(),
    hackney_altsvc:clear_all(),
    ok.

%%====================================================================
%% HTTP/3 Async Streaming Tests
%%====================================================================

h3_async_streaming_test_() ->
    {
        "HTTP/3 async streaming tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"async=true streams continuously", fun test_h3_async_true/0},
                {"async=once streams on demand", fun test_h3_async_once/0},
                {"async streaming receives status", fun test_h3_async_status/0},
                {"async streaming receives headers", fun test_h3_async_headers/0}
            ]
        }
    }.

%%====================================================================
%% HTTP/3 Pull-based Streaming Tests (stream_body)
%%====================================================================

h3_stream_body_test_() ->
    {
        "HTTP/3 stream_body tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"stream_body returns chunks", fun test_h3_stream_body/0},
                {"stream_body returns done", fun test_h3_stream_body_done/0}
            ]
        }
    }.

test_h3_stream_body() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Connect with HTTP/3
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}],
            case hackney:connect(hackney_ssl, "cloudflare.com", 443, Opts) of
                {ok, ConnPid} ->
                    %% Use request_streaming to get headers first
                    case hackney_conn:request_streaming(ConnPid, <<"GET">>, <<"/cdn-cgi/trace">>, [], <<>>) of
                        {ok, Status, Headers} ->
                            ?debugFmt("stream_body: Status=~p, Headers=~p", [Status, Headers]),
                            ?assert(Status >= 200 andalso Status < 400),
                            ?assert(is_list(Headers)),
                            %% Now read body with stream_body
                            Body = read_all_chunks(ConnPid),
                            ?debugFmt("stream_body: Body=~s", [Body]),
                            ?assert(byte_size(Body) > 0),
                            hackney:close(ConnPid);
                        {error, Reason} ->
                            hackney:close(ConnPid),
                            ?debugFmt("request_streaming failed: ~p", [Reason])
                    end;
                {error, Reason} ->
                    ?debugFmt("H3 connect failed: ~p", [Reason])
            end
    end.

test_h3_stream_body_done() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}],
            case hackney:connect(hackney_ssl, "cloudflare.com", 443, Opts) of
                {ok, ConnPid} ->
                    case hackney_conn:request_streaming(ConnPid, <<"GET">>, <<"/cdn-cgi/trace">>, [], <<>>) of
                        {ok, _Status, _Headers} ->
                            %% Read all chunks until done
                            Body = read_all_chunks(ConnPid),
                            ?assert(byte_size(Body) > 0),
                            %% Calling stream_body again should return error (no stream)
                            Result = hackney_conn:stream_body(ConnPid),
                            ?debugFmt("After done, stream_body returns: ~p", [Result]),
                            hackney:close(ConnPid);
                        {error, Reason} ->
                            hackney:close(ConnPid),
                            ?debugFmt("request_streaming failed: ~p", [Reason])
                    end;
                {error, Reason} ->
                    ?debugFmt("H3 connect failed: ~p", [Reason])
            end
    end.

read_all_chunks(ConnPid) ->
    read_all_chunks(ConnPid, <<>>).

read_all_chunks(ConnPid, Acc) ->
    case hackney_conn:stream_body(ConnPid) of
        {ok, Chunk} ->
            read_all_chunks(ConnPid, <<Acc/binary, Chunk/binary>>);
        done ->
            Acc;
        {error, _Reason} ->
            Acc
    end.

%%====================================================================
%% HTTP/3 Body Sending Tests (streaming uploads)
%%====================================================================

h3_send_body_test_() ->
    {
        "HTTP/3 send_body tests",
        {
            setup,
            fun setup/0, fun cleanup/1,
            [
                {"send body in chunks", fun test_h3_send_body_chunks/0}
            ]
        }
    }.

test_h3_send_body_chunks() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Connect with HTTP/3 to cloudflare (supports H3)
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}],
            case hackney:connect(hackney_ssl, "cloudflare.com", 443, Opts) of
                {ok, ConnPid} ->
                    %% Send headers for POST request (will get redirect, but tests the mechanism)
                    Headers = [{<<"content-type">>, <<"text/plain">>}],
                    case hackney_conn:send_request_headers(ConnPid, <<"POST">>, <<"/">>, Headers) of
                        ok ->
                            ?debugFmt("send_request_headers ok", []),
                            %% Send body in chunks
                            ok = hackney_conn:send_body_chunk(ConnPid, <<"Hello ">>),
                            ?debugFmt("send_body_chunk 1 ok", []),
                            ok = hackney_conn:send_body_chunk(ConnPid, <<"World!">>),
                            ?debugFmt("send_body_chunk 2 ok", []),
                            ok = hackney_conn:finish_send_body(ConnPid),
                            ?debugFmt("finish_send_body ok", []),
                            %% Get response
                            case hackney_conn:start_response(ConnPid) of
                                {ok, Status, RespHeaders, _Pid} ->
                                    ?debugFmt("send_body: Status=~p", [Status]),
                                    %% Cloudflare returns redirect (301/302) for POST to /
                                    ?assert(is_integer(Status)),
                                    ?assert(is_list(RespHeaders)),
                                    hackney:close(ConnPid);
                                {error, Reason} ->
                                    hackney:close(ConnPid),
                                    ?debugFmt("start_response failed: ~p", [Reason])
                            end;
                        {error, Reason} ->
                            hackney:close(ConnPid),
                            ?debugFmt("send_request_headers failed: ~p", [Reason])
                    end;
                {error, Reason} ->
                    ?debugFmt("H3 connect failed: ~p", [Reason])
            end
    end.

test_h3_async_true() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Async streaming request over HTTP/3
            URL = <<"https://cloudflare.com/cdn-cgi/trace">>,
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}, {async, true}],
            case hackney:get(URL, [], <<>>, Opts) of
                {ok, Ref} when is_pid(Ref) ->
                    %% Collect all async messages
                    Messages = collect_async_messages(Ref, 10000),
                    ?debugFmt("Async messages: ~p", [Messages]),
                    %% Verify we got status, headers, body chunks, and done
                    ?assert(has_status_message(Messages)),
                    ?assert(has_headers_message(Messages)),
                    ?assert(has_done_message(Messages));
                {error, Reason} ->
                    ?debugFmt("H3 async request failed (may be blocked): ~p", [Reason])
            end
    end.

test_h3_async_once() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            %% Async once mode - should still work (data is pushed by QUIC)
            URL = <<"https://cloudflare.com/cdn-cgi/trace">>,
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}, {async, once}],
            case hackney:get(URL, [], <<>>, Opts) of
                {ok, Ref} when is_pid(Ref) ->
                    %% Collect messages
                    Messages = collect_async_messages(Ref, 10000),
                    ?debugFmt("Async once messages: ~p", [Messages]),
                    ?assert(has_status_message(Messages)),
                    ?assert(has_headers_message(Messages));
                {error, Reason} ->
                    ?debugFmt("H3 async once request failed: ~p", [Reason])
            end
    end.

test_h3_async_status() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            URL = <<"https://cloudflare.com/">>,
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}, {async, true}],
            case hackney:get(URL, [], <<>>, Opts) of
                {ok, Ref} when is_pid(Ref) ->
                    Messages = collect_async_messages(Ref, 10000),
                    %% Find status message
                    StatusMsg = find_message(fun({hackney_response, _, {status, S, _}}) -> is_integer(S);
                                                (_) -> false
                                             end, Messages),
                    ?assertNotEqual(undefined, StatusMsg),
                    {hackney_response, _, {status, Status, _}} = StatusMsg,
                    ?assert(Status >= 200 andalso Status < 400);
                {error, Reason} ->
                    ?debugFmt("H3 async status test failed: ~p", [Reason])
            end
    end.

test_h3_async_headers() ->
    case hackney_quic:is_available() of
        false ->
            {skip, "QUIC NIF not available"};
        true ->
            URL = <<"https://cloudflare.com/">>,
            Opts = [{protocols, [http3]}, {connect_timeout, 15000}, {async, true}],
            case hackney:get(URL, [], <<>>, Opts) of
                {ok, Ref} when is_pid(Ref) ->
                    Messages = collect_async_messages(Ref, 10000),
                    %% Find headers message
                    HeadersMsg = find_message(fun({hackney_response, _, {headers, H}}) -> is_list(H);
                                                 (_) -> false
                                              end, Messages),
                    ?assertNotEqual(undefined, HeadersMsg),
                    {hackney_response, _, {headers, Headers}} = HeadersMsg,
                    ?assert(length(Headers) > 0),
                    ?debugFmt("H3 async headers: ~p", [Headers]);
                {error, Reason} ->
                    ?debugFmt("H3 async headers test failed: ~p", [Reason])
            end
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

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

find_message(Pred, Messages) ->
    case lists:filter(Pred, Messages) of
        [First | _] -> First;
        [] -> undefined
    end.
