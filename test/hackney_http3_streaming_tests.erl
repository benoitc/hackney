%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2025 Benoit Chesneau
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
