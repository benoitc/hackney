%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(hackney_keepalive_tests).

-include_lib("eunit/include/eunit.hrl").

%% Build a hackney_headers object from a {Key, Value} list.
h(List) -> hackney_headers:from_list(List).

%%====================================================================
%% should_close/3
%%====================================================================

http11_no_header_keeps_alive_test() ->
    %% HTTP/1.1 default is keep-alive: an absent Connection header is poolable.
    ?assertEqual(false, hackney_keepalive:should_close({1, 1}, h([]), false)).

http11_connection_close_closes_test() ->
    ?assertEqual(true,
                 hackney_keepalive:should_close(
                   {1, 1}, h([{<<"Connection">>, <<"close">>}]), false)).

http11_connection_keep_alive_keeps_alive_test() ->
    ?assertEqual(false,
                 hackney_keepalive:should_close(
                   {1, 1}, h([{<<"Connection">>, <<"keep-alive">>}]), false)).

http10_no_header_closes_test() ->
    %% HTTP/1.0 default is close.
    ?assertEqual(true, hackney_keepalive:should_close({1, 0}, h([]), false)).

http10_keep_alive_keeps_alive_test() ->
    ?assertEqual(false,
                 hackney_keepalive:should_close(
                   {1, 0}, h([{<<"Connection">>, <<"keep-alive">>}]), false)).

request_close_closes_test() ->
    %% Caller asked the server to close: never reuse, regardless of response.
    ?assertEqual(true, hackney_keepalive:should_close({1, 1}, h([]), true)),
    ?assertEqual(true,
                 hackney_keepalive:should_close(
                   {1, 1}, h([{<<"Connection">>, <<"keep-alive">>}]), true)).

token_list_without_close_keeps_alive_test() ->
    %% "keep-alive, Upgrade" is a token list with no close token.
    ?assertEqual(false,
                 hackney_keepalive:should_close(
                   {1, 1}, h([{<<"Connection">>, <<"keep-alive, Upgrade">>}]), false)).

token_list_with_close_closes_test() ->
    ?assertEqual(true,
                 hackney_keepalive:should_close(
                   {1, 1}, h([{<<"Connection">>, <<"close, Foo">>}]), false)).

multiple_connection_headers_close_closes_test() ->
    %% Connection split across two header lines (RFC 7230 list field).
    ?assertEqual(true,
                 hackney_keepalive:should_close(
                   {1, 1},
                   h([{<<"Connection">>, <<"keep-alive">>},
                      {<<"Connection">>, <<"close">>}]),
                   false)).

mixed_case_close_closes_test() ->
    ?assertEqual(true,
                 hackney_keepalive:should_close(
                   {1, 1}, h([{<<"Connection">>, <<"Close">>}]), false)).

whitespace_around_token_test() ->
    ?assertEqual(true,
                 hackney_keepalive:should_close(
                   {1, 1}, h([{<<"Connection">>, <<"  close  ">>}]), false)).

unknown_version_parsed_closes_test() ->
    %% Anomalous: a parsed response with an unknown version closes on the safe side.
    ?assertEqual(true, hackney_keepalive:should_close(undefined, h([]), false)).

%%====================================================================
%% request_closes/1
%%====================================================================

request_closes_true_test() ->
    ?assertEqual(true,
                 hackney_keepalive:request_closes(h([{<<"Connection">>, <<"close">>}]))).

request_closes_false_test() ->
    ?assertEqual(false,
                 hackney_keepalive:request_closes(h([{<<"Connection">>, <<"keep-alive">>}]))),
    ?assertEqual(false, hackney_keepalive:request_closes(h([]))).

request_closes_token_list_test() ->
    ?assertEqual(true,
                 hackney_keepalive:request_closes(
                   h([{<<"Connection">>, <<"keep-alive, close">>}]))).

%%====================================================================
%% connection_tokens/1 - defensive
%%====================================================================

tokens_undefined_test() ->
    ?assertEqual([], hackney_keepalive:connection_tokens(undefined)).

tokens_malformed_object_test() ->
    %% Not a hackney_headers {_, dict} object: must not crash.
    ?assertEqual([], hackney_keepalive:connection_tokens(not_a_headers_object)),
    ?assertEqual([], hackney_keepalive:connection_tokens({bad})).

tokens_list_value_test() ->
    %% A header value supplied as a string (list) is converted, not crashed on.
    ?assertEqual([<<"close">>],
                 hackney_keepalive:connection_tokens(h([{<<"Connection">>, "close"}]))).

tokens_empty_value_test() ->
    %% Empty / comma-only values drop to no tokens.
    ?assertEqual([], hackney_keepalive:connection_tokens(h([{<<"Connection">>, <<>>}]))),
    ?assertEqual([], hackney_keepalive:connection_tokens(h([{<<"Connection">>, <<" , ">>}]))).
