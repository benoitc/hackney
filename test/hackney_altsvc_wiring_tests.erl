%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% End-to-end test that response-side Alt-Svc parsing actually fires.
%%% Stands up a tiny cowboy listener that emits a chosen Alt-Svc header
%%% and asserts the cache reflects what the server said.
-module(hackney_altsvc_wiring_tests).
-include_lib("eunit/include/eunit.hrl").

-export([init/2]).  %% cowboy handler callback

-define(PORT, 9982).

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", ?MODULE, []}]}
    ]),
    {ok, _} = cowboy:start_clear(altsvc_test_http,
                                 [{port, ?PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    hackney_altsvc:init(),
    hackney_altsvc:clear_all(),
    ok.

cleanup(_) ->
    cowboy:stop_listener(altsvc_test_http),
    hackney_altsvc:clear_all(),
    ok.

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?PORT))/binary, Path/binary>>.

altsvc_wiring_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
        fun http1_response_populates_cache/0,
        fun http1_clear_directive_invalidates_cache/0,
        fun absent_header_leaves_cache_alone/0
     ]}.

%% ============================================================================

http1_response_populates_cache() ->
    hackney_altsvc:clear_all(),
    %% Cowboy handler reads the encoded Alt-Svc value from ?value= and
    %% emits it on the response.
    URL = url(<<"/altsvc?value=", (url_encode(<<"h3=\":8443\"; ma=600">>))/binary>>),
    {ok, 200, _, _} = hackney:request(get, URL, [], <<>>, [{pool, false}]),
    ?assertEqual({ok, h3, 8443},
                 hackney_altsvc:lookup(<<"localhost">>, ?PORT)).

http1_clear_directive_invalidates_cache() ->
    hackney_altsvc:cache(<<"localhost">>, ?PORT, 8443, 3600),
    ?assertEqual({ok, h3, 8443},
                 hackney_altsvc:lookup(<<"localhost">>, ?PORT)),
    URL = url(<<"/altsvc?value=clear">>),
    {ok, 200, _, _} = hackney:request(get, URL, [], <<>>, [{pool, false}]),
    ?assertEqual(none, hackney_altsvc:lookup(<<"localhost">>, ?PORT)).

absent_header_leaves_cache_alone() ->
    hackney_altsvc:cache(<<"localhost">>, ?PORT, 8443, 3600),
    URL = url(<<"/noaltsvc">>),
    {ok, 200, _, _} = hackney:request(get, URL, [], <<>>, [{pool, false}]),
    %% Cache untouched.
    ?assertEqual({ok, h3, 8443},
                 hackney_altsvc:lookup(<<"localhost">>, ?PORT)).

%% ============================================================================
%% cowboy handler — sets Alt-Svc from ?value= query parameter, or none.
%% ============================================================================

init(Req0, State) ->
    QS = cowboy_req:parse_qs(Req0),
    Headers0 = #{<<"content-type">> => <<"text/plain">>},
    Headers = case proplists:get_value(<<"value">>, QS) of
        undefined -> Headers0;
        Value     -> Headers0#{<<"alt-svc">> => Value}
    end,
    Req = cowboy_req:reply(200, Headers, <<"ok">>, Req0),
    {ok, Req, State}.

url_encode(Bin) ->
    list_to_binary(uri_string:quote(binary_to_list(Bin))).
