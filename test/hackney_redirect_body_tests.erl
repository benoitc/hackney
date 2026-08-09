%%% Request body must not be forwarded to a cross-origin 307/308 redirect
%%% target unless the caller opts into location_trusted.
%%%
%%% localhost and 127.0.0.1 resolve to the same listener but are distinct host
%%% strings, which is exactly hackney's cross-host test, so one server is enough.
-module(hackney_redirect_body_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 9879).

redirect_body_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"307 body dropped on cross-origin", fun cross_origin_drops_body/0},
      {"307 body kept same-origin", fun same_origin_keeps_body/0},
      {"307 body kept when location_trusted", fun trusted_keeps_body/0}]}.

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(test_redirect_body_http, [{port, ?PORT}],
                                 #{env => #{dispatch => Dispatch}}),
    ok.

cleanup(_) ->
    cowboy:stop_listener(test_redirect_body_http),
    ok.

-define(SECRET, <<"SUPER-SECRET-VALUE-12345">>).

cross_origin_drops_body() ->
    Body = post_follow(<<"localhost">>, sink_url(<<"127.0.0.1">>), []),
    ?assertEqual(nomatch, binary:match(Body, ?SECRET)).

same_origin_keeps_body() ->
    Body = post_follow(<<"localhost">>, sink_url(<<"localhost">>), []),
    ?assertNotEqual(nomatch, binary:match(Body, ?SECRET)).

trusted_keeps_body() ->
    Body = post_follow(<<"localhost">>, sink_url(<<"127.0.0.1">>), [location_trusted]),
    ?assertNotEqual(nomatch, binary:match(Body, ?SECRET)).

sink_url(Host) ->
    <<"http://", Host/binary, ":", (integer_to_binary(?PORT))/binary, "/post">>.

%% POST to /redirect-to on FromHost, which 307s to SinkUrl (the /post echo).
post_follow(FromHost, SinkUrl, ExtraOpts) ->
    Url = <<"http://", FromHost/binary, ":", (integer_to_binary(?PORT))/binary,
            "/redirect-to?status_code=307&url=", (cow_uri(SinkUrl))/binary>>,
    Opts = [{follow_redirect, true}, {max_redirect, 3}, with_body | ExtraOpts],
    {ok, 200, _H, RespBody} =
        hackney:request(post, Url,
                        [{<<"Content-Type">>, <<"text/plain">>}], ?SECRET, Opts),
    RespBody.

cow_uri(Bin) ->
    list_to_binary(http_uri_encode(binary_to_list(Bin))).

http_uri_encode(Str) ->
    lists:flatten([encode_char(C) || C <- Str]).

encode_char(C) when C >= $a, C =< $z -> C;
encode_char(C) when C >= $A, C =< $Z -> C;
encode_char(C) when C >= $0, C =< $9 -> C;
encode_char($.) -> $.;
encode_char($-) -> $-;
encode_char($/) -> "%2F";
encode_char($:) -> "%3A";
encode_char(C) -> io_lib:format("%~2.16.0B", [C]).
