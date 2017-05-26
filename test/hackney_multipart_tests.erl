-module(hackney_multipart_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

%% This seems necessary to list the tests including the generator
dummy_test() ->
    ?assertEqual(ok, ok).

multipart_test_() ->
    {setup, fun start/0, fun stop/1,
      [multipart_post()]}.

start() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Host = '_',
    Resource = {"/mp", upload_resource, []},
    Dispatch = cowboy_router:compile([{Host, [Resource]}]),
    cowboy:start_http(test_server, 10, [{port, 8123}], [{env, [{dispatch, Dispatch}]}]).

stop({ok, _Pid}) ->
    cowboy:stop_listener(test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

multipart_post() ->
    fun() ->
        URL = <<"http://localhost:8123/mp">>,
        Headers = [],
        Parts = [
            {<<"part1">>, <<"foo">>},
            {<<"part2">>, <<"bar">>},
            {<<"part3">>, <<"baz">>}],
        case hackney:request(post, URL, Headers, {multipart, Parts}, []) of
            {ok, Code, _Headers, Ref} when code >= 200, Code < 300 ->
                {ok, Body} = hackney:body(Ref),
                hackney:close(Ref),
                ?assertEqual(Parts, binary_to_term(Body))
        end
    end.

