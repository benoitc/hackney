-module(hackney_pool_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

%% This seems necessary to list the tests including the generator
dummy_test() ->
    ?assertEqual(ok, ok).

multipart_test_() ->
    {setup, fun start/0, fun stop/1,
      [queue_timeout()]}.

start() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Host = '_',
    Resource = {"/pool", pool_resource, []},
    Dispatch = cowboy_router:compile([{Host, [Resource]}]),
    cowboy:start_http(test_server, 10, [{port, 8123}], [{env, [{dispatch, Dispatch}]}]).

stop({ok, _Pid}) ->
    cowboy:stop_listener(test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

queue_timeout() ->
    fun() ->
        URL = <<"http://localhost:8123/pool">>,
        Headers = [],
        Opts = [{pool, true}, {connect_timeout, 100}],
        ok = application:set_env(hackney, max_connections, 1),
        case hackney:request(post, URL, Headers, stream, Opts) of
            {ok, Ref} ->
                {error, _} = hackney:request(post, URL, Headers, stream, Opts),
                ok = hackney:finish_send_body(Ref),
                {ok, _Status, _Headers, Ref} = hackney:start_response(Ref),
                ok = hackney:skip_body(Ref),
                {ok, _} = hackney:request(post, URL, Headers, stream, Opts)
        end
    end.

