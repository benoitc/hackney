%%% A GOAWAY while a streamed request or response body is in flight.
%%%
%%% Same rule as hackney_http2_goaway_drain_tests: streams up to the GOAWAY's
%%% last_stream_id finish, streams above it fail with {goaway, _}. The streamed
%%% request body adds one wrinkle: when it is refused, nobody is parked on it,
%%% since its caller is between send_body/finish_send_body/start_response
%%% calls, so the refusal has to wait for the caller's next call.
%%%
%%% The server is hackney_h2_goaway_server; each response body is the stream id.
-module(hackney_http2_goaway_streaming_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, goaway_streaming_test_pool).

goaway_streaming_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{timeout, 30, fun accepted_upload_completes/0},
      {timeout, 30, fun() -> refused_upload_fails_on(send_body) end},
      {timeout, 30, fun() -> refused_upload_fails_on(finish_send_body) end},
      {timeout, 30, fun() -> refused_upload_fails_on(start_response) end},
      {timeout, 30, fun accepted_streamed_response_completes/0},
      {timeout, 30, fun refused_streamed_response_fails_fast/0}]}.

setup() ->
    _ = application:ensure_all_started(hackney),
    _ = application:ensure_all_started(h2),
    stop_pool(),
    ok = hackney_pool:start_pool(?POOL, [{max_connections, 5}]),
    ok.

cleanup(_) ->
    stop_pool().

stop_pool() ->
    try hackney_pool:stop_pool(?POOL) catch _:_ -> ok end,
    ok.

%% The server sends GOAWAY covering the streamed request as soon as its first
%% body chunk arrives. The rest of the body goes out, the response comes back,
%% and the connection is gone for the request after.
accepted_upload_completes() ->
    {Server, Url} = hackney_h2_goaway_server:start(first_data),
    try
        {ok, Conn} = hackney:request(post, Url, [], stream, opts()),
        ok = hackney:send_body(Conn, <<"abc">>),
        timer:sleep(300),
        ok = hackney:finish_send_body(Conn),
        {ok, 200, _Headers, Conn} = hackney:start_response(Conn),
        ?assertEqual({ok, <<"1">>}, hackney:body(Conn)),
        ?assertEqual({ok, 200, <<"1">>}, fetch(Url))
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% Stream 1 is a plain request the server holds, stream 3 a streamed request,
%% and the GOAWAY covers only stream 1. Whichever call the sender makes next is
%% the one that fails, stream 1 still completes, and the connection is gone for
%% the request after.
refused_upload_fails_on(Call) ->
    {Server, Url} = hackney_h2_goaway_server:start({second_stream, fun(First, _Second) -> First end}),
    try
        P1 = spawn_fetch(Url),
        timer:sleep(300),
        {ok, Conn} = hackney:request(post, Url, [], stream, opts()),
        timer:sleep(300),
        Result = case Call of
            send_body -> hackney:send_body(Conn, <<"abc">>);
            finish_send_body -> hackney:finish_send_body(Conn);
            start_response -> hackney:start_response(Conn)
        end,
        ?assertEqual({error, {goaway, no_error}}, Result),
        ?assertEqual({ok, 200, <<"1">>}, await(P1)),
        ?assertEqual({ok, 200, <<"1">>}, fetch(Url))
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% Stream 1 is a request whose response is read as a stream, stream 3 a plain
%% request, and the GOAWAY covers only stream 1.
accepted_streamed_response_completes() ->
    {Server, Url} = hackney_h2_goaway_server:start({second_stream, fun(First, _Second) -> First end}),
    try
        {ok, Conn} = connect(Url),
        Self = self(),
        P1 = spawn_link(fun() ->
                            R = case hackney:send_request(Conn, {get, <<"/">>, [], <<>>}) of
                                {ok, 200, _Headers, Conn} -> hackney:body(Conn);
                                Other -> Other
                            end,
                            Self ! {self(), R}
                        end),
        timer:sleep(300),
        ?assertEqual({error, {goaway, no_error}}, fetch(Url)),
        ?assertEqual({ok, <<"1">>}, await(P1))
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% Stream 1 is a plain request the server holds, stream 3 a request whose
%% response would be read as a stream, and the GOAWAY covers only stream 1.
refused_streamed_response_fails_fast() ->
    {Server, Url} = hackney_h2_goaway_server:start({second_stream, fun(First, _Second) -> First end}),
    try
        P1 = spawn_fetch(Url),
        timer:sleep(300),
        {ok, Conn} = connect(Url),
        ?assertEqual({error, {goaway, no_error}},
                     hackney:send_request(Conn, {get, <<"/">>, [], <<>>})),
        ?assertEqual({ok, 200, <<"1">>}, await(P1))
    after
        hackney_h2_goaway_server:stop(Server)
    end.

spawn_fetch(Url) ->
    Self = self(),
    spawn_link(fun() -> Self ! {self(), fetch(Url)} end).

await(Pid) ->
    receive {Pid, R} -> R after 10000 -> {error, test_timeout} end.

fetch(Url) ->
    case hackney:request(get, Url, [], <<>>, opts()) of
        {ok, S, _H, B} when is_binary(B) -> {ok, S, B};
        {error, E} -> {error, E}
    end.

connect(Url) ->
    #{host := Host, port := Port} = uri_string:parse(Url),
    hackney:connect(hackney_ssl, Host, Port, opts()).

opts() ->
    [{pool, ?POOL}, {protocols, [http2]}, {recv_timeout, 5000},
     {ssl_options, [{insecure, true}, {verify, verify_none}]}].
