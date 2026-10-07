%%% A request that finds the shared HTTP/2 connection at the peer's
%%% SETTINGS_MAX_CONCURRENT_STREAMS waits for a stream to end instead of
%%% failing with max_streams_exceeded.
%%%
%%% The wait is bounded by connect_timeout and answers checkout_timeout when it
%%% runs out, the way a pool with every connection in use does. A GOAWAY while
%%% a request waits fails it with the goaway reason: nothing of it was sent.
%%%
%%% The server (hackney_h2_goaway_server) advertises max_streams and answers
%%% each request with its stream id as the body.
-module(hackney_http2_stream_limit_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, stream_limit_test_pool).

stream_limit_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{timeout, 30, fun waits_for_a_free_stream/0},
      {timeout, 30, fun gives_up_like_a_full_pool/0},
      {timeout, 30, fun goaway_fails_the_waiting_request/0},
      {timeout, 30, fun slot_freed_during_an_upload_is_used_after_it/0}]}.

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

%% One stream at a time, each answered after 600ms. The second request has
%% to wait for the first to end, then goes out as stream 3.
waits_for_a_free_stream() ->
    {Server, Url} = hackney_h2_goaway_server:start(none, #{max_streams => 1, answer_after => 600}),
    try
        [R1, R2] = concurrent_fetches(Url, [0, 300], []),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({ok, 200, <<"3">>}, R2)
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% The first stream is never answered, so the slot never frees. The waiting
%% request gives up after connect_timeout with the answer a full pool gives.
gives_up_like_a_full_pool() ->
    {Server, Url} = hackney_h2_goaway_server:start(none, #{max_streams => 1, answer => false}),
    try
        [R1, R2] = concurrent_fetches(Url, [0, 300], [{connect_timeout, 300}, {recv_timeout, 1000}]),
        ?assertEqual({error, timeout}, R1),
        ?assertEqual({error, checkout_timeout}, R2)
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% The server sends GOAWAY covering stream 1 while a second request waits for
%% its slot. The waiting request is refused at once with the goaway reason,
%% since nothing of it was sent, while stream 1 finishes.
goaway_fails_the_waiting_request() ->
    {Server, Url} = hackney_h2_goaway_server:start({first_stream, 600}, #{max_streams => 1}),
    try
        [R1, R2] = concurrent_fetches(Url, [0, 300], []),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({error, {goaway, no_error}}, R2)
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% Two streams at a time. With both busy, a request with a streamed body and
%% then a plain request queue up. The first slot goes to the body: while it is
%% being sent, the second stream ends, and that slot goes to the plain request
%% once the body is done and the connection takes requests again.
slot_freed_during_an_upload_is_used_after_it() ->
    {Server, Url} = hackney_h2_goaway_server:start(none, #{max_streams => 2, answer_after => 600}),
    try
        Self = self(),
        %% The first request registers the shared connection before the second
        %% checks one out, so both streams share it.
        Plain = [begin
                     timer:sleep(Delay),
                     spawn_link(fun() -> Self ! {self(), fetch(Url, [])} end)
                 end || Delay <- [0, 300]],
        timer:sleep(100),
        %% Queues behind the streamed request below, while that one still waits.
        Waiter = spawn_link(fun() -> timer:sleep(50), Self ! {self(), fetch(Url, [])} end),
        {ok, Conn} = hackney:request(post, Url, [], stream, opts([])),
        %% The second plain stream ends while the body is still being sent.
        timer:sleep(1000),
        ok = hackney:finish_send_body(Conn),
        {ok, 200, _Headers, Conn} = hackney:start_response(Conn),
        ?assertEqual({ok, <<"5">>}, hackney:body(Conn)),
        ?assertEqual([{ok, 200, <<"1">>}, {ok, 200, <<"3">>}], [await(P) || P <- Plain]),
        ?assertEqual({ok, 200, <<"7">>}, await(Waiter))
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% The first request registers the shared connection before the next checks
%% one out, so they share it: 300ms covers the TLS handshake on localhost.
concurrent_fetches(Url, Delays, Extra) ->
    Self = self(),
    Pids = [begin
                timer:sleep(Delay),
                spawn_link(fun() -> Self ! {self(), fetch(Url, Extra)} end)
            end || Delay <- Delays],
    [await(P) || P <- Pids].

await(Pid) ->
    receive {Pid, R} -> R after 10000 -> {error, test_timeout} end.

fetch(Url, Extra) ->
    case hackney:request(get, Url, [], <<>>, opts(Extra)) of
        {ok, S, _H, B} when is_binary(B) -> {ok, S, B};
        {error, E} -> {error, E}
    end.

opts(Extra) ->
    Extra ++ [{pool, ?POOL}, {protocols, [http2]}, {recv_timeout, 5000},
              {ssl_options, [{insecure, true}, {verify, verify_none}]}].
