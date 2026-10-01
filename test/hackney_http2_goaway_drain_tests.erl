%%% GOAWAY must only fail the streams the peer did not accept.
%%%
%%% RFC 9113 6.8: streams up to and including the GOAWAY's last_stream_id may
%%% still be processed, and the peer keeps the connection open to finish them.
%%% hackney used to abort every in-flight stream with {error, {goaway, _}}, so a
%%% request the server went on to complete came back as an error: for a payment
%%% API that is a charge that succeeded and was reported as failed.
%%%
%%% The server (hackney_h2_goaway_server) holds the first two streams of its
%%% first connection, sends GOAWAY with a chosen last_stream_id, then answers
%%% only the streams at or below it.
-module(hackney_http2_goaway_drain_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, goaway_drain_test_pool).

goaway_drain_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{timeout, 30, fun accepted_streams_complete/0},
      {timeout, 30, fun unaccepted_stream_fails_fast/0},
      {timeout, 30, fun two_step_shutdown/0},
      {timeout, 30, fun stalled_drain_ends_with_the_stream/0}]}.

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

%% GOAWAY(last_stream_id = 3) after streams 1 and 3: both were accepted, so both
%% complete. A request made while they drain must not land on the draining
%% connection, where the peer would ignore it, but dial a fresh one.
accepted_streams_complete() ->
    {Server, Url} = hackney_h2_goaway_server:start({second_stream, fun(_First, Second) -> Second end}),
    try
        [R1, R2, R3] = concurrent_requests(Url, 3),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({ok, 200, <<"3">>}, R2),
        ?assertEqual({ok, 200, <<"1">>}, R3)
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% GOAWAY(last_stream_id = 1) after streams 1 and 3: stream 3 was not accepted
%% and fails at once with the goaway reason, stream 1 still completes.
unaccepted_stream_fails_fast() ->
    {Server, Url} = hackney_h2_goaway_server:start({second_stream, fun(First, _Second) -> First end}),
    try
        [R1, R2] = concurrent_requests(Url, 2),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({error, {goaway, no_error}}, R2)
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% The graceful shutdown of RFC 9113 6.8: GOAWAY(2^31-1) stops new streams
%% without refusing any, then a second GOAWAY gives the real last_stream_id.
%% The first frame alone must not fail anything.
two_step_shutdown() ->
    {Server, Url} = hackney_h2_goaway_server:start({second_stream, fun(First, _Second) -> First end},
                                                   #{goaway => two_step}),
    try
        [R1, R2] = concurrent_requests(Url, 2),
        ?assertEqual({ok, 200, <<"1">>}, R1),
        ?assertEqual({error, {goaway, no_error}}, R2)
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% A server that accepts a stream and then never answers it must not keep the
%% draining connection around: the stream's own recv_timeout ends it, and with
%% it the connection, so the next request gets a fresh one.
stalled_drain_ends_with_the_stream() ->
    {Server, Url} = hackney_h2_goaway_server:start({second_stream, fun(First, _Second) -> First end},
                                                   #{answer => false}),
    try
        [R1, R2] = concurrent_requests(Url, 2, [{recv_timeout, 1000}]),
        ?assertEqual({error, timeout}, R1),
        ?assertEqual({error, {goaway, no_error}}, R2),
        ?assertEqual({ok, 200, <<"1">>}, fetch(Url, []))
    after
        hackney_h2_goaway_server:stop(Server)
    end.

%% The first request registers the shared connection before the second checks
%% one out, so both streams share it. The server sends GOAWAY on the second and
%% waits 200ms before answering, so a third lands inside that drain.
concurrent_requests(Url, N) ->
    concurrent_requests(Url, N, []).

concurrent_requests(Url, N, Extra) ->
    Self = self(),
    Pids = [begin
                timer:sleep(Delay),
                spawn_link(fun() -> Self ! {self(), fetch(Url, Extra)} end)
            end || Delay <- lists:sublist([0, 300, 100], N)],
    [receive {P, R} -> R after 10000 -> {error, test_timeout} end || P <- Pids].

fetch(Url, Extra) ->
    Opts = Extra ++ [{pool, ?POOL}, {protocols, [http2]}, {recv_timeout, 5000},
                     {ssl_options, [{insecure, true}, {verify, verify_none}]}],
    case hackney:request(get, Url, [], <<>>, Opts) of
        {ok, S, _H, B} when is_binary(B) -> {ok, S, B};
        {error, E} -> {error, E}
    end.
