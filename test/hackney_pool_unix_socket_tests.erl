%% @doc Pooled requests over unix domain sockets (http+unix URLs).
-module(hackney_pool_unix_socket_tests).
-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, test_pool_unix_socket_http).
-define(POOL, test_pool_unix_socket).
-define(PREWARM_POOL, test_pool_unix_socket_prewarm).

setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    %% sun_path is limited to about 100 bytes, so stay under /tmp rather than
    %% a possibly long per-user temp dir. The mixed-case file name checks the
    %% pool does not case-fold the path like a hostname.
    Dir = filename:join("/tmp", "hackney-unix-" ++
                                integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    Path = filename:join(Dir, "Hackney.sock"),
    Dispatch = cowboy_router:compile([{'_', [{"/delay/:seconds", delay_handler, []},
                                             {"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{ip, {local, Path}}, {port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    ok = hackney_pool:start_pool(?POOL, [{pool_size, 4}, {prewarm_count, 0}]),
    ok = hackney_pool:start_pool(?PREWARM_POOL, [{pool_size, 1}, {prewarm_count, 2}]),
    {Dir, Path}.

cleanup({Dir, Path}) ->
    hackney_pool:stop_pool(?PREWARM_POOL),
    hackney_pool:stop_pool(?POOL),
    cowboy:stop_listener(?LISTENER),
    _ = file:delete(Path),
    _ = file:del_dir(Dir),
    ok.

url(SocketPath, ReqPath) ->
    Netloc = hackney_url:urlencode(list_to_binary(SocketPath)),
    <<"http+unix://", Netloc/binary, ReqPath/binary>>.

%% The default Host for an http+unix URL is the percent-encoded socket path,
%% which cowboy rejects (a reg-name without pct-encoding), so name a host.
unix_get(Path, ReqPath, Pool) ->
    hackney:request(get, url(Path, ReqPath), [{<<"Host">>, <<"localhost">>}], <<>>,
                    [{pool, Pool}, with_body]).

%% The tests run in order: the first one relies on being the only client
%% of the listener so far.
pool_unix_socket_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Dir, Path}) ->
         [
          {"pooled requests over a unix socket reuse one connection",
           fun() -> test_pooled_reuse(Path) end},
          {"closing an overflow unix socket connection prewarms over the socket",
           fun() -> test_prewarm_after_close(Path) end}
         ]
     end}.

test_pooled_reuse(Path) ->
    ?assertMatch({ok, 200, _, _}, unix_get(Path, <<"/get">>, ?POOL)),
    ok = wait_until(fun() -> free_count(?POOL) =:= 1 end),
    ?assertMatch({ok, 200, _, _}, unix_get(Path, <<"/get">>, ?POOL)),
    ok = wait_until(fun() -> free_count(?POOL) =:= 1 end),
    ?assertEqual(1, length(ranch:procs(?LISTENER, connections))).

%% With pool_size 1, the second of two overlapping requests gets an overflow
%% connection that is closed at checkin, which tops the host up to
%% prewarm_count. The replacement must be dialled over the socket too.
test_prewarm_after_close(Path) ->
    Self = self(),
    Callers = [spawn_link(fun() ->
                              Self ! {self(), unix_get(Path, <<"/delay/1">>, ?PREWARM_POOL)}
                          end)
               || _ <- [1, 2]],
    [?assertMatch({ok, 200, _, _}, receive {Caller, Res} -> Res after 5000 -> timeout end)
     || Caller <- Callers],
    ?assertEqual(ok, wait_until(fun() -> free_count(?PREWARM_POOL) =:= 2 end)).

free_count(Pool) ->
    proplists:get_value(free_count, hackney_pool:get_stats(Pool)).

wait_until(Fun) ->
    wait_until(Fun, 200).

wait_until(_Fun, 0) ->
    timeout;
wait_until(Fun, Retries) ->
    case Fun() of
        true -> ok;
        false ->
            timer:sleep(10),
            wait_until(Fun, Retries - 1)
    end.
