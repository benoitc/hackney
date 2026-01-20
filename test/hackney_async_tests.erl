%% @doc Tests for async request ownership (issue #646)
%% When stream_to is provided, the stream_to process should be the owner.
-module(hackney_async_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 9878).

%% Setup/teardown for integration tests
setup() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(test_async_http, [{port, ?PORT}], #{
        env => #{dispatch => Dispatch}
    }),
    ok.

cleanup(_) ->
    cowboy:stop_listener(test_async_http),
    ok.

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?PORT))/binary, Path/binary>>.

%% =============================================================================
%% Issue #646: stream_to as owner for async requests
%% =============================================================================

stream_to_owner_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"stream_to becomes connection owner", fun test_stream_to_becomes_owner/0},
      {"connection terminates when stream_to dies", fun test_connection_terminates_when_stream_to_dies/0},
      {"original caller death does not affect connection when stream_to is different",
       fun test_caller_death_with_different_stream_to/0},
      {"backward compat: stream_to = self() works normally", fun test_stream_to_self_compat/0}
     ]}.

%% Test that when stream_to is provided, it becomes the connection owner
test_stream_to_becomes_owner() ->
    %% Create a separate receiver process
    Self = self(),
    Receiver = spawn_link(fun() -> receiver_loop(Self) end),

    %% Make async request with stream_to = Receiver
    {ok, Ref} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                [{async, true}, {stream_to, Receiver}]),
    ?assert(is_pid(Ref)),

    %% Wait for receiver to collect all messages
    timer:sleep(500),

    %% Ask receiver for messages received
    Receiver ! {get_messages, self()},
    Messages = receive
        {messages, Msgs} -> Msgs
    after 2000 -> []
    end,

    %% Should have received status and headers
    ?assert(lists:any(fun({status, S, _}) -> S >= 200; (_) -> false end, Messages)),
    ?assert(lists:any(fun({headers, _}) -> true; (_) -> false end, Messages)),
    ?assert(lists:member(done, Messages)),

    %% Clean up
    Receiver ! stop,
    ok.

%% Test that connection terminates when stream_to process dies
test_connection_terminates_when_stream_to_dies() ->
    %% Create a receiver process that will die on command
    Self = self(),
    Receiver = spawn(fun() ->
        receive
            die -> exit(killed);
            {get_messages, Pid} -> Pid ! {messages, []}, receiver_loop(Self)
        end
    end),

    %% Make async request with stream_to = Receiver
    %% Use a large chunked response to ensure we have time to kill the receiver
    {ok, Ref} = hackney:request(get, url(<<"/chunked/1000000">>), [], <<>>,
                                [{async, true}, {stream_to, Receiver}]),
    ?assert(is_pid(Ref)),

    %% Give time for ownership transfer to complete
    timer:sleep(200),

    %% Connection should be alive
    ?assert(is_process_alive(Ref)),

    %% Kill the receiver (stream_to)
    Receiver ! die,
    timer:sleep(200),
    ?assertNot(is_process_alive(Receiver)),

    %% Connection should be terminated because stream_to died
    %% Wait a bit longer for the monitor DOWN message to propagate
    timer:sleep(500),
    ?assertNot(is_process_alive(Ref)),
    ok.

%% Test that when stream_to is different from caller, caller death doesn't affect connection
test_caller_death_with_different_stream_to() ->
    %% We spawn a process to make the request, then kill it
    %% The receiver should still get all messages
    Self = self(),
    Receiver = spawn_link(fun() -> receiver_loop(Self) end),

    %% Spawn a caller that makes the request then dies
    Caller = spawn(fun() ->
        {ok, Ref} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                    [{async, true}, {stream_to, Receiver}]),
        Self ! {started, Ref},
        %% Die immediately after starting request
        exit(normal)
    end),

    %% Wait for caller to start the request and get the ref
    ConnRef = receive
        {started, R} -> R
    after 2000 -> error(timeout_waiting_for_start)
    end,

    %% Wait for caller to die
    timer:sleep(100),
    ?assertNot(is_process_alive(Caller)),

    %% Connection should still be alive (owned by Receiver now)
    ?assert(is_process_alive(ConnRef)),

    %% Give it some time to complete the request
    timer:sleep(500),

    %% Ask receiver for messages
    Receiver ! {get_messages, self()},
    Messages = receive
        {messages, Msgs} -> Msgs
    after 2000 -> []
    end,

    %% Should have received complete response despite caller death
    ?assert(lists:any(fun({status, S, _}) -> S >= 200; (_) -> false end, Messages)),
    ?assert(lists:any(fun({headers, _}) -> true; (_) -> false end, Messages)),
    ?assert(lists:member(done, Messages)),

    %% Clean up
    Receiver ! stop,
    ok.

%% Test backward compatibility: when stream_to = self(), behavior unchanged
test_stream_to_self_compat() ->
    %% When stream_to is self (default), messages come to caller
    {ok, Ref} = hackney:request(get, url(<<"/get">>), [], <<>>,
                                [{async, true}]),  %% Default stream_to = self()
    ?assert(is_pid(Ref)),

    %% Should receive status
    receive
        {hackney_response, Ref, {status, Status, _Reason}} ->
            ?assert(Status >= 200 andalso Status < 400)
    after 5000 ->
        ?assert(false)
    end,

    %% Should receive headers
    receive
        {hackney_response, Ref, {headers, Headers}} ->
            ?assert(is_list(Headers))
    after 5000 ->
        ?assert(false)
    end,

    %% Collect remaining messages
    Messages = receive_all_async(Ref, []),
    ?assert(lists:member(done, Messages)),

    ok.

%% =============================================================================
%% Helper functions
%% =============================================================================

%% A receiver process that collects async messages
receiver_loop(Parent) ->
    receiver_loop(Parent, []).

receiver_loop(Parent, Acc) ->
    receive
        {hackney_response, _Ref, done} ->
            receiver_loop(Parent, [done | Acc]);
        {hackney_response, _Ref, {error, _} = Error} ->
            receiver_loop(Parent, [Error | Acc]);
        {hackney_response, _Ref, Msg} ->
            receiver_loop(Parent, [Msg | Acc]);
        {get_messages, Pid} ->
            Pid ! {messages, lists:reverse(Acc)},
            receiver_loop(Parent, Acc);
        stop ->
            ok
    after 30000 ->
        ok
    end.

receive_all_async(Ref, Acc) ->
    receive
        {hackney_response, Ref, done} ->
            lists:reverse([done | Acc]);
        {hackney_response, Ref, {error, _} = Error} ->
            lists:reverse([Error | Acc]);
        {hackney_response, Ref, Msg} ->
            receive_all_async(Ref, [Msg | Acc])
    after 5000 ->
        lists:reverse(Acc)
    end.
