-module(hackney_stream_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

-export([checkout/4]).

-define(PORT, 8124).

%% This seems necessary to list the tests including the generator
all_tests() ->
   [
    async_request(),
    handle_connection_close()
   ].

stream_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
      fun({ok, _}) ->
          {inparallel, all_tests()}
      end}.

start() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),
    Host = '_',
    Routes = [
        {"/get", test_http_resource, []},
        {"/connection-close", test_http_resource, []}
    ],
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    cowboy:start_clear(stream_test_server, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}).

stop({ok, _Pid}) ->
    cowboy:stop_listener(stream_test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

async_request() ->
    URL = <<"http://localhost:", (integer_to_binary(?PORT))/binary, "/get">>,
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    %% With process-per-connection model, no ETS/dict tracking needed
    %% Just verify the async request worked correctly
    [?_assertEqual(200, StatusCode),
     ?_assertEqual([body, headers, status], Keys)].

receive_response(Ref) ->
    Dict = receive_response(Ref, orddict:new(), 10000),
    Keys = orddict:fetch_keys(Dict),
    StatusCode = orddict:fetch(status, Dict),
    {StatusCode, Keys}.

checkout(_Host, _Port, _Transport, _Client) -> {error, no_socket, make_ref()}.

handle_connection_close() ->
    URL = <<"http://localhost:", (integer_to_binary(?PORT))/binary, "/connection-close">>,
    Options = [async, {pool, false}],

    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    Dict = receive_response(ClientRef, orddict:new(), 2000),
    Headers = hackney_headers_new:from_list(orddict:fetch(headers, Dict)),
    CloseHeader = hackney_headers_new:get_value(<<"connection">>, Headers),
    %% Verify response completed successfully with Connection: close header
    %% In process-per-connection model, async returns the connection PID
    %% After Connection: close, process should terminate
    timer:sleep(10),
    [?_assertEqual(<<"close">>, CloseHeader),
     ?_assert(is_pid(ClientRef)),
     ?_assertEqual(false, is_process_alive(ClientRef))
    ].

receive_response(Ref, Dict0, Timeout) ->
    receive
        {hackney_response, Ref, {status, Status, _Reason}} ->
            Dict1 = orddict:store(status, Status, Dict0),
            receive_response(Ref, Dict1, Timeout);
        {hackney_response, Ref, {headers, Headers}} ->
            Dict1 = orddict:store(headers, Headers, Dict0),
            receive_response(Ref, Dict1, Timeout);
        {hackney_response, Ref, done} -> Dict0;
        {hackney_response, Ref, Bin} ->
            Dict1 = orddict:append(body, Bin, Dict0),
            receive_response(Ref, Dict1, Timeout)
    after Timeout -> {error, timeout}
    end.
