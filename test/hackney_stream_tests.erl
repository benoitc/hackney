-module(hackney_stream_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

-export([checkout/4]).

%% This seems necessary to list the tests including the generator
all_tests() ->
   [
    async_request(),
    heed_connection_close()
   ].

stream_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
      fun(ok) ->
          {inparallel, all_tests()}
      end}.


start() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

stop(ok) -> ok.


async_request() ->
    URL = <<"http://localhost:8000/get">>,
    Options = [async],
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    {StatusCode, Keys} = receive_response(ClientRef),
    timer:sleep(100),
    {mstate, Dict, _} = sys:get_state(hackney_manager),
    [?_assertEqual(0, dict:size(Dict)),
     ?_assertEqual(200, StatusCode),
     ?_assertEqual([body, headers, status], Keys)].

receive_response(Ref) ->
    Dict = receive_response(Ref, orddict:new()),
    Keys = orddict:fetch_keys(Dict),
    StatusCode = orddict:fetch(status, Dict),
    {StatusCode, Keys}.

checkout(_Host, _Port, _Transport, _Client) -> {error, no_socket, make_ref()}.
heed_connection_close() ->
    URL = <<"http://localhost:8000/get">>,
    Options = [async, pool],

    % Notice that ?MODULE has checkout/4 but not checkin, so if this test
    % passes, it means that checkin was not called (which is intended for closed
    % sockets)
    application:set_env(hackney, pool_handler, ?MODULE),
    {ok, ClientRef} = hackney:get(URL, [], <<>>, Options),
    application:set_env(hackney, pool_handler, hackney_pool),

    Socket = proplists:get_value(socket, hackney:request_info(ClientRef)),
    Dict = receive_response(ClientRef, orddict:new()),
    Headers = hackney_headers_new:from_list(orddict:fetch(headers, Dict)),
    CloseHeader = hackney_headers_new:get_value(<<"connection">>, Headers),
    [?_assertEqual(<<"close">>, CloseHeader),
     ?_assertEqual({error, closed}, gen_tcp:send(Socket, <<>>))
    ].

receive_response(Ref, Dict0) ->
    receive
        {hackney_response, Ref, {status, Status, _Reason}} ->
            Dict1 = orddict:store(status, Status, Dict0),
            receive_response(Ref, Dict1);
        {hackney_response, Ref, {headers, Headers}} ->
            Dict1 = orddict:store(headers, Headers, Dict0),
            receive_response(Ref, Dict1);
        {hackney_response, Ref, done} -> Dict0;
        {hackney_response, Ref, Bin} ->
            Dict1 = orddict:append(body, Bin, Dict0),
            receive_response(Ref, Dict1)
    after 5000 -> {error, timeout}
    end.
