-module(hackney_stream_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

%% This seems necessary to list the tests including the generator
all_tests() ->
   [
    async_request()
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
    end.
