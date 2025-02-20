-module(hackney_integration_tests_async_long_headers).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

all_tests() ->
  [fun absolute_redirect_request_follow/1].

http_requests_test_() ->
  TestMatrix = [
                #{status_code => <<"301">>, method => get},
                #{status_code => <<"303">>, method => post}
           ],
  lists:map(fun(Props) ->
                {foreach,
                 fun() -> start(Props) end,
                 fun(StartResult) -> stop(StartResult, Props) end,
                 all_tests()
                }
            end,
            TestMatrix).

start(#{status_code := StatusCode, method := Method}) ->
  {ok, _} = application:ensure_all_started(hackney),
  {ok, LSock} = gen_tcp:listen(0, [{active, false}]),
  {ok, {_, Port}} = inet:sockname(LSock),
  Pid = spawn_link(fun () -> dummy_server_loop(LSock, Port, StatusCode) end),
  gen_tcp:controlling_process(LSock, Pid),
  #{
    dummy_http_pid => Pid,
    method => Method,
    port => Port
   }.

stop(#{dummy_http_pid := Pid}, _Props) ->
  exit(Pid, normal),
  application:stop(hackney),
  error_logger:tty(true),
  ok.


absolute_redirect_request_follow(#{method := Method, port := Port}) ->
  PortBin = list_to_binary(integer_to_list(Port)),
  URL = <<"http://localhost:", PortBin/binary>>,
  ExpectedRedirectUrl = <<"http://localhost:", PortBin/binary, "/redirected">>,
  Options = [{follow_redirect, true}, {async, true}],
  {ok, Ref} = hackney:request(Method, URL, [], <<>>, Options),
  case Method of
    get ->
      [
       receive
         {hackney_response, Ref, {redirect, RedirectUrl, _headers}} ->
           ?_assertEqual(ExpectedRedirectUrl, RedirectUrl);
         Other ->
           throw({error, Other})
       after 1000 ->
               throw(timeout)
       end
      ];
    post ->
      [
       receive
         {hackney_response, Ref, {see_other, RedirectUrl, _headers}} ->
           ?_assertEqual(ExpectedRedirectUrl, RedirectUrl);
         Other ->
           throw({error, Other})
       after 1000 ->
               throw(timeout)
       end
      ]
  end.

dummy_server_loop(LSock, Port, StatusCode) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  PortBin = list_to_binary(integer_to_list(Port)),
  RedirectUrl = <<"http://localhost:", PortBin/binary, "/redirected">>,
  Response = iolist_to_binary([
                               "HTTP/1.1 ",
                               StatusCode,
                               " Moved Permanently\r\nLocation: ",
                               RedirectUrl,
                               "\r\nExtra-Header:",
                               binary:copy(<<"a">>, 1024),
                               "\r\n\r\n"
                              ]),
  send(Sock, Response),
  ok = gen_tcp:shutdown(Sock, read_write),
  dummy_server_loop(LSock, RedirectUrl, StatusCode).

send(Sock, << Data :128/binary, Rest/binary>>) ->
  ok = gen_tcp:send(Sock, Data),
  send(Sock, Rest);

send(Sock, Data) ->
  ok = gen_tcp:send(Sock, Data).
