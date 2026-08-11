%%% Request-line and streaming-header serialization must not let a caller pass
%%% CR/LF through into the wire (header injection / request splitting).
%%%
%%%  - the method is validated at the entry points, like the request target
%%%  - the streaming header serializer sanitizes values, like the buffered one
-module(hackney_conn_request_injection_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BADMETHOD, <<"GET\r\nX-Injected: yes">>).

%% The method sits on the request line next to the path; a CR/LF in it is
%% rejected at every entry point that also validates the path.
method_rejected_test_() ->
    [?_assertMatch({error, {invalid_method, _}},
                   hackney_conn:request(self(), ?BADMETHOD, <<"/">>, [], <<>>, 1000, [])),
     ?_assertMatch({error, {invalid_method, _}},
                   hackney_conn:request_streaming(self(), ?BADMETHOD, <<"/">>, [], <<>>)),
     ?_assertMatch({error, {invalid_method, _}},
                   hackney_conn:send_request_headers(self(), ?BADMETHOD, <<"/">>, [])),
     ?_assertMatch({error, {invalid_method, _}},
                   hackney_conn:request_async(self(), ?BADMETHOD, <<"/">>, [], <<>>, once,
                                              self(), false))].

%% The streaming header serializer must strip CR/LF from header values, matching
%% the buffered path. Drive a real HTTP/1.1 streaming request into a raw socket
%% and inspect the exact bytes on the wire.
streaming_header_value_sanitized_test_() ->
    {timeout, 20, fun streaming_header_value_sanitized/0}.

streaming_header_value_sanitized() ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false},
                                     {reuseaddr, true}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    {ok, Pid} = hackney_conn:start_link(#{host => "127.0.0.1", port => Port,
                                          transport => hackney_tcp,
                                          connect_timeout => 1000}),
    ok = hackney_conn:connect(Pid, 1000),
    {ok, SSock} = gen_tcp:accept(LSock, 1000),
    ok = hackney_conn:send_request_headers(
           Pid, <<"GET">>, <<"/">>,
           [{<<"X-Custom">>, <<"benign\r\nX-Injected: yes">>}]),
    {ok, Wire} = gen_tcp:recv(SSock, 0, 1000),
    catch hackney_conn:stop(Pid),
    gen_tcp:close(SSock),
    gen_tcp:close(LSock),
    %% No injected header line, and the CR/LF collapsed within the value.
    ?assertEqual(nomatch, binary:match(Wire, <<"\r\nX-Injected: yes">>)),
    ?assertNotEqual(nomatch, binary:match(Wire, <<"X-Custom: benignX-Injected: yes\r\n">>)).
