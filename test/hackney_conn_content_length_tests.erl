%%% Content-Length on empty request bodies, curl-style: body-bearing methods
%%% (POST/PUT/PATCH) get an explicit Content-Length: 0; bodyless methods do not,
%%% and a caller-supplied Content-Length is never duplicated.
-module(hackney_conn_content_length_tests).

-include_lib("eunit/include/eunit.hrl").

post_empty_body_gets_content_length_zero_test() ->
    Req = capture(<<"POST">>, [], <<>>),
    ?assertNotEqual(nomatch, cl(Req, <<"0">>)).

put_empty_body_gets_content_length_zero_test() ->
    Req = capture(<<"PUT">>, [], <<>>),
    ?assertNotEqual(nomatch, cl(Req, <<"0">>)).

get_empty_body_has_no_content_length_test() ->
    Req = capture(<<"GET">>, [], <<>>),
    ?assertEqual(nomatch, binary:match(lower(Req), <<"content-length:">>)).

delete_empty_body_has_no_content_length_test() ->
    Req = capture(<<"DELETE">>, [], <<>>),
    ?assertEqual(nomatch, binary:match(lower(Req), <<"content-length:">>)).

user_content_length_not_duplicated_test() ->
    Req = capture(<<"POST">>, [{<<"Content-Length">>, <<"5">>}], <<>>),
    %% Exactly one content-length header, and it is the caller's value.
    Matches = binary:matches(lower(Req), <<"content-length:">>),
    ?assertEqual(1, length(Matches)),
    ?assertNotEqual(nomatch, cl(Req, <<"5">>)).

%% Find "content-length: <V>" case-insensitively.
cl(Req, V) ->
    binary:match(lower(Req), <<"content-length: ", V/binary, "\r\n">>).

lower(Bin) -> hackney_bstr:to_lower(Bin).

%% Drive a real buffered request into a raw listener and return the request
%% bytes the server received.
capture(Method, Headers, Body) ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false},
                                     {reuseaddr, true}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    Parent = self(),
    spawn(fun() ->
        case gen_tcp:accept(LSock, 3000) of
            {ok, S} ->
                ReqBytes = recv_headers(S, <<>>),
                gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n">>),
                Parent ! {captured, ReqBytes},
                gen_tcp:close(S);
            _ -> Parent ! {captured, <<>>}
        end,
        gen_tcp:close(LSock)
    end),
    {ok, Pid} = hackney_conn:start_link(#{host => "127.0.0.1", port => Port,
                                          transport => hackney_tcp,
                                          connect_timeout => 1000}),
    ok = hackney_conn:connect(Pid, 1000),
    {ok, 200, _} = hackney_conn:request(Pid, Method, <<"/">>, Headers, Body),
    catch hackney_conn:stop(Pid),
    receive {captured, Req} -> Req after 3000 -> error(no_capture) end.

recv_headers(S, Acc) ->
    case binary:match(Acc, <<"\r\n\r\n">>) of
        nomatch ->
            case gen_tcp:recv(S, 0, 2000) of
                {ok, D} -> recv_headers(S, <<Acc/binary, D/binary>>);
                {error, _} -> Acc
            end;
        _ -> Acc
    end.
