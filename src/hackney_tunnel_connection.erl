-module(hackney_tunnel_connection).

-export([open/5, open/6]).
-export([do_handshake/4]).

-define(RESPONSE_RECV_TIMEOUT, 300000).   %% timeout waiting for response line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).

open(Host, Port, TargetHost, TargetPort, Options) ->
    open(Host, Port, TargetHost, TargetPort, Options, infinity).

open(Host, Port, TargetHost, TargetPort, Options0, Timeout) ->
    Options = hackney_socket:add_prefix("tunnel", Options0),
    case hackney_socket:connect(Host, Port, [binary, {active, false}], Timeout) of
        {ok, Socket} ->
            do_handshake(Socket, TargetHost, TargetPort, Options);
        Error ->
            Error
    end.

do_handshake(Socket, Host, Port, Options) ->
    ProxyUser = proplists:get_value(user, Options),
    ProxyPass = proplists:get_value(password, Options, <<>>),
    Secure = proplists:get_value(secure, Options, false),

    UA = hackney_request:default_ua(),
    Path = iolist_to_binary([Host, ":", integer_to_list(Port)]),
    HostHdr = case Port of
                  80 -> list_to_binary(Host);
                  _ -> Path
              end,
    Headers = case ProxyUser of
                  undefined ->
                      [<<"Host: ", HostHdr/binary >>,
                       <<"User-Agent: ", UA/binary >>];
                  _ ->
                      Credentials = base64:encode(<<ProxyUser/binary, ":",
                                                    ProxyPass/binary >>),
                      [<<"Host: ", HostHdr/binary >>,
                       <<"User-Agent: ", UA/binary >>,
                       <<"Proxy-Authorization: Basic ", Credentials/binary >>]
              end,

    Payload = [<<"CONNECT ", Path/binary, " HTTP/1.1\r\n" >>,
               hackney_bstr:join(lists:reverse(Headers), <<"\r\n" >>),
               <<"\r\n\r\n">>],

    Result = case hackney_socket:send(Socket, Payload) of
                 ok ->
                     try
                         wait_response(Socket)
                     catch
                         'EXIT':Reason -> {error, Reason}
                     end;
                 Error ->
                     Error
             end,

    case {Result, Secure} of
        {ok, false} -> 
            {ok, Socket};
        {ok, true} -> 
            hackney_socket:secure(Socket, Options);
        _ ->
            catch hackney_socket:close(Socket),
            Result
    end.


wait_response(Socket) ->
    ok = hackney_socket:exit_if_closed(hackney_socket:setopts(Socket, [{active, once}])),
    receive
        {http, _, {http_response, _, Status, Reason}} ->
            case lists:member(Status, [200, 201]) of
                true ->
                    ok = hackney_socket:exit_if_closed(hackney_socket:setopts(Socket, [{packet, http_hbin}])),
                    wait_headers(Socket, 0);
                false ->
                    {error, {proxy_error, {Status, Reason}}}
            end;
        {http, _, {http_error, <<"\r\n">>}} ->
            wait_response(Socket);
        {http, _, {http_error, <<"\n">>}} ->
            wait_response(Socket);
        tcp_closed ->
            {error, closed};
        {tcp_error, Reason} ->
            {error, Reason}
    after ?RESPONSE_RECV_TIMEOUT ->
              {error, proxy_timeout}
    end.

wait_headers(Socket, ?MAX_HEADERS) ->
    ok = hackney_socket:exit_if_closed(hackney_socket:setopts(Socket, [{packet, raw}])),
    {error, max_headers};
wait_headers(Socket, Count) ->
    ok = hackney_socket:exit_if_closed(hackney_socket:setopts(Socket, [{active, once}])),
    receive
        {http, _, http_eoh} ->
            hackney_socket:exit_if_closed(hackney_socket:setopts(Socket, [{packet, raw}]));
        {http, _,  {http_header, _, _, _, _}} ->
            wait_headers(Socket, Count + 1);
        tcp_closed ->
            {error, closed};
        {tcp_error, Reason} ->
            {error, Reason}
    after 
        ?HEADERS_RECV_TIMEOUT ->
            {error, proxy_timeout}
    end.
