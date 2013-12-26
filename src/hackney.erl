%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%

-module(hackney).
-export([start/0, start/1, stop/0]).
-export([connect/1, connect/2, connect/3, connect/4,
         close/1,
         request/1, request/2, request/3, request/4, request/5,
         send_request/2,
         start_response/1,
         cookies/1,
         send_body/2, finish_send_body/1,
         send_multipart_body/2,
         body/1, body/2, skip_body/1,
         stream_body/1,
         stream_multipart/1,
         skip_multipart/1,
         controlling_process/2,
         cancel_request/1]).

-export([redirect_location/1]).

-export([stream_next/1,
         stop_async/1,
         pause_stream/1,
         resume_stream/1]).

-define(METHOD_TPL(Method),
        -export([Method/1, Method/2, Method/3, Method/4])).
-include("hackney_methods.hrl").

-include("hackney.hrl").

-type url() :: #hackney_url{}.
-export_type([url/0]).

-opaque client() :: #client{}.
-export_type([client/0]).

-type client_ref() :: term().
-export_type([client_ref/0]).

%% @doc Start the couchbeam process. Useful when testing using the shell.
start() ->
    hackney_deps:ensure(),
    application:load(hackney),
    hackney_app:ensure_deps_started(),
    application:start(hackney).

start(PoolHandler) ->
    application:set_env(hackney, pool_handler, PoolHandler),
    start().

%% @doc Stop the couchbeam process. Useful when testing using the shell.
stop() ->
    application:stop(hackney).


connect(URL) ->
    connect(URL, []).

connect(#hackney_url{}=URL, Options) ->
    #hackney_url{transport=Transport,
                 host=Host,
                 port=Port} = URL,
    connect(Transport, Host, Port, Options);
connect(URL, Options) when is_binary(URL) orelse is_list(URL) ->
    connect(hackney_url:parse_url(URL), Options).

%% @doc connect a socket and create a client state.
connect(Transport, Host, Port) ->
    hackney_connect:connect(Transport, Host, Port, []).

connect(Transport, Host, Port, Options) ->
    hackney_connect:connect(Transport, Host, Port, Options).


%% @doc Assign a new controlling process <em>Pid</em> to <em>Client</em>.
-spec controlling_process(client_ref(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Ref, Pid) ->
    hackney_manager:controlling_process(Ref, Pid).

%% @doc Extract raw informations from the client context
%% This feature can be useful when you want to create a simple proxy, rerouting on the headers and the status line and continue to forward the connection for example.
%%
%% return: `{ResponseState, Transport, Socket, Buffer} | {error, Reason}'
%% <ul>
%% <li>`Response': waiting_response, on_status, on_headers, on_body</li>
%% <li>`Transport': The current transport module</li>
%% <li>`Socket': the current socket</li>
%% <li>`Buffer': Data fetched but not yet processed</li>
%% </ul>
-spec cancel_request(client_ref()) ->
    {atom(), inet:socket(), binary(), hackney_response:response_state()}
    | {error, term()}.
cancel_request(Ref) ->
    hackney_manager:cancel_request(Ref).

%% @doc close the client
close(Ref) ->
    hackney_connect:close(Ref).

%% @doc make a request
-spec request(binary()|list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(URL) ->
    request(get, URL).

%% @doc make a request
-spec request(term(), binary()|list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL) ->
    request(Method, URL, [], <<>>, []).

%% @doc make a request
-spec request(term(), binary()|list(), list())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL, Headers) ->
    request(Method, URL, Headers, <<>>, []).

%% @doc make a request
-spec request(term(), binary()|list(), list(), term())
    -> {ok, integer(), list(), #client{}} | {error, term()}.
request(Method, URL, Headers, Body) ->
    request(Method, URL, Headers, Body, []).

%% @doc make a request
%%
%% Args:
%% <ul>
%% <li><strong>Method</strong>>: method used for the request (get, post,
%% ...)</li>
%% <li><strong>Url</strong>: full url of the request</li>
%% <li><strong>Headers</strong> Proplists </li>
%% <li><strong>Body</strong>:
%%      <ul>
%%      <li>{form, [{K, V}, ...]}: send a form url encoded</li>
%%      <li>{multipart, [{K, V}, ...]}: send a form using multipart</li>
%%      <li>{file, "/path/to/file"}: to send a file</li>
%%      <li>Bin: binary or iolist</li>
%%      </ul>
%%  </li>
%%  <li><strong>Options:</strong> `[{connect_options, connect_options(),
%%  {ssl_options, ssl_options()}, Others]'</li>
%%      <li>`connect_options()': The default connect_options are
%%      `[binary, {active, false}, {packet, raw}])' . Vor valid options
%%      see the gen_tcp options.</li>
%%
%%      <li>`ssl_options()': See the ssl options from the ssl
%%      module.</li>
%%
%%      <li>`async': receive the response asynchronously
%%      The function return {ok, {response_stream, StreamRef}}.
%%      When {async, once} is used the socket will receive only once. To
%%      receive the other messages use the function
%%      `hackney:stream_next/1'
%%      </li>
%%
%%      <li>`{cookie, list() | binary()}' : to set a cookie or a
%%      list of cookies.</li>
%%
%%      <li><em>Others options are</em>:
%%      <ul>
%%          <li>`{follow_redirect, boolean}': false by default, follow a
%%          redirection</li>
%%          <li>`{max_redirect, integer}': 5 by default, the maximum of
%%          redirection for a request</li>
%%          <li>`{force_redirect, boolean}': false by default, to force the
%%          redirection even on POST</li>
%%          <li>`{proxy, proxy_options()}': to connect via a proxy.</li>
%%          <li>`insecure': to perform "insecure" SSL connections and
%%          transfers without checking the certificate</li>
%%          <li>`{connect_timeout, infinity | integer()}': timeout used when
%%          estabilishing a connection, in milliseconds. Default is 8000</li>
%%          <li>`{recv_timeout, infinity | integer()}': timeout used when
%%          receiving a connection. Default is infinity</li>
%%      </ul>
%%
%%      <blocquote>Note: if the response is async, only
%%      `follow_redirect' is take in consideration for the redirection.
%%      If a valid redirection happen you receive the messages:
%%      <ul>
%%        <li>`{redirect, To, Headers'}</li>
%%        <li>`{see_other, To, Headers}' for status 303 POST requests.</li>
%%      </ul></blocquote>
%%
%%      </li>
%%
%%      <li>`proxy_options()':  options to connect by a proxy:
%%      <p><ul>
%%          <li>binary(): url to use for the proxy. Used for basic HTTP
%%          proxy</li>
%%          <li>{Host::binary(), Port::binary}: Host and port to connect,
%%          for HTTP proxy</li>
%%      </ul></p>
%%      </li>
%%  </ul>
%%
%%  <bloquote>Note: instead of doing `hackney:request(Method, ...)' you can
%%  also do `hackney:Method(...)' if you prefer to use the REST
%%  syntax.</bloquote>
-spec request(term(), url() | binary(), list(), term(), list())
    -> {ok, integer(), list(), client_ref()}
    | {ok, client_ref()}
    | {error, term()}.
request(Method, #hackney_url{}=URL, Headers, Body, Options0) ->
    #hackney_url{transport=Transport,
                 host = Host,
                 port = Port,
                 user = User,
                 password = Password} = URL,

    Options = case User of
        <<>> ->
            Options0;
        _ ->
            lists:keystore(basic_auth, 1, Options0,
                           {basic_auth, {User, Password}})
    end,

    Request = make_request(Method, URL, Headers, Body),

    case maybe_proxy(Transport, Host, Port, Options) of
        {ok, Ref} ->
            send_request(Ref, Request);
        Error ->
            Error
    end;
request(Method, URL, Headers, Body, Options)
        when is_binary(URL) orelse is_list(URL) ->
    request(Method, hackney_url:parse_url(URL), Headers, Body, Options).


%% @doc send a request using the current client state

send_request(Ref, Req) when is_reference(Ref) ->
    case hackney_manager:get_state(Ref) of
        req_not_found ->
            {error, closed};
        State ->
            send_request(State, Req)
    end;
send_request(#client{response_state=done}=Client0 ,
             {Method, Path, Headers, Body}) ->
    Client = Client0#client{response_state=start, body_state=waiting},
    send_request(Client, {Method, Path, Headers, Body});

send_request(Client0, {Method, Path, Headers, Body}=Req) ->
    case hackney_connect:maybe_connect(Client0) of
        {ok, Client} ->
            case {Client#client.response_state, Client#client.body_state} of
                {start, waiting} ->
                    Resp = hackney_request:perform(Client, {Method,
                                                            Path,
                                                            Headers,
                                                            Body}),
                    Reply = maybe_redirect(Resp, Req, 0),
                    reply_response(Reply, Client);
                _ ->
                    {error, invalide_state}
            end;
        Error ->
            Error
    end.

%% @doc send the request body until eob. It's issued after sending a request using
%% the `request' and `send_request' functions.
-spec send_body(client_ref(), term())
    -> ok | {error, term()}.
send_body(Ref, Body) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_request:stream_body(Body, State),
                reply(Reply, State)
        end).

finish_send_body(Ref) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_request:end_stream_body(State),
                reply(Reply, State)
        end).


%% @doc send a multipart body until eof
%% Possible value are :
%% <ul>
%% <li>`eof': end the multipart request</li>
%% <li>`{Id, {File, FileName}}': to stream a file</li>
%% %% <li>`{Id, {File, FileName, FileOptions}}': to stream a file</li>
%% <li>`{data, {start, Id, DileName, ContentType}}': to start to stream
%% arbitrary binary content</li>
%% <li>`{data, Bin}`: send a binary. Use it only after emitting a
%% **start**</li>
%% <li>`{data, eof}`: stop sending an arbitary content. It doesn't stop
%% the multipart request</li>
%% <li>`{Id, {file, Filename, Content}': send a full content as a
%% boundary</li>
%% <li>`{Id, Value}': send an arbitrary value as a boundary. Filename and
%% Id are identique</li>
%% </ul>
%% File options can be:
%% <ul>
%% <li>`{offset, Offset}': start to send file from this offset</li>
%% <li>`{bytes, Bytes}': number of bytes to send</li>
%% <li>`{chunk_size, ChunkSize}': the size of the chunk to send</li>
%% </ul>
-spec send_multipart_body(client_ref(), term()) -> ok | {error, term()}.
send_multipart_body(Ref, Body) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_request:stream_multipart(Body, State),
                reply(Reply, State)
        end).

%% @doc start a response.
%% Useful if you stream the body by yourself. It will fetch the status
%% and headers of the response. and return
-spec start_response(client_ref())
    -> {ok, integer(), list(), client_ref()} | {ok, client_ref()} | {error, term()}.
start_response(Ref) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_response:start_response(State),
                reply_response(Reply, State)
        end).

%% return all parsed cookies from the response headers.
-spec cookies(list()) -> list().
cookies(Headers) ->
    lists:foldl(fun({K, V}, Acc) ->
                case hackney_bstr:to_lower(K) of
                    <<"set-cookie">> ->
                        case hackney_cookie:parse_cookie(V) of
                            {error, _} -> Acc;
                            [{Name, _} | _]=Cookie ->
                                [{Name, Cookie} | Acc]
                        end;
                    _ ->
                        Acc
                end
        end, [], Headers).

%% @doc Stream the response body.
-spec stream_body(client_ref())
    -> {ok, binary()} | done | {error, term()}.
stream_body(Ref) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_response:stream_body(State),
                reply(Reply, State)
        end).

%% @doc Stream the response body.
-spec stream_multipart(client_ref())
    -> {headers, list()} | {body, binary()} | eof | end_of_part
    | {error, term()}.
stream_multipart(Ref) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_response:stream_multipart(State),
                mp_reply(Reply, State)
        end).

%% @doc Stream the response body.
-spec skip_multipart(client_ref()) -> ok | {error, term()}.
skip_multipart(Ref) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_response:skip_multipart(State),
                mp_reply(Reply, State)
        end).

%% @doc Return the full body sent with the response.
-spec body(client_ref()) -> {ok, binary()} | {error, atom()}.
body(Ref) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_response:body(State),
                reply(Reply, State)
        end).

%% @doc Return the full body sent with the response as long as the body
%% length doesn't go over MaxLength.
-spec body(client_ref(), non_neg_integer() | infinity)
	-> {ok, binary()} | {error, atom()}.
body(Ref, MaxLength) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_response:body(MaxLength, State),
                reply(Reply, State)
        end).


%% @doc skip the full body. (read all the body if needed).
-spec skip_body(client_ref()) -> ok | {error, atom()}.
skip_body(Ref) ->
    hackney_manager:get_state(Ref, fun(State) ->
                Reply = hackney_response:skip_body(State),
                reply(Reply, State)
        end).


%% @doc continue to the next stream message. Only use it when
%% `{async, once}' is set in the client options.
-spec stream_next(client_ref()) -> ok | {error, req_not_found}.
stream_next(Ref) ->
    hackney_manager:with_async_response_pid(Ref, fun(Pid) ->
                Pid ! {Ref, stream_next},
                ok
        end).

%% @doc pause a response stream, the stream process will hibernate and
%% be woken later by the resume function
-spec pause_stream(client_ref()) -> ok | {error, req_not_found}.
pause_stream(Ref) ->
    hackney_manager:with_async_response_pid(Ref, fun(Pid) ->
                Pid ! {Ref, pauwse},
                ok
        end).

%% @doc resume a paused response stream, the stream process will be
%% awoken
-spec resume_stream(client_ref()) -> ok | {error, req_not_found}.
resume_stream(Ref) ->
    hackney_manager:with_async_response_pid(Ref, fun(Pid) ->
                Pid ! {Ref, resume},
                ok
        end).

%% @doc stop to receive asynchronously.
-spec stop_async(client_ref()) -> ok | {error, req_not_found} | {error, term()}.
stop_async(Ref) ->
    hackney_manager:stop_async_response(Ref).



%% internal functions
%%
make_request(Method, #hackney_url{}=URL, Headers0, Body) ->
    #hackney_url{host = Host,
                 port = Port,
                 path = Path,
                 qs = Query} = URL,


    FinalPath = case Query of
        <<>> ->
            Path;
        _ ->
            <<Path/binary, "?", Query/binary>>
    end,

    Headers = case lists:keyfind(<<"Host">>, 1, Headers0) of
        false ->
          Headers0 ++ [{<<"Host">>, iolist_to_binary([Host, ":",
                                                      integer_to_list(Port)])}];
        _ ->
          Headers0
    end,

    {Method, FinalPath, Headers, Body}.


maybe_proxy(Transport, Host, Port, Options)
        when is_list(Host), is_integer(Port), is_list(Options) ->

    case proplists:get_value(proxy, Options) of
        Url when is_binary(Url) orelse is_list(Url) ->
            ProxyOpts = [{basic_auth, proplists:get_value(proxy_auth,
                                                          Options)}],
            #hackney_url{transport=PTransport} = hackney_url:parse_url(Url),

            if PTransport =/= Transport ->
                    {error, invalid_proxy_transport};
                true ->
                    hackney_http_proxy:connect(Url, ProxyOpts, Host, Port, Options)
            end;
        {ProxyHost, ProxyPort} ->
            Netloc = iolist_to_binary([ProxyHost, ":",
                                       integer_to_list(ProxyPort)]),
            Scheme = hackney_url:transport_scheme(Transport),
            Url = #hackney_url{scheme=Scheme, netloc=Netloc},
            ProxyOpts = [{basic_auth, proplists:get_value(proxy_auth,
                                                          Options)}],
            hackney_http_proxy:connect(hackney_url:unparse_url(Url), ProxyOpts, Host,
                             Port, Options, true);
        {socks5, ProxyHost, ProxyPort} ->
            %% create connection options
            ProxyUser = proplists:get_value(socks5_user, Options),
            ProxyPass = proplists:get_value(socks5_pass, Options),
            ConnectOpts = proplists:get_value(connect_options, Options,
                                              []),
            ConnectOpts1 = [{socks5_host, ProxyHost},
                           {socks5_port, ProxyPort},
                           {socks5_user, ProxyUser},
                           {socks5_pass, ProxyPass},
                           {socks5_transport, Transport} | ConnectOpts],

            %% ssl options?
            Insecure = proplists:get_value(insecure, Options, false),
            ConnectOpts2 =  case proplists:get_value(ssl_options,
                                                     Options) of
                undefined ->
                    [{insecure, Insecure}] ++ ConnectOpts1;
                SslOpts ->
                    [{ssl_opttions, SslOpts},
                     {insecure, Insecure}] ++ ConnectOpts1
            end,

            Options1 = lists:keystore(connect_options, 1, Options,
                                      {connect_options, ConnectOpts2}),

            %% connect using a socks5 proxy
            hackney_connect:connect(hackney_socks5, Host, Port, Options1,
                                    true);
        _ ->
            hackney_connect:connect(Transport, Host, Port, Options, true)
    end.


maybe_redirect({ok, _}=Resp, _Req, _Tries) ->
    Resp;
maybe_redirect({ok, S, H, #client{follow_redirect=true,
                                  max_redirect=Max,
                                  force_redirect=ForceRedirect}=Client}=Resp,
               Req, Tries) when Tries < Max ->

    {Method, _Path, Headers, Body} = Req,
    case lists:member(S, [301, 302, 307]) of
        true ->
            Location = redirect_location(H),
            %% redirect the location if possible. If the method is
            %% different from  get or head it will return
            %% `{ok, {maybe_redirect, Status, Headers, Client}}' to let
            %% the  user make his choice.
            case {Location, lists:member(Method, [get, head])} of
                {undefined, _} ->
                    {error, {invalid_redirection, Resp}};
                {_, true} ->
                        NewReq = {Method, Location, Headers, Body},
                        maybe_redirect(redirect(Client, NewReq), Req,
                                       Tries+1);
                {_, _} when ForceRedirect =:= true ->
                        NewReq = {Method, Location, Headers, Body},
                        maybe_redirect(redirect(Client, NewReq), Req,
                                       Tries+1);
                {_, _} ->
                    {ok, {maybe_redirect, S, H, Client}}
            end;
        false when S =:= 303 ->
            %% see other. If methos is not POST we consider it as an
            %% invalid redirection
            Location = redirect_location(H),
            case {Location, Method} of
                {undefined, _} ->
                    {error, {invalid_redirection, Resp}};
                {_, post} ->
                    NewReq = {get, Location, [], <<>>},
                    maybe_redirect(redirect(Client, NewReq), Req, Tries+1);
                {_, _} ->

                    {error, {invalid_redirection, Resp}}
            end;
        _ ->
            Resp
    end;
maybe_redirect({ok, S, _H, #client{follow_redirect=true}}=Resp,
               _Req, _Tries) ->
    case lists:member(S, [301, 302, 303, 307]) of
        true ->
            {error, {max_redirect_overflow, Resp}};
        false ->
            Resp
    end;
maybe_redirect(Resp, _Req, _Tries) ->
    Resp.


redirect(Client0, {Method, NewLocation, Headers, Body}) ->
    %% skip the body
    {ok, Client} = hackney_response:skip_body(Client0),

    %% close the connection if we don't use a pool
    RedirectUrl = hackney_url:parse_url(NewLocation),
    #hackney_url{transport=RedirectTransport,
                 host=RedirectHost,
                 port=RedirectPort}=RedirectUrl,
    RedirectRequest = make_request(Method, RedirectUrl, Headers, Body),
    %% make a request without any redirection
    #client{transport=Transport,
            host=Host,
            port=Port,
            options=Opts0,
            redirect=Redirect} = Client,

    Opts = lists:keystore(follow_redirect, 1, Opts0,
                          {follow_redirect, false}),

    %% update the state with the redirect info
    Client1 = Client#client{transport=RedirectTransport,
                            host=RedirectHost,
                            port=RedirectPort,
                            options=Opts},

    %% send a request to the new location
    case send_request(Client1, RedirectRequest) of
        {ok,  S, H, RedirectRef} when is_reference(RedirectRef) ->
            RedirectState = hackney_manager:get_state(RedirectRef),
            RedirectState1 = case Redirect of
                nil ->
                    RedirectState#client{redirect=Redirect,
                                         options=Opts0};
                _ ->
                    NewRedirect = {Transport, Host, Port, Opts0},
                    RedirectState#client{redirect=NewRedirect,
                                         options=Opts0}
            end,
            {ok, S, H, RedirectState1};
        {ok,  S, H, RedirectClient} when Redirect /= nil ->
            NewClient = RedirectClient#client{redirect=Redirect,
                                              options=Opts0},
            {ok, S, H, NewClient};
        {ok, S, H, RedirectClient} ->
            NewRedirect = {Transport, Host, Port, Opts0},
            NewClient = RedirectClient#client{redirect=NewRedirect,
                                              options=Opts0},
            {ok, S, H, NewClient};
        Error ->
            Error
    end.

redirect_location(Headers) ->
    hackney_headers:get_value(<<"location">>, hackney_headers:new(Headers)).


%% handle send response
reply({ok, Data, NState}, _State) ->
    maybe_update_req(NState),
    {ok, Data};
reply({done, NState}, _State) ->
    maybe_update_req(NState),
    done;
reply({ok, NState}, _State) ->
    hackney_manager:update_state(NState),
    ok;
reply(Error, State) ->
    hackney_manager:handle_error(State),
    Error.

mp_reply({headers, Headers, NState}, _State) ->
    maybe_update_req(NState),
    {headers, Headers};
mp_reply({body, Body, NState}, _State) ->
    maybe_update_req(NState),
    {body, Body};
mp_reply({mp_mixed, NState}, _State) ->
    maybe_update_req(NState),
    mp_mixed;
mp_reply({mp_mixed_eof, NState}, _State) ->
    maybe_update_req(NState),
    mp_mixed_eof;
mp_reply({eof, NState}, _State) ->
    maybe_update_req(NState),
    eof;
mp_reply({end_of_part, NState}, _State) ->
    maybe_update_req(NState),
    end_of_part;
mp_reply({ok, NState}, _State) ->
    hackney_manager:update_state(NState),
    ok;
mp_reply(Error, State) ->
    hackney_manager:handle_error(State),
    Error.

%% response reply
reply_response({ok, Status, Headers, #client{request_ref=Ref}=NState},
               _State) ->
    hackney_manager:update_state(NState),
    {ok, Status, Headers, Ref};
reply_response({ok, #client{request_ref=Ref}=NState}, _State) ->
    hackney_manager:update_state(NState),
    {ok, Ref};
reply_response({ok, Ref}, _State) when is_reference(Ref) ->
    {ok, Ref};
reply_response(Error, State) ->
    hackney_manager:handle_error(State),
    Error.


maybe_update_req(#client{dynamic=true, response_state=done}=State) ->
    hackney_manager:cancel_request(State);
maybe_update_req(State) ->
    hackney_manager:update_state(State).

-define(METHOD_TPL(Method),
        Method(URL) ->
            hackney:request(Method, URL)).
-include("hackney_methods.hrl").

-define(METHOD_TPL(Method),
        Method(URL, Headers) ->
            hackney:request(Method, URL, Headers)).
-include("hackney_methods.hrl").


-define(METHOD_TPL(Method),
        Method(URL, Headers, Body) ->
            hackney:request(Method, URL, Headers, Body)).
-include("hackney_methods.hrl").

-define(METHOD_TPL(Method),
        Method(URL, Headers, Body, Options) ->
            hackney:request(Method, URL, Headers, Body, Options)).
-include("hackney_methods.hrl").
