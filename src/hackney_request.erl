%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%

%% @doc module handling the request

-module(hackney_request).

-include("hackney.hrl").

-export([perform/2,
         send/2, send_chunk/2,
         sendfile/3,
         stream_body/2, end_stream_body/1,
         stream_multipart/2,
         encode_form/1]).

-define(CHUNK_SIZE, 20480).

perform(Client0, {Method0, Path, Headers0, Body0}) ->
    Method = hackney_bstr:to_upper(hackney_bstr:to_binary(Method0)),

    #client{host=Host, port=Port, options=Options} = Client0,

    %% set initial headers
    HostHdr = case is_default_port(Client0) of
        true ->
            list_to_binary(Host);
        false ->
            iolist_to_binary([Host, ":", integer_to_list(Port)])
    end,
    DefaultHeaders0 = [{<<"Host">>, HostHdr},
                       {<<"User-Agent">>, default_ua()}],

    %% basic authorization handling
    DefaultHeaders = case proplists:get_value(basic_auth, Options) of
        undefined ->
            DefaultHeaders0;
        {User, Pwd} ->
            Credentials = base64:encode(<< User/binary, ":", Pwd/binary >>),
            DefaultHeaders0 ++ [{<<"Authorization">>,
                                 <<"Basic ", Credentials/binary>>}]
    end,

    %% add any cookies passed ot options
    Cookies = proplists:get_value(cookie, Options, []),
    DefaultHeaders1 = maybe_add_cookies(Cookies, DefaultHeaders),

    HeadersDict0 = hackney_headers:update(hackney_headers:new(DefaultHeaders1),
                                          Headers0),
    {HeadersDict, ReqType0} = req_type(HeadersDict0, Body0),
    Expect = expectation(HeadersDict),

    %% build headers with the body.
    {HeaderDict1, ReqType, Body, Client1} = case Body0 of
        stream ->
            {HeadersDict, ReqType0, stream, Client0};
        stream_multipart ->
            handle_multipart_body(HeadersDict, ReqType0, Client0);
        {stream_multipart, Size} ->
            handle_multipart_body(HeadersDict, ReqType0, Size, Client0);
        {stream_multipart, Size, Boundary} ->
            handle_multipart_body(HeadersDict, ReqType0, Size,
                                  Boundary, Client0);
        <<>> when Method =:= <<"POST">> orelse Method =:= <<"PUT">> ->
            handle_body(HeadersDict, ReqType0, Body0, Client0);
        <<>> ->
            {HeadersDict, ReqType0, Body0, Client0};
        _ ->
            handle_body(HeadersDict, ReqType0, Body0, Client0)
    end,

    Client = case ReqType of
        normal ->
            Client1#client{send_fun=fun hackney_request:send/2,
                           req_type=normal};
        chunked ->
            Client1#client{send_fun=fun hackney_request:send_chunk/2,
                           req_type=chunked}
    end,


    HeadersLines = hackney_headers:fold(fun({K, V}, Lines) ->
                    V1 = hackney_bstr:to_binary(V),
                    [ << K/binary, ": ", V1/binary, "\r\n" >> | Lines]
            end, [], HeaderDict1),

    HostOrPath = case Method0 of
        connect ->
            iolist_to_binary([Host, ":", integer_to_list(Port)]);
        _ ->
            Path
    end,

    HeadersData = iolist_to_binary([
                << Method/binary, " ", HostOrPath/binary, " HTTP/1.1", "\r\n" >>,
                HeadersLines,
                <<"\r\n">>]),

    PerformAll = proplists:get_value(perform_all, Options, true),

    case can_perform_all(Body, Expect, PerformAll) of
        true ->
            perform_all(Client, HeadersData, Body, Method, Expect);
        _ ->
            case hackney_request:send(Client, HeadersData) of
                ok when Body =:= stream ->
                    {ok, Client#client{response_state=stream, method=Method,
                                       expect=Expect}};
                ok ->
                    case stream_body(Body, Client#client{expect=Expect}) of
                        {error, _Reason}=E ->
                            E;
                        {stop, Client2} ->
                            FinalClient = Client2#client{method=Method},
                            hackney_response:start_response(FinalClient);
                        {ok, Client2} ->
                            case end_stream_body(Client2) of
                              {ok, Client3} ->
                                FinalClient = Client3#client{method=Method},
                                hackney_response:start_response(FinalClient);
                              Error ->
                                Error
                            end
                    end;
                Error ->
                    Error
            end
    end.

stream_body(Msg, #client{expect=true}=Client) ->
    case hackney_response:expect_response(Client) of
        {continue, Client2} ->
            stream_body(Msg, Client2);
        {stop, Client2} ->
            {stop, Client2};
        Error ->
            Error
    end;
stream_body(eof, Client) ->
    {ok, Client};
stream_body(<<>>, Client) ->
    {ok, Client};
stream_body(Func, Client) when is_function(Func) ->
    case Func() of
        {ok, Data} ->
            case stream_body(Data, Client) of
                {ok, Client1} ->
                    stream_body(Func, Client1);
                Error ->
                    Error
            end;
        eof ->
            stream_body(eof, Client);
        Err ->
            Err
    end;
stream_body({Func, State}, Client) when is_function(Func) ->
    case Func(State) of
        {ok, Data, NewState} ->
            case stream_body(Data, Client) of
                {ok, Client1} ->
                    stream_body({Func, NewState}, Client1);
                Error ->
                    Error
            end;
        eof ->
            stream_body(eof, Client);
        Err ->
            Err
    end;
stream_body(Body, #client{req_chunk_size=ChunkSize, send_fun=Send}=Client)
        when is_binary(Body) ->

    case Body of
        _ when byte_size(Body) >= ChunkSize ->
            << Data:ChunkSize/binary, Rest/binary >> = Body,
            case Send(Client, Data) of
                ok ->
                    stream_body(Rest, Client);
                Error ->
                    Error
            end;
        _ ->
            case Send(Client, Body) of
                ok ->
                    {ok, Client};
                Error ->
                    Error
            end
    end;
stream_body(Body, #client{send_fun=Send}=Client) when is_list(Body) ->
    case Send(Client, Body) of
        ok ->
            {ok, Client};
        Error ->
            Error
    end;
stream_body({file, FileName}, Client) ->
    stream_body({file, FileName, []}, Client);
stream_body({file, FileName, Opts}, Client) ->
    case sendfile(FileName, Opts, Client) of
        {ok, _BytesSent} ->
            {ok, Client};
        Error ->
            Error
    end.

%% @doc stream multipart
stream_multipart(eof, #client{response_state=waiting}=Client) ->
    {ok, Client};
stream_multipart(eof, #client{mp_boundary=Boundary}=Client) ->
    Line = <<"--", Boundary/binary, "--", "\r\n\r\n">>,
    case stream_body(Line, Client) of
        {ok, Client1} ->
            end_stream_body(Client1);
        Error ->
            Error
    end;
stream_multipart({Id, {file, Name}}, Client) ->
    stream_multipart({Id, {file, Name, []}}, Client);
stream_multipart({Id, {file, Name, _Opts}=File},
                 #client{mp_boundary=Boundary}=Client) ->
    Field = hackney_multipart:field(Id),
    CType = hackney_bstr:content_type(Name),
    Bin = hackney_multipart:mp_header(Field, Name, CType, Boundary),
    case stream_body(Bin, Client) of
        {ok, Client1} ->
            case stream_body(File, Client1) of
                {ok, Client2} ->
                   stream_body(<<"\r\n">>, Client2);
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
stream_multipart({data, {start, Name, FileName, CType}},
                 #client{mp_boundary=Boundary}=Client) ->
    Bin = hackney_multipart:mp_header(Name, FileName, CType, Boundary),
    stream_body(Bin, Client);
stream_multipart({data, eof}, Client) ->
    stream_body(<<"\r\n">>, Client);
stream_multipart({data, Bin}, Client) ->
    stream_body(Bin, Client);
stream_multipart({Id, {file, Name, Content}},
                 #client{mp_boundary=Boundary}=Client) ->

    Bin = hackney_multipart:encode({Id, {file, Name, Content}}, Boundary),
    stream_body(Bin, Client);
stream_multipart({Id, Value}, #client{mp_boundary=Boundary}=Client) ->
    Bin = hackney_multipart:encode({Id, Value}, Boundary),
    stream_body(Bin, Client).


send(#client{transport=Transport, socket=Skt}, Data) ->
    Transport:send(Skt, Data).

send_chunk(Client, Data) ->
    Length = iolist_size(Data),
    send(Client, [io_lib:format("~.16b\r\n", [Length]), Data,
                  <<"\r\n">>]).

sendfile(FileName, Opts, #client{transport=hackney_tcp_tansport, socket=Skt,
                           req_type=normal}) ->
    Offset = proplists:get_value(offset, Opts, 0),
    Bytes = proplists:get_value(bytes, Opts, 0),

    SendFileOpts = case proplists:get_value(chunk_size, Opts, ?CHUNK_SIZE) of
        undefined -> Opts;
        ChunkSize -> [{chunk_size, ChunkSize}]
    end,
    file:sendfile(FileName, Skt, Offset, Bytes, SendFileOpts);
sendfile(FileName, Opts, Client) ->
    case file:open(FileName, [read, raw, binary]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Fd} ->
	    Res = sendfile_fallback(Fd, Opts, Client),
	    file:close(Fd),
	    Res
    end.

%% @doc encode a list of properties in a form.
encode_form(KVs) ->
    Lines = hackney_url:qs(KVs),
    CType = <<"application/x-www-form-urlencoded; charset=utf-8">>,
    {erlang:byte_size(Lines), CType, Lines}.


%% internal
handle_body(Headers, ReqType0, Body0, Client) ->
    {CLen, CType, Body} = case Body0 of
        {form, KVs} ->
            encode_form(KVs);
        {multipart, KVs} ->
            hackney_multipart:encode_form(KVs);
        {file, FileName} ->
            S= filelib:file_size(FileName),
	        CT = hackney_headers:get_value(<<"content-type">>, Headers,
					   hackney_util:content_type(FileName)),
            {S, CT, Body0};
        Func when is_function(Func) ->
            CT = hackney_headers:get_value(<<"content-type">>, Headers,
                                           <<"application/octet-stream">>),
            S = hackney_headers:get_value(<<"content-length">>,
                                          Headers),
            {S, CT, Body0};
        {Func, _} when is_function(Func) ->
            CT = hackney_headers:get_value(<<"content-type">>, Headers,
                                           <<"application/octet-stream">>),
            S = hackney_headers:get_value(<<"content-length">>,
                                          Headers),
            {S, CT, Body0};

        _ when is_list(Body0) -> % iolist case
            Body1 = iolist_to_binary(Body0),
            S = size(Body1),
            CT = hackney_headers:get_value(<<"content-type">>, Headers,
                                           <<"application/octet-stream">>),
            {S, CT, iolist_to_binary(Body1)};
        _ when is_binary(Body0) ->
            S = erlang:size(Body0),
            CT = hackney_headers:get_value(<<"content-type">>, Headers,
                                           <<"application/octet-stream">>),
            {S, CT, Body0}
    end,

    {NewHeaders, ReqType} = case {ReqType0, Body} of
        {chunked, {file, _}} ->

            NewHeadersKV = [{<<"Content-Type">>, CType},
                            {<<"Content-Length">>, CLen}],
            Headers1 = hackney_headers:delete(<<"transfer-encoding">>,
                                              Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), normal};
        {chunked, F} when is_function(F) ->
            NewHeadersKV = [{<<"Content-Type">>, CType}],
            Headers1 = hackney_headers:delete(<<"content-length">>,
                                              Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), chunked};
        {chunked, {F, _}} when is_function(F) ->
            NewHeadersKV = [{<<"Content-Type">>, CType}],
            Headers1 = hackney_headers:delete(<<"content-length">>,
                                              Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), chunked};

        {chunked, _} ->
            NewHeadersKV = [{<<"Content-Type">>, CType}],
            Headers1 = hackney_headers:delete(<<"content-length">>,
                                              Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), chunked};

        {_, _} when CLen =:= undefined ->
            NewHeadersKV = [{<<"Content-Type">>, CType},
                            {<<"Transfer-Encoding">>, <<"chunked">>}],
            Headers1 = hackney_headers:delete(<<"content-length">>,
                                              Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), chunked};

        {_, _} ->
            NewHeadersKV = [{<<"Content-Type">>, CType},
                            {<<"Content-Length">>, CLen}],
            {hackney_headers:update(Headers, NewHeadersKV), normal}
    end,
    {NewHeaders, ReqType, Body, Client}.

handle_multipart_body(Headers, ReqType, Client) ->
    handle_multipart_body(Headers, ReqType, chunked,
                          hackney_multipart:boundary(), Client).

handle_multipart_body(Headers, ReqType, CLen, Client) ->
    handle_multipart_body(Headers, ReqType, CLen,
                          hackney_multipart:boundary(), Client).

handle_multipart_body(Headers, ReqType, CLen, Boundary, Client) ->
    CType = case hackney_headers:parse(<<"content-type">>, Headers) of
        {<<"multipart">>, _, _} ->
            hackney_headers:get_value(<<"content-type">>, Headers);
        _ ->
            << "multipart/form-data; boundary=", Boundary/binary >>
    end,

    {NewHeaders, ReqType1}  = case {CLen, ReqType} of
        {chunked, normal} ->
            NewHeadersKV = [{<<"Content-Type">>, CType},
                            {<<"Transfer-Encoding">>, <<"chunked">>}],
            Headers1 = hackney_headers:delete(<<"content-length">>,  Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), chunked};
        {chunked, _} ->
            NewHeadersKV = [{<<"Content-Type">>, CType}],
            {hackney_headers:update(Headers, NewHeadersKV), chunked};
        {_, chunked} ->
            NewHeadersKV = [{<<"Content-Type">>, CType},
                            {<<"Content-Length">>, CLen}],
            Headers1 = hackney_headers:delete(<<"transfer-encoding">>,
                                              Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), normal};
        {_, _} ->
            NewHeadersKV = [{<<"Content-Type">>, CType},
                            {<<"Content-Length">>, CLen}],
            {hackney_headers:update(Headers, NewHeadersKV), normal}
    end,
    {NewHeaders, ReqType1, stream, Client#client{response_state=stream,
                                                 mp_boundary=Boundary}}.

req_type(Headers, stream) ->
    TE = hackney_headers:get_value(<<"Transfer-Encoding">>, Headers, <<>>),
    CLen = hackney_headers:get_value(<<"Content-Length">>, Headers,
                                     undefined),

    case hackney_bstr:to_lower(TE) of
        <<"chunked">> ->
            {Headers, chunked};
        _ when CLen =:= undefined ->
            Headers1 = hackney_headers:update(Headers,
                                              [{<<"Transfer-Encoding">>,
                                                <<"chunked">>}]),
            {Headers1, chunked};
        _ ->
            {Headers, normal}
    end;
req_type(Headers, _Body) ->
    TE = hackney_headers:get_value(<<"Transfer-Encoding">>, Headers, <<>>),
    case hackney_bstr:to_lower(TE) of
        <<"chunked">> -> {Headers, chunked};
        _ -> {Headers, normal}
    end.

expectation(Headers) ->
    ExpectHdr = hackney_headers:get_value(<<"Expect">>, Headers, <<>>),
    case hackney_bstr:to_lower(ExpectHdr) of
        <<"100-continue">> -> true;
        _ -> false
    end.

end_stream_body(#client{req_type=chunked}=Client) ->
    case send_chunk(Client, <<>>) of
        ok ->
            {ok, Client#client{response_state=waiting}};
        Error ->
            Error
    end;
end_stream_body(Client) ->
    {ok, Client#client{response_state=waiting}}.

can_perform_all(Body, Expect, PerformAll) when Expect =:= false,
    (is_list(Body) orelse is_binary(Body)) ->
    PerformAll;
can_perform_all(_Body, _Expect, _PerformAll) ->
    false.

perform_all(Client, HeadersData, Body, Method, Expect) ->
    case stream_body(iolist_to_binary([HeadersData, Body]), Client#client{expect=Expect}) of
        {error, _Reason}=E ->
            E;
        {stop, Client2} ->
            FinalClient = Client2#client{method=Method},
            hackney_response:start_response(FinalClient);
        {ok, Client2} ->
            case end_stream_body(Client2) of
                {ok, Client3} ->
                    FinalClient = Client3#client{method=Method},
                    hackney_response:start_response(FinalClient);
                Error ->
                    Error
            end
    end.

sendfile_fallback(Fd, Opts, Client) ->
    Offset = proplists:get_value(offset, Opts, 0),
    Bytes = proplists:get_value(bytes, Opts, 0),
    ChunkSize = proplists:get_value(chunk_size, Opts, ?CHUNK_SIZE),

    {ok, CurrPos} = file:position(Fd, {cur, 0}),
    {ok, _NewPos} = file:position(Fd, {bof, Offset}),
    Res = sendfile_fallback(Fd, Bytes, ChunkSize, Client, 0),
    file:position(Fd, {bof, CurrPos}),
    Res.


sendfile_fallback(Fd, Bytes, ChunkSize, #client{send_fun=Send}=Client, Sent)
        when Bytes > Sent orelse Bytes =:= 0 ->

    Length = if Bytes > 0 -> erlang:min(ChunkSize, Bytes - Sent);
        true -> ChunkSize
    end,

    case file:read(Fd, Length) of
        {ok, Data} ->
            Len = iolist_size(Data),
            case Send(Client, Data) of
                ok ->
                    sendfile_fallback(Fd, Bytes, ChunkSize, Client,
                                      Sent + Len);
                Error ->
                    Error
            end;
        eof ->
            {ok, Sent};
        Error ->
            Error
    end;
sendfile_fallback(_, _, _, _, Sent) ->
    {ok, Sent}.

maybe_add_cookies([], Headers) ->
    Headers;
maybe_add_cookies(Cookie, Headers) when is_binary(Cookie) ->
    Headers ++ [{<<"Cookie">>, Cookie}];
maybe_add_cookies({Name, Value}, Headers) ->
    Cookie = hackney_cookie:setcookie(Name, Value, []),
    Headers ++ [{<<"Cookie">>, Cookie}];
maybe_add_cookies({Name, Value, Opts}, Headers) ->
    Cookie = hackney_cookie:setcookie(Name, Value, Opts),
    Headers ++ [{<<"Cookie">>, Cookie}];
maybe_add_cookies([{Name, Value} | Rest], Headers) ->
    Cookie = hackney_cookie:setcookie(Name, Value, []),
    Headers1 = Headers ++ [{<<"Cookie">>, Cookie}],
    maybe_add_cookies(Rest, Headers1);
maybe_add_cookies([{Name, Value, Opts} | Rest], Headers) ->
    Cookie = hackney_cookie:setcookie(Name, Value, Opts),
    Headers1 = Headers ++ [{<<"Cookie">>, Cookie}],
    maybe_add_cookies(Rest, Headers1);
maybe_add_cookies([Cookie | Rest], Headers) ->
    Headers1 = Headers ++ [{<<"Cookie">>, Cookie}],
    maybe_add_cookies(Rest, Headers1).

default_ua() ->
    Version = case application:get_key(hackney, vsn) of
        {ok, FullVersion} ->
            list_to_binary(hd(string:tokens(FullVersion, "-")));
        _ ->
            << "0.0.0" >>
    end,
    << "hackney/", Version/binary >>.


is_default_port(#client{transport=hackney_tcp_transport, port=80}) ->
    true;
is_default_port(#client{transport=hackney_ssl_transport, port=443}) ->
    true;
is_default_port(_) ->
    false.
