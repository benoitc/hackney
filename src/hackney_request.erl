%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.


-module(hackney_request).

-include("hackney.hrl").

-export([perform/2,
         send/2, send_chunk/2,
         sendfile/2,
         stream_body/2, end_stream_body/1]).


perform(Client0, {Method0, Path, Headers0, Body0}) ->
    Method = hackney_util:to_upper(hackney_util:to_binary(Method0)),

    #client{host=Host, port=Port, options=Options} = Client0,

    HostHdr = case is_default_port(Client0) of
        true ->
            list_to_binary(Host);
        false ->
            iolist_to_binary([Host, ":", integer_to_list(Port)])
    end,

    %% make header dict
    DefaultHeaders0 = [{<<"Host">>, HostHdr},
                       {<<"User-Agent">>, default_ua()}],

    %% basic authorization handling
    DefaultHeaders = case proplists:get_value(basic_auth, Options) of
        undefined ->
            DefaultHeaders0;
        {User, Pwd} ->
            Credentials = base64:encode(<< User/binary, ":", Pwd/binary >>),
            DefaultHeaders0 ++ [{<<"Authorization">>, <<"Basic ", Credentials/binary>>}]
    end,

    HeadersDict = hackney_headers:update(hackney_headers:new(DefaultHeaders),
                                         Headers0),

    ReqType0 = req_type(HeadersDict),

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
                    V1 = hackney_util:to_binary(V),
                    [ << K/binary, ": ", V1/binary, "\r\n" >> | Lines]
            end, [], HeaderDict1),

    HeadersData = iolist_to_binary([
                << Method/binary, " ", Path/binary, " HTTP/1.1", "\r\n" >>,
                HeadersLines,
                <<"\r\n">>]),

    %% send headers data
    case hackney_request:send(Client, HeadersData) of
        ok when Body =:= stream ->
            {ok, Client#client{response_state=stream, method=Method}};
        ok ->
            case stream_body(Body, Client) of
                {error, _Reason}=E ->
                    E;
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
    end.

stream_body(eof, Client) ->
    {ok, Client#client{response_state=waiting}};
stream_body(<<>>, Client) ->
    {ok, Client#client{response_state=waiting}};
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
    case sendfile(FileName, Client) of
        {ok, _BytesSent} ->
            {ok, Client};
        Error ->
            Error
    end.

send(#client{transport=Transport, socket=Skt}, Data) ->
    Transport:send(Skt, Data).

send_chunk(Client, Data) ->
    Length = iolist_size(Data),
    send(Client, [io_lib:format("~.16b\r\n", [Length]), Data,
                  <<"\r\n">>]).

sendfile(FileName, #client{transport=hackney_tcp_tansport, socket=Skt,
                           req_type=normal}) ->
    file:sendfile(FileName, Skt);
sendfile(FileName, Client) ->
    case file:open(FileName, [read, raw, binary]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Fd} ->
	    Res = sendfile_fallback(Fd, Client),
	    file:close(Fd),
	    Res
    end.


%% internal
handle_body(Headers, ReqType0, Body0, Client) ->
    {CLen, CType, Body} = case Body0 of
        {form, KVs} ->
            hackney_form:encode_form(KVs);
        {multipart, KVs} ->
            hackney_multipart:encode_form(KVs);
        {file, FileName} ->
            S= filelib:file_size(FileName),
            CT = hackney_util:content_type(FileName),
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
            S = erlang:length(Body0),
            CT = hackney_headers:get_value(<<"content-type">>, Headers,
                                           <<"application/octet-stream">>),
            {S, CT, Body0};
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
    CType = << "multipart/form-data; boundary=", Boundary/binary >>,
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

req_type(Headers) ->
    TE = hackney_headers:get_value(<<"Transfer-Encoding">>, Headers, <<>>),
    case hackney_util:to_lower(TE) of
        <<"chunked">> -> chunked;
        _ -> normal
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


sendfile_fallback(Fd, Client) ->
    {ok, CurrPos} = file:position(Fd, {cur, 0}),
    {ok, _NewPos} = file:position(Fd, {bof, 0}),
    Res = sendfile_fallback(Fd, Client, 0),
    file:position(Fd, {bof, CurrPos}),
    Res.

sendfile_fallback(Fd, #client{req_chunk_size=ChunkSize,
                              send_fun=Send}=Client, Old) ->
    case file:read(Fd, ChunkSize) of
        {ok, Data} ->
            Len = iolist_size(Data),
            case Send(Client, Data) of
                ok ->
                    sendfile_fallback(Fd, Client, Len+Old);
                Error ->
                    Error
            end;
        eof ->
            {ok, Old};
        Error ->
            Error
    end.


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
