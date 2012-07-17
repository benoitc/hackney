%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.


-module(hackney_request).

-include("hackney.hrl").

-export([perform/2,
         send/2, send_chunk/2,
         sendfile/2,
         stream_body/2]).


perform(Client0, {Method0, Path, Headers0, Body0}) ->
    Method = hackney_util:to_upper(hackney_util:to_binary(Method0)),

    #client{host=Host, port=Port, options=Options} = Client0,

    HostHdr = iolist_to_binary([Host, ":", integer_to_list(Port)]),

    %% make header dict
    DefaultHeaders0 = [{<<"Host">>, HostHdr},
                       {<<"User-Agent">>, default_ua()}],

    %% basic authorization handling
    DefaultHeaders = case proplists:get_value(basic_auth, Options) of
        undefined ->
            DefaultHeaders0;
        {User, Pwd} ->
            Credentials = base64:encode(<< User/binary, ":", Pwd/binary >>),
            DefaultHeaders0 ++ [{<<"Authorization">>, Credentials}]
    end,

    HeadersDict = hackney_headers:update(hackney_headers:new(DefaultHeaders),
                                         Headers0),

    ReqType0 = req_type(HeadersDict),

    %% build headers with the body.
    {HeaderDict1, ReqType, Body} = case Body0 of
        stream ->
            {HeadersDict, ReqType0, stream};
        <<>> when Method =:= <<"POST">> orelse Method =:= <<"PUT">> ->
            handle_body(HeadersDict, ReqType0, Body0);
        <<>> ->
            {HeadersDict, ReqType0, Body0};
        _ ->
            handle_body(HeadersDict, ReqType0, Body0)
    end,

    Client = case ReqType of
        normal ->
            Client0#client{send_fun=fun hackney_request:send/2,
                           req_type=normal};
        chunked ->
            Client0#client{send_fun=fun hackney_request:send_chunk/2,
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
            {ok, Client#client{response_state=stream}};
        ok ->
            case stream_body(Body, Client) of
                {error, _Reason}=E ->
                    E;
                {ok, Client1} ->
                    hackney_response:start_response(Client1)
            end;
        Error ->
            Error
    end.

stream_body(<<>>, Client) ->
    {ok, Client#client{response_state=waiting}};
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
                    end_stream_body(Client);
                Error ->
                    Error
            end
    end;
stream_body(Body, #client{send_fun=Send}=Client) when is_list(Body) ->
    case Send(Client, Body) of
        ok ->
            end_stream_body(Client);
        Error ->
            Error
    end;
stream_body({file, FileName}, Client) ->
    case sendfile(FileName, Client) of
        {ok, _BytesSent} ->
            {ok, Client#client{response_state=waiting}};
        Error ->
            Error
    end.

send(#client{transport=Transport, socket=Skt}, Data) ->
    Transport:send(Skt, Data).

send_chunk(Client, Data) ->
    Length = iolist_size(Data),
    send(Client, [io_lib:format("~.16b\r\n", [Length]), Data,
                  <<"\r\n">>]).


sendfile(FileName, #client{transport=hackney_tcp_tansport, socket=Skt}) ->
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
handle_body(Headers, ReqType0, Body0) ->
    {CLen, CType, Body} = case Body0 of
        {form, KVs} ->
            hackney_form:encode_form(KVs);
        {file, FileName} ->
            S= filelib:file_size(FileName),
            CT = case mimetypes:filename(FileName) of
                [CT0 | _] ->
                    CT0;
                CT0 when is_binary(CT0) ->
                    CT0
            end,
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
        {chunked, _} ->
            NewHeadersKV = [{<<"Content-Type">>, CType}],
            Headers1 = hackney_headers:delete(<<"content-length">>,
                                              Headers),
            {hackney_headers:update(Headers1, NewHeadersKV), chunked};

        {_, _} ->
            NewHeadersKV = [{<<"Content-Type">>, CType},
                            {<<"Content-Length">>, CLen}],
            {hackney_headers:update(Headers, NewHeadersKV), normal}
    end,
    {NewHeaders, ReqType, Body}.

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

sendfile_fallback(Fd, #client{req_chunk_size=ChunkSize}=Client, Old) ->
    case file:read(Fd, ChunkSize) of
        {ok, Data} ->
            Len = iolist_size(Data),
            case send(Client, Data) of
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
