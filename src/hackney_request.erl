%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.


-module(hackney_request).

-include("hackney.hrl").

-export([perform/2, send/2, sendfile/2]).


perform(Client, {Method0, Path, Headers0, Body0}) ->
    Method = hackney_util:to_upper(hackney_util:to_binary(Method0)),

    #client{host=Host, port=Port, options=Options} = Client,

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
    %% build headers with the body.
    {HeaderDict1, Body} = case Body0 of
        <<>> when Method =:= <<"POST">> orelse Method =:= <<"PUT">> ->
            hackney_request:handle_body(HeadersDict, Body0);
        <<>> ->
            {HeadersDict, Body0};
        _ ->
            handle_body(HeadersDict, Body0)
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
        ok ->
            %% send body
            Result = stream_body(Body, Client),

            case Result of
                {error, _Reason}=E ->
                    E;
                _ ->
                    hackney_response:init(Client)
            end;
        Error ->
            Error
    end.


send(#client{transport=Transport, socket=Skt}, Data) ->
    Transport:send(Skt, Data).

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

handle_body(Headers, Body0) ->
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
    NewHeadersKV = [{<<"Content-Type">>, CType},
                    {<<"Content-Length">>, CLen}],
    NewHeaders = hackney_headers:update(Headers, NewHeadersKV),
    {NewHeaders, Body}.

stream_body(<<>>, _Client) ->
    ok;
stream_body(Body, #client{req_chunk_size=ChunkSize}=Client)
        when is_binary(Body) ->

    case Body of
        _ when byte_size(Body) >= ChunkSize ->
            << Data:ChunkSize/binary, Rest/binary >> = Body,
            case send(Client, Data) of
                ok ->
                    stream_body(Rest, Client);
                Error ->
                    Error
            end;
        _ ->
            send(Client, Body)
    end;
stream_body(Body, Client) when is_list(Body) ->
    send(Client, Body);
stream_body({file, FileName}, Client) ->
    case sendfile(FileName, Client) of
        {ok, _BytesSent} ->
            ok;
        Error ->
            Error
    end.

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
