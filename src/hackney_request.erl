-module(hackney_request).

-include("hackney.hrl").

-export([handle_body/2, stream_body/2, send/2, sendfile/2]).


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
stream_body({file, FileName}, Client) ->
    case sendfile(FileName, Client) of
        {ok, _BytesSent} ->
            ok;
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
