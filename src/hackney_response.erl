%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
%%% Copyright (c) 2012 Benoît Chesneau <benoitc@e-engura.org>
%%%
-module(hackney_response).

-include("hackney.hrl").

-export([start_response/1,
         stream_status/1,
         stream_headers/1, stream_header/1,
         stream_body/1,
         body/1, body/2, skip_body/1,
         close/1]).

%% @doc Start the response It parse the request lines and headers.
start_response(#client{response_state=waiting} = Client) ->
     case stream_status(Client) of
        {ok, Status, _Reason, Client1} ->
            case stream_headers(Client1) of
                {ok, Headers, Client2} ->
                    {ok, Status, Headers, Client2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
start_response(_) ->
    {error, invalide_state}.

%% @doc parse the status line
stream_status(#client{buffer=Buf}=Client) ->
    case binary:split(Buf, <<"\r\n">>) of
        [Line, Rest] ->
            parse_status(Line, Client#client{buffer=Rest});
        _ ->
             case recv(Client) of
                {ok, Data} ->
                    NewBuf = << Buf/binary, Data/binary >>,
                    stream_status(Client#client{buffer=NewBuf});
                Error  ->
                    Error
            end
    end.

parse_status(<< "HTTP/", High, ".", Low, " ", Status/binary >>, Client)
        when High >= $0, High =< $9, Low >= $0, Low =< $9 ->

    Version = { High -$0, Low - $0},
    [StatusCode, Reason] = binary:split(Status, <<" ">>, [trim]),
    StatusInt = list_to_integer(binary_to_list(StatusCode)),
    {ok, StatusInt, Reason, Client#client{version=Version,
                                          response_state=on_header}}.

%% @doc fetch all headers
stream_headers(Client) ->
    stream_headers(Client, []).

stream_headers(Client, Headers) ->
    case stream_header(Client) of
        {headers_complete, Client1} ->
            {ok, lists:reverse(Headers), Client1};
        {header, KV, Client1} ->
            stream_headers(Client1, [KV | Headers]);
        {error, Reason, Acc} ->
            {error, {Reason, Acc}, Headers, Client}
    end.


stream_header(Client) ->
    stream_header(Client, []).

stream_header(#client{buffer=Buf}=Client, Acc) ->
    case binary:split(Buf, <<"\r\n">>) of
        [<<>>, Rest] ->
            {headers_complete, Client#client{buffer=Rest,
                                             response_state=on_body}};
        [<< " ", Line/binary >>, Rest] ->
            stream_header(Client#client{buffer=Rest}, [ Line | Acc ]);
        [<< "\t", Line/binary >>, Rest] ->
            stream_header(Client#client{buffer=Rest}, [ Line | Acc ]);
        [_Line, _Rest] when Acc /= []->
            parse_header(iolist_to_binary(lists:reverse(Acc)), Client);
        [Line, Rest] ->
            stream_header(Client#client{buffer=Rest}, [Line]);
        [Buf] ->
            case recv(Client) of
                {ok, Data} ->
                    NewBuf = << Buf/binary, Data/binary >>,
                    stream_header(Client#client{buffer=NewBuf}, Acc);
                {error, Reason} ->
                    {error, Reason, Acc}
            end
    end.


parse_header(Line, Client) ->
    [Key, Value] = binary:split(Line, <<": ">>, [trim]),
    Client1 = case hackney_util:to_lower(Key) of
        <<"content-length">> ->
            CLen = list_to_integer(binary_to_list(Value)),
            Client#client{clen=CLen};
        <<"transfer-encoding">> ->
            Client#client{te=hackney_util:to_lower(Value)};
        <<"connection">> ->
            Client#client{connection=hackney_util:to_lower(Value)};
        <<"content-type">> ->
            Client#client{ctype=hackney_util:to_lower(Value)};
        _ ->
            Client
    end,
    {header, {Key, Value}, Client1}.


stream_body(Client=#client{body_state=waiting, te=TE, clen=Length}) ->
	case TE of
		<<"chunked">> ->
			stream_body(Client#client{body_state=
				{stream, fun te_chunked/2, {0, 0}, fun ce_identity/1}});
		_ when Length =:= 0 ->
            {done, Client#client{body_state=done}};
        _ ->
		    stream_body(Client#client{body_state=
						{stream, fun te_identity/2, {0, Length},
						 fun ce_identity/1}})
	end;
stream_body(Client=#client{buffer=Buffer, body_state={stream, _, _, _}})
		when Buffer =/= <<>> ->
	transfer_decode(Buffer, Client#client{buffer= <<>>});
stream_body(Client=#client{body_state={stream, _, _, _}}) ->
	stream_body_recv(Client);
stream_body(Client=#client{body_state=done}) ->
	{done, Client}.

-spec stream_body_recv(#client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
stream_body_recv(Client=#client{
		transport=Transport, socket=Socket, buffer=Buffer}) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} -> transfer_decode(<< Buffer/binary, Data/binary >>, Client);
		{error, Reason} -> {error, Reason}
	end.


%% @doc Return the full body sent with the request.
-spec body(#client{}) -> {ok, binary(), #client{}} | {error, atom()}.
body(Client) ->
	read_body(infinity, Client, <<>>).

%% @doc Return the full body sent with the request as long as the body
%% length doesn't go over MaxLength.
%%
%% This is most useful to quickly be able to get the full body while
%% avoiding filling your memory with huge request bodies when you're
%% not expecting it.
-spec body(non_neg_integer() | infinity, #client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
body(MaxLength, Client) ->
	read_body(MaxLength, Client, <<>>).

-spec skip_body(#client{}) -> {ok, #client{}} | {error, atom()}.
skip_body(Client) ->
	case stream_body(Client) of
		{ok, _, Client2} -> skip_body(Client2);
		{done, Client2} -> {ok, Client2};
		{error, Reason} -> {error, Reason}
	end.


-spec transfer_decode(binary(), #client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
transfer_decode(Data, Client=#client{
		body_state={stream, TransferDecode, TransferState, ContentDecode}}) ->
	case TransferDecode(Data, TransferState) of
		{ok, Data2, TransferState2} ->
			content_decode(ContentDecode, Data2, Client#client{body_state=
				{stream, TransferDecode, TransferState2, ContentDecode}});
		{ok, Data2, Rest, TransferState2} ->
			content_decode(ContentDecode, Data2, Client#client{
				buffer=Rest, body_state=
				{stream, TransferDecode, TransferState2, ContentDecode}});
		%% @todo {header(s) for chunked
		more ->
			stream_body_recv(Client#client{buffer=Data});
		{done, Length, Rest} ->
            Client2 = transfer_decode_done(Length, Rest, Client),
			{done, Client2};
		{done, Data2, Length, Rest} ->
            Client2 = transfer_decode_done(Length, Rest, Client),
			content_decode(ContentDecode, Data2, Client2);
		{error, Reason} ->
			{error, Reason}
	end.


transfer_decode_done(_Length, Rest, Client0) ->
    Client = Client0#client{response_state=done,
                            body_state=done,
                            buffer=Rest},

    Pool = hackney:pool(Client),
    case maybe_close(Client) of
        true ->
            close(Client);
        false when Pool /= undefined ->
            #client{host=Host, port=Port, transport=Transport,
                    socket=Socket}=Client,
            hackney_pool:release(Pool, {Transport, Host, Port}, Socket),
            Client#client{state=closed, socket=nil};
        false ->
            Client
    end.

%% @todo Probably needs a Rest.
-spec content_decode(fun(), binary(), #client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
content_decode(ContentDecode, Data, Client) ->
	case ContentDecode(Data) of
		{ok, Data2} -> {ok, Data2, Client};
		{error, Reason} -> {error, Reason}
	end.




-spec read_body(non_neg_integer() | infinity, #client{}, binary())
	-> {ok, binary(), #client{}} | {error, atom()}.
read_body(MaxLength, Client, Acc) when MaxLength > byte_size(Acc) ->
	case stream_body(Client) of
		{ok, Data, Client2} ->
			read_body(MaxLength, Client2, << Acc/binary, Data/binary >>);
		{done, Client2} ->
			{ok, Acc, Client2};
		{error, Reason} ->
			{error, Reason}
	end.


maybe_close(#client{version={Min,Maj}, connection=Connection}) ->
    case Connection of
        <<"close">> -> true;
        <<"keepalive">> -> false;
        _ when Min =< 0 orelse Maj < 1 -> true;
        _ -> false
    end.


%% @doc Decode a stream of chunks.
-spec te_chunked(binary(), {non_neg_integer(), non_neg_integer()})
	-> more | {ok, binary(), {non_neg_integer(), non_neg_integer()}}
	| {ok, binary(), binary(),  {non_neg_integer(), non_neg_integer()}}
	| {done, non_neg_integer(), binary()} | {error, badarg}.
te_chunked(<<>>, _) ->
	more;
te_chunked(<< "0\r\n\r\n", Rest/binary >>, {0, Streamed}) ->
	{done, Streamed, Rest};
te_chunked(Data, {0, Streamed}) ->
	%% @todo We are expecting an hex size, not a general token.
	hackney_util:token(Data,
		fun (Rest, _) when byte_size(Rest) < 4 ->
				more;
			(<< "\r\n", Rest/binary >>, BinLen) ->
				Len = list_to_integer(binary_to_list(BinLen), 16),
				te_chunked(Rest, {Len, Streamed});
			(_, _) ->
				{error, badarg}
		end);
te_chunked(Data, {ChunkRem, Streamed}) when byte_size(Data) >= ChunkRem + 2 ->
	<< Chunk:ChunkRem/binary, "\r\n", Rest/binary >> = Data,
	{ok, Chunk, Rest, {0, Streamed + byte_size(Chunk)}};
te_chunked(Data, {ChunkRem, Streamed}) ->
	Size = byte_size(Data),
	{ok, Data, {ChunkRem - Size, Streamed + Size}}.

%% @doc Decode an identity stream.
-spec te_identity(binary(), {non_neg_integer(), non_neg_integer()})
	-> {ok, binary(), {non_neg_integer(), non_neg_integer()}}
	| {done, binary(), non_neg_integer(), binary()}.
te_identity(Data, {Streamed, Total})
		when Streamed + byte_size(Data) < Total ->
	{ok, Data, {Streamed + byte_size(Data), Total}};
te_identity(Data, {Streamed, Total}) ->
	Size = Total - Streamed,
	<< Data2:Size/binary, Rest/binary >> = Data,
	{done, Data2, Total, Rest}.

%% @doc Decode an identity content.
-spec ce_identity(binary()) -> {ok, binary()}.
ce_identity(Data) ->
	{ok, Data}.

recv(#client{transport=Transport, socket=Skt}) ->
    Transport:recv(Skt, 0).


close(#client{socket=nil}=Client) ->
    Client#client{state = closed};
close(#client{transport=Transport, socket=Skt}=Client) ->
    Transport:close(Skt),
    Client#client{state = closed, socket=nil}.
