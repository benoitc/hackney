-module(hackney_response).

-include("hackney.hrl").

-export([init/1,
         stream_status/1,
         stream_headers/1, stream_header/1]).

%% @doc init response
init(Client) ->
     case stream_status(Client) of
        {ok, Status, Reason, Client1} ->
            case stream_headers(Client1) of
                {ok, Headers, Client2} ->
                    {ok, Status, Reason, Headers, Client2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


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
        [<< "\t", Line >>, Rest] ->
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
                    io:format("ici", []),
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
        _ ->
            Client
    end,
    {header, {Key, Value}, Client1}.


stream_body(Req=#client{body_state=waiting, te=TE, clen=Length}) ->
	case TE of
		<<"chunked">> ->
			stream_body(Req#client{body_state=
				{stream, fun te_chunked/2, {0, 0}, fun ce_identity/1}});
		_ when Length =:= 0 ->
            {done, Req#client{body_state=done}};
        _ >
		    stream_body(Req#client{body_state=
						{stream, fun te_identity/2, {0, Length},
						 fun ce_identity/1}})
	end;
stream_body(Req=#client{buffer=Buffer, body_state={stream, _, _, _}})
		when Buffer =/= <<>> ->
	transfer_decode(Buffer, Client#client{buffer= <<>>});
stream_body(Req=#client{body_state={stream, _, _, _}}) ->
	stream_body_recv(Req);
stream_body(Req=#client{body_state=done}) ->
	{done, Req}.

-spec stream_body_recv(#client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
stream_body_recv(Req=#client{
		transport=Transport, socket=Socket, buffer=Buffer}) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} -> transfer_decode(<< Buffer/binary, Data/binary >>, Req);
		{error, Reason} -> {error, Reason}
	end.

-spec transfer_decode(binary(), #client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
transfer_decode(Data, Req=#client{
		body_state={stream, TransferDecode, TransferState, ContentDecode}}) ->
	case TransferDecode(Data, TransferState) of
		{ok, Data2, TransferState2} ->
			content_decode(ContentDecode, Data2, Req#client{body_state=
				{stream, TransferDecode, TransferState2, ContentDecode}});
		{ok, Data2, Rest, TransferState2} ->
			content_decode(ContentDecode, Data2, Req#client{
				buffer=Rest, body_state=
				{stream, TransferDecode, TransferState2, ContentDecode}});
		%% @todo {header(s) for chunked
		more ->
			stream_body_recv(Req#client{buffer=Data});
		{done, Length, Rest} ->
			Req2 = transfer_decode_done(Length, Rest, Req),
			{done, Req2};
		{done, Data2, Length, Rest} ->
			Req2 = transfer_decode_done(Length, Rest, Req),
			content_decode(ContentDecode, Data2, Req2);
		{error, Reason} ->
			{error, Reason}
	end.

-spec transfer_decode_done(non_neg_integer(), binary(), #client{})
	-> #client{}.
transfer_decode_done(Length, Rest, Req=#client{
		headers=Headers, p_headers=PHeaders}) ->
	Headers2 = lists:keystore('Content-Length', 1, Headers,
		{'Content-Length', list_to_binary(integer_to_list(Length))}),
	%% At this point we just assume TEs were all decoded.
	Headers3 = lists:keydelete('Transfer-Encoding', 1, Headers2),
	PHeaders2 = lists:keystore('Content-Length', 1, PHeaders,
		{'Content-Length', Length}),
	PHeaders3 = lists:keydelete('Transfer-Encoding', 1, PHeaders2),
	Req#client{buffer=Rest, body_state=done,
		headers=Headers3, p_headers=PHeaders3}.

%% @todo Probably needs a Rest.
-spec content_decode(fun(), binary(), #client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
content_decode(ContentDecode, Data, Req) ->
	case ContentDecode(Data) of
		{ok, Data2} -> {ok, Data2, Req};
		{error, Reason} -> {error, Reason}
	end.

%% @doc Return the full body sent with the request.
-spec body(#client{}) -> {ok, binary(), #client{}} | {error, atom()}.
body(Req) ->
	read_body(infinity, Req, <<>>).

%% @doc Return the full body sent with the request as long as the body
%% length doesn't go over MaxLength.
%%
%% This is most useful to quickly be able to get the full body while
%% avoiding filling your memory with huge request bodies when you're
%% not expecting it.
-spec body(non_neg_integer() | infinity, #client{})
	-> {ok, binary(), #client{}} | {error, atom()}.
body(MaxLength, Req) ->
	read_body(MaxLength, Req, <<>>).

-spec read_body(non_neg_integer() | infinity, #client{}, binary())
	-> {ok, binary(), #client{}} | {error, atom()}.
read_body(MaxLength, Req, Acc) when MaxLength > byte_size(Acc) ->
	case stream_body(Req) of
		{ok, Data, Req2} ->
			read_body(MaxLength, Req2, << Acc/binary, Data/binary >>);
		{done, Req2} ->
			{ok, Acc, Req2};
		{error, Reason} ->
			{error, Reason}
	end.

-spec skip_body(#client{}) -> {ok, #client{}} | {error, atom()}.
skip_body(Req) ->
	case stream_body(Req) of
		{ok, _, Req2} -> skip_body(Req2);
		{done, Req2} -> {ok, Req2};
		{error, Reason} -> {error, Reason}
	end.


%% @doc Return data from the multipart parser.
%%
%% Use this function for multipart streaming. For each part in the request,
%% this function returns <em>{headers, Headers}</em> followed by a sequence of
%% <em>{body, Data}</em> tuples and finally <em>end_of_part</em>. When there
%% is no part to parse anymore, <em>eof</em> is returned.
%%
%% If the request Content-Type is not a multipart one, <em>{error, badarg}</em>
%% is returned.
-spec multipart_data(#client{})
		-> {{headers, cowboy_http:headers()}
				| {body, binary()} | end_of_part | eof,
			#client{}}.
multipart_data(Req=#client{body_state=waiting}) ->
	{{<<"multipart">>, _SubType, Params}, Req2} =
		parse_header('Content-Type', Req),
	{_, Boundary} = lists:keyfind(<<"boundary">>, 1, Params),
	{Length, Req3} = parse_header('Content-Length', Req2),
	multipart_data(Req3, Length, {more, cowboy_multipart:parser(Boundary)});
multipart_data(Req=#client{body_state={multipart, Length, Cont}}) ->
	multipart_data(Req, Length, Cont());
multipart_data(Req=#client{body_state=done}) ->
	{eof, Req}.

multipart_data(Req, Length, {headers, Headers, Cont}) ->
	{{headers, Headers}, Req#client{body_state={multipart, Length, Cont}}};
multipart_data(Req, Length, {body, Data, Cont}) ->
	{{body, Data}, Req#client{body_state={multipart, Length, Cont}}};
multipart_data(Req, Length, {end_of_part, Cont}) ->
	{end_of_part, Req#client{body_state={multipart, Length, Cont}}};
multipart_data(Req, 0, eof) ->
	{eof, Req#client{body_state=done}};
multipart_data(Req=#client{socket=Socket, transport=Transport},
		Length, eof) ->
	%% We just want to skip so no need to stream data here.
	{ok, _Data} = Transport:recv(Socket, Length, 5000),
	{eof, Req#client{body_state=done}};
multipart_data(Req, Length, {more, Parser}) when Length > 0 ->
	case stream_body(Req) of
		{ok, << Data:Length/binary, Buffer/binary >>, Req2} ->
			multipart_data(Req2#client{buffer=Buffer}, 0, Parser(Data));
		{ok, Data, Req2} ->
			multipart_data(Req2, Length - byte_size(Data), Parser(Data))
	end.

%% @doc Skip a part returned by the multipart parser.
%%
%% This function repeatedly calls <em>multipart_data/1</em> until
%% <em>end_of_part</em> or <em>eof</em> is parsed.
multipart_skip(Req) ->
	case multipart_data(Req) of
		{end_of_part, Req2} -> {ok, Req2};
		{eof, Req2} -> {ok, Req2};
		{_Other, Req2} -> multipart_skip(Req2)
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
	token(Data,
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
