%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%% Copyright (c) 2012 Ilya Khlopotov <ilya.khlopotov@gmail.com>
%%%

%% @doc module to encode/decode forms

-module(hackney_multipart).

-include("hackney.hrl").

-export([encode_form/1,
         decode_form/2, decode_form/3,
         boundary/0,
         parser/1,
         stream/2]).

-type part_parser() :: parser(more(part_result())).
-type parser(T) :: fun((binary()) -> T).
-type more(T) :: T | {more, parser(T)}.
-type part_result() :: headers() | eof.
-type headers() :: {headers, http_headers(), body_cont()}.
-type http_headers() :: [{binary(), binary()}].
-type body_cont() :: cont(more(body_result())).
-type cont(T) :: fun(() -> T).
-type body_result() :: {body, binary(), body_cont()} | end_of_part().
-type end_of_part() :: {end_of_part, cont(more(part_result()))}.

%% @doc encode a list of properties in a form.
encode_form(KVs) ->
    encode_form(KVs, boundary(), []).

%% @doc decode a multipart form.
-spec decode_form(binary(), binary()) -> {ok, list()} | {error, term()}.
decode_form(Boundary, Body) ->
    decode_form(Boundary, Body, []).

decode_form(Boundary, Body, Acc) ->
    Parser = parser(Boundary),
    decode_form1(Parser(Body), Acc).

%% @doc Return a multipart parser for the given boundary.
-spec parser(binary()) -> part_parser().
parser(Boundary) when is_binary(Boundary) ->
        fun (Bin) when is_binary(Bin) -> parse(Bin, Boundary) end.

boundary() ->
    Unique = unique(16),
    <<"---------------------------", Unique/binary>>.

stream(eof, #client{response_state=waiting}=Client) ->
    {ok, Client};
stream(eof, #client{mp_boundary=Boundary}=Client) ->
    Line = <<"--", Boundary/binary, "--", "\r\n\r\n">>,
    case hackney_request:stream_body(Line, Client) of
        {ok, Client1} ->
            hackney_request:end_stream_body(Client1);
        Error ->
            Error
    end;
stream({Id, {file, Name}}, Client) ->
    stream({Id, {file, Name, []}}, Client);
stream({Id, {file, Name, _Opts}=File}, #client{mp_boundary=Boundary}=Client) ->
    Field = field(Id),
    CType = hackney_util:content_type(Name),
    Bin = mp_header(Field, Name, CType, Boundary),
    case hackney_request:stream_body(Bin, Client) of
        {ok, Client1} ->
            case hackney_request:stream_body(File, Client1) of
                {ok, Client2} ->
                    hackney_request:stream_body(<<"\r\n">>, Client2);
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
stream({data, {start, Name, FileName, CType}},
                 #client{mp_boundary=Boundary}=Client) ->
    Bin = mp_header(Name, FileName, CType, Boundary),
    hackney_request:stream_body(Bin, Client);
stream({data, eof}, Client) ->
    hackney_request:stream_body(<<"\r\n">>, Client);
stream({data, Bin}, Client) ->
    hackney_request:stream_body(Bin, Client);
stream({Id, {file, Name, Content}},
                 #client{mp_boundary=Boundary}=Client) ->

    Bin = encode({Id, {file, Name, Content}}, Boundary),
    hackney_request:stream_body(Bin, Client);
stream({Id, Value}, #client{mp_boundary=Boundary}=Client) ->
    Bin = encode({Id, Value}, Boundary),
    hackney_request:stream_body(Bin, Client).


%% internal functions
%%
unique(Size) -> unique(Size, <<>>).
unique(Size, Acc) when size(Acc) == Size -> Acc;
unique(Size, Acc) ->
  Random = $a + random:uniform($z - $a),
  unique(Size, <<Acc/binary, Random>>).

encode_form([], Boundary, Acc) ->
    CType = <<"multipart/form-data; boundary=", Boundary/binary>>,
    Lines = <<Acc/binary, "--", Boundary/binary, "--", "\r\n">>,
    {erlang:size(Lines), CType, Lines};
encode_form([C | R], Boundary, Acc) ->
    Field = encode(C, Boundary),
    encode_form(R, Boundary, iolist_to_binary([Acc, Field])).

encode({Id, {file, Name, Content}}, Boundary) ->
    CType = hackney_util:content_type(Name),
    encode({Id, {file, Name, Content, CType}}, Boundary);
encode({Id, {file, Name, Content, CType}}, Boundary) ->
    Field = field(Id),
    Parts = [
        <<"--", Boundary/binary>>,
        <<"Content-Disposition: form-data; name=\"",
            Field/binary, "\"; filename=\"", Name/binary, "\"">>,
        <<"Content-Type: ", CType/binary >>,
        <<>>, Content, <<>>],
    hackney_util:join(Parts, <<"\r\n">>);
encode({Id, Value}, Boundary) ->
    Field = field(Id),
    Parts = [
        <<"--", Boundary/binary>>,
        <<"Content-Disposition: form-data; name=\"", Field/binary, "\"">>,
        <<"Content-Type: application/octet-stream">>,
        <<>>, Value, <<>>],
    hackney_util:join(Parts, <<"\r\n">>).

mp_header(Field, FileName, CType, Boundary) ->
    FileName1 = hackney_util:to_binary(FileName),
    Parts = [
            <<"--", Boundary/binary>>,
            <<"Content-Disposition: form-data; name=\"", Field/binary, "\"; filename=\"", FileName1/binary, "\"">>,
            <<"Content-Type: ", CType/binary >>, <<>>, <<>>],
    hackney_util:join(Parts, <<"\r\n">>).

field(V) when is_list(V) ->
    list_to_binary(V);
field(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
field(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
field(V) when is_binary(V) ->
    V.

decode_form1(eof, Acc) ->
    {ok, lists:reverse(Acc)};
decode_form1({headers, Headers, Fun}, [Last | Rest]) ->
    Last1 = [{headers, Headers} | Last],
    decode_form1(Fun(), [Last1| Rest]);
decode_form1({body, Bin, Fun}, [Last | Rest]) ->
    Last1 = case proplists:get_value(body, Last) of
        undefined ->
            [{body, Bin} | Last];
        Body ->
            lists:keyreplace(body, 1, Last,
                             {body, << Body/binary, Bin/binary >>})
    end,
    decode_form1(Fun(), [Last1| Rest]);
decode_form1({more, Fun}, Acc) ->
    {error, {more, Fun, Acc}}.

%% @doc Entry point of the multipart parser, skips over the preamble if any.
-spec parse(binary(), binary()) -> more(part_result()).
parse(Bin, Boundary) when byte_size(Bin) >= byte_size(Boundary) + 2 ->
    BoundarySize = byte_size(Boundary),
    Pattern = pattern(Boundary),
    case Bin of
        <<"--", Boundary:BoundarySize/binary, Rest/binary>> ->
            % Data starts with initial boundary, skip preamble parsing.
            parse_boundary_tail(Rest, Pattern);
        _ ->
            % Parse preamble.
            skip(Bin, Pattern)
    end;
parse(Bin, Boundary) ->
    % Not enough data to know if the data begins with a boundary.
    more(Bin, fun (NewBin) -> parse(NewBin, Boundary) end).

-type pattern() :: {binary:cp(), non_neg_integer()}.
-type patterns() :: {pattern(), pattern()}.

%% @doc Return two compiled binary patterns with their sizes in bytes.
%% The boundary pattern is the boundary prepended with "\r\n--".
%% The boundary suffix pattern matches all prefixes of the boundary.
-spec pattern(binary()) -> patterns().
pattern(Boundary) ->
    MatchPattern = <<"\r\n--", Boundary/binary>>,
    MatchPrefixes = prefixes(MatchPattern),
    {{binary:compile_pattern(MatchPattern), byte_size(MatchPattern)},
     {binary:compile_pattern(MatchPrefixes), byte_size(MatchPattern)}}.

%% @doc Return all prefixes of a binary string.
%% The list of prefixes includes the full string.
-spec prefixes(binary()) -> [binary()].
prefixes(<<C, Rest/binary>>) ->
    prefixes(Rest, <<C>>).

-spec prefixes(binary(), binary()) -> [binary()].
prefixes(<<C, Rest/binary>>, Acc) ->
    [Acc|prefixes(Rest, <<Acc/binary, C>>)];
prefixes(<<>>, Acc) ->
    [Acc].

%% @doc Test if a boundary is a possble suffix.
%% The patterns are expected to have been returned from `pattern/1'.
-spec suffix_match(binary(), patterns()) -> nomatch | {integer(), integer()}.
suffix_match(Bin, {_Boundary, {Pat, Len}}) ->
    Size = byte_size(Bin),
    suffix_match(Bin, Pat, Size, max(-Size, -Len)).

-spec suffix_match(binary(), binary:cp(), non_neg_integer(), 0|neg_integer()) ->
    nomatch | {integer(), integer()}.
suffix_match(_Bin, _Pat, _Size, _Match=0) ->
    nomatch;
suffix_match(Bin, Pat, Size, Match) when Match < 0 ->
    case binary:match(Bin, Pat, [{scope, {Size, Match}}]) of
        {Pos, Len}=Part when Pos + Len =:= Size -> Part;
        {_, Len} -> suffix_match(Bin, Pat, Size, Match + Len);
        nomatch -> nomatch
    end.

%% @doc Parse remaining characters of a line beginning with the boundary.
%% If followed by "--", <em>eof</em> is returned and parsing is finished.
-spec parse_boundary_tail(binary(), patterns()) -> more(part_result()).
parse_boundary_tail(Bin, Pattern) when byte_size(Bin) >= 2 ->
    case Bin of
        <<"--", _Rest/binary>> ->
            % Boundary is followed by "--", end parsing.
            eof;
        _ ->
            % No dash after boundary, proceed with unknown chars and lwsp
            % removal.
            parse_boundary_eol(Bin, Pattern)
    end;
parse_boundary_tail(Bin, Pattern) ->
    % Boundary may be followed by "--", need more data.
    more(Bin, fun (NewBin) -> parse_boundary_tail(NewBin, Pattern) end).

%% @doc Skip whitespace and unknown chars until CRLF.
-spec parse_boundary_eol(binary(), patterns()) -> more(part_result()).
parse_boundary_eol(Bin, Pattern) ->
    case binary:match(Bin, <<"\r\n">>) of
        {CrlfStart, _Length} ->
            % End of line found, remove optional whitespace.
            <<_:CrlfStart/binary, Rest/binary>> = Bin,
            Fun = fun (Rest2) -> parse_boundary_crlf(Rest2, Pattern) end,
            cowboy_http:whitespace(Rest, Fun);
        nomatch ->
            % CRLF not found in the given binary.
            RestStart = max(byte_size(Bin) - 1, 0),
            <<_:RestStart/binary, Rest/binary>> = Bin,
            more(Rest, fun (NewBin) -> parse_boundary_eol(NewBin, Pattern) end)
    end.

-spec parse_boundary_crlf(binary(), patterns()) -> more(part_result()).
parse_boundary_crlf(<<"\r\n", Rest/binary>>, Pattern) ->
    % The binary is at least 2 bytes long as this function is only called by
    % parse_boundary_eol/3 when CRLF has been found so a more tuple will never
    % be returned from here.
    parse_headers(Rest, Pattern);
parse_boundary_crlf(Bin, Pattern) ->
    % Unspecified behaviour here: RFC 2046 doesn't say what to do when LWSP is
    % not followed directly by a new line. In this implementation it is
    % considered part of the boundary so EOL needs to be searched again.
    parse_boundary_eol(Bin, Pattern).

-spec parse_headers(binary(), patterns()) -> more(part_result()).
parse_headers(Bin, Pattern) ->
    parse_headers(Bin, Pattern, []).

-spec parse_headers(binary(), patterns(), http_headers()) -> more(part_result()).
parse_headers(Bin, Pattern, Acc) ->
    case erlang:decode_packet(httph_bin, Bin, []) of
        {ok, {http_header, _, Name, _, Value}, Rest} ->
            Name2 = case is_atom(Name) of
                true -> atom_to_binary(Name, latin1);
                false -> Name
            end,
            parse_headers(Rest, Pattern, [{Name2, Value} | Acc]);
        {ok, http_eoh, Rest} ->
            Headers = lists:reverse(Acc),
            {headers, Headers, fun () -> parse_body(Rest, Pattern) end};
        {ok, {http_error, _}, _} ->
            % Skip malformed parts.
            skip(Bin, Pattern);
        {more, _} ->
            more(Bin, fun (NewBin) -> parse_headers(NewBin, Pattern, Acc) end)
    end.

-spec parse_body(binary(), patterns()) -> more(body_result()).
parse_body(Bin, Pattern = {{P, PSize}, _}) when byte_size(Bin) >= PSize ->
    case binary:match(Bin, P) of
        {0, _Length} ->
            <<_:PSize/binary, Rest/binary>> = Bin,
            end_of_part(Rest, Pattern);
        {BoundaryStart, _Length} ->
            % Boundary found, this is the latest partial body that will be
            % returned for this part.
            <<PBody:BoundaryStart/binary, _:PSize/binary, Rest/binary>> = Bin,
            FResult = end_of_part(Rest, Pattern),
            {body, PBody, fun () -> FResult end};
        nomatch ->
            case suffix_match(Bin, Pattern) of
                nomatch ->
                    %% Prefix of boundary not found at end of input. it's
                    %% safe to return the whole binary. Saves copying of
                    %% next input onto tail of current input binary.
                    {body, Bin, fun () -> parse_body(<<>>, Pattern) end};
                {BoundaryStart, Len} ->
                    PBody = binary:part(Bin, 0, BoundaryStart),
                    Rest = binary:part(Bin, BoundaryStart, Len),
                    {body, PBody, fun () -> parse_body(Rest, Pattern) end}
            end
    end;
parse_body(Bin, Pattern) ->
    more(Bin, fun (NewBin) -> parse_body(NewBin, Pattern) end).

-spec end_of_part(binary(), patterns()) -> end_of_part().
end_of_part(Bin, Pattern) ->
    {end_of_part, fun () -> parse_boundary_tail(Bin, Pattern) end}.

-spec skip(binary(), patterns()) -> more(part_result()).
skip(Bin, Pattern = {{P, PSize}, _}) ->
    case binary:match(Bin, P) of
        {BoundaryStart, _Length} ->
            % Boundary found, proceed with parsing of the next part.
            RestStart = BoundaryStart + PSize,
            <<_:RestStart/binary, Rest/binary>> = Bin,
            parse_boundary_tail(Rest, Pattern);
        nomatch ->
            % Boundary not found, need more data.
            RestStart = max(byte_size(Bin) - PSize + 1, 0),
            <<_:RestStart/binary, Rest/binary>> = Bin,
            more(Rest, fun (NewBin) -> skip(NewBin, Pattern) end)
    end.

-spec more(binary(), parser(T)) -> {more, parser(T)}.
more(<<>>, F) ->
    {more, F};
more(Bin, InnerF) ->
    F = fun (NewData) when is_binary(NewData) ->
            InnerF(<<Bin/binary, NewData/binary>>)
    end,
    {more, F}.
