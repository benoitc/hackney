%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012 Benoît Chesneau <benoitc@e-engura.org>
%%% Copyright (c) 2012 Ilya Khlopotov <ilya.khlopotov@gmail.com>
%%%

%% @doc module to encode/decode forms

-module(hackney_multipart).

-include("hackney.hrl").

-export([encode_form/1,
         decode_form/1,
         boundary/0,
         stream/2]).

%% @doc encode a list of properties in a form.
encode_form(KVs) ->
    encode_form(KVs, boundary(), []).

%% @private
encode_form([], Boundary, Acc) ->
    CType = <<"multipart/form-data; boundary=", Boundary/binary>>,
    Lines = <<Acc/binary, "--", Boundary/binary, "--", "\r\n">>,
    {erlang:size(Lines), CType, Lines};
encode_form([C | R], Boundary, Acc) ->
    Field = encode(C, Boundary),
    encode_form(R, Boundary, hackney_util:join([Acc, Field], "\r\n")).

decode_form(_) -> {error, not_implemented}.

boundary() ->
    Unique = unique(16),
    <<"---------------------------", Unique/binary>>.

unique(Size) -> unique(Size, <<>>).
unique(Size, Acc) when size(Acc) == Size -> Acc;
unique(Size, Acc) ->
  Random = $a + random:uniform($z - $a),
  unique(Size, <<Acc/binary, Random>>).

encode({Id, {file, Name, Content}}, Boundary) ->
    Field = field(Id),
    CType = hackney_util:content_type(Name),
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


stream(eof, #client{mp_boundary=Boundary}=Client) ->
    Line = <<"--", Boundary/binary, "--", "\r\n">>,
    case hackney_request:stream_body(Line, Client) of
        {ok, Client1} ->
            hackney_request:end_stream_body(Client1);
        Error ->
            Error
    end;
stream({Id, {file, Name}}, #client{mp_boundary=Boundary}=Client) ->
    Field = field(Id),
    CType = hackney_util:content_type(Name),
    Bin = mp_header(Field, Name, CType, Boundary),
    case hackney_request:stream_body(Bin, Client) of
        {ok, Client1} ->
            case hackney_request:sendfile(Name, Client1) of
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

mp_header(Field, FileName, CType, Boundary) ->
    Parts = [
            <<"--", Boundary/binary>>,
            <<"Content-Disposition: form-data; name=\"",
              Field/binary, "\"; filename=\"", FileName/binary, "\"">>,
            <<"Content-Type: ", CType/binary >>, <<>>],
    hackney_util:join(Parts, <<"\r\n">>).

field(V) when is_list(V) ->
    list_to_binary(V);
field(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
field(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
field(V) when is_binary(V) ->
    V.
