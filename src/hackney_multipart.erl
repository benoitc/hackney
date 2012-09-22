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

-export([encode_form/1,
         decode_form/1]).

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
    Field = atom_to_binary(Id, utf8),
    CType = hackney_util:content_type(Name),
    Parts = [
        <<"--", Boundary/binary>>,
        <<"Content-Disposition: form-data; name=\"",
            Field/binary, "\"; filename=\"", Name/binary, "\"">>,
        <<"Content-Type: ", CType/binary >>,
        <<>>, Content, <<>>],
    hackney_util:join(Parts, "\r\n");
encode({Id, Value}, Boundary) ->
    Field = atom_to_binary(Id, utf8),
    Parts = [
        <<"--", Boundary/binary>>,
        <<"Content-Disposition: form-data; name=\"", Field/binary, "\"">>,
        <<"Content-Type: application/octet-stream">>,
        <<>>, Value, <<>>],
    hackney_util:join(Parts, "\r\n").
