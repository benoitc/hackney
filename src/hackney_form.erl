%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%%

%% @doc module to encode/decode forms

-module(hackney_form).

-export([encode_form/1,
         decode_form/1]).

%% @doc encode a list of properties in a form.
encode_form(KVs) ->
    Lines = hackney_url:qs(KVs),
    CType = <<"application/x-www-form-urlencoded; charset=utf-8">>,
    {erlang:byte_size(Lines), CType, Lines}.

decode_form(Bin) ->
    hackney_url:parse_qs(Bin).
