%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%

-module(hackney_idna).

-export([to_ascii/1]).

%% @doc encode a IDNA domain to ascii
to_ascii(Domain) ->
    idna:utf8_to_ascii(Domain).
