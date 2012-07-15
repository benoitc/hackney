%% @doc module to encode/decode forms

-module(hackney_form).

-export([encode_form/1,
         decode_form/1]).

%% @doc encode a list of properties in a form.
encode_form(KVs) ->
    encode_form(KVs, []).


%% @private
encode_form([], Acc) ->
    Lines = hackney_util:join(lists:reverse(Acc), <<"&">>),
    CType = <<"application/x-www-form-urlencoded; charset=utf-8">>,
    {erlang:length(Lines), CType, Lines};
encode_form([{K,V}|R], Acc) ->
    K1 = hackney_url:urlencode(K),
    V1 = hackney_url:urlencode(V),
    Line = << K1/binary, "=", V1/binary >>,
    encode_form(R, [Line | Acc]).

decode_form(<<>>) ->
    [];
decode_form(Bin) ->
    Tokens = binary:split(Bin, <<"&">>, [trim, global]),
    [case binary:split(Token, <<"=">>, [trim]) of
            [T] ->
                {hackney_url:urldecode(T), true};
            [Name, Value] ->
                {hackney_url:urldecode(Name), hackney_url:urldecode(Value)}
        end || Token <- Tokens].
