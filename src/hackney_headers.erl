%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2013 Benoît Chesneau <benoitc@e-engura.org>
%%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
%%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
-module(hackney_headers).

-export([new/0, new/1,
         update/2,
         to_list/1,
         get_value/2, get_value/3,
         store/3,
         insert/3,
         delete/2,
         fold/3]).

-export([parse/2]).
-export([content_type/1]).


%% @doc initialise an header dict
new() ->
    dict:new().

new({dict, _}=D) ->
    D;

new(Headers) when is_list(Headers) ->
    lists:foldl(fun({K, V}, D) ->
                insert(K, V, D)
        end, dict:new(), Headers).

update(Headers, KVs) ->
    lists:foldl(fun({K,_V}=KV, D) ->
                dict:store(hackney_util:to_lower(K), KV, D)
        end, Headers, KVs).

%% convert the header to a list
to_list(Headers) ->
    dict:fold(fun(_K, KV, Acc) ->
                    [KV | Acc]
            end, [], Headers).

%% @doc get the value of the header
get_value(Key, Headers) ->
    get_value(Key, Headers, undefined).

get_value(Key, Headers, Default) ->
    case dict:find(hackney_util:to_lower(Key), Headers) of
        {ok, {_K, V}} ->
            V;
        _ ->
            Default
    end.

%% @doc store the pair into the headers, replacing any pre-existing key.
store(Key, Value, Headers) ->
    dict:store(hackney_util:to_lower(Key), {Key, Value}, Headers).


%% @doc Insert the pair into the headers, merging with any pre-existing key.
%% A merge is done with Value = V0 ++ ", " ++ V1.
insert(Key, Value, Headers) ->
    Key1 = hackney_util:to_lower(Key),
    Value1 = case dict:find(Key1, Headers) of
        {ok, {_, OldValue}} ->
            << OldValue/binary, ", ", Value/binary >>;
        _ ->
            Value
    end,
    dict:store(Key1, {Key, Value1}, Headers).

%% @doc Delete the header corresponding to key if it is present.
delete(Key, Headers) ->
    dict:erase(hackney_util:to_lower(Key), Headers).

%% @doc fold the list of headers
fold(Fun, Acc0, Headers) ->
    Wrapper = fun(_K, KV, Acc) ->
            Fun(KV, Acc)
    end,
    dict:fold(Wrapper, Acc0, Headers).


%% @doc Semantically parse headers.
%%
%% When the value isn't found, a proper default value for the type
%% returned is used as a return value.
%% @see parse/3
-spec parse(binary(), list())
    -> any() | undefined | {error, badarg}.
parse(Name, Headers) when is_list(Headers) ->
    parse(Name, new(Headers));
parse(Name, Headers) ->
    parse(Name, Headers, header_default(Name)).


%% @doc Semantically parse headers.
%%
%% When the header is unknown, the value is returned directly without parsing.
-spec parse(binary(), list(), any())
    -> any() | undefined | {error, badarg}.
parse(Name = <<"accept">>, Headers, Default) ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_util:list(Value, fun media_range/2)
        end);
parse(Name = <<"accept-charset">>, Headers, Default) ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_util:nonempty_list(Value, fun conneg/2)
        end);
parse(Name = <<"accept-encoding">>, Headers, Default) ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_util:list(Value, fun conneg/2)
        end);
parse(Name = <<"accept-language">>, Headers, Default) ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_util:nonempty_list(Value, fun language_range/2)
        end);
parse(Name = <<"authorization">>, Headers, Default) ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_utils:token_ci(Value, fun authorization/2)
        end);
parse(Name = <<"content-length">>, Headers, Default) ->
    parse(Name, Headers, Default, fun hackney_util:digits/1);
parse(Name = <<"content-type">>, Headers, Default) ->
    parse(Name, Headers, Default, fun content_type/1);
parse(Name = <<"cookie">>, Headers, Default) ->
    parse(Name, Headers, Default, fun hackney_cookie:parse_cookie/1);
parse(Name = <<"expect">>, Headers, Default) ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_util:nonempty_list(Value, fun expectation/2)
        end);
parse(Name, Headers, Default)
        when Name =:= <<"if-match">>;
        Name =:= <<"if-none-match">> ->
    parse(Name, Headers, Default, fun entity_tag_match/1);
parse(Name, Headers, Default)
        when Name =:= <<"if-modified-since">>;
        Name =:= <<"if-unmodified-since">> ->
    parse(Name, Headers, Default, fun hackney_date:parse_http_date/1);
parse(Name = <<"range">>, Headers, Default) ->
    parse(Name, Headers, Default, fun range/1);
parse(Name, Headers, Default)
        when Name =:= <<"sec-websocket-protocol">>;
        Name =:= <<"x-forwarded-for">> ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_util:nonempty_list(Value, fun hackney_util:token/2)
        end);
%% @todo Extension parameters.
parse(Name, Headers, Default)
        when Name =:= <<"transfer-encoding">>;
        Name =:= <<"upgrade">> ->
    parse(Name, Headers, Default,
          fun (Value) ->
                hackney_util:nonempty_list(Value, fun hackney_util:token_ci/2)
        end);
parse(Name = <<"sec-websocket-extensions">>, Headers, Default) ->
    parse(Name, Headers, Default, fun hackney_util:parameterized_tokens/1);
parse(Name, Headers, Default) ->
    get_value(Name, Headers, Default).

parse(Name, Headers, Default, Fun) ->
    case get_value(Name, Headers) of
        undefined ->
            Default;
        Value ->
            case Fun(Value) of
                {error, badarg} ->
                    {error, badarg};
                P ->
                    P
            end
    end.


%% @doc Parse a content type.
%%
%% We lowercase the charset header as we know it's case insensitive.
-spec content_type(binary()) -> any().
content_type(Data) ->
    media_type(Data,
               fun (Rest, Type, SubType) ->
                hackney_util:params(Rest,
                       fun (<<>>, Params) ->
                            case lists:keyfind(<<"charset">>, 1, Params) of
                                false ->
                                    {Type, SubType, Params};
                                {_, Charset} ->
                                    Charset2 = hackney_util:to_lower(Charset),
                                    Params2 = lists:keyreplace(<<"charset">>,
                                                               1, Params,
                                                               {<<"charset">>, Charset2}),
                                    {Type, SubType, Params2}
                            end;
                        (_Rest2, _) ->
                            {error, badarg}
                    end)
        end).

%% @doc Parse a media range.
-spec media_range(binary(), fun()) -> any().
media_range(Data, Fun) ->
    media_type(Data,
               fun (Rest, Type, SubType) ->
                media_range_params(Rest, Fun, Type, SubType, [])
        end).

-spec media_range_params(binary(), fun(), binary(), binary(),
                    [{binary(), binary()}]) -> any().
media_range_params(Data, Fun, Type, SubType, Acc) ->
    hackney_util:whitespace(Data,
               fun (<< $;, Rest/binary >>) ->
                hackney_util:whitespace(Rest,
                           fun (Rest2) ->
                            media_range_param_attr(Rest2, Fun, Type, SubType,
                                                   Acc)
                    end);
            (Rest) -> Fun(Rest, {{Type, SubType, lists:reverse(Acc)}, 1000,
                                 []})
        end).

-spec media_range_param_attr(binary(), fun(), binary(), binary(),
                    [{binary(), binary()}]) -> any().
media_range_param_attr(Data, Fun, Type, SubType, Acc) ->
    hackney_util:token_ci(Data,
             fun (_Rest, <<>>) -> {error, badarg};
            (<< $=, Rest/binary >>, Attr) ->
                media_range_param_value(Rest, Fun, Type, SubType, Acc, Attr)
        end).

-spec media_range_param_value(binary(), fun(), binary(), binary(),
                    [{binary(), binary()}], binary()) -> any().
media_range_param_value(Data, Fun, Type, SubType, Acc, <<"q">>) ->
    qvalue(Data,
           fun (Rest, Quality) ->
                accept_ext(Rest, Fun, Type, SubType, Acc, Quality, [])
        end);
media_range_param_value(Data, Fun, Type, SubType, Acc, Attr) ->
    hackney_util:word(Data,
         fun (Rest, Value) ->
                media_range_params(Rest, Fun,
                                   Type, SubType, [{Attr, Value}|Acc])
        end).

%% @doc Parse a media type.
-spec media_type(binary(), fun()) -> any().
media_type(Data, Fun) ->
    hackney_util:token_ci(Data,
             fun (_Rest, <<>>) -> {error, badarg};
            (<< $/, Rest/binary >>, Type) ->
                hackney_util:token_ci(Rest,
                         fun (_Rest2, <<>>) -> {error, badarg};
                        (Rest2, SubType) -> Fun(Rest2, Type, SubType)
                    end);
            %% This is a non-strict parsing clause required by some user agents
            %% that use * instead of */* in the list of media types.
            (Rest, <<"*">> = Type) ->
                hackney_util:token_ci(<<"*", Rest/binary>>,
                         fun (_Rest2, <<>>) -> {error, badarg};
                        (Rest2, SubType) -> Fun(Rest2, Type, SubType)
                    end);
            (_Rest, _Type) -> {error, badarg}
        end).

-spec accept_ext(binary(), fun(), binary(), binary(),
                    [{binary(), binary()}], 0..1000,
                    [{binary(), binary()} | binary()]) -> any().
accept_ext(Data, Fun, Type, SubType, Params, Quality, Acc) ->
    hackney_util:whitespace(Data,
               fun (<< $;, Rest/binary >>) ->
                hackney_util:whitespace(Rest,
                           fun (Rest2) ->
                            accept_ext_attr(Rest2, Fun,
                                            Type, SubType, Params, Quality, Acc)
                    end);
            (Rest) ->
                Fun(Rest, {{Type, SubType, lists:reverse(Params)},
                           Quality, lists:reverse(Acc)})
        end).

-spec accept_ext_attr(binary(), fun(), binary(), binary(),
                    [{binary(), binary()}], 0..1000,
                    [{binary(), binary()} | binary()]) -> any().
accept_ext_attr(Data, Fun, Type, SubType, Params, Quality, Acc) ->
    hackney_util:token_ci(Data,
             fun (_Rest, <<>>) -> {error, badarg};
            (<< $=, Rest/binary >>, Attr) ->
                accept_ext_value(Rest, Fun, Type, SubType, Params,
                                 Quality, Acc, Attr);
            (Rest, Attr) ->
                accept_ext(Rest, Fun, Type, SubType, Params,
                           Quality, [Attr|Acc])
        end).

-spec accept_ext_value(binary(), fun(), binary(), binary(),
                    [{binary(), binary()}], 0..1000,
                    [{binary(), binary()} | binary()], binary()) -> any().
accept_ext_value(Data, Fun, Type, SubType, Params, Quality, Acc, Attr) ->
    hackney_util:word(Data,
         fun (Rest, Value) ->
                accept_ext(Rest, Fun,
                           Type, SubType, Params, Quality, [{Attr, Value}|Acc])
        end).

%% @doc Parse a conneg header (Accept-Charset, Accept-Encoding),
%% followed by an optional quality value.
-spec conneg(binary(), fun()) -> any().
conneg(Data, Fun) ->
    hackney_util:token_ci(Data,
             fun (_Rest, <<>>) -> {error, badarg};
            (Rest, Conneg) ->
                maybe_qparam(Rest,
                             fun (Rest2, Quality) ->
                            Fun(Rest2, {Conneg, Quality})
                    end)
        end).

%% @doc Parse a language range, followed by an optional quality value.
-spec language_range(binary(), fun()) -> any().
language_range(<< $*, Rest/binary >>, Fun) ->
    language_range_ret(Rest, Fun, '*');
language_range(Data, Fun) ->
    language_tag(Data,
                 fun (Rest, LanguageTag) ->
                language_range_ret(Rest, Fun, LanguageTag)
        end).

-spec language_range_ret(binary(), fun(), '*' | {binary(), [binary()]}) -> any().
language_range_ret(Data, Fun, LanguageTag) ->
    maybe_qparam(Data,
                 fun (Rest, Quality) ->
                Fun(Rest, {LanguageTag, Quality})
        end).

-spec language_tag(binary(), fun()) -> any().
language_tag(Data, Fun) ->
    hackney_util:alpha(Data,
          fun (_Rest, Tag) when byte_size(Tag) =:= 0; byte_size(Tag) > 8 ->
                {error, badarg};
            (<< $-, Rest/binary >>, Tag) ->
                language_subtag(Rest, Fun, Tag, []);
            (Rest, Tag) ->
                Fun(Rest, Tag)
        end).

-spec language_subtag(binary(), fun(), binary(), [binary()]) -> any().
language_subtag(Data, Fun, Tag, Acc) ->
    hackney_util:alpha(Data,
          fun (_Rest, SubTag) when byte_size(SubTag) =:= 0;
                    byte_size(SubTag) > 8 -> {error, badarg};
            (<< $-, Rest/binary >>, SubTag) ->
                language_subtag(Rest, Fun, Tag, [SubTag|Acc]);
            (Rest, SubTag) ->
                %% Rebuild the full tag now that we know it's correct
                Sub = << << $-, S/binary >> || S <- lists:reverse([SubTag|Acc]) >>,
                Fun(Rest, << Tag/binary, Sub/binary >>)
        end).

-spec maybe_qparam(binary(), fun()) -> any().
maybe_qparam(Data, Fun) ->
    hackney_util:whitespace(Data,
               fun (<< $;, Rest/binary >>) ->
                hackney_util:whitespace(Rest,
                           fun (Rest2) ->
                            %% This is a non-strict parsing clause required by some user agents
                            %% that use the wrong delimiter putting a charset where a qparam is
                            %% expected.
                            try qparam(Rest2, Fun) of
                                Result -> Result
                            catch
                                error:function_clause ->
                                    Fun(<<",", Rest2/binary>>, 1000)
                            end
                    end);
            (Rest) ->
                Fun(Rest, 1000)
        end).

%% @doc Parse a quality parameter string (for example q=0.500).
-spec qparam(binary(), fun()) -> any().
qparam(<< Q, $=, Data/binary >>, Fun) when Q =:= $q; Q =:= $Q ->
    qvalue(Data, Fun).

%% @doc Parse either a list of entity tags or a "*".
-spec entity_tag_match(binary()) -> any().
entity_tag_match(<< $*, Rest/binary >>) ->
    hackney_util:whitespace(Rest,
               fun (<<>>) -> '*';
            (_Any) -> {error, badarg}
        end);
entity_tag_match(Data) ->
    hackney_util:nonempty_list(Data, fun entity_tag/2).

%% @doc Parse an entity-tag.
-spec entity_tag(binary(), fun()) -> any().
entity_tag(<< "W/", Rest/binary >>, Fun) ->
    opaque_tag(Rest, Fun, weak);
entity_tag(Data, Fun) ->
    opaque_tag(Data, Fun, strong).

-spec opaque_tag(binary(), fun(), weak | strong) -> any().
opaque_tag(Data, Fun, Strength) ->
    hackney_util:quoted_string(Data,
                  fun (_Rest, <<>>) -> {error, badarg};
            (Rest, OpaqueTag) -> Fun(Rest, {Strength, OpaqueTag})
        end).

%% @doc Parse an expectation.
-spec expectation(binary(), fun()) -> any().
expectation(Data, Fun) ->
    hackney_util:token_ci(Data,
             fun (_Rest, <<>>) -> {error, badarg};
            (<< $=, Rest/binary >>, Expectation) ->
                hackney_util:word(Rest,
                     fun (Rest2, ExtValue) ->
                            hackney_util:params(Rest2, fun (Rest3, ExtParams) ->
                                        Fun(Rest3, {Expectation, ExtValue, ExtParams})
                                end)
                    end);
            (Rest, Expectation) ->
                Fun(Rest, Expectation)
        end).



%% @doc Parse a quality value.
-spec qvalue(binary(), fun()) -> any().
qvalue(<< $0, $., Rest/binary >>, Fun) ->
    qvalue(Rest, Fun, 0, 100);
%% Some user agents use q=.x instead of q=0.x
qvalue(<< $., Rest/binary >>, Fun) ->
    qvalue(Rest, Fun, 0, 100);
qvalue(<< $0, Rest/binary >>, Fun) ->
    Fun(Rest, 0);
qvalue(<< $1, $., $0, $0, $0, Rest/binary >>, Fun) ->
    Fun(Rest, 1000);
qvalue(<< $1, $., $0, $0, Rest/binary >>, Fun) ->
    Fun(Rest, 1000);
qvalue(<< $1, $., $0, Rest/binary >>, Fun) ->
    Fun(Rest, 1000);
qvalue(<< $1, Rest/binary >>, Fun) ->
    Fun(Rest, 1000);
qvalue(_Data, _Fun) ->
    {error, badarg}.

-spec qvalue(binary(), fun(), integer(), 1 | 10 | 100) -> any().
qvalue(Data, Fun, Q, 0) ->
    Fun(Data, Q);
qvalue(<< C, Rest/binary >>, Fun, Q, M)
        when C >= $0, C =< $9 ->
    qvalue(Rest, Fun, Q + (C - $0) * M, M div 10);
qvalue(Data, Fun, Q, _M) ->
    Fun(Data, Q).

%% @doc Parse authorization value according rfc 2617.
%% Only Basic authorization is supported so far.
-spec authorization(binary(), binary()) -> {binary(), any()} | {error, badarg}.
authorization(UserPass, Type = <<"basic">>) ->
    hackney_util:whitespace(UserPass,
               fun(D) ->
                authorization_basic_userid(base64:mime_decode(D),
                                           fun(Rest, Userid) ->
                            authorization_basic_password(Rest,
                                                         fun(Password) ->
                                        {Type, {Userid, Password}}
                                end)
                    end)
        end);
authorization(String, Type) ->
    hackney_util:whitespace(String, fun(Rest) -> {Type, Rest} end).

%% @doc Parse user credentials.
-spec authorization_basic_userid(binary(), fun()) -> any().
authorization_basic_userid(Data, Fun) ->
    authorization_basic_userid(Data, Fun, <<>>).

authorization_basic_userid(<<>>, _Fun, _Acc) ->
    {error, badarg};
authorization_basic_userid(<<C, _Rest/binary>>, _Fun, Acc)
        when C < 32; C =:= 127; (C =:=$: andalso Acc =:= <<>>) ->
    {error, badarg};
authorization_basic_userid(<<$:, Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
authorization_basic_userid(<<C, Rest/binary>>, Fun, Acc) ->
    authorization_basic_userid(Rest, Fun, <<Acc/binary, C>>).

-spec authorization_basic_password(binary(), fun()) -> any().
authorization_basic_password(Data, Fun) ->
    authorization_basic_password(Data, Fun, <<>>).

authorization_basic_password(<<>>, _Fun, <<>>) ->
    {error, badarg};
authorization_basic_password(<<C, _Rest/binary>>, _Fun, _Acc)
        when C < 32; C=:= 127 ->
    {error, badarg};
authorization_basic_password(<<>>, Fun, Acc) ->
    Fun(Acc);
authorization_basic_password(<<C, Rest/binary>>, Fun, Acc) ->
    authorization_basic_password(Rest, Fun, <<Acc/binary, C>>).

%% @doc Parse range header according rfc 2616.
-spec range(binary()) -> {Unit, [Range]} | {error, badarg} when
    Unit :: binary(),
    Range :: {non_neg_integer(), non_neg_integer() | infinity} | neg_integer().
range(Data) ->
    hackney_util:token_ci(Data, fun range/2).

range(Data, Token) ->
    hackney_util:whitespace(Data,
               fun(<<"=", Rest/binary>>) ->
                case hackney_util:list(Rest, fun range_beginning/2) of
                    {error, badarg} ->
                        {error, badarg};
                    Ranges ->
                        {Token, Ranges}
                end;
            (_) ->
                {error, badarg}
        end).

range_beginning(Data, Fun) ->
    range_digits(Data, suffix,
                 fun(D, RangeBeginning) ->
                range_ending(D, Fun, RangeBeginning)
        end).

range_ending(Data, Fun, RangeBeginning) ->
    hackney_util:whitespace(Data,
               fun(<<"-", R/binary>>) ->
                case RangeBeginning of
                    suffix ->
                        range_digits(R, fun(D, RangeEnding) ->
                                    Fun(D, -RangeEnding) end);
                    _ ->
                        range_digits(R, infinity, fun(D, RangeEnding) ->
                                    Fun(D, {RangeBeginning, RangeEnding})
                            end)
                end;
            (_) ->
                {error, badarg}
        end).

-spec range_digits(binary(), fun()) -> any().
range_digits(Data, Fun) ->
    hackney_util:whitespace(Data,
               fun(D) ->
                hackney_util:digits(D, Fun)
        end).

-spec range_digits(binary(), any(), fun()) -> any().
range_digits(Data, Default, Fun) ->
    hackney_util:whitespace(Data,
               fun(<< C, Rest/binary >>) when C >= $0, C =< $9 ->
                hackney_util:digits(Rest, Fun, C - $0);
            (_) ->
                Fun(Data, Default)
        end).

-spec header_default(binary()) -> any().
header_default(<<"transfer-encoding">>) ->
    [<<"identity">>];
header_default(_Name) ->
    undefined.
