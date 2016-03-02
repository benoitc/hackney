%% -*- erlang -*-
%%%
%%% This file is part of hackney_lib released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2015 Benoît Chesneau <benoitc@e-engura.org>
%%% Copyright (c) 2011, Magnus Klaar <magnus.klaar@gmail.com>
%%%

%% @doc module to manage urls.

-module(hackney_url).

-export([parse_url/1,
         transport_scheme/1,
         unparse_url/1,
         urldecode/1, urldecode/2,
         urlencode/1, urlencode/2,
         parse_qs/1,
         qs/1,
         make_url/3,
         fix_path/1,
         pathencode/1,
         normalize/1]).

-include("hackney_lib.hrl").

-type qs_vals() :: [{binary(), binary() | true}].

%% @doc Parse an url and return a #hackney_url record.
-spec parse_url(URL::binary()|list()) -> hackney_url().
parse_url(URL) when is_list(URL) ->
    case unicode:characters_to_binary(URL) of
        URL1 when is_binary(URL1) ->
            parse_url(URL1);
        _ ->
            parse_url(unicode:characters_to_binary(list_to_binary(URL)))
    end;
parse_url(<<"http://", Rest/binary>>) ->
    parse_url(Rest, #hackney_url{transport=hackney_tcp_transport,
                                         scheme=http});
parse_url(<<"https://", Rest/binary>>) ->
    parse_url(Rest, #hackney_url{transport=hackney_ssl_transport,
                                 scheme=https});
parse_url(URL) ->
    parse_url(URL, #hackney_url{transport=hackney_tcp_transport,
                                        scheme=http}).
parse_url(URL, S) ->
    case binary:split(URL, <<"/">>) of
        [Addr] ->
            Path = <<"/">>,
            parse_addr(Addr, S#hackney_url{raw_path = Path,
                                           path = Path });
        [Addr, Path] ->
            RawPath =  <<"/", Path/binary>>,
            {Path1, Query, Fragment} = parse_path(RawPath),
            parse_addr(Addr, S#hackney_url{raw_path = RawPath,
                                           path = Path1,
                                           qs = Query,
                                           fragment = Fragment})
    end.

%% @doc Normalizes the encoding of a Url
normalize(Url) when is_list(Url) orelse is_binary(Url) ->
    normalize(parse_url(Url));
normalize(#hackney_url{}=Url) ->
    #hackney_url{scheme=Scheme,
                 host = Host0,
                 port = Port,
                 netloc = Netloc0,
                 path = Path} = Url,

    {Host, Netloc} = case inet_parse:address(Host0) of
        {ok, {_, _, _, _}} ->
            {Host0, Netloc0};
        {ok, {_, _, _, _, _, _, _, _}} ->
            {Host0, Netloc0};
        _ ->
            Host1 = unicode:characters_to_list(
                    urldecode(unicode:characters_to_binary(Host0))),

            %% encode domain if needed
            Host2 = idna:to_ascii(Host1),
            Netloc1 = case {Scheme, Port} of
                {http, 80} -> list_to_binary(Host2);
                {https, 443} -> list_to_binary(Host2);
                _ ->
                    iolist_to_binary([Host2, ":", integer_to_list(Port)])
            end,
            {Host2, Netloc1}
    end,
    Path1 = pathencode(Path),
    Url#hackney_url{host=Host, netloc=Netloc, path=Path1}.

transport_scheme(hackney_tcp_transport) ->
    http;
transport_scheme(hackney_ssl_transport) ->
    https.

unparse_url(#hackney_url{}=Url) ->
    #hackney_url{scheme = Scheme,
                 netloc = Netloc,
                 path = Path,
                 qs = Qs,
                 fragment = Fragment,
                 user = User,
                 password = Password} = Url,

    Scheme1 = case Scheme of
        http -> <<"http://">>;
        https -> <<"https://">>
    end,

    Netloc1 = case User of
        <<>> ->
            Netloc;
        _ when Password /= <<>>, Password /= <<"">> ->
            << User/binary, ":", Password/binary, "@", Netloc/binary >>;
        _ ->
            << User/binary, "@", Netloc/binary >>
    end,

    Qs1 = case Qs of
        <<>> -> <<>>;
        _ -> << "?", Qs/binary >>
    end,

    Fragment1 = case Fragment of
        <<>> -> <<>>;
        _ -> << "#", Fragment/binary >>
    end,

    Path1 = case Path of
                nil -> <<>>;
                undefined -> <<>>;
                <<>> -> <<"/">>;
                _ -> Path
            end,

    << Scheme1/binary, Netloc1/binary, Path1/binary, Qs1/binary, Fragment1/binary >>.

%% @private
parse_addr(Addr, S) ->
    case binary:split(Addr, <<"@">>) of
        [Addr] ->
            parse_netloc(Addr, S#hackney_url{netloc=Addr});
        [Credentials, Addr1] ->
            case binary:split(Credentials, <<":">>) of
                [User, Password] ->
                    parse_netloc(Addr1, S#hackney_url{netloc=Addr1,
                                                      user = User,
                                                      password = Password});
                [User] ->
                    parse_netloc(Addr1, S#hackney_url{netloc = Addr1,
                                                      user = User,
                                                      password = <<>> })
            end

    end.

parse_netloc(<<"[", Rest/binary>>, #hackney_url{transport=Transport}=S) ->
    case binary:split(Rest, <<"]">>) of
        [Host, <<>>] when Transport =:= hackney_tcp_transport ->
            S#hackney_url{host=binary_to_list(Host), port=80};
        [Host, <<>>] when Transport =:= hackney_ssl_transport ->
            S#hackney_url{host=binary_to_list(Host), port=443};
        [Host, <<":", Port/binary>>] ->
            S#hackney_url{host=binary_to_list(Host),
                          port=list_to_integer(binary_to_list(Port))};
        _ ->
            parse_netloc(Rest, S)
    end;

parse_netloc(Netloc, #hackney_url{transport=Transport}=S) ->
    case binary:split(Netloc, <<":">>) of
        [Host] when Transport =:= hackney_tcp_transport ->
            S#hackney_url{host=unicode:characters_to_list((Host)),
                          port=80};
        [Host] when Transport =:= hackney_ssl_transport ->
            S#hackney_url{host=unicode:characters_to_list(Host),
                          port=443};
        [Host, Port] ->
            S#hackney_url{host=unicode:characters_to_list(Host),
                          port=list_to_integer(binary_to_list(Port))}
    end.


parse_path(Path) ->
    case binary:split(Path, <<"?">>) of
        [_Path] ->
            {Path1, Fragment} = parse_fragment(Path),
            {Path1, <<>>, Fragment};
        [Path1, Query] ->
            {Query1, Fragment} = parse_fragment(Query),
            {Path1, Query1, Fragment}
    end.

parse_fragment(S) ->
    case binary:split(S, <<"#">>) of
        [_S] ->
            {S, <<>>};
        [S1, F] ->
            {S1, F}
    end.


%% @doc Decode a URL encoded binary.
%% @equiv urldecode(Bin, crash)
-spec urldecode(binary()) -> binary().
urldecode(Bin) when is_binary(Bin) ->
	urldecode(Bin, <<>>, crash).

%% @doc Decode a URL encoded binary.
%% The second argument specifies how to handle percent characters that are not
%% followed by two valid hex characters. Use `skip' to ignore such errors,
%% if `crash' is used the function will fail with the reason `badarg'.
-spec urldecode(binary(), crash | skip) -> binary().
urldecode(Bin, OnError) when is_binary(Bin) ->
	urldecode(Bin, <<>>, OnError).

-spec urldecode(binary(), binary(), crash | skip) -> binary().
urldecode(<<$%, H, L, Rest/binary>>, Acc, OnError) ->
	G = unhex(H),
	M = unhex(L),
	if	G =:= error; M =:= error ->
		case OnError of skip -> ok; crash -> erlang:error(badarg) end,
		urldecode(<<H, L, Rest/binary>>, <<Acc/binary, $%>>, OnError);
		true ->
		urldecode(Rest, <<Acc/binary, (G bsl 4 bor M)>>, OnError)
	end;
urldecode(<<$%, Rest/binary>>, Acc, OnError) ->
	case OnError of skip -> ok; crash -> erlang:error(badarg) end,
	urldecode(Rest, <<Acc/binary, $%>>, OnError);
urldecode(<<$+, Rest/binary>>, Acc, OnError) ->
	urldecode(Rest, <<Acc/binary, $ >>, OnError);
urldecode(<<C, Rest/binary>>, Acc, OnError) ->
	urldecode(Rest, <<Acc/binary, C>>, OnError);
urldecode(<<>>, Acc, _OnError) ->
	Acc.

-spec unhex(byte()) -> byte() | error.
unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(_) -> error.

%% @doc URL encode a string binary.
-spec urlencode(binary() | string()) -> binary().
urlencode(Bin) ->
	urlencode(Bin, []).

%% @doc URL encode a string binary.
%% The `noplus' option disables the default behaviour of quoting space
%% characters, `\s', as `+'. The `upper' option overrides the default behaviour
%% of writing hex numbers using lowecase letters to using uppercase letters
%% instead.
-spec urlencode(binary() | string(), [noplus|upper]) -> binary().
urlencode(Bin, Opts) ->
	Plus = not proplists:get_value(noplus, Opts, false),
	Upper = proplists:get_value(upper, Opts, false),
	urlencode(hackney_bstr:to_binary(Bin), <<>>, Plus, Upper).

-spec urlencode(binary(), binary(), boolean(), boolean()) -> binary().
urlencode(<<C, Rest/binary>>, Acc, P=Plus, U=Upper) ->
	if	C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
		urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $ , Plus ->
		urlencode(Rest, <<Acc/binary, $+>>, P, U);
		true ->
		H = C band 16#F0 bsr 4, L = C band 16#0F,
		H1 = if Upper -> tohexu(H); true -> tohexl(H) end,
		L1 = if Upper -> tohexu(L); true -> tohexl(L) end,
		urlencode(Rest, <<Acc/binary, $%, H1, L1>>, P, U)
	end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
	Acc.

-spec tohexu(byte()) -> byte().
tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 16 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 16 -> $a + C - 10.


%% parse a query or a form from a binary and return a list of properties
-spec parse_qs(binary()) -> qs_vals().
parse_qs(<<>>) ->
    [];
parse_qs(Bin) ->
    Tokens = binary:split(Bin, <<"&">>, [trim, global]),
    [case binary:split(Token, <<"=">>, [trim]) of
            [T] ->
                {urldecode(T), true};
            [Name, Value] ->
                {urldecode(Name), urldecode(Value)}
        end || Token <- Tokens].


%% @doc encode query properties to binary
-spec qs(qs_vals()) -> binary().
qs(KVs) ->
   qs(KVs, []).

qs([], Acc) ->
    hackney_bstr:join(lists:reverse(Acc), <<"&">>);
qs([{K, V}|R], Acc) ->
    K1 = urlencode(K),
    V1 = urlencode(V),
    Line = << K1/binary, "=", V1/binary >>,
    qs(R, [Line | Acc]).

%% @doc  construct an url from a base url, a path and a list of
%% properties to give to the url.
-spec make_url(binary(), binary() | [binary()], binary() | qs_vals())
    -> binary().
make_url(Url, Path, Query) when is_list(Query) ->
    %% a list of properties has been passed
    make_url(Url, Path, qs(Query));
make_url(Url, Path, Query) when is_binary(Path) ->
    make_url(Url, [Path], Query);
make_url(Url, PathParts, Query) when is_binary(Query) ->
    %% create path
    PathParts1 = [fix_path(P) || P <- PathParts, P /= "", P /= "/" orelse P /= <<"/">>],
    Path = hackney_bstr:join([<<>> | PathParts1], <<"/">>),

    %% initialise the query
    Query1 = case Query of
        <<>> -> <<>>;
        _ -> << "?", Query/binary >>
    end,

    %% make the final uri
    iolist_to_binary([fix_path(Url), Path, Query1]).

fix_path(Path) when is_list(Path) ->
    fix_path(list_to_binary(Path));
fix_path(<<>>) ->
    <<>>;
fix_path(<<"/", Path/binary>>) ->
    fix_path(Path);
fix_path(Path) ->
    case binary:part(Path, {size(Path), -1}) of
        <<"/">> -> binary:part(Path, {0, size(Path) - 1});
        _ -> Path
    end.

%% @doc encode a URL path
%% @equiv pathencode(Bin, [])
-spec pathencode(binary()) -> binary().
pathencode(Bin) ->
    Parts = re:split(hackney_bstr:to_binary(Bin), <<"/">>,
                     [{return, binary}]),
	do_partial_pathencode(Parts, []).

do_partial_pathencode([], Acc) ->
    hackney_bstr:join(lists:reverse(Acc), <<"/">>);
do_partial_pathencode([Part | Rest], Acc) ->
    do_partial_pathencode(Rest, [partial_pathencode(Part, <<>>) | Acc]).



partial_pathencode(<<C, Rest/binary>> = Bin, Acc) ->
    if	C >= $0, C =< $9 -> partial_pathencode(Rest, <<Acc/binary, C>>);
        C >= $A, C =< $Z -> partial_pathencode(Rest, <<Acc/binary, C>>);
        C >= $a, C =< $z -> partial_pathencode(Rest, <<Acc/binary, C>>);
        C =:= $;; C =:= $=; C =:= $,; C =:= $:; C =:= $@ ->
            partial_pathencode(Rest, <<Acc/binary, C>>);
        C =:= $.; C =:= $-; C =:= $+; C =:= $~; C =:= $_ ->
            partial_pathencode(Rest, <<Acc/binary, C>>);
        C =:= $  ->
		    partial_pathencode(Rest, <<Acc/binary, $+>>);
        C =:= $% ->
            %% special case, when a % is passed to the path, check if
            %% it's a valid escape sequence. If the sequence is valid we
            %% don't try to encode it and continue, else, we encode it.
            %% the behaviour is similar to the one you find in chrome:
            %% http://src.chromium.org/viewvc/chrome/trunk/src/url/url_canon_path.cc
            case Bin of
                << $%, H, L, Rest1/binary >> ->
                    G = unhex(H),
	                M = unhex(L),
	                if	G =:= error; M =:= error ->
                        H1 = C band 16#F0 bsr 4, L1 = C band 16#0F,
                        H2 = tohexl(H1),
                        L2 = tohexl(L1),
                        partial_pathencode(Rest, <<Acc/binary, $%, H2, L2>>);
                    true ->
                        partial_pathencode(Rest1, <<Acc/binary, $%, H, L>>)
                    end;
                _ ->
                    H1 = C band 16#F0 bsr 4, L1 = C band 16#0F,
                    H2 = tohexl(H1),
                    L2 = tohexl(L1),
                    partial_pathencode(Rest, <<Acc/binary, $%, H2, L2>>)
                end;
        true ->
            H = C band 16#F0 bsr 4, L = C band 16#0F,
            H1 = tohexl(H),
            L1 = tohexl(L),
            partial_pathencode(Rest, <<Acc/binary, $%, H1, L1>>)
    end;
partial_pathencode(<<>>, Acc) ->
    Acc.
