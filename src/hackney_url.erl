%% @doc module to manage urls.
-module(hackney_url).

-export([parse_url/1,
         urldecode/1, urldecode/2,
         urlencode/1, urlencode/2]).

-include("hackney.hrl").

%% @doc Parse an url and return a #hackney_url record.
-spec parse_url(URL::binary()|list()) -> hackney_url().
parse_url(URL) when is_list(URL) ->
    parse_url(list_to_binary(URL));

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
            {Path, Query, Fragment} = parse_path(Path),
            parse_addr(Addr, S#hackney_url{raw_path = RawPath,
                                           path = Path,
                                           qs = Query,
                                           fragment = Fragment})
    end.

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
            S#hackney_url{host=binary_to_list(Host), port=80};
        [Host] when Transport =:= hackney_tcp_transport ->
            S#hackney_url{host=binary_to_list(Host), port=443};
        [Host, Port] ->
            S#hackney_url{host=binary_to_list(Host),
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
%% @equiv urlencode(Bin, [])
-spec urlencode(binary()) -> binary().
urlencode(Bin) ->
	urlencode(Bin, []).

%% @doc URL encode a string binary.
%% The `noplus' option disables the default behaviour of quoting space
%% characters, `\s', as `+'. The `upper' option overrides the default behaviour
%% of writing hex numbers using lowecase letters to using uppercase letters
%% instead.
-spec urlencode(binary(), [noplus|upper]) -> binary().
urlencode(Bin, Opts) ->
	Plus = not proplists:get_value(noplus, Opts, false),
	Upper = proplists:get_value(upper, Opts, false),
	urlencode(Bin, <<>>, Plus, Upper).

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
tohexu(C) when C < 17 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 17 -> $a + C - 10.

