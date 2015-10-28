-module(hackney_socket).

-include("hackney_socket.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%% public api
-export([connect/3, connect/4,
		 release/1, close/1,
		 secure/2, secure/3,
		 recv/2, recv/3,
		 send/2,
		 setopts/2,
		 controlling_process/2,
		 info/1]).

%% internal functions
-export([exit_if_closed/1]).
-export([groupname/1]).
-export([add_prefix/2]).

-type hackney_socket() :: #hackney_socket{}.
-export_types([hackney_socket/0]).


%% @doc make a connection
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).

%% @doc make a connection
connect(Host, Port, Options, Timeout) ->
    Pool = proplists:get_value(pool, Options, default),
    Prefix = proplists:get_value(prefix, Options),
    Secure = proplists:get_value(secure, Options, false),

    Parts0 = case Secure of
    			true -> ["ssl", netloc(Host,Port)];
    			false -> [netloc(Host, Port)]
    end,
    Group = if 
    			Prefix =:= undefined -> groupname(Parts0);
    			true -> groupname([Prefix | Parts0])
    		end,

    Res = hackney_pool:request(Pool, Group, Host, Port, Options, Timeout),
    case {Res, Secure} of
    	{{ok, HS}, true} -> secure(HS, Options);
    	{{ok, _HS}, _} -> Res;
    	{Error, _} -> Error
    end.

release(HS) ->
	hackney_pool:release(HS).

close(#hackney_socket{transport=T, sock=Sock}) ->
	T:close(Sock).

%% @doc Start a TLS client connection over a connected TCP client socket.
secure(HS, Options) ->
	secure(HS, Options, infinity).

%% @doc Start a TLS client connection over a connected TCP client socket.
secure(#hackney_socket{transport=hackney_ssl}=HS, _Options, _Timeout) ->
	{ok, HS};
secure(#hackney_socket{transport=hackney_tcp, sock=Sock}=HS, Options, Timeout) ->
	SSLOpts = ssl_opts(HS#hackney_socket.host, Options),
	%% Upgrades a gen_tcp, or equivalent, connected socket to an SSL socket
	case ssl:connect(Sock, SSLOpts, Timeout) of
		{ok, SslSock} ->
			%% maybe edit the groupname
			GroupParts = string:tokens(HS#hackney_socket.group, "/"),
			Group = case lists:member("ssl", GroupParts) of
						true -> HS#hackney_socket.group;
						false ->
							Len = length(GroupParts),
							GroupParts2 = lists:sublist(GroupParts, Len -  1) ++ ["ssl", lists:last(GroupParts)],
							groupname(GroupParts2)
					end,
			{ok, HS#hackney_socket{transport=hackney_ssl, 
								   sock=SslSock,
								   group=Group, 
								   secure=true}};
		Error -> 
			Error
	end.

%% @doc Receive a packet from a socket in passive mode.
-spec recv(hackney_socket(), non_neg_integer())
	-> {ok, any()} | {error, closed | atom()}.
recv(HS, Length) ->
	recv(HS, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_tcp:recv/3
-spec recv(hackney_socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(#hackney_socket{transport=T, sock=S}, Length, Timeout) ->
	T:recv(S, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see gen_tcp:send/2
-spec send(hackney_socket(), iolist()) -> ok | {error, atom()}.
send(#hackney_socket{transport=T, sock=S}, Packet) ->
	T:send(S, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(hackney_socket(), list()) -> ok | {error, atom()}.
setopts(#hackney_socket{transport=T, sock=S}, Opts) ->
	T:setopts(S, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_tcp:controlling_process/2
-spec controlling_process(hackney_socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(#hackney_socket{transport=T, sock=S}, Pid) ->
	T:controlling_process(S, Pid).


info(#hackney_socket{transport=Transport, sock=Sock}=HS) ->
    {ok, Peername} = Transport:peername(Sock),
    {ok, Sockname} = Transport:sockname(Sock),
	[{type, HS#hackney_socket.type,
	  connected, HS#hackney_socket.connected,
	  secure, HS#hackney_socket.secure,
	  peername, Peername,
	  sockname, Sockname,
	  tranport, Transport,
	  socket, Sock}].


%% private functions

%% @hidden
groupname(Parts) ->
    string:join(Parts, "/").

netloc(Host, Port) when is_list(Host), is_integer(Port) ->
    Host ++ ":" ++ integer_to_list(Port).

%% @hidden
add_prefix(Prefix, Options) ->
	case proplists:get_value(prefix, Options) of
		undefined -> [{prefix, Prefix} | Options];
		OldPrefix ->
			NewPrefix = groupname([Prefix, OldPrefix]),
			lists:keyreplace(prefix, 1, Options, {prefix, NewPrefix})
	end.

%% @hidden
exit_if_closed({error, closed}) -> exit({error, closed});
exit_if_closed(Res) -> Res.



ssl_opts(Host, Options) ->
    case proplists:get_value(ssl_options, Options) of
        undefined ->
            Insecure =  proplists:get_value(insecure, Options),
            UseSecureSsl = check_ssl_version(),
            CACerts = certifi:cacerts(),

            case {Insecure, UseSecureSsl} of
                {true, _} ->
                    [{verify, verify_none},
                     {reuse_sessions, true}];
                {_, true} ->

                    VerifyFun = {fun ssl_verify_hostname:verify_fun/3,
                                 [{check_hostname, Host}]},
                    [{verify, verify_peer},
                     {depth, 99},
                     {cacerts, CACerts},
                     {partial_chain, fun partial_chain/1},
                     {verify_fun, VerifyFun}];
                {_, _} ->
                    [{cacerts, CACerts},
                     {verify, verify_peer}, {depth, 2}]
            end;
        SSLOpts ->
            SSLOpts
    end.

%% code from rebar3 undert BSD license
partial_chain(Certs) ->
    Certs1 = lists:reverse([{Cert, public_key:pkix_decode_cert(Cert, otp)} ||
                            Cert <- Certs]),
    CACerts = certifi:cacerts(),
    CACerts1 = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CACerts],


    case find(fun({_, Cert}) ->
                      check_cert(CACerts1, Cert)
              end, Certs1) of
        {ok, Trusted} ->
            {trusted_ca, element(1, Trusted)};
        _ ->
            unknown_ca
    end.

extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

check_cert(CACerts, Cert) ->
    lists:any(fun(CACert) ->
                      extract_public_key_info(CACert) == extract_public_key_info(Cert)
              end, CACerts).

check_ssl_version() ->
    case application:get_key(ssl, vsn) of
        {ok, Vsn} ->
            parse_vsn(Vsn) >= {5, 3, 6};
        _ ->
            false
    end.

parse_vsn(Vsn) ->
    version_pad(string:tokens(Vsn, ".")).

version_pad([Major]) ->
    {list_to_integer(Major), 0, 0};
version_pad([Major, Minor]) ->
    {list_to_integer(Major), list_to_integer(Minor), 0};
version_pad([Major, Minor, Patch]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)};
version_pad([Major, Minor, Patch | _]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)}.

-spec find(fun(), list()) -> {ok, term()} | error.
find(Fun, [Head|Tail]) when is_function(Fun) ->
    case Fun(Head) of
        true ->
            {ok, Head};
        false ->
            find(Fun, Tail)
    end;
find(_Fun, []) ->
    error.