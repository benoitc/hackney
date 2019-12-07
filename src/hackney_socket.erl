-module(hackney_socket).

-export([get_fqdn/0]).


%% @doc Return a fully qualified domain name for the localhost
-spec get_fqdn() -> {ok, Name::string()} | {error, Host::atom() | string()}.
get_fqdn() ->
  net_adm:dns_hostname(net_adm:localhost()).
