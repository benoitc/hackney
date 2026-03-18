%%% Test that cacertfile option is respected when making HTTPS requests.
%%%
%%% Currently fails because hackney_conn.erl bypasses hackney_ssl:ssl_opts/2
%%% and calls hackney_ssl:check_hostname_opts/1 directly, which always sets
%%% {cacerts, certifi:cacerts()}. Erlang SSL then ignores cacertfile.

-module(hackney_cacertfile_bug_test).
-include_lib("eunit/include/eunit.hrl").

-define(HTTPS_PORT, 8126).

cacertfile_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"cacertfile respected in ssl_opts/2", fun test_ssl_opts_handles_cacertfile/0},
      {"cacertfile respected in request", fun test_request_with_cacertfile/0}
     ]}.

setup() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(hackney),

    CertDir = cert_dir(),
    CertFile = filename:join(CertDir, "server.pem"),
    KeyFile = filename:join(CertDir, "server.key"),

    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_tls(cacertfile_test_server,
                                [{certfile, CertFile},
                                 {keyfile, KeyFile},
                                 {port, ?HTTPS_PORT}],
                                #{env => #{dispatch => Dispatch}}),
    ok.

teardown(_) ->
    cowboy:stop_listener(cacertfile_test_server),
    application:stop(cowboy),
    application:stop(hackney),
    error_logger:tty(true),
    ok.

test_ssl_opts_handles_cacertfile() ->
    CACertFile = filename:join(cert_dir(), "ca.pem"),
    Options = [{ssl_options, [{cacertfile, CACertFile}]}],
    SslOpts = hackney_ssl:ssl_opts("localhost", Options),
    ?assert(lists:keymember(cacertfile, 1, SslOpts)),
    ?assertNot(lists:keymember(cacerts, 1, SslOpts)).

test_request_with_cacertfile() ->
    CACertFile = filename:join(cert_dir(), "ca.pem"),
    Url = "https://localhost:" ++ integer_to_list(?HTTPS_PORT) ++ "/get",
    Opts = [{ssl_options, [{cacertfile, CACertFile}]}, {pool, false}],
    {ok, 200, _, _} = hackney:request(get, Url, [], <<>>, Opts).

cert_dir() ->
    filename:join([filename:dirname(code:which(?MODULE)), "..", "test", "certs"]).
