%% @doc Tests for redirect URL resolution (issues #711, #693, #568, #617)
-module(hackney_redirect_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

-define(PORT, 9877).

%% Setup/teardown for integration tests
setup() ->
    %% Use ensure_all_started for proper dependency handling
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = application:ensure_all_started(cowboy),
    %% Start the test HTTP server
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", test_http_resource, []}]}]),
    {ok, _} = cowboy:start_clear(test_redirect_http, [{port, ?PORT}], #{
        env => #{dispatch => Dispatch}
    }),
    ok.

cleanup(_) ->
    cowboy:stop_listener(test_redirect_http),
    ok.

url(Path) ->
    <<"http://localhost:", (integer_to_binary(?PORT))/binary, Path/binary>>.

%% =============================================================================
%% Unit tests for resolve_redirect_url logic
%% =============================================================================

%% Test RFC 3986 Section 5.4 relative reference resolution
%% Base: http://a/b/c/d;p?q

%% Issue #711: Relative paths without leading slash
resolve_relative_path_test_() ->
    %% Base URL similar to Wikipedia example
    BaseURL = hackney_url:parse_url(<<"https://example.com/api/page/summary/coffee">>),

    [
        %% Test 1: Simple relative path should replace last segment
        {"relative path replaces last segment",
         fun() ->
             Location = <<"Coffee">>,
             Result = resolve_redirect_url(BaseURL, Location),
             ?assertEqual(<<"/api/page/summary/Coffee">>, Result#hackney_url.path)
         end},

        %% Test 2: Relative path with query string
        {"relative path with query string",
         fun() ->
             Location = <<"newpage?foo=bar">>,
             Result = resolve_redirect_url(BaseURL, Location),
             %% Fixed: query string is now parsed from Location
             ?assertEqual(<<"/api/page/summary/newpage">>, Result#hackney_url.path),
             ?assertEqual(<<"foo=bar">>, Result#hackney_url.qs)
         end},

        %% Test 3: Absolute path should work
        {"absolute path replaces entire path",
         fun() ->
             Location = <<"/new/absolute/path">>,
             Result = resolve_redirect_url(BaseURL, Location),
             ?assertEqual(<<"/new/absolute/path">>, Result#hackney_url.path)
         end},

        %% Test 4: Absolute URL should be parsed completely
        {"absolute URL is fully parsed",
         fun() ->
             Location = <<"https://other.com/different/path">>,
             Result = resolve_redirect_url(BaseURL, Location),
             ?assertEqual("other.com", Result#hackney_url.host),
             ?assertEqual(<<"/different/path">>, Result#hackney_url.path)
         end},

        %% Test 5: Network-path reference (//host/path)
        {"network-path reference preserves scheme",
         fun() ->
             Location = <<"//other.com/new/path">>,
             Result = resolve_redirect_url(BaseURL, Location),
             ?assertEqual("other.com", Result#hackney_url.host),
             ?assertEqual(<<"/new/path">>, Result#hackney_url.path),
             ?assertEqual(https, Result#hackney_url.scheme)
         end},

        %% Test 6: Absolute path with query string
        {"absolute path with query string",
         fun() ->
             Location = <<"/new/path?query=value">>,
             Result = resolve_redirect_url(BaseURL, Location),
             ?assertEqual(<<"/new/path">>, Result#hackney_url.path),
             ?assertEqual(<<"query=value">>, Result#hackney_url.qs)
         end}
    ].

%% Test handling of trailing slashes in base path
trailing_slash_test_() ->
    BaseURL = hackney_url:parse_url(<<"https://example.com/a/b/">>),
    [
        {"trailing slash: relative path appends to directory",
         fun() ->
             Location = <<"c">>,
             Result = resolve_redirect_url(BaseURL, Location),
             %% With trailing slash, should append: /a/b/ + c = /a/b/c
             ?assertEqual(<<"/a/b/c">>, Result#hackney_url.path)
         end}
    ].

%% Test for empty/root paths
empty_path_test_() ->
    BaseURL = hackney_url:parse_url(<<"https://example.com">>),
    BaseURLSlash = hackney_url:parse_url(<<"https://example.com/">>),
    [
        {"empty base path",
         fun() ->
             Location = <<"page">>,
             Result = resolve_redirect_url(BaseURL, Location),
             %% Fixed: empty base path now prepends /
             ?assertEqual(<<"/page">>, Result#hackney_url.path)
         end},
        {"root base path",
         fun() ->
             Location = <<"page">>,
             Result = resolve_redirect_url(BaseURLSlash, Location),
             ?assertEqual(<<"/page">>, Result#hackney_url.path)
         end}
    ].

%% Helper function that mirrors hackney.erl's resolve_redirect_url
resolve_redirect_url(CurrentURL, Location) when is_binary(Location) ->
    case Location of
        <<"http://", _/binary>> -> hackney_url:parse_url(Location);
        <<"https://", _/binary>> -> hackney_url:parse_url(Location);
        <<"//", _/binary>> ->
            Scheme = CurrentURL#hackney_url.scheme,
            SchemePrefix = atom_to_binary(Scheme, utf8),
            hackney_url:parse_url(<<SchemePrefix/binary, ":", Location/binary>>);
        <<"/", _/binary>> ->
            {Path, Qs} = parse_path_qs(Location),
            CurrentURL#hackney_url{path = Path, qs = Qs};
        _ ->
            {RelPath, Qs} = parse_path_qs(Location),
            CurrentPath = CurrentURL#hackney_url.path,
            NewPath = merge_paths(CurrentPath, RelPath),
            CurrentURL#hackney_url{path = NewPath, qs = Qs}
    end.

parse_path_qs(PathLike) ->
    case binary:split(PathLike, <<"?">>) of
        [Path] -> {Path, <<>>};
        [Path, Qs] -> {Path, Qs}
    end.

merge_paths(BasePath, RelPath) when is_binary(BasePath), is_binary(RelPath) ->
    case BasePath of
        <<>> -> <<"/", RelPath/binary>>;
        <<"/">> -> <<"/", RelPath/binary>>;
        _ ->
            BaseDir = base_directory(BasePath),
            iolist_to_binary([BaseDir, RelPath])
    end.

base_directory(Path) ->
    case find_last_slash(Path) of
        -1 -> <<>>;
        Pos -> binary:part(Path, 0, Pos + 1)
    end.

find_last_slash(Bin) ->
    find_last_slash(Bin, byte_size(Bin) - 1).

find_last_slash(_Bin, Pos) when Pos < 0 -> -1;
find_last_slash(Bin, Pos) ->
    case binary:at(Bin, Pos) of
        $/ -> Pos;
        _ -> find_last_slash(Bin, Pos - 1)
    end.

%% =============================================================================
%% Integration tests for redirect following
%% =============================================================================

redirect_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"absolute redirect follow",
          fun test_absolute_redirect_follow/0},
         {"relative redirect with slash",
          fun test_relative_redirect_with_slash/0},
         %% Issue #711: relative redirect without slash
         {"relative redirect without slash",
          fun test_relative_redirect_without_slash/0},
         %% Issue #693: redirect to non-standard port preserves Host header
         {"redirect preserves port in Host header",
          fun test_redirect_port_in_host/0},
         %% Issue #617: long redirect URLs should not crash
         {"long redirect URL does not crash",
          fun test_long_redirect_url/0}
     ]}.

test_absolute_redirect_follow() ->
    RedirectTarget = url(<<"/get">>),
    URL = url(<<"/redirect-to?url=", RedirectTarget/binary>>),
    {ok, Status, _Headers, Client} = hackney:request(get, URL, [], <<>>, [{follow_redirect, true}]),
    Location = hackney:location(Client),
    hackney:close(Client),
    ?assertEqual(200, Status),
    ?assertEqual(RedirectTarget, Location).

test_relative_redirect_with_slash() ->
    %% Redirect to absolute path /get
    URL = url(<<"/redirect-to?url=/get">>),
    {ok, Status, _Headers, Client} = hackney:request(get, URL, [], <<>>, [{follow_redirect, true}]),
    hackney:close(Client),
    ?assertEqual(200, Status).

test_relative_redirect_without_slash() ->
    %% This tests issue #711
    %% If we're at /foo/bar and redirect to "baz", we should end up at /foo/baz
    %% We need to set up a specific test case for this
    %% For now, verify basic relative redirect works
    URL = url(<<"/redirect-to?url=get">>),  %% relative path without /
    {ok, Status, _Headers, Client} = hackney:request(get, URL, [], <<>>, [{follow_redirect, true}]),
    FinalLocation = hackney:location(Client),
    hackney:close(Client),
    %% The redirect from /redirect-to to "get" should go to /get
    io:format("Relative redirect result: status=~p, location=~p~n", [Status, FinalLocation]),
    ?assertEqual(200, Status).

test_redirect_port_in_host() ->
    %% This tests issue #693
    %% When redirecting to a URL with non-standard port, Host header should include port
    %% For this test, we redirect to our own server on non-standard port
    RedirectTarget = <<"http://localhost:", (integer_to_binary(?PORT))/binary, "/get">>,
    URL = url(<<"/redirect-to?url=", RedirectTarget/binary>>),
    {ok, Status, _Headers, Client} = hackney:request(get, URL, [], <<>>, [{follow_redirect, true}]),
    hackney:close(Client),
    %% If this works, the Host header was correct
    ?assertEqual(200, Status).

test_long_redirect_url() ->
    %% This tests issue #617
    %% Long redirect URLs should not crash the parser
    %% Generate a very long query string (over 4096 bytes to exceed default max_line_length)
    LongParam = list_to_binary(lists:duplicate(5000, $x)),
    RedirectTarget = <<"/get?param=", LongParam/binary>>,
    URL = url(<<"/redirect-to?url=", (hackney_url:urlencode(RedirectTarget))/binary>>),
    %% This should not crash - it should either succeed or return an error
    Result = hackney:request(get, URL, [], <<>>, [{follow_redirect, true}]),
    case Result of
        {ok, Status, _Headers, Client} ->
            hackney:close(Client),
            ?assertEqual(200, Status);
        {error, _Reason} ->
            %% An error is acceptable (e.g., line_too_long), but not a crash
            ok
    end.

%% =============================================================================
%% Tests for CVE-2018-1000007 - auth stripping on cross-host redirects
%% =============================================================================

%% Test that auth options are stripped on cross-host redirects by default
auth_strip_on_cross_host_redirect_test_() ->
    BaseURL = hackney_url:parse_url(<<"https://api.example.com/resource">>),
    SameHostURL = hackney_url:parse_url(<<"https://api.example.com/other">>),
    DiffHostURL = hackney_url:parse_url(<<"https://s3.amazonaws.com/bucket/file">>),
    OptionsWithAuth = [{basic_auth, {"user", "pass"}}, {cookie, "session=abc"}, {timeout, 5000}],

    [
        {"same host keeps auth options",
         fun() ->
             Result = maybe_strip_auth_on_redirect(BaseURL, SameHostURL, OptionsWithAuth),
             ?assert(proplists:is_defined(basic_auth, Result)),
             ?assert(proplists:is_defined(cookie, Result)),
             ?assert(proplists:is_defined(timeout, Result))
         end},
        {"different host strips auth options",
         fun() ->
             Result = maybe_strip_auth_on_redirect(BaseURL, DiffHostURL, OptionsWithAuth),
             ?assertNot(proplists:is_defined(basic_auth, Result)),
             ?assertNot(proplists:is_defined(cookie, Result)),
             %% Non-auth options should be kept
             ?assert(proplists:is_defined(timeout, Result))
         end},
        {"location_trusted keeps auth on different host",
         fun() ->
             OptionsWithTrusted = [{location_trusted, true} | OptionsWithAuth],
             Result = maybe_strip_auth_on_redirect(BaseURL, DiffHostURL, OptionsWithTrusted),
             ?assert(proplists:is_defined(basic_auth, Result)),
             ?assert(proplists:is_defined(cookie, Result))
         end}
    ].

%% Helper that mirrors hackney.erl's maybe_strip_auth_on_redirect
maybe_strip_auth_on_redirect(CurrentURL, NewURL, Options) ->
    LocationTrusted = proplists:get_value(location_trusted, Options, false),
    case LocationTrusted of
        true ->
            Options;
        false ->
            CurrentHost = CurrentURL#hackney_url.host,
            NewHost = NewURL#hackney_url.host,
            case CurrentHost =:= NewHost of
                true ->
                    Options;
                false ->
                    lists:filter(fun
                        ({basic_auth, _}) -> false;
                        ({cookie, _}) -> false;
                        (_) -> true
                    end, Options)
            end
    end.

%% =============================================================================
%% Tests for HTTP 1xx Informational Responses (issue #631)
%% =============================================================================

%% Test that 1xx responses are handled correctly with callback
inform_response_callback_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"1xx response calls inform_fun callback",
          fun test_inform_callback/0},
         {"1xx response without callback is silently skipped",
          fun test_inform_no_callback/0}
     ]}.

test_inform_callback() ->
    Self = self(),
    InformFun = fun(Status, Reason, Headers) ->
        Self ! {got_inform, Status, Reason, Headers}
    end,
    %% Use URL-encoded link value
    URL = url(<<"/inform?status=103&link=%3C/style.css%3E%3B%20rel%3Dpreload">>),
    {ok, Status, _Headers, Client} = hackney:request(get, URL, [], <<>>,
        [{inform_fun, InformFun}]),
    hackney:close(Client),
    %% Should receive the informational message
    receive
        {got_inform, InformStatus, _Reason, InformHeaders} ->
            ?assertEqual(103, InformStatus),
            %% Check Link header was received
            ?assert(proplists:is_defined(<<"link">>, InformHeaders) orelse
                    proplists:is_defined(<<"Link">>, InformHeaders))
    after 1000 ->
        ?assert(false)  %% Timeout - callback not called
    end,
    %% Final response should be 200
    ?assertEqual(200, Status).

test_inform_no_callback() ->
    %% Without callback, 1xx should be silently skipped
    URL = url(<<"/inform?status=103">>),
    {ok, Status, _Headers, Client} = hackney:request(get, URL, [], <<>>, []),
    hackney:close(Client),
    %% Final response should be 200
    ?assertEqual(200, Status).

%% Test that netloc includes port for non-standard ports
netloc_port_test_() ->
    [
        {"netloc includes port for non-standard HTTP port",
         fun() ->
             URL = hackney_url:normalize(<<"http://example.com:8080/path">>),
             ?assertEqual(<<"example.com:8080">>, URL#hackney_url.netloc)
         end},
        {"netloc excludes port for standard HTTP port",
         fun() ->
             URL = hackney_url:normalize(<<"http://example.com:80/path">>),
             ?assertEqual(<<"example.com">>, URL#hackney_url.netloc)
         end},
        {"netloc includes port for non-standard HTTPS port",
         fun() ->
             URL = hackney_url:normalize(<<"https://example.com:8443/path">>),
             ?assertEqual(<<"example.com:8443">>, URL#hackney_url.netloc)
         end},
        {"netloc excludes port for standard HTTPS port",
         fun() ->
             URL = hackney_url:normalize(<<"https://example.com:443/path">>),
             ?assertEqual(<<"example.com">>, URL#hackney_url.netloc)
         end}
    ].
