% coding: latin-1
-module(hackney_url_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hackney_lib.hrl").

parse_and_unparse_url_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"http://www.example.com/path?key=value#Section%205">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://www.example.com">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"">>,
                          path = <<>>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://www.example.com/">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/">>,
                          path = <<"/">>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://[db8:0cec::99:123a]/">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"[db8:0cec::99:123a]">>,
                          raw_path = <<"/">>,
                          path = <<"/">>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "db8:0cec::99:123a",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"https://[db8:0cec::99:123a]/">>,
             #hackney_url{transport =hackney_ssl,
                          scheme = https,
                          netloc = <<"[db8:0cec::99:123a]">>,
                          raw_path = <<"/">>,
                          path = <<"/">>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "db8:0cec::99:123a",
                          port = 443,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"https://[db8:0cec::99:123a]:8080/">>,
             #hackney_url{transport =hackney_ssl,
                          scheme = https,
                          netloc = <<"[db8:0cec::99:123a]:8080">>,
                          raw_path = <<"/">>,
                          path = <<"/">>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "db8:0cec::99:123a",
                          port = 8080,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://www.example.com/?key=value#Section%205">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/?key=value#Section%205">>,
                          path = <<"/">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://www.example.com:8080/path?key=value#Section%205">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com:8080">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 8080,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"https://user:passwd@www.example.com/path?key=value#Section%205">>,
             #hackney_url{transport =hackney_ssl,
                          scheme = https,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 443,
                          user = <<"user">>,
                          password = <<"passwd">>}
            },
            {<<"https://user:pass%26word@www.example.com/path?key=value#Section%205">>,
             #hackney_url{transport =hackney_ssl,
                          scheme = https,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 443,
                          user = <<"user">>,
                          password = <<"pass&word">>}
            },
            {<<"https://user@www.example.com/path?key=value#Section%205">>,
             #hackney_url{transport =hackney_ssl,
                          scheme = https,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 443,
                          user = <<"user">>,
                          password = <<"">>}
            },
            {<<"http+unix://user@%2Fvar%2Frun%2Ftest.sock/path?key=value#Section%205">>,
             #hackney_url{transport =hackney_local_tcp,
                          scheme = http_unix,
                          netloc = <<"%2Fvar%2Frun%2Ftest.sock">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "/var/run/test.sock",
                          port = 0,
                          user = <<"user">>,
                          password = <<"">>}
            },
            {<<"http://example.com#-@127.2.2.2/232d40">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"example.com">>,
                          raw_path = <<"#-@127.2.2.2/232d40">>,
                          path = <<>>,
                          qs = <<"">>,
                          fragment = <<"-@127.2.2.2/232d40">>,
                          host = "example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            }
            ],
    [{V, fun() -> R = hackney_url:parse_url(V) end} || {V, R} <- Tests] ++
    [{V, fun() -> V = hackney_url:unparse_url(R) end} || {V, R} <- Tests].

parse_url_test_() ->
    %% {Value, Result}.
    Tests = [
            {"http://www.example.com/path?key=value#Section%205", % list as argument
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"www.example.com/path?key=value#Section%205">>, % without http://
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"/path?key=value#Section%205">>,
                          path = <<"/path">>,
                          qs = <<"key=value">>,
                          fragment = <<"Section%205">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://www.example.com">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"">>,
                          path = <<>>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://www.example.com?q=123">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"?q=123">>,
                          path = <<>>,
                          qs = <<"q=123">>,
                          fragment = <<"">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            },
            {<<"http://username:password@www.example.com">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"">>,
                          path = <<>>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"username">>,
                          password = <<"password">>}
            },
            {<<"http://username:pass%20word@www.example.com">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"">>,
                          path = <<>>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"username">>,
                          password = <<"pass word">>}
            },
            % From HTTP Basic Authentication RFC https://tools.ietf.org/html/rfc7617
            {<<"http://Aladdin:open%20sesame@www.example.com">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"www.example.com">>,
                          raw_path = <<"">>,
                          path = <<>>,
                          qs = <<"">>,
                          fragment = <<"">>,
                          host = "www.example.com",
                          port = 80,
                          user = <<"Aladdin">>,
                          password = <<"open sesame">>}
            },
            {<<"http://example.com#-@127.2.2.2/232d40">>,
             #hackney_url{transport =hackney_tcp,
                          scheme = http,
                          netloc = <<"example.com">>,
                          raw_path = <<"#-@127.2.2.2/232d40">>,
                          path = <<>>,
                          qs = <<"">>,
                          fragment = <<"-@127.2.2.2/232d40">>,
                          host = "example.com",
                          port = 80,
                          user = <<"">>,
                          password = <<"">>}
            }
            ],
    [{V, fun() -> R = hackney_url:parse_url(V) end} || {V, R} <- Tests].

transport_scheme_test_() ->
    %% {Value, Result}.
    Tests = [
            {hackney_tcp, http},
            {hackney_ssl, https}
            ],
    [{atom_to_list(V), fun() -> R = hackney_url:transport_scheme(V) end} || {V, R} <- Tests].

url_encode_and_decode_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"HelloGünter">>, <<"HelloG%C3%BCnter">>},
            {<<"Hello.-~_">>, <<"Hello.-~_">>},
            {<<"€£©®ÀÁÂÃÄÅ">>, <<"%E2%82%AC%C2%A3%C2%A9%C2%AE%C3%80%C3%81%C3%82%C3%83%C3%84%C3%85">>}
            ],
    [{V, fun() -> R = hackney_url:urlencode(V) end} || {V, R} <- Tests] ++
    [{R, fun() -> V = hackney_url:urldecode(R) end} || {V, R} <- Tests].

url_encode_test_() ->
    %% {{Url, Options}, Result}.
    Tests = [
            {{<<"HelloGünter">>, [lower]}, <<"HelloG%c3%bcnter">>},
            {{<<"Hello+Günter">>, []}, <<"Hello%2BG%C3%BCnter">>},
            {{<<"Hello ">>, []}, <<"Hello+">>},
            {{<<"Hello ">>, [noplus]}, <<"Hello%20">>}
            ],
    [{U, fun() -> R = hackney_url:urlencode(U, O) end} || {{U, O}, R} <- Tests].

url_decode_test_() ->
    F = fun(U, O) ->
            try hackney_url:urldecode(U, O) of
                R -> R
            catch Error:Reason -> {Error, Reason}
            end
    end,
    %% {{Url, Options}, Result}.
    Tests = [
            {{<<"HelloG%C3%BCnter">>, skip}, <<"HelloGünter">>},
            {{<<"Hello%KC">>, skip}, <<"Hello%KC">>},
            {{<<"Hello%">>, skip}, <<"Hello%">>},
            {{<<"Hello%">>, crash}, {error, badarg}},
            {{<<"Hello%KC">>, crash}, {error, badarg}},
            {{<<"Hello+">>, crash}, <<"Hello ">>}
            ],
    [{U, fun() -> R = F(U, O) end} || {{U, O}, R} <- Tests].

parse_qs_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"a=b">>, [{<<"a">>,<<"b">>}]},
            {<<"a=b&c=d">>, [{<<"a">>,<<"b">>}, {<<"c">>, <<"d">>}]},
            {<<"&a=b&&c=d&">>, [{<<"a">>,<<"b">>}, {<<"c">>, <<"d">>}]},
            {<<"a=b&c">>, [{<<"a">>,<<"b">>}, {<<"c">>, true}]},
            {<<"&a=b&c&&">>, [{<<"a">>,<<"b">>}, {<<"c">>, true}]}
            ],
    [{V, fun() -> R = hackney_url:parse_qs(V) end} || {V, R} <- Tests].

qs_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"a=b">>, [{<<"a">>,<<"b">>}]},
            {<<"a=b&c=d">>, [{<<"a">>,<<"b">>}, {<<"c">>, <<"d">>}]}
            ],
    [{V, fun() -> V = hackney_url:qs(R) end} || {V, R} <- Tests].

make_url_test_() ->
    %% {Value, Result}.
    Tests = [
            {[<<"http://example.com">>, <<"path">>, [{<<"a">>, <<"b">>}]], <<"http://example.com/path?a=b">>},
            {[<<"http://example.com">>, [<<"path1">>, <<"path2">>], []], <<"http://example.com/path1/path2">>}
            ],
    [{R, fun() -> R = hackney_url:make_url(U, P, Q) end} || {[U, P, Q], R} <- Tests].

fix_path_test_() ->
    %% {Value, Result}.
    Tests = [
            {"/path1/path2", <<"path1/path2">>},
            {<<"path1/path2">>, <<"path1/path2">>},
            {<<"path1/path2/">>, <<"path1/path2">>}
            ],
    [{V, fun() -> R = hackney_url:fix_path(V) end} || {V, R} <- Tests].

pathencode_test_() ->
    %% {Value, Result}.
    Tests = [
            {<<"/path1/path2">>, <<"/path1/path2">>},
            {<<"/path1/path2%2fa">>, <<"/path1/path2%2fa">>},
            {<<"/path1/path2%2fa%2fb">>, <<"/path1/path2%2fa%2fb">>},
            {<<"/path1/path2%2test">>, <<"/path1/path2%252test">>},
            {<<"/id/name:107/name2;p=1,3">>, <<"/id/name:107/name2;p=1,3">>},
            {<<"/@foobar">>, <<"/@foobar">>},
            {<<"/500x720/filters:quality(75):format(jpg)/spree/product/s/p/spree2018september12picslgzh0650.jpg">>,
             <<"/500x720/filters:quality(75):format(jpg)/spree/product/s/p/spree2018september12picslgzh0650.jpg">>},
            {<<"/1/indexes/*/queries">>, <<"/1/indexes/*/queries">>}
            ],
    [{V, fun() -> R = hackney_url:pathencode(V) end} || {V, R} <- Tests].

normalize_test_() ->
    Tests = [
        {"http://www.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA" ++
         "%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3" ++
         "%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82" ++
         "%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0" ++
         "%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3" ++
         "%81%9F%E3%82%8A%E3%81%AA%E3%81%84.w3.mag.keio.ac.jp",
         << "http://www.xn--n8jaaaaai5bhf7as8fsfk3jnknefdde3f",
           "g11amb5gzdb4wi9bya3kc6lra.w3.mag.keio.ac.jp" >>}],
    [{V, fun() -> R = hackney_url:unparse_url(hackney_url:normalize(V))
            end} || {V, R} <- Tests].


normalize2_test_() ->
    Tests = [<<"http://www.詹姆斯.com/atomtests/iri/詹.html"/utf8>>,
             <<"http://www.xn--8ws00zhy3a.com/atomtests/iri/%e8%a9%b9.html">>],
    [{V, fun() -> R = hackney_url:unparse_url(hackney_url:normalize(V))
            end} || {V, R} <- Tests].
