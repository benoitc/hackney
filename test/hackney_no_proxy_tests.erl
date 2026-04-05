-module(hackney_no_proxy_tests).
-include_lib("eunit/include/eunit.hrl").

-define(clear_cache(), application:unset_env(hackney, no_proxy)).

parse_single_host_test() ->
  ?clear_cache(),
  Result = hackney:parse_no_proxy_env(["my-internal-host"], []),
  ?assertEqual([{host, ["my-internal-host"]}], Result).

parse_multiple_hosts_test() ->
  ?clear_cache(),
  Result = hackney:parse_no_proxy_env(["my-internal-host", "api.example.org"], []),
  ?assertEqual([
    {host, ["my-internal-host"]},
    {host, ["org", "example", "api"]}
  ], Result).

parse_domain_test() ->
  ?clear_cache(),
  Result = hackney:parse_no_proxy_env(["example.com"], []),
  ?assertEqual([{host, ["com", "example"]}], Result).

parse_leading_dot_domain_test() ->
  ?clear_cache(),
  Result = hackney:parse_no_proxy_env([".example.com"], []),
  ?assertEqual(["example", "com"], string:tokens(".example.com", ".")),
  ?assertEqual([{host, ["com", "example"]}], Result).

parse_wildcard_test() ->
  ?clear_cache(),
  Result = hackney:parse_no_proxy_env(["*"], []),
  ?assertEqual('*', Result).

parse_cidr_test() ->
  ?clear_cache(),
  Result = hackney:parse_no_proxy_env(["192.168.0.0/16"], []),
  ?assertMatch([{cidr, _}], Result).

parse_mixed_test() ->
  ?clear_cache(),
  Result = hackney:parse_no_proxy_env(["192.168.0.0/16", "example.com", "localhost"], []),
  ?assertMatch([{cidr, _}, {host, ["com", "example"]}, {host, ["localhost"]}], Result).

match_exact_host_test() ->
  ?clear_cache(),
  Patterns = hackney:parse_no_proxy_env(["my-internal-host"], []),
  ?assert(hackney:match_no_proxy_env(Patterns, "my-internal-host")),
  ?assertNot(hackney:match_no_proxy_env(Patterns, "other-host")).

match_domain_suffix_test() ->
  ?clear_cache(),
  Patterns = hackney:parse_no_proxy_env([".example.com"], []),
  ?assert(hackney:match_no_proxy_env(Patterns, "example.com")),
  ?assert(hackney:match_no_proxy_env(Patterns, "sub.example.com")),
  ?assertNot(hackney:match_no_proxy_env(Patterns, "other.com")).

match_subdomain_test() ->
  ?clear_cache(),
  Patterns = hackney:parse_no_proxy_env(["example.com"], []),
  ?assert(hackney:match_no_proxy_env(Patterns, "example.com")),
  ?assert(hackney:match_no_proxy_env(Patterns, "sub.example.com")),
  ?assert(hackney:match_no_proxy_env(Patterns, "a.b.example.com")),
  ?assertNot(hackney:match_no_proxy_env(Patterns, "other.com")).

match_multiple_hosts_test() ->
  ?clear_cache(),
  Patterns = hackney:parse_no_proxy_env(["my-internal-host", "api.example.org"], []),
  ?assert(hackney:match_no_proxy_env(Patterns, "my-internal-host")),
  ?assert(hackney:match_no_proxy_env(Patterns, "api.example.org")),
  ?assertNot(hackney:match_no_proxy_env(Patterns, "other-host")),
  ?assertNot(hackney:match_no_proxy_env(Patterns, "other.example.org")).

match_no_proxy_false_test() ->
  ?assertNot(hackney:match_no_proxy_env(false, "example.com")),
  ?assertNot(hackney:match_no_proxy_env(false, "any-host")).

match_no_proxy_wildcard_test() ->
  ?assert(hackney:match_no_proxy_env('*', "anything")),
  ?assert(hackney:match_no_proxy_env('*', "localhost")),
  ?assert(hackney:match_no_proxy_env('*', "192.168.1.1")).
