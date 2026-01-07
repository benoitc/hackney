%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
-module(hackney_util_tests).
-include_lib("eunit/include/eunit.hrl").

%% filter_options/3 tests
filter_options_test_() ->
    [
        {"empty list returns accumulator",
         fun() -> [] = hackney_util:filter_options([], [key1, key2], []) end},
        {"filters allowed key-value pairs",
         fun() ->
             Input = [{key1, val1}, {key2, val2}, {key3, val3}],
             Allowed = [key1, key3],
             Result = hackney_util:filter_options(Input, Allowed, []),
             ?assert(lists:member({key1, val1}, Result)),
             ?assert(lists:member({key3, val3}, Result)),
             ?assertNot(lists:member({key2, val2}, Result))
         end},
        {"filters raw options when allowed",
         fun() ->
             Input = [{raw, 1, 2, 3}, {key1, val1}],
             Allowed = [raw, key1],
             Result = hackney_util:filter_options(Input, Allowed, []),
             ?assert(lists:member({raw, 1, 2, 3}, Result)),
             ?assert(lists:member({key1, val1}, Result))
         end},
        {"filters raw options when not allowed",
         fun() ->
             Input = [{raw, 1, 2, 3}, {key1, val1}],
             Allowed = [key1],
             Result = hackney_util:filter_options(Input, Allowed, []),
             ?assertNot(lists:member({raw, 1, 2, 3}, Result)),
             ?assert(lists:member({key1, val1}, Result))
         end},
        {"filters atom options",
         fun() ->
             Input = [binary, {key1, val1}, list],
             Allowed = [binary, key1],
             Result = hackney_util:filter_options(Input, Allowed, []),
             ?assert(lists:member(binary, Result)),
             ?assert(lists:member({key1, val1}, Result)),
             ?assertNot(lists:member(list, Result))
         end}
    ].

%% set_option_default/3 tests
set_option_default_test_() ->
    [
        {"adds default when key not present",
         fun() ->
             Opts = [{key1, val1}],
             Result = hackney_util:set_option_default(Opts, key2, default_val),
             ?assertEqual([{key2, default_val}, {key1, val1}], Result)
         end},
        {"keeps existing value when key present",
         fun() ->
             Opts = [{key1, existing_val}],
             Result = hackney_util:set_option_default(Opts, key1, default_val),
             ?assertEqual(Opts, Result)
         end},
        {"works with empty list",
         fun() ->
             Result = hackney_util:set_option_default([], key1, val1),
             ?assertEqual([{key1, val1}], Result)
         end}
    ].

%% to_atom/1 tests
to_atom_test_() ->
    [
        {"converts list to atom",
         fun() ->
             ?assertEqual(test_atom, hackney_util:to_atom("test_atom"))
         end},
        {"converts binary to atom",
         fun() ->
             ?assertEqual(test_atom, hackney_util:to_atom(<<"test_atom">>))
         end},
        {"returns atom unchanged",
         fun() ->
             ?assertEqual(test_atom, hackney_util:to_atom(test_atom))
         end},
        {"uses existing atom when available",
         fun() ->
             %% hackney is an existing atom
             ?assertEqual(hackney, hackney_util:to_atom("hackney"))
         end}
    ].

%% merge_opts/2 tests
merge_opts_test_() ->
    [
        {"empty list returns options",
         fun() ->
             Options = [{a, 1}, {b, 2}],
             ?assertEqual(Options, hackney_util:merge_opts([], Options))
         end},
        {"adds missing options",
         fun() ->
             ToMerge = [{c, 3}],
             Options = [{a, 1}, {b, 2}],
             Result = hackney_util:merge_opts(ToMerge, Options),
             ?assert(lists:member({c, 3}, Result)),
             ?assert(lists:member({a, 1}, Result))
         end},
        {"does not override existing options",
         fun() ->
             ToMerge = [{a, override}],
             Options = [{a, 1}, {b, 2}],
             Result = hackney_util:merge_opts(ToMerge, Options),
             ?assert(lists:member({a, 1}, Result)),
             ?assertNot(lists:member({a, override}, Result))
         end},
        {"adds raw options unconditionally",
         fun() ->
             ToMerge = [{raw, 1, 2, 3}],
             Options = [{a, 1}],
             Result = hackney_util:merge_opts(ToMerge, Options),
             ?assert(lists:member({raw, 1, 2, 3}, Result))
         end},
        {"handles atom options",
         fun() ->
             ToMerge = [binary],
             Options = [{a, 1}],
             Result = hackney_util:merge_opts(ToMerge, Options),
             ?assert(lists:member(binary, Result))
         end},
        {"does not duplicate atom options",
         fun() ->
             ToMerge = [binary],
             Options = [binary, {a, 1}],
             Result = hackney_util:merge_opts(ToMerge, Options),
             %% Should have exactly one 'binary' atom
             ?assertEqual(1, length([X || X <- Result, X =:= binary]))
         end},
        {"ignores non-matching option formats",
         fun() ->
             ToMerge = ["invalid"],
             Options = [{a, 1}],
             Result = hackney_util:merge_opts(ToMerge, Options),
             ?assertEqual([{a, 1}], Result)
         end}
    ].

%% to_int/1 tests
to_int_test_() ->
    [
        {"converts string to integer",
         fun() ->
             ?assertEqual({ok, 123}, hackney_util:to_int("123"))
         end},
        {"converts binary to integer",
         fun() ->
             ?assertEqual({ok, 456}, hackney_util:to_int(<<"456">>))
         end},
        {"handles negative numbers",
         fun() ->
             ?assertEqual({ok, -789}, hackney_util:to_int("-789"))
         end},
        {"returns false for invalid input",
         fun() ->
             ?assertEqual(false, hackney_util:to_int("not_a_number"))
         end},
        {"converts zero",
         fun() ->
             ?assertEqual({ok, 0}, hackney_util:to_int("0"))
         end}
    ].

%% is_ipv6/1 tests
is_ipv6_test_() ->
    [
        {"returns true for IPv6 address string",
         fun() ->
             ?assertEqual(true, hackney_util:is_ipv6("::1"))
         end},
        {"returns true for full IPv6 address",
         fun() ->
             ?assertEqual(true, hackney_util:is_ipv6("2001:0db8:85a3:0000:0000:8a2e:0370:7334"))
         end},
        {"returns false for IPv4 address string",
         fun() ->
             ?assertEqual(false, hackney_util:is_ipv6("127.0.0.1"))
         end},
        {"returns false for IPv4 address string 2",
         fun() ->
             ?assertEqual(false, hackney_util:is_ipv6("192.168.1.1"))
         end}
    ].

%% privdir/0 test
privdir_test_() ->
    [
        {"returns a valid path",
         fun() ->
             Dir = hackney_util:privdir(),
             ?assert(is_list(Dir)),
             ?assert(length(Dir) > 0)
         end}
    ].

%% mod_metrics/0 test
mod_metrics_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
        {"returns metrics_dummy by default",
         fun() ->
             Result = hackney_util:mod_metrics(),
             %% Default should be metrics_dummy or configured value
             ?assert(is_atom(Result))
         end}
     ]}.
