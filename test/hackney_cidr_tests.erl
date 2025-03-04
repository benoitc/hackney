-module(hackney_cidr_tests).

-include_lib("eunit/include/eunit.hrl").

parse_ipv4_test_() ->
    [?_assertEqual({{0, 0, 0, 0}, {255, 255, 255, 255}, 0},
                   hackney_cidr:parse("192.168.0.0/0", true)),
     ?_assertEqual({{192, 0, 0, 0}, {192, 255, 255, 255}, 8},
                   hackney_cidr:parse("192.168.0.0/8", true)),
     ?_assertEqual({{192, 168, 0, 0}, {192, 169, 255, 255}, 15},
                   hackney_cidr:parse("192.168.0.0/15", true)),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 255, 255}, 16},
                   hackney_cidr:parse("192.168.0.0/16")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 127, 255}, 17},
                   hackney_cidr:parse("192.168.0.0/17")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 63, 255}, 18},
                   hackney_cidr:parse("192.168.0.0/18")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 31, 255}, 19},
                   hackney_cidr:parse("192.168.0.0/19")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 15, 255}, 20},
                   hackney_cidr:parse("192.168.0.0/20")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 7, 255}, 21},
                   hackney_cidr:parse("192.168.0.0/21")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 3, 255}, 22},
                   hackney_cidr:parse("192.168.0.0/22")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 1, 255}, 23},
                   hackney_cidr:parse("192.168.0.0/23")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 0, 255}, 24},
                   hackney_cidr:parse("192.168.0.0/24")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 0, 1}, 31},
                   hackney_cidr:parse("192.168.0.0/31")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 0, 0}, 32},
                   hackney_cidr:parse("192.168.0.0/32")),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 0, 0}, 32},
                   hackney_cidr:parse(<<"192.168.0.0/32">>)),
     ?_assertEqual({{192, 168, 0, 0}, {192, 168, 0, 0}, 32},
                   hackney_cidr:parse(<<"192.168.0.0">>))].

parse_ipv6_test_() ->
    [?_assertEqual({{0, 0, 0, 0, 0, 0, 0, 0},
                    {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
                    0},
                   hackney_cidr:parse("2001:abcd::/0", true)),
     ?_assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0},
                    {8193, 43981, 65535, 65535, 65535, 65535, 65535, 65535},
                    32},
                   hackney_cidr:parse("2001:abcd::/32")),
     ?_assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0},
                    {8193, 43981, 32767, 65535, 65535, 65535, 65535, 65535},
                    33},
                   hackney_cidr:parse("2001:abcd::/33")),
     ?_assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0},
                    {8193, 43981, 16383, 65535, 65535, 65535, 65535, 65535},
                    34},
                   hackney_cidr:parse("2001:abcd::/34")),
     ?_assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0},
                    {8193, 43981, 8191, 65535, 65535, 65535, 65535, 65535},
                    35},
                   hackney_cidr:parse("2001:abcd::/35")),
     ?_assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0},
                    {8193, 43981, 4095, 65535, 65535, 65535, 65535, 65535},
                    36},
                   hackney_cidr:parse("2001:abcd::/36")),
     ?_assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0},
                    {8193, 43981, 0, 0, 0, 0, 0, 0},
                    128},
                   hackney_cidr:parse("2001:abcd::/128")),
     ?_assertEqual({{8193, 3512, 0, 0, 0, 0, 0, 0},
                    {8193, 3512, 0, 65535, 65535, 65535, 65535, 65535},
                    48},
                   hackney_cidr:parse("2001:db8::/48")),
     ?_assertEqual({{8193, 3512, 0, 0, 0, 0, 0, 0},
                    {8193, 3512, 0, 65535, 65535, 65535, 65535, 65535},
                    48},
                   hackney_cidr:parse(<<"2001:db8::/48">>)),
     ?_assertEqual({{8193,3512,0,0,0,0,0,1},{8193,3512,0,0,0,0,0,1},128},
                   hackney_cidr:parse(<<"2001:0db8::1/128">>)),
     ?_assertEqual({{8193,3512,0,0,0,0,0,1},{8193,3512,0,0,0,0,0,1},128},
                   hackney_cidr:parse(<<"2001:0db8::1">>))]
     .


to_string_test_() ->
    [?_assertEqual("192.168.0.0/16",
                   hackney_cidr:to_string({{192, 168, 0, 0}, {192, 168, 255, 255}, 16})),
     ?_assertEqual("2001:abcd::/32",
                   hackney_cidr:to_string({{8193, 43981, 0, 0, 0, 0, 0, 0},
                                        {8193, 43981, 65535, 65535, 65535, 65535, 65535, 65535},
                                        32}))].

to_binary_test_() ->
    [?_assertEqual(<<"192.168.0.0/16">>,
                   hackney_cidr:to_binary({{192, 168, 0, 0}, {192, 168, 255, 255}, 16})),
     ?_assertEqual(<<"2001:abcd::/32">>,
                   hackney_cidr:to_binary({{8193, 43981, 0, 0, 0, 0, 0, 0},
                                        {8193, 43981, 65535, 65535, 65535, 65535, 65535, 65535},
                                        32}))].

ipv4_address_count_test_() ->
    {ok, Addr} = inet:parse_address("192.168.0.0"),
    [?_assertEqual(4294967296, hackney_cidr:address_count(Addr, 0)),
     ?_assertEqual(     65536, hackney_cidr:address_count(Addr, 16)),
     ?_assertEqual(     32768, hackney_cidr:address_count(Addr, 17)),
     ?_assertEqual(       256, hackney_cidr:address_count(Addr, 24)),
     ?_assertEqual(         1, hackney_cidr:address_count(Addr, 32))].

ipv6_address_count_test_() ->
    {ok, Addr} = inet:parse_address("2001::abcd"),
    [?_assertEqual(1 bsl 128, hackney_cidr:address_count(Addr, 0)),
     ?_assertEqual(1 bsl  64, hackney_cidr:address_count(Addr, 64)),
     ?_assertEqual(1,         hackney_cidr:address_count(Addr, 128))].

contains_test_() ->
    Block = hackney_cidr:parse("192.168.1.0/24"),
    [?_assertNot(hackney_cidr:contains(Block, {}))].

ipv4_contains_test_() ->
    Block = {{192, 168, 0, 0}, {192, 168, 255, 255}, 16},
    [?_assert(hackney_cidr:contains(Block, {192, 168, 0, 0})),
     ?_assert(hackney_cidr:contains(Block, {192, 168, 0, 1})),
     ?_assert(hackney_cidr:contains(Block, {192, 168, 1, 0})),
     ?_assert(hackney_cidr:contains(Block, {192, 168, 0, 255})),
     ?_assert(hackney_cidr:contains(Block, {192, 168, 255, 0})),
     ?_assert(hackney_cidr:contains(Block, {192, 168, 255, 255})),
     ?_assertNot(hackney_cidr:contains(Block, {192, 168, 255, 256})),
     ?_assertNot(hackney_cidr:contains(Block, {192, 169, 0, 0})),
     ?_assertNot(hackney_cidr:contains(Block, {192, 167, 255, 255}))].

ipv4_contains_cidr_test_() ->
    Block = {{192, 168, 0, 0}, {192, 168, 255, 255}, 16},
    [?_assert(hackney_cidr:contains(Block, Block)),
     ?_assert(hackney_cidr:contains(Block, hackney_cidr:parse("192.168.1.0/24"))),
     ?_assert(hackney_cidr:contains(Block, hackney_cidr:parse("192.168.254.0/24"))),
     ?_assert(hackney_cidr:contains(Block, hackney_cidr:parse("192.168.1.2/31"))),
     ?_assert(hackney_cidr:contains(Block, hackney_cidr:parse("192.168.1.1/32"))),
     ?_assertNot(hackney_cidr:contains(Block, hackney_cidr:parse("10.0.0.0/16"))),
     ?_assertNot(hackney_cidr:contains(Block, hackney_cidr:parse("10.0.0.1/32"))),
     ?_assertNot(hackney_cidr:contains(Block, hackney_cidr:parse("192.169.0.0/16"))),
     ?_assertNot(hackney_cidr:contains(Block, hackney_cidr:parse("192.168.0.0/15")))].

ipv6_contains_test_() ->
    Block = {{8193, 43981, 0, 0, 0, 0, 0, 0},
             {8193, 43981, 8191, 65535, 65535, 65535, 65535, 65535},
             35},
    [?_assert(hackney_cidr:contains(Block, {8193, 43981, 0, 0, 0, 0, 0, 0})),
     ?_assert(hackney_cidr:contains(Block, {8193, 43981, 0, 0, 0, 0, 0, 1})),
     ?_assert(hackney_cidr:contains(Block, {8193, 43981, 8191, 65535, 65535, 65535, 65535, 65534})),
     ?_assert(hackney_cidr:contains(Block, {8193, 43981, 8191, 65535, 65535, 65535, 65535, 65535})),
     ?_assertNot(hackney_cidr:contains(Block, {8193, 43981, 8192, 65535, 65535, 65535, 65535, 65535})),
     ?_assertNot(hackney_cidr:contains(Block, {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535}))].

ipv6_contains_cidr_test_() ->
    Block = hackney_cidr:parse("2001:abcd::/32"),
    [?_assert(hackney_cidr:contains(Block, Block)),
     ?_assert(hackney_cidr:contains(Block, hackney_cidr:parse("2001:abcd:1:1:1:1:1:1/128"))),
     ?_assert(hackney_cidr:contains(Block, hackney_cidr:parse("2001:abcd:2000::/35"))),
     ?_assert(hackney_cidr:contains(Block, hackney_cidr:parse("2001:abcd:2000:1:1:1::/96"))),
     ?_assertNot(hackney_cidr:contains(Block, hackney_cidr:parse("2002:abcd::/35"))),
     ?_assertNot(hackney_cidr:contains(Block, hackney_cidr:parse("2002:abcd:1:1:1:1:1:1/128"))),
     ?_assertNot(hackney_cidr:contains(Block, hackney_cidr:parse("2001:ffff::/35")))].

usort_cidrs_test_() ->
    [?_assertEqual([], hackney_cidr:usort_cidrs([])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/8")],
                    hackney_cidr:usort_cidrs([hackney_cidr:parse("10.0.0.0/8")])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/8")],
                    hackney_cidr:usort_cidrs([hackney_cidr:parse("10.0.0.0/8"),
                                           hackney_cidr:parse("10.0.0.0/8")])),
     ?_assertEqual([hackney_cidr:parse("2001:abcd::/32")],
                    hackney_cidr:usort_cidrs([hackney_cidr:parse("2001:abcd::/32")])),
     ?_assertEqual([hackney_cidr:parse("2001:abcd::/32")],
                    hackney_cidr:usort_cidrs([hackney_cidr:parse("2001:abcd::/32"),
                                           hackney_cidr:parse("2001:abcd::/32")])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/8"),
                    hackney_cidr:parse("2001:abcd::/32")],
                    hackney_cidr:usort_cidrs([hackney_cidr:parse("2001:abcd::/32"),
                                           hackney_cidr:parse("10.0.0.0/8"),
                                           hackney_cidr:parse("2001:abcd::/32"),
                                           hackney_cidr:parse("10.0.0.0/8")])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/8"),
                    hackney_cidr:parse("10.0.0.0/16"),
                    hackney_cidr:parse("10.1.0.0/16"),
                    hackney_cidr:parse("2001:abcd::/32")],
                    hackney_cidr:usort_cidrs([hackney_cidr:parse("2001:abcd::/32"),
                                           hackney_cidr:parse("10.1.0.0/16"),
                                           hackney_cidr:parse("10.0.0.0/8"),
                                           hackney_cidr:parse("10.0.0.0/16"),
                                           hackney_cidr:parse("10.0.0.0/8")])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/8"),
                    hackney_cidr:parse("2001:abcd::/32"),
                    hackney_cidr:parse("2001:abcd:1::/48"),
                    hackney_cidr:parse("2001:abcd:1::/64"),
                    hackney_cidr:parse("2001:abcd:1::1/128")],
                    hackney_cidr:usort_cidrs([hackney_cidr:parse("2001:abcd::/32"),
                                           hackney_cidr:parse("2001:abcd:1::1/128"),
                                           hackney_cidr:parse("2001:abcd:1::/64"),
                                           hackney_cidr:parse("10.0.0.0/8"),
                                           hackney_cidr:parse("2001:abcd:1::1/128"),
                                           hackney_cidr:parse("2001:abcd:1::/48"),
                                           hackney_cidr:parse("10.0.0.0/8"),
                                           hackney_cidr:parse("2001:abcd:1::/64")]))].

merge_cidrs_test_() ->
    [?_assertEqual([], hackney_cidr:merge_cidrs([])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/16"),
                    hackney_cidr:parse("10.10.0.0/16")],
                   hackney_cidr:merge_cidrs([hackney_cidr:parse("10.10.0.0/16"),
                                          hackney_cidr:parse("10.0.0.1/32"),
                                          hackney_cidr:parse("10.10.99.0/24"),
                                          hackney_cidr:parse("10.0.99.0/24"),
                                          hackney_cidr:parse("10.0.0.0/16")])),
     ?_assertEqual([hackney_cidr:parse("10.0.1.1/32"),
                    hackney_cidr:parse("10.0.1.2/31"),
                    hackney_cidr:parse("10.0.1.4/30")],
                   hackney_cidr:merge_cidrs([hackney_cidr:parse("10.0.1.4/30"),
                                          hackney_cidr:parse("10.0.1.4/32"),
                                          hackney_cidr:parse("10.0.1.2/31"),
                                          hackney_cidr:parse("10.0.1.1/32"),
                                          hackney_cidr:parse("10.0.1.3/32"),
                                          hackney_cidr:parse("10.0.1.4/30")])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/16")],
                   hackney_cidr:merge_cidrs([hackney_cidr:parse("10.0.1.0/24"),
                                          hackney_cidr:parse("10.0.254.0/24"),
                                          hackney_cidr:parse("10.0.0.0/16"),
                                          hackney_cidr:parse("10.0.100.99/32"),
                                          hackney_cidr:parse("10.0.0.0/16")])),
     ?_assertEqual([hackney_cidr:parse("10.0.0.0/8"),
                    hackney_cidr:parse("2001:abcd::/32")],
                    hackney_cidr:merge_cidrs([hackney_cidr:parse("2001:abcd::/32"),
                                           hackney_cidr:parse("2001:abcd:1::1/128"),
                                           hackney_cidr:parse("2001:abcd:1::/64"),
                                           hackney_cidr:parse("10.0.0.0/8"),
                                           hackney_cidr:parse("2001:abcd:1::1/128"),
                                           hackney_cidr:parse("2001:abcd:1::/48"),
                                           hackney_cidr:parse("10.0.0.0/8"),
                                           hackney_cidr:parse("2001:abcd:1::/64")])),
     ?_assertEqual([hackney_cidr:parse("2001:abcd::/32")],
                   hackney_cidr:merge_cidrs([hackney_cidr:parse("2001:abcd:abcd::/48"),
                                          hackney_cidr:parse("2001:abcd:1234::/48"),
                                          hackney_cidr:parse("2001:abcd:9999::/48"),
                                          hackney_cidr:parse("2001:abcd::/32"),
                                          hackney_cidr:parse("2001:abcd:abcd::1/128"),
                                          hackney_cidr:parse("2001:abcd:abcd::4/126"),
                                          hackney_cidr:parse("2001:abcd:abcd:0:0:abcd::/96")]))].

is_ipv4_test_() ->
    {ok, Addr} = inet:parse_address("2001::abcd"),
    [?_assert(hackney_cidr:is_ipv4({192, 168, 0, 0})),
     ?_assertNot(hackney_cidr:is_ipv4({192, 168, 0, 256})),
     ?_assertNot(hackney_cidr:is_ipv4({192, 168, 0})),
     ?_assertNot(hackney_cidr:is_ipv4({192, 168, 0, 0, 0})),
     ?_assertNot(hackney_cidr:is_ipv4(Addr))].

is_ipv6_test_() ->
    [?_assert(hackney_cidr:is_ipv6({8193, 43981, 0, 0, 0, 0, 0, 0})),
     ?_assertNot(hackney_cidr:is_ipv6({192, 168, 0, 0})),
     ?_assertNot(hackney_cidr:is_ipv6({8193, 43981, 0, 0, 0, 0, 0, 70000})),
     ?_assertNot(hackney_cidr:is_ipv6({8193, 43981, 0, 0, 0, 0, 0})),
     ?_assertNot(hackney_cidr:is_ipv6({8193, 43981, 0, 0, 0, 0, 0, 0, 0}))].

ipv4_ip_lte_test_() ->
    [?_assert(hackney_cidr:ip_lte({0, 0, 0, 0}, {0, 0, 0, 0})),
     ?_assert(hackney_cidr:ip_lte({255, 255, 255, 255}, {255, 255, 255, 255})),
     ?_assert(hackney_cidr:ip_lte({192, 168, 1, 1}, {192, 168, 1, 1})),
     ?_assert(hackney_cidr:ip_lte({192, 168, 1, 1}, {192, 168, 1, 2})),
     ?_assert(hackney_cidr:ip_lte({192, 168, 1, 1}, {192, 168, 2, 1})),
     ?_assert(hackney_cidr:ip_lte({192, 168, 1, 1}, {192, 169, 1, 1})),
     ?_assert(hackney_cidr:ip_lte({192, 168, 1, 1}, {193, 168, 1, 1})),
     ?_assertNot(hackney_cidr:ip_lte({192, 168, 1, 1}, {192, 168, 1, 0})),
     ?_assertNot(hackney_cidr:ip_lte({192, 168, 1, 1}, {192, 168, 0, 1})),
     ?_assertNot(hackney_cidr:ip_lte({192, 168, 1, 1}, {192, 167, 1, 1})),
     ?_assertNot(hackney_cidr:ip_lte({192, 168, 1, 1}, {191, 168, 1, 1}))].

ipv6_ip_lte_test_() ->
    [?_assert(hackney_cidr:ip_lte({0, 0, 0, 0, 0, 0, 0, 0},
                               {0, 0, 0, 0, 0, 0, 0, 0})),
     ?_assert(hackney_cidr:ip_lte({65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
                               {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535})),
     ?_assert(hackney_cidr:ip_lte({0, 0, 0, 0, 0, 0, 0, 0},
                               {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 4, 3, 2, 2})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 4, 3, 3, 1})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 4, 4, 2, 1})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 5, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 6, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 7, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43982, 6, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8194, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
                                  {8193, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 2},
                                  {8193, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 5, 4, 3, 1, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 5, 4, 2, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 5, 3, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 4, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 5, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43980, 6, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_lte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8192, 43981, 6, 5, 4, 3, 2, 1}))].

ipv4_ip_gte_test_() ->
    [?_assert(hackney_cidr:ip_gte({0, 0, 0, 0}, {0, 0, 0, 0})),
     ?_assert(hackney_cidr:ip_gte({255, 255, 255, 255}, {255, 255, 255, 255})),
     ?_assert(hackney_cidr:ip_gte({192, 168, 1, 1}, {192, 168, 1, 1})),
     ?_assert(hackney_cidr:ip_gte({192, 168, 1, 1}, {192, 168, 1, 0})),
     ?_assert(hackney_cidr:ip_gte({192, 168, 1, 1}, {192, 168, 0, 1})),
     ?_assert(hackney_cidr:ip_gte({192, 168, 1, 1}, {192, 167, 1, 1})),
     ?_assert(hackney_cidr:ip_gte({192, 168, 1, 1}, {191, 168, 1, 1})),
     ?_assertNot(hackney_cidr:ip_gte({192, 168, 1, 1}, {192, 168, 1, 2})),
     ?_assertNot(hackney_cidr:ip_gte({192, 168, 1, 1}, {192, 168, 2, 1})),
     ?_assertNot(hackney_cidr:ip_gte({192, 168, 1, 1}, {192, 169, 1, 1})),
     ?_assertNot(hackney_cidr:ip_gte({192, 168, 1, 1}, {193, 168, 1, 1}))].

ipv6_ip_gte_test_() ->
    [?_assert(hackney_cidr:ip_gte({0, 0, 0, 0, 0, 0, 0, 0},
                               {0, 0, 0, 0, 0, 0, 0, 0})),
     ?_assert(hackney_cidr:ip_gte({65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
                               {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535})),
     ?_assert(hackney_cidr:ip_gte({65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
                               {8193, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 2},
                               {8193, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 4, 3, 1, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 4, 2, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 5, 3, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 6, 4, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43981, 5, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8193, 43980, 6, 5, 4, 3, 2, 1})),
     ?_assert(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                               {8192, 43981, 6, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_gte({0, 0, 0, 0, 0, 0, 0, 0},
                                  {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 5, 4, 3, 2, 2})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 5, 4, 3, 3, 1})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 5, 4, 4, 2, 1})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 5, 5, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 6, 6, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43981, 7, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8193, 43982, 6, 5, 4, 3, 2, 1})),
     ?_assertNot(hackney_cidr:ip_gte({8193, 43981, 6, 5, 4, 3, 2, 1},
                                  {8194, 43981, 6, 5, 4, 3, 2, 1}))].
