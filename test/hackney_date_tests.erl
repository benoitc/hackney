-module(hackney_date_tests).
-include_lib("eunit/include/eunit.hrl").

parse_date_2109_test() ->
    BDate     = <<"Sat, 28-Dec-2013 16:27:30 GMT">>,
    TDate     = {{2013,12,28},{16,27,30}},
    ?assertEqual(hackney_date:date_to_rfc2109(TDate),    BDate).


parse_over_http_test() ->
    BDate2109 = <<"Mon, 13-Jan-2014 00:00:00 GMT">>,
    BDate1123 = <<"Mon, 13 Jan 2014 00:00:00 GMT">>,
    BDate850  = <<"Monday, 13-Jan-14 00:00:00 GMT">>,
    BAscTime  = <<"Mon Jan 13 00:00:00 2014">>,
    Berr1     = <<"Mon, 13-Jan-2014 00:00:00">>,
    TDate     = {{2014,01,13},{00,00,00}},
    ?assertEqual(hackney_date:parse_http_date(BDate2109), TDate),
    ?assertEqual(hackney_date:parse_http_date(BDate1123), TDate),
    ?assertEqual(hackney_date:parse_http_date(BDate850),  TDate),
    ?assertEqual(hackney_date:parse_http_date(BAscTime),  TDate),
    ?assertEqual(hackney_date:parse_http_date(
		   hackney_date:rfc2109_to_date(Berr1)),{error,badarg}).

parse_2109_date_test() ->
    BDate     = <<"Mon, 13-Jan-2014 00:00:00 GMT">>,
    Berr1     = <<"Mon, 13-Jan-2014 00:00:00">>,
    TDate     = {{2014,01,13},{00,00,00}},
    ?assertEqual(hackney_date:rfc2109_to_date(BDate),  TDate),
    ?assertEqual(hackney_date:rfc2109_to_date(Berr1),  {error,badarg}).



parse_1123_date_test() ->
    BDate     = <<"Sat, 28 Dec 2013 16:27:30 GMT">>,
    Berr1     = <<"Sat, 28 Dec 2013 16:27:30">>,
    TDate     = {{2013,12,28},{16,27,30}},
    ?assertEqual(hackney_date:rfc1123_to_date(BDate),  TDate),
    ?assertEqual(hackney_date:rfc1123_to_date(Berr1),  {error,badarg}).


parse_850_date_test() ->
    BDate850  = <<"Saturday, 28-Dec-13 16:27:30 GMT">>,
    BErr  = <<"Bla, 28-Dec-13 16:27:30 GMT">>,
    TDate     = {{2013,12,28},{16,27,30}},
    ?assertEqual(hackney_date:rfc850_to_date(BErr),  {error,badarg}),
    ?assertEqual(hackney_date:rfc850_to_date(BDate850),  TDate).

parse_850_wday_test() ->
    BMon  = <<"Monday, 01-Jan-13 00:00:00 GMT">>,
    BTue  = <<"Tuesday, 01-Jan-13 00:00:00 GMT">>,
    BWed  = <<"Wednesday, 01-Jan-13 00:00:00 GMT">>,
    BThu  = <<"Thursday, 01-Jan-13 00:00:00 GMT">>,
    BFri  = <<"Friday, 01-Jan-13 00:00:00 GMT">>,
    BSat  = <<"Saturday, 01-Jan-13 00:00:00 GMT">>,
    BSun  = <<"Sunday, 01-Jan-13 00:00:00 GMT">>,
    TDate     = {{2013,01,01},{00,00,00}},
    ?assertEqual(hackney_date:rfc850_to_date(BMon),  TDate),
    ?assertEqual(hackney_date:rfc850_to_date(BTue),  TDate),
    ?assertEqual(hackney_date:rfc850_to_date(BWed),  TDate),
    ?assertEqual(hackney_date:rfc850_to_date(BThu),  TDate),
    ?assertEqual(hackney_date:rfc850_to_date(BFri),  TDate),
    ?assertEqual(hackney_date:rfc850_to_date(BSat),  TDate),
    ?assertEqual(hackney_date:rfc850_to_date(BSun),  TDate).

parse_asctime_date_test() ->
    BAscTime  = <<"Sat Dec 28 16:27:30 2013">>,
    TDate     = {{2013,12,28},{16,27,30}},
    ?assertEqual(hackney_date:asctime_to_date(BAscTime), TDate).

cover_years_test() ->
    T = [{{Y,01,01},{00,00,00}} || Y <- lists:seq(1970,2029)],
    BD = [hackney_date:date_to_rfc2109(TS)|| TS <- T],
    TT = [hackney_date:rfc2109_to_date(B) || B <- BD],
    ?assertEqual(T,TT).


cover_month_test() ->
    T = [{{2011,M,01},{00,00,00}} || M <- lists:seq(1,12)],
    BD = [hackney_date:date_to_rfc2109(TS)|| TS <- T],
    TT = [hackney_date:rfc2109_to_date(B) || B <- BD],
    ?assertEqual(T,TT).

cover_day_test() ->
    T = [{{2011,01,D},{00,00,00}} || D <- lists:seq(1,31)],
    BD = [hackney_date:date_to_rfc2109(TS)|| TS <- T],
    TT = [hackney_date:rfc2109_to_date(B) || B <- BD],
    ?assertEqual(T,TT).

cover_pad_test() ->
    T = [{{2011,01,01},{00,M,00}} || M <- lists:seq(0,59)],
    BD = [hackney_date:date_to_rfc2109(TS)|| TS <- T],
    TT = [hackney_date:rfc2109_to_date(B) || B <- BD],
    ?assertEqual(T,TT).
