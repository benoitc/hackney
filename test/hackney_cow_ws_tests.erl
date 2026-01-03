%%% -*- erlang -*-
%%%
%%% Tests for vendored hackney_cow_ws module

-module(hackney_cow_ws_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Key generation tests
%%====================================================================

key_test_() ->
    [
     {"key generates 24 character base64 string",
      fun() ->
          Key = hackney_cow_ws:key(),
          ?assertEqual(24, byte_size(Key)),
          %% Verify it's valid base64
          ?assertEqual(16, byte_size(base64:decode(Key)))
      end},
     {"key generates unique values",
      fun() ->
          Key1 = hackney_cow_ws:key(),
          Key2 = hackney_cow_ws:key(),
          ?assertNotEqual(Key1, Key2)
      end}
    ].

encode_key_test_() ->
    [
     {"encode_key produces correct accept value",
      fun() ->
          %% RFC 6455 example
          Key = <<"dGhlIHNhbXBsZSBub25jZQ==">>,
          Expected = <<"s3pPLMBiTxaQ9kYGzzhZRbK+xOo=">>,
          ?assertEqual(Expected, hackney_cow_ws:encode_key(Key))
      end}
    ].

%%====================================================================
%% Frame building tests
%%====================================================================

frame_test_() ->
    [
     {"text frame without compression",
      fun() ->
          Frame = hackney_cow_ws:frame({text, <<"Hello">>}, #{}),
          ?assertMatch([<<_:8, _/binary>>, <<"Hello">>], Frame)
      end},
     {"binary frame without compression",
      fun() ->
          Frame = hackney_cow_ws:frame({binary, <<1,2,3>>}, #{}),
          ?assertMatch([<<_:8, _/binary>>, <<1,2,3>>], Frame)
      end},
     {"ping frame",
      fun() ->
          Frame = hackney_cow_ws:frame(ping, #{}),
          ?assertMatch(<<_:16>>, iolist_to_binary(Frame))
      end},
     {"pong frame",
      fun() ->
          Frame = hackney_cow_ws:frame(pong, #{}),
          ?assertMatch(<<_:16>>, iolist_to_binary(Frame))
      end},
     {"close frame",
      fun() ->
          Frame = hackney_cow_ws:frame(close, #{}),
          ?assertMatch(<<_:16>>, iolist_to_binary(Frame))
      end},
     {"close frame with code and reason",
      fun() ->
          Frame = hackney_cow_ws:frame({close, 1000, <<"goodbye">>}, #{}),
          Bin = iolist_to_binary(Frame),
          ?assert(byte_size(Bin) > 2)
      end}
    ].

masked_frame_test_() ->
    [
     {"masked text frame",
      fun() ->
          Frame = hackney_cow_ws:masked_frame({text, <<"Hello">>}, #{}),
          Bin = iolist_to_binary(Frame),
          %% First byte: FIN=1, RSV=0, opcode=1 (text)
          <<Fin:1, _Rsv:3, Opcode:4, _Rest/binary>> = Bin,
          ?assertEqual(1, Fin),
          ?assertEqual(1, Opcode),
          %% Second byte should have mask bit set
          <<_:8, Mask:1, _Len:7, _/binary>> = Bin,
          ?assertEqual(1, Mask)
      end},
     {"masked binary frame",
      fun() ->
          Frame = hackney_cow_ws:masked_frame({binary, <<1,2,3>>}, #{}),
          Bin = iolist_to_binary(Frame),
          <<Fin:1, _Rsv:3, Opcode:4, _Rest/binary>> = Bin,
          ?assertEqual(1, Fin),
          ?assertEqual(2, Opcode)
      end}
    ].

%%====================================================================
%% Frame parsing tests
%%====================================================================

parse_header_test_() ->
    [
     {"parse unmasked text frame header",
      fun() ->
          %% FIN=1, RSV=0, opcode=1 (text), MASK=0, len=5
          Header = <<1:1, 0:3, 1:4, 0:1, 5:7, "Hello">>,
          Result = hackney_cow_ws:parse_header(Header, #{}, undefined),
          ?assertMatch({text, undefined, <<0:3>>, 5, undefined, <<"Hello">>}, Result)
      end},
     {"parse masked text frame header",
      fun() ->
          MaskKey = 16#12345678,
          %% FIN=1, RSV=0, opcode=1 (text), MASK=1, len=5, mask key, data
          Header = <<1:1, 0:3, 1:4, 1:1, 5:7, MaskKey:32, "Hello">>,
          Result = hackney_cow_ws:parse_header(Header, #{}, undefined),
          ?assertMatch({text, undefined, <<0:3>>, 5, MaskKey, <<"Hello">>}, Result)
      end},
     {"parse ping frame header",
      fun() ->
          Header = <<1:1, 0:3, 9:4, 0:1, 0:7>>,
          Result = hackney_cow_ws:parse_header(Header, #{}, undefined),
          ?assertMatch({ping, undefined, <<0:3>>, 0, undefined, <<>>}, Result)
      end},
     {"parse close frame header",
      fun() ->
          Header = <<1:1, 0:3, 8:4, 0:1, 0:7>>,
          Result = hackney_cow_ws:parse_header(Header, #{}, undefined),
          ?assertMatch({close, undefined, <<0:3>>, 0, undefined, <<>>}, Result)
      end},
     {"need more data returns more",
      fun() ->
          %% Incomplete header
          Header = <<1:1, 0:3>>,
          Result = hackney_cow_ws:parse_header(Header, #{}, undefined),
          ?assertEqual(more, Result)
      end},
     {"invalid rsv without extension returns error",
      fun() ->
          %% RSV1 set without extension
          Header = <<1:1, 1:1, 0:2, 1:4, 0:1, 5:7, "Hello">>,
          Result = hackney_cow_ws:parse_header(Header, #{}, undefined),
          ?assertEqual(error, Result)
      end}
    ].

%%====================================================================
%% make_frame tests
%%====================================================================

make_frame_test_() ->
    [
     {"make text frame",
      fun() ->
          Result = hackney_cow_ws:make_frame(text, <<"Hello">>, undefined, undefined),
          ?assertEqual({text, <<"Hello">>}, Result)
      end},
     {"make binary frame",
      fun() ->
          Result = hackney_cow_ws:make_frame(binary, <<1,2,3>>, undefined, undefined),
          ?assertEqual({binary, <<1,2,3>>}, Result)
      end},
     {"make close frame without code",
      fun() ->
          Result = hackney_cow_ws:make_frame(close, <<>>, undefined, undefined),
          ?assertEqual(close, Result)
      end},
     {"make close frame with code",
      fun() ->
          Result = hackney_cow_ws:make_frame(close, <<"goodbye">>, 1000, undefined),
          ?assertEqual({close, 1000, <<"goodbye">>}, Result)
      end},
     {"make ping frame",
      fun() ->
          Result = hackney_cow_ws:make_frame(ping, <<>>, undefined, undefined),
          ?assertEqual(ping, Result)
      end},
     {"make pong frame",
      fun() ->
          Result = hackney_cow_ws:make_frame(pong, <<"data">>, undefined, undefined),
          ?assertEqual({pong, <<"data">>}, Result)
      end}
    ].
