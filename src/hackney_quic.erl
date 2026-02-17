%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc QUIC/HTTP3 transport using pure Erlang QUIC library.
%%%
%%% This module wraps the `quic' application to provide HTTP/3 support.
%%% It handles:
%%% - QUIC connection management
%%% - HTTP/3 framing (HEADERS, DATA frames)
%%% - QPACK header compression
%%%
%%% == Messages ==
%%%
%%% Messages sent to owner process:
%%% <ul>
%%%   <li>`{quic, ConnRef, {connected, Info}}' - Connection established</li>
%%%   <li>`{quic, ConnRef, {stream_headers, StreamId, Headers, Fin}}' - Headers received</li>
%%%   <li>`{quic, ConnRef, {stream_data, StreamId, Bin, Fin}}' - Data received</li>
%%%   <li>`{quic, ConnRef, {stream_reset, StreamId, ErrorCode}}' - Stream reset</li>
%%%   <li>`{quic, ConnRef, {closed, Reason}}' - Connection closed</li>
%%%   <li>`{quic, ConnRef, {transport_error, Code, Reason}}' - Transport error</li>
%%% </ul>
%%%
%%% @end

-module(hackney_quic).

-behaviour(gen_server).

%% Suppress dialyzer warnings due to incomplete type specs in quic library
-dialyzer({nowarn_function, [
    init/1,
    ensure_table/0,
    register_conn/2,
    handle_info/2,
    setup_h3_streams/1
]}).
-dialyzer({no_match, [init/1, process_h3_frames/5]}).

%% API
-export([
    connect/4,
    close/2,
    open_stream/1,
    send_headers/4,
    send_data/4,
    reset_stream/3,
    handle_timeout/2,
    process/1,
    peername/1,
    sockname/1,
    setopts/2,
    get_fd/1,
    is_available/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    quic_conn :: reference() | undefined,
    owner :: pid(),
    owner_mon :: reference(),
    streams = #{} :: #{non_neg_integer() => stream_state()},
    uni_streams = #{} :: #{non_neg_integer() => uni_stream_info()},
    next_stream_id = 0 :: non_neg_integer(),
    control_stream :: non_neg_integer() | undefined,
    qpack_encoder_stream :: non_neg_integer() | undefined,
    qpack_decoder_stream :: non_neg_integer() | undefined,
    peer_control_stream :: non_neg_integer() | undefined,
    peer_qpack_encoder :: non_neg_integer() | undefined,
    peer_qpack_decoder :: non_neg_integer() | undefined,
    settings_sent = false :: boolean(),
    settings_received = false :: boolean(),
    host :: binary(),
    port :: inet:port_number()
}).

-record(uni_stream_info, {
    type :: control | push | qpack_encoder | qpack_decoder | unknown,
    buffer = <<>> :: binary()
}).

-record(stream_state, {
    buffer = <<>> :: binary(),
    headers_received = false :: boolean(),
    headers = [] :: [{binary(), binary()}],
    fin_received = false :: boolean()
}).

-type stream_state() :: #stream_state{}.
-type uni_stream_info() :: #uni_stream_info{}.

%% HTTP/3 frame types (RFC 9114)
-define(H3_DATA, 16#00).
-define(H3_HEADERS, 16#01).
-define(H3_CANCEL_PUSH, 16#03).
-define(H3_SETTINGS, 16#04).
-define(H3_PUSH_PROMISE, 16#05).
-define(H3_GOAWAY, 16#07).
-define(H3_MAX_PUSH_ID, 16#0D).

%% HTTP/3 unidirectional stream types (RFC 9114)
-define(H3_STREAM_CONTROL, 16#00).
-define(H3_STREAM_PUSH, 16#01).
-define(H3_STREAM_QPACK_ENCODER, 16#02).
-define(H3_STREAM_QPACK_DECODER, 16#03).

%%====================================================================
%% API
%%====================================================================

%% @doc Check if QUIC/HTTP3 support is available.
%% Always returns true as pure Erlang implementation is always available.
-spec is_available() -> true.
is_available() -> true.

%% @doc Get the file descriptor from a gen_udp socket.
%% Not needed for pure Erlang implementation, kept for API compatibility.
-spec get_fd(gen_udp:socket()) -> {ok, integer()} | {error, term()}.
get_fd(Socket) ->
    inet:getfd(Socket).

%% @doc Connect to a QUIC/HTTP3 server.
-spec connect(Host, Port, Opts, Owner) -> {ok, reference()} | {error, term()}
    when Host :: binary() | string(),
         Port :: inet:port_number(),
         Opts :: map(),
         Owner :: pid().
connect(Host, Port, Opts, Owner) when is_list(Host) ->
    connect(list_to_binary(Host), Port, Opts, Owner);
connect(Host, Port, Opts, Owner) when is_binary(Host), is_integer(Port),
                                       Port > 0, Port =< 65535,
                                       is_map(Opts), is_pid(Owner) ->
    %% Start a gen_server to manage this connection
    case gen_server:start(?MODULE, {Host, Port, Opts, Owner}, []) of
        {ok, Pid} ->
            %% Return the connection reference (same as the underlying QUIC ref)
            gen_server:call(Pid, get_conn_ref);
        {error, _} = Error ->
            Error
    end;
connect(_Host, _Port, _Opts, _Owner) ->
    {error, badarg}.

%% @doc Close a QUIC connection.
-spec close(ConnRef, Reason) -> ok
    when ConnRef :: reference(),
         Reason :: term().
close(ConnRef, Reason) ->
    case get_conn_pid(ConnRef) of
        {ok, Pid} ->
            gen_server:cast(Pid, {close, Reason});
        error ->
            ok
    end.

%% @doc Open a new bidirectional stream.
-spec open_stream(ConnRef) -> {ok, non_neg_integer()} | {error, term()}
    when ConnRef :: reference().
open_stream(ConnRef) ->
    case get_conn_pid(ConnRef) of
        {ok, Pid} ->
            gen_server:call(Pid, open_stream);
        error ->
            {error, not_connected}
    end.

%% @doc Send HTTP/3 headers on a stream.
-spec send_headers(ConnRef, StreamId, Headers, Fin) -> ok | {error, term()}
    when ConnRef :: reference(),
         StreamId :: non_neg_integer(),
         Headers :: [{binary(), binary()}],
         Fin :: boolean().
send_headers(ConnRef, StreamId, Headers, Fin) when is_list(Headers), is_boolean(Fin) ->
    case get_conn_pid(ConnRef) of
        {ok, Pid} ->
            gen_server:call(Pid, {send_headers, StreamId, Headers, Fin});
        error ->
            {error, not_connected}
    end;
send_headers(_ConnRef, _StreamId, _Headers, _Fin) ->
    {error, badarg}.

%% @doc Send data on a stream.
-spec send_data(ConnRef, StreamId, Data, Fin) -> ok | {error, term()}
    when ConnRef :: reference(),
         StreamId :: non_neg_integer(),
         Data :: iodata(),
         Fin :: boolean().
send_data(ConnRef, StreamId, Data, Fin) when is_boolean(Fin) ->
    case get_conn_pid(ConnRef) of
        {ok, Pid} ->
            gen_server:call(Pid, {send_data, StreamId, Data, Fin});
        error ->
            {error, not_connected}
    end;
send_data(_ConnRef, _StreamId, _Data, _Fin) ->
    {error, badarg}.

%% @doc Reset a stream with an error code.
-spec reset_stream(ConnRef, StreamId, ErrorCode) -> ok | {error, term()}
    when ConnRef :: reference(),
         StreamId :: non_neg_integer(),
         ErrorCode :: non_neg_integer().
reset_stream(ConnRef, StreamId, ErrorCode) when is_integer(ErrorCode), ErrorCode >= 0 ->
    case get_conn_pid(ConnRef) of
        {ok, Pid} ->
            gen_server:call(Pid, {reset_stream, StreamId, ErrorCode});
        error ->
            {error, not_connected}
    end;
reset_stream(_ConnRef, _StreamId, _ErrorCode) ->
    {error, badarg}.

%% @doc Handle connection timeout.
-spec handle_timeout(ConnRef, NowMs) -> non_neg_integer() | infinity
    when ConnRef :: reference(),
         NowMs :: non_neg_integer().
handle_timeout(_ConnRef, _NowMs) ->
    %% Timeouts are handled internally by the quic library
    infinity.

%% @doc Process pending QUIC events.
-spec process(ConnRef) -> non_neg_integer() | infinity
    when ConnRef :: reference().
process(_ConnRef) ->
    %% Events are handled via messages, no explicit processing needed
    infinity.

%% @doc Get the remote address of the connection.
-spec peername(ConnRef) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}
    when ConnRef :: reference().
peername(ConnRef) ->
    case get_conn_pid(ConnRef) of
        {ok, Pid} ->
            gen_server:call(Pid, peername);
        error ->
            {error, not_connected}
    end.

%% @doc Get the local address of the connection.
-spec sockname(ConnRef) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}
    when ConnRef :: reference().
sockname(ConnRef) ->
    case get_conn_pid(ConnRef) of
        {ok, Pid} ->
            gen_server:call(Pid, sockname);
        error ->
            {error, not_connected}
    end.

%% @doc Set connection options.
-spec setopts(ConnRef, Opts) -> ok | {error, term()}
    when ConnRef :: reference(),
         Opts :: [{atom(), term()}].
setopts(_ConnRef, _Opts) ->
    %% Options not currently supported
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Host, Port, Opts, Owner}) ->
    %% Monitor the owner
    MonRef = erlang:monitor(process, Owner),

    %% ALPN for HTTP/3
    QuicOpts = Opts#{
        alpn => [<<"h3">>],
        verify => maps:get(verify, Opts, false)
    },

    %% Connect using the pure Erlang QUIC library
    case quic:connect(binary_to_list(Host), Port, QuicOpts, self()) of
        {ok, QuicConn} ->
            %% Register this connection
            register_conn(QuicConn, self()),
            {ok, #state{
                quic_conn = QuicConn,
                owner = Owner,
                owner_mon = MonRef,
                host = Host,
                port = Port
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_conn_ref, _From, #state{quic_conn = ConnRef} = State) ->
    {reply, {ok, ConnRef}, State};

handle_call(open_stream, _From, #state{quic_conn = QuicConn, streams = Streams} = State) ->
    case quic:open_stream(QuicConn) of
        {ok, StreamId} ->
            NewStreams = maps:put(StreamId, #stream_state{}, Streams),
            {reply, {ok, StreamId}, State#state{streams = NewStreams}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({send_headers, StreamId, Headers, Fin}, _From,
            #state{quic_conn = QuicConn} = State) ->
    %% Encode headers using QPACK
    EncodedHeaders = hackney_qpack:encode(Headers),

    %% Wrap in HTTP/3 HEADERS frame
    Frame = encode_h3_frame(?H3_HEADERS, EncodedHeaders),

    %% Send on the QUIC stream
    Result = quic:send_data(QuicConn, StreamId, Frame, Fin),
    {reply, Result, State};

handle_call({send_data, StreamId, Data, Fin}, _From,
            #state{quic_conn = QuicConn} = State) ->
    %% Wrap in HTTP/3 DATA frame
    DataBin = iolist_to_binary(Data),
    Frame = encode_h3_frame(?H3_DATA, DataBin),

    %% Send on the QUIC stream
    Result = quic:send_data(QuicConn, StreamId, Frame, Fin),
    {reply, Result, State};

handle_call({reset_stream, StreamId, ErrorCode}, _From,
            #state{quic_conn = QuicConn, streams = Streams} = State) ->
    %% Reset the stream
    Result = quic:reset_stream(QuicConn, StreamId, ErrorCode),
    NewStreams = maps:remove(StreamId, Streams),
    {reply, Result, State#state{streams = NewStreams}};

handle_call(peername, _From, #state{quic_conn = QuicConn} = State) ->
    Result = quic:peername(QuicConn),
    {reply, Result, State};

handle_call(sockname, _From, #state{quic_conn = QuicConn} = State) ->
    Result = quic:sockname(QuicConn),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({close, Reason}, #state{quic_conn = QuicConn} = State) ->
    quic:close(QuicConn, Reason),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({quic, QuicConn, {connected, Info}},
            #state{quic_conn = QuicConn, owner = Owner} = State) ->
    %% Set up HTTP/3 control streams
    State1 = setup_h3_streams(State),
    %% Forward connection established to owner
    Owner ! {quic, QuicConn, {connected, Info}},
    {noreply, State1};

handle_info({quic, QuicConn, {stream_data, StreamId, Data, Fin}},
            #state{quic_conn = QuicConn, owner = Owner, streams = Streams} = State) ->
    %% Check if this is a unidirectional stream (low 2 bits = 10 or 11)
    case is_unidirectional_stream(StreamId) of
        true ->
            %% Handle unidirectional stream (control, QPACK, etc.)
            {NewState, Messages} = process_uni_stream_data(StreamId, Data, Fin, State),
            lists:foreach(fun(Msg) -> Owner ! {quic, QuicConn, Msg} end, Messages),
            {noreply, NewState};
        false ->
            %% Bidirectional stream - parse HTTP/3 frames
            StreamState = maps:get(StreamId, Streams, #stream_state{}),
            {NewStreamState, Messages} = process_h3_data(StreamId, Data, Fin, StreamState),
            lists:foreach(fun(Msg) -> Owner ! {quic, QuicConn, Msg} end, Messages),
            NewStreams = case Fin andalso NewStreamState#stream_state.fin_received of
                true -> maps:remove(StreamId, Streams);
                false -> maps:put(StreamId, NewStreamState, Streams)
            end,
            {noreply, State#state{streams = NewStreams}}
    end;

handle_info({quic, QuicConn, {closed, Reason}},
            #state{quic_conn = QuicConn, owner = Owner} = State) ->
    Owner ! {quic, QuicConn, {closed, Reason}},
    {stop, normal, State};

handle_info({quic, QuicConn, {error, Code, Reason}},
            #state{quic_conn = QuicConn, owner = Owner} = State) ->
    Owner ! {quic, QuicConn, {transport_error, Code, Reason}},
    {stop, {error, Code}, State};

handle_info({'DOWN', MonRef, process, _Pid, _Reason},
            #state{owner_mon = MonRef, quic_conn = QuicConn} = State) ->
    %% Owner died, close connection
    quic:close(QuicConn, owner_died),
    {stop, normal, State};

handle_info({select, _Resource, _Ref, ready_input} = Msg, #state{quic_conn = QuicConn} = State) ->
    %% Socket ready for reading - forward to QUIC connection
    QuicPid = case quic_connection:lookup(QuicConn) of
        {ok, Pid} -> Pid;
        error -> undefined
    end,
    case QuicPid of
        undefined -> ok;
        _ -> QuicPid ! Msg
    end,
    {noreply, State};

handle_info({quic_timer, _} = Msg, #state{quic_conn = QuicConn} = State) ->
    %% Timer expired - forward to QUIC connection
    QuicPid = case quic_connection:lookup(QuicConn) of
        {ok, Pid} -> Pid;
        error -> undefined
    end,
    case QuicPid of
        undefined -> ok;
        _ -> QuicPid ! Msg
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{quic_conn = QuicConn}) ->
    case QuicConn of
        undefined -> ok;
        _ ->
            unregister_conn(QuicConn),
            catch quic:close(QuicConn, shutdown)
    end,
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% Connection registry using ETS
%% Table created on first use

-define(CONN_TABLE, hackney_quic_conns).

ensure_table() ->
    case ets:whereis(?CONN_TABLE) of
        undefined ->
            try
                ets:new(?CONN_TABLE, [named_table, public, set, {read_concurrency, true}])
            catch
                error:badarg ->
                    %% Table already exists (race condition)
                    ok
            end;
        _ ->
            ok
    end.

register_conn(ConnRef, Pid) ->
    ensure_table(),
    ets:insert(?CONN_TABLE, {ConnRef, Pid}).

unregister_conn(ConnRef) ->
    case ets:whereis(?CONN_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?CONN_TABLE, ConnRef)
    end.

get_conn_pid(ConnRef) ->
    case ets:whereis(?CONN_TABLE) of
        undefined ->
            error;
        _ ->
            case ets:lookup(?CONN_TABLE, ConnRef) of
                [{ConnRef, Pid}] -> {ok, Pid};
                [] -> error
            end
    end.

%% Encode HTTP/3 frame
encode_h3_frame(Type, Payload) ->
    TypeEnc = encode_varint(Type),
    LenEnc = encode_varint(byte_size(Payload)),
    <<TypeEnc/binary, LenEnc/binary, Payload/binary>>.

%% QUIC variable-length integer encoding
encode_varint(N) when N < 64 ->
    <<N>>;
encode_varint(N) when N < 16384 ->
    <<1:2, N:14>>;
encode_varint(N) when N < 1073741824 ->
    <<2:2, N:30>>;
encode_varint(N) ->
    <<3:2, N:62>>.

%% Process incoming HTTP/3 data
process_h3_data(StreamId, Data, Fin, StreamState) ->
    Buffer = <<(StreamState#stream_state.buffer)/binary, Data/binary>>,
    process_h3_frames(StreamId, Buffer, Fin, StreamState#stream_state{buffer = <<>>}, []).

process_h3_frames(StreamId, Buffer, Fin, StreamState, Messages) ->
    case decode_h3_frame(Buffer) of
        {ok, Type, Payload, Rest} ->
            {NewState, NewMsgs} = handle_h3_frame(StreamId, Type, Payload, Fin andalso Rest =:= <<>>, StreamState),
            process_h3_frames(StreamId, Rest, Fin, NewState, Messages ++ NewMsgs);
        incomplete ->
            %% Not enough data, buffer for later
            FinalState = StreamState#stream_state{
                buffer = Buffer,
                fin_received = Fin
            },
            %% If this is fin and we have no pending frames, we're done
            FinalMsgs = case Fin andalso Buffer =:= <<>> of
                true -> Messages;
                false -> Messages
            end,
            {FinalState, FinalMsgs};
        {error, _Reason} ->
            %% Parsing error, just buffer and hope for more data
            {StreamState#stream_state{buffer = Buffer, fin_received = Fin}, Messages}
    end.

decode_h3_frame(Data) ->
    case decode_varint(Data) of
        {ok, Type, Rest1} ->
            case decode_varint(Rest1) of
                {ok, Length, Rest2} when byte_size(Rest2) >= Length ->
                    <<Payload:Length/binary, Rest3/binary>> = Rest2,
                    {ok, Type, Payload, Rest3};
                {ok, _Length, _Rest2} ->
                    incomplete;
                incomplete ->
                    incomplete
            end;
        incomplete ->
            incomplete
    end.

decode_varint(<<0:2, N:6, Rest/binary>>) ->
    {ok, N, Rest};
decode_varint(<<1:2, N:14, Rest/binary>>) ->
    {ok, N, Rest};
decode_varint(<<2:2, N:30, Rest/binary>>) ->
    {ok, N, Rest};
decode_varint(<<3:2, N:62, Rest/binary>>) ->
    {ok, N, Rest};
decode_varint(_) ->
    incomplete.

handle_h3_frame(StreamId, ?H3_HEADERS, Payload, Fin, StreamState) ->
    %% Decode QPACK headers
    case hackney_qpack:decode(Payload) of
        {ok, Headers} ->
            NewState = StreamState#stream_state{
                headers_received = true,
                headers = Headers
            },
            Msg = {stream_headers, StreamId, Headers, Fin},
            {NewState, [Msg]};
        {error, _Reason} ->
            %% Failed to decode headers
            {StreamState, []}
    end;

handle_h3_frame(StreamId, ?H3_DATA, Payload, Fin, StreamState) ->
    Msg = {stream_data, StreamId, Payload, Fin},
    {StreamState, [Msg]};

handle_h3_frame(_StreamId, ?H3_SETTINGS, _Payload, _Fin, StreamState) ->
    %% Ignore SETTINGS for now
    {StreamState, []};

handle_h3_frame(_StreamId, ?H3_GOAWAY, Payload, _Fin, StreamState) ->
    %% Parse GOAWAY
    case decode_varint(Payload) of
        {ok, LastStreamId, _} ->
            Msg = {goaway, LastStreamId, 0, <<>>},
            {StreamState, [Msg]};
        _ ->
            {StreamState, []}
    end;

handle_h3_frame(_StreamId, _Type, _Payload, _Fin, StreamState) ->
    %% Unknown or reserved frame type - ignore (RFC 9114 Section 7.2.8)
    %% This includes GREASE types: (31 × N) + 21
    {StreamState, []}.

%% Check if stream is unidirectional based on stream ID
%% QUIC stream IDs: low 2 bits indicate type
%% 00 = client-initiated bidirectional
%% 01 = server-initiated bidirectional
%% 10 = client-initiated unidirectional
%% 11 = server-initiated unidirectional
is_unidirectional_stream(StreamId) ->
    (StreamId band 2) =:= 2.

%% Set up HTTP/3 control streams on connection
setup_h3_streams(#state{quic_conn = QuicConn} = State) ->
    %% Open control stream (unidirectional)
    case quic:open_unidirectional_stream(QuicConn) of
        {ok, ControlStreamId} ->
            %% Send stream type (0x00 = control)
            StreamType = encode_varint(?H3_STREAM_CONTROL),
            %% Send SETTINGS frame (empty for now)
            SettingsFrame = encode_h3_frame(?H3_SETTINGS, <<>>),
            quic:send_data(QuicConn, ControlStreamId, <<StreamType/binary, SettingsFrame/binary>>, false),

            %% Open QPACK encoder stream
            case quic:open_unidirectional_stream(QuicConn) of
                {ok, QpackEncStreamId} ->
                    EncType = encode_varint(?H3_STREAM_QPACK_ENCODER),
                    quic:send_data(QuicConn, QpackEncStreamId, EncType, false),

                    %% Open QPACK decoder stream
                    case quic:open_unidirectional_stream(QuicConn) of
                        {ok, QpackDecStreamId} ->
                            DecType = encode_varint(?H3_STREAM_QPACK_DECODER),
                            quic:send_data(QuicConn, QpackDecStreamId, DecType, false),
                            State#state{
                                control_stream = ControlStreamId,
                                qpack_encoder_stream = QpackEncStreamId,
                                qpack_decoder_stream = QpackDecStreamId,
                                settings_sent = true
                            };
                        _ ->
                            State#state{
                                control_stream = ControlStreamId,
                                qpack_encoder_stream = QpackEncStreamId,
                                settings_sent = true
                            }
                    end;
                _ ->
                    State#state{
                        control_stream = ControlStreamId,
                        settings_sent = true
                    }
            end;
        _ ->
            State
    end.

%% Process data on unidirectional streams
process_uni_stream_data(StreamId, Data, Fin, #state{uni_streams = UniStreams} = State) ->
    StreamInfo = maps:get(StreamId, UniStreams, #uni_stream_info{type = unknown}),
    Buffer = <<(StreamInfo#uni_stream_info.buffer)/binary, Data/binary>>,

    case StreamInfo#uni_stream_info.type of
        unknown ->
            %% First data on this stream - parse stream type
            case decode_varint(Buffer) of
                {ok, Type, Rest} ->
                    StreamType = stream_type_atom(Type),
                    NewInfo = #uni_stream_info{type = StreamType, buffer = Rest},
                    NewState = register_peer_stream(StreamId, StreamType, State),
                    NewState2 = NewState#state{uni_streams = maps:put(StreamId, NewInfo, UniStreams)},
                    %% Process any remaining data
                    process_uni_stream_by_type(StreamId, StreamType, Rest, Fin, NewState2);
                incomplete ->
                    %% Need more data
                    NewInfo = StreamInfo#uni_stream_info{buffer = Buffer},
                    {State#state{uni_streams = maps:put(StreamId, NewInfo, UniStreams)}, []}
            end;
        Type ->
            %% Already know the type, process data
            process_uni_stream_by_type(StreamId, Type, Buffer, Fin, State)
    end.

stream_type_atom(?H3_STREAM_CONTROL) -> control;
stream_type_atom(?H3_STREAM_PUSH) -> push;
stream_type_atom(?H3_STREAM_QPACK_ENCODER) -> qpack_encoder;
stream_type_atom(?H3_STREAM_QPACK_DECODER) -> qpack_decoder;
stream_type_atom(_) -> unknown.

register_peer_stream(StreamId, control, State) ->
    State#state{peer_control_stream = StreamId};
register_peer_stream(StreamId, qpack_encoder, State) ->
    State#state{peer_qpack_encoder = StreamId};
register_peer_stream(StreamId, qpack_decoder, State) ->
    State#state{peer_qpack_decoder = StreamId};
register_peer_stream(_StreamId, _Type, State) ->
    State.

process_uni_stream_by_type(StreamId, control, Data, _Fin, #state{uni_streams = UniStreams} = State) ->
    %% Control stream carries HTTP/3 frames (SETTINGS, GOAWAY, etc.)
    {Messages, Rest} = parse_control_frames(Data, []),
    NewInfo = #uni_stream_info{type = control, buffer = Rest},
    NewState = case Messages of
        [{settings, _}|_] -> State#state{settings_received = true};
        _ -> State
    end,
    {NewState#state{uni_streams = maps:put(StreamId, NewInfo, UniStreams)}, Messages};

process_uni_stream_by_type(StreamId, qpack_encoder, Data, _Fin, #state{uni_streams = UniStreams} = State) ->
    %% QPACK encoder stream - instructions for dynamic table
    %% For now, just buffer and ignore (we use static table only)
    NewInfo = #uni_stream_info{type = qpack_encoder, buffer = Data},
    {State#state{uni_streams = maps:put(StreamId, NewInfo, UniStreams)}, []};

process_uni_stream_by_type(StreamId, qpack_decoder, Data, _Fin, #state{uni_streams = UniStreams} = State) ->
    %% QPACK decoder stream - acknowledgments
    %% For now, just buffer and ignore
    NewInfo = #uni_stream_info{type = qpack_decoder, buffer = Data},
    {State#state{uni_streams = maps:put(StreamId, NewInfo, UniStreams)}, []};

process_uni_stream_by_type(StreamId, push, _Data, _Fin, #state{uni_streams = UniStreams} = State) ->
    %% Push stream - we don't support server push
    NewInfo = #uni_stream_info{type = push, buffer = <<>>},
    {State#state{uni_streams = maps:put(StreamId, NewInfo, UniStreams)}, []};

process_uni_stream_by_type(StreamId, unknown, Data, _Fin, #state{uni_streams = UniStreams} = State) ->
    %% Unknown stream type - ignore (RFC 9114 allows this for GREASE)
    NewInfo = #uni_stream_info{type = unknown, buffer = Data},
    {State#state{uni_streams = maps:put(StreamId, NewInfo, UniStreams)}, []}.

%% Parse frames from control stream
parse_control_frames(Data, Acc) ->
    case decode_h3_frame(Data) of
        {ok, ?H3_SETTINGS, Payload, Rest} ->
            Settings = parse_settings(Payload),
            parse_control_frames(Rest, Acc ++ [{settings, Settings}]);
        {ok, ?H3_GOAWAY, Payload, Rest} ->
            case decode_varint(Payload) of
                {ok, LastStreamId, _} ->
                    parse_control_frames(Rest, Acc ++ [{goaway, LastStreamId}]);
                _ ->
                    parse_control_frames(Rest, Acc)
            end;
        {ok, _Type, _Payload, Rest} ->
            %% Unknown frame type on control stream - skip
            parse_control_frames(Rest, Acc);
        incomplete ->
            {Acc, Data}
    end.

%% Parse SETTINGS frame payload
parse_settings(Data) ->
    parse_settings(Data, #{}).

parse_settings(<<>>, Acc) ->
    Acc;
parse_settings(Data, Acc) ->
    case decode_varint(Data) of
        {ok, Id, Rest1} ->
            case decode_varint(Rest1) of
                {ok, Value, Rest2} ->
                    parse_settings(Rest2, Acc#{Id => Value});
                incomplete ->
                    Acc
            end;
        incomplete ->
            Acc
    end.
