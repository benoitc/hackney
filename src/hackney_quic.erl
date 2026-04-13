%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2024-2026 Benoit Chesneau
%%%
%%% @doc QUIC/HTTP3 transport adapter over the quic_h3 API.
%%%
%%% This module is a thin gen_server wrapper around `quic_h3'. The HTTP/3
%%% framing, SETTINGS exchange, QPACK encoding/decoding, and control/push
%%% streams are handled by the `quic' (erlang_quic) library; this module only
%%% adapts the `{quic_h3, Conn, _}' event protocol to the `{quic, ConnRef, _}'
%%% protocol that hackney_conn/hackney_h3 already consume.
%%%
%%% == Messages delivered to the owner ==
%%%
%%% <ul>
%%%   <li>`{quic, ConnRef, {connected, Info}}' — Connection established</li>
%%%   <li>`{quic, ConnRef, {stream_headers, StreamId, Headers, Fin}}' — Response
%%%     headers (or trailers with `Fin=true')</li>
%%%   <li>`{quic, ConnRef, {stream_data, StreamId, Bin, Fin}}' — Response body</li>
%%%   <li>`{quic, ConnRef, {stream_reset, StreamId, ErrorCode}}'</li>
%%%   <li>`{quic, ConnRef, {closed, Reason}}'</li>
%%%   <li>`{quic, ConnRef, {transport_error, Code, Reason}}'</li>
%%% </ul>
%%%
%%% @end

-module(hackney_quic).

-behaviour(gen_server).

%% API
-export([
    connect/4,
    close/2,
    send_request/3,
    send_data/4,
    reset_stream/3,
    handle_timeout/2,
    process/1,
    get_fd/1,
    is_available/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    h3_conn :: pid() | undefined,
    conn_ref :: reference(),
    owner :: pid(),
    owner_mon :: reference()
}).

-define(CONN_TABLE, hackney_quic_conns).

%%====================================================================
%% API
%%====================================================================

-spec is_available() -> true.
is_available() -> true.

-spec get_fd(gen_udp:socket()) -> {ok, integer()} | {error, term()}.
get_fd(Socket) ->
    inet:getfd(Socket).

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
    case gen_server:start(?MODULE, {Host, Port, Opts, Owner}, []) of
        {ok, Pid} ->
            gen_server:call(Pid, get_conn_ref);
        {error, _} = Error ->
            Error
    end;
connect(_Host, _Port, _Opts, _Owner) ->
    {error, badarg}.

-spec close(reference(), term()) -> ok.
close(ConnRef, Reason) ->
    with_pid(ConnRef, fun(Pid) -> gen_server:cast(Pid, {close, Reason}) end, ok).

%% @doc Open a request stream and send HEADERS atomically.
%%
%% `quic_h3:request/2' creates the stream and sends the HEADERS frame in one
%% step. If `Fin' is true and the request has no body, an empty DATA frame
%% with Fin is sent immediately to close the send side.
-spec send_request(reference(), [{binary(), binary()}], boolean()) ->
    {ok, non_neg_integer()} | {error, term()}.
send_request(ConnRef, Headers, Fin) when is_list(Headers), is_boolean(Fin) ->
    with_pid(ConnRef,
             fun(Pid) -> gen_server:call(Pid, {send_request, Headers, Fin}) end,
             {error, not_connected}).

-spec send_data(reference(), non_neg_integer(), iodata(), boolean()) ->
    ok | {error, term()}.
send_data(ConnRef, StreamId, Data, Fin) when is_boolean(Fin) ->
    with_pid(ConnRef,
             fun(Pid) -> gen_server:call(Pid, {send_data, StreamId, Data, Fin}) end,
             {error, not_connected});
send_data(_ConnRef, _StreamId, _Data, _Fin) ->
    {error, badarg}.

-spec reset_stream(reference(), non_neg_integer(), non_neg_integer()) ->
    ok | {error, term()}.
reset_stream(ConnRef, StreamId, ErrorCode)
  when is_integer(ErrorCode), ErrorCode >= 0 ->
    with_pid(ConnRef,
             fun(Pid) -> gen_server:call(Pid, {reset_stream, StreamId, ErrorCode}) end,
             {error, not_connected});
reset_stream(_ConnRef, _StreamId, _ErrorCode) ->
    {error, badarg}.

-spec handle_timeout(reference(), non_neg_integer()) -> non_neg_integer() | infinity.
handle_timeout(_ConnRef, _NowMs) ->
    infinity.

-spec process(reference()) -> non_neg_integer() | infinity.
process(_ConnRef) ->
    infinity.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Host, Port, Opts, Owner}) ->
    MonRef = erlang:monitor(process, Owner),
    H3Opts = build_h3_opts(Host, Opts),
    case quic_h3:connect(Host, Port, H3Opts) of
        {ok, H3Conn} ->
            ConnRef = make_ref(),
            _ = ensure_table(),
            ets:insert(?CONN_TABLE, {ConnRef, self()}),
            {ok, #state{h3_conn = H3Conn,
                        conn_ref = ConnRef,
                        owner = Owner,
                        owner_mon = MonRef}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_conn_ref, _From, #state{conn_ref = Ref} = State) ->
    {reply, {ok, Ref}, State};

handle_call({send_request, Headers, Fin}, _From, #state{h3_conn = Conn} = State) ->
    case quic_h3:request(Conn, Headers, #{end_stream => Fin}) of
        {ok, StreamId} ->
            {reply, {ok, StreamId}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({send_data, StreamId, Data, Fin}, _From, #state{h3_conn = Conn} = State) ->
    Bin = iolist_to_binary(Data),
    {reply, quic_h3:send_data(Conn, StreamId, Bin, Fin), State};

handle_call({reset_stream, StreamId, ErrorCode}, _From, #state{h3_conn = Conn} = State) ->
    {reply, quic_h3:cancel(Conn, StreamId, ErrorCode), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({close, _Reason}, #state{h3_conn = Conn} = State) ->
    catch quic_h3:close(Conn),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({quic_h3, Conn, connected},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {quic, Ref, {connected, #{}}},
    {noreply, State};

handle_info({quic_h3, Conn, {response, StreamId, Status, Headers}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Full = [{<<":status">>, integer_to_binary(Status)} | Headers],
    Owner ! {quic, Ref, {stream_headers, StreamId, Full, false}},
    {noreply, State};

handle_info({quic_h3, Conn, {data, StreamId, Data, Fin}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {quic, Ref, {stream_data, StreamId, Data, Fin}},
    {noreply, State};

handle_info({quic_h3, Conn, {trailers, StreamId, Trailers}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {quic, Ref, {stream_headers, StreamId, Trailers, true}},
    {noreply, State};

handle_info({quic_h3, Conn, {stream_reset, StreamId, ErrorCode}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {quic, Ref, {stream_reset, StreamId, ErrorCode}},
    {noreply, State};

handle_info({quic_h3, Conn, {goaway, LastStreamId}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {quic, Ref, {goaway, LastStreamId}},
    {noreply, State};

handle_info({quic_h3, Conn, {goaway_sent, _}}, #state{h3_conn = Conn} = State) ->
    {noreply, State};

handle_info({quic_h3, Conn, closed},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {quic, Ref, {closed, normal}},
    {stop, normal, State};

handle_info({quic_h3, Conn, {error, Code, Reason}},
            #state{h3_conn = Conn, conn_ref = Ref, owner = Owner} = State) ->
    Owner ! {quic, Ref, {transport_error, Code, Reason}},
    {stop, {transport_error, Code}, State};

handle_info({'DOWN', MonRef, process, _Pid, _Reason},
            #state{owner_mon = MonRef, h3_conn = Conn} = State) ->
    catch quic_h3:close(Conn),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn_ref = Ref, h3_conn = Conn}) ->
    case ets:whereis(?CONN_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?CONN_TABLE, Ref)
    end,
    case Conn of
        undefined -> ok;
        _ -> catch quic_h3:close(Conn)
    end,
    ok.

%%====================================================================
%% Internal helpers
%%====================================================================

build_h3_opts(Host, Opts) ->
    HostStr = binary_to_list(Host),
    Verify = case maps:get(insecure_skip_verify, Opts, false) of
        true -> verify_none;
        false ->
            case maps:get(verify, Opts, verify_peer) of
                verify_peer -> verify_peer;
                verify_none -> verify_none;
                true -> verify_peer;
                false -> verify_none
            end
    end,
    QuicOpts0 = #{server_name_indication => HostStr},
    QuicOpts = case maps:get(cacerts, Opts, undefined) of
        undefined ->
            case maps:get(cacertfile, Opts, undefined) of
                undefined -> QuicOpts0;
                File -> QuicOpts0#{cacertfile => File}
            end;
        CACerts -> QuicOpts0#{cacerts => CACerts}
    end,
    Base = #{verify => Verify, quic_opts => QuicOpts},
    case maps:get(settings, Opts, undefined) of
        undefined -> Base;
        Settings -> Base#{settings => Settings}
    end.

ensure_table() ->
    case ets:whereis(?CONN_TABLE) of
        undefined ->
            try
                ets:new(?CONN_TABLE,
                        [named_table, public, set, {read_concurrency, true}])
            catch
                error:badarg -> ok
            end;
        _ -> ok
    end.

with_pid(ConnRef, Fun, Default) ->
    case ets:whereis(?CONN_TABLE) of
        undefined -> Default;
        _ ->
            case ets:lookup(?CONN_TABLE, ConnRef) of
                [{ConnRef, Pid}] -> Fun(Pid);
                [] -> Default
            end
    end.
