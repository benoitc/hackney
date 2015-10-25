-module(hackney_connector).

-export([start_link/2,
         init/2]).

-include("hackney.hrl").
-include("hackney_socket.hrl").

-record(state, {pool,
                fallback_time}).

-define(DEFAULT_LOOKUP_ORDER, [inet6, inet]).

start_link(Pool, FallbackTime) ->
    Pid = spawn_link(?MODULE, init, [Pool, FallbackTime]),
    {ok, Pid}.


init(Pool, FallbackTime) ->
    hackney_pool:register_connector(Pool, self()),
    loop(#state{pool=Pool, fallback_time=FallbackTime}).


loop(State) ->
    receive
        {connect, Group, {Host, Port, Options, Timeout}} ->
            Ref = make_ref(),
            Req = {Group, Host, Port, Options, Timeout},
            LookupOrder = lru:get(?LOOKUP_CACHE, Host, ?DEFAULT_LOOKUP_ORDER),
            handle_connect(Ref, Req, LookupOrder, State),
            loop(State);
        stop ->
            exit(normal)
    end.


handle_connect(Ref, Req, LookupOrder, State) ->
    [F1, F2] = LookupOrder,
    Pid = spawn_connection(Ref, Req, F1, State#state.pool),
    TRef = erlang:send_after(State#state.fallback_time, self(), {Ref, fallback, F2}),
    connect_loop(Ref, TRef, Req, Pid, nil, LookupOrder, State, 2).

connect_loop(_, _, _, _, _, _, State, 0) ->
    loop(State);
connect_loop(Ref, TRef, {_Group, H, _, _, _} = Req, P1, P2, LookupOrder, State, Wait) ->
    receive
        {Ref, connected, P1} ->
            maybe_kill_job(Ref, TRef, P2),
            loop(State);
        {Ref, connected, P2} ->
            catch exit(P1, normal),
            flush(Ref, P2),
            %% lookup order is reversed, store it.
            lru:add(?LOOKUP_CACHE, H, lists:reverse(LookupOrder)),
            loop(State);
        {Ref, fallback, Familly} ->
            Pid = spawn_connection(Ref, Req, Familly, State#state.pool),
            connect_loop(Ref, TRef, Req, P1, Pid, LookupOrder, State, Wait);
        {Ref, 'DOWN', P1, _Error} ->
            connect_loop(Ref, TRef, Req, P1, P2, LookupOrder, State, Wait -1);
        {Ref, 'DOWN', P2, Error} ->
            error_logger:error_msg(
                    "hackney connector: connection failure; "
                    "with reason: ~p~n", [Error]),
            connect_loop(Ref, TRef, Req, P1, P2, LookupOrder, State, Wait -1)
    end.

maybe_kill_job(Ref, TRef, Pid) ->
    case is_pid(Pid) of
        true -> catch exit(Pid, normal);
        false -> erlang:cancel_timer(TRef)
    end,
    flush(Ref, Pid).


flush(Ref, Pid) ->
    receive
        {Ref, connected, Pid} -> ok;
        {Ref, fallback, _Familly} -> ok;
        _Else -> 
            ok
    after 0 -> ok
    end.

spawn_connection(Ref, {Group, Host, Port, Opts0, Timeout}, Familly, Pool) ->
    Opts = [Familly | Opts0],
    Connector = self(),
    spawn_link(fun() ->
                       case hackney_tcp:connect(Host, Port, Opts, Timeout) of
                           {ok, Sock} ->
                               HS = #hackney_socket{transport=hackney_tcp,
                                                    sock=Sock,
                                                    host=Host,
                                                    port=Port,
                                                    group=Group,
                                                    pool=Pool},

                               Connector ! {Ref, connected, self()},
                               hackney_pool:release(HS);
                           Error ->
                               Connector ! {Ref, 'DOWN', self(), Error}
                       end
               end).
