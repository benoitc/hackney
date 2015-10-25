-module(hackney_server).
-behaviour(gen_server).


-export([start_link/0]).
-export([get_pool/1,
         register_pool/2]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("hackney.hrl").

-define(SERVER, ?MODULE).
-record(state, {monitors=[]}).

start_link() ->
    _ = create_tabs(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).




get_pool(Ref) ->
    ets:lookup_element(?TAB, {pool, Ref}, 2).

register_pool(Ref, Pid) ->
    case ets:insert_new(?TAB, {{pool, Ref}, self()}) of
        false -> false;
        true ->
            %% registering a pool is synchronous
            call({monitor_pool, Ref, Pid}),
            true
    end.


call(Msg) ->
    gen_server:call(?MODULE, Msg).

create_tabs() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [ordered_set, public, named_table,
                           {read_concurrency, true},
                           {write_concurrency, true}]);
        _ ->
            ok
    end.


init([]) ->
    %% reste monitor for pools
    Monitors = init_monitors(),
    {ok, #state{monitors=Monitors}}.

handle_call({monitor_pool, Ref, Pid}, _From, State) ->
    #state{monitors=Monitors}=State,
    MRef = erlang:monitor(process, Pid),
    {reply, ok, State#state{monitors=[{{MRef, Pid}, Ref} | Monitors]}};

handle_call(_Msg, _From, State) ->
    {reply, badarg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, process, Pid}, #state{monitors=Monitors}=State) ->
    {_, Ref} = lists:keyfind({MRef, Pid}, 1, Monitors),
    true = ets:delete(?TAB, {pool, Ref}),
    {noreply, State#state{monitors=lists:keydelete({MRef, Pid}, 1, Monitors)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


init_monitors() ->
    [{{erlang:monitor(process, Pid), Pid}, Ref} ||
     [Ref, Pid] <- ets:match(?TAB, {{pool, '$1'}, '$2'})].
