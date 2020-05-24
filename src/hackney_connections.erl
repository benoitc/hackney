-module(hackney_connections).
-behaviour(gen_server).


-export([new_connection_id/4,
         release_connection_id/1]).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).


-include("hackney.hrl").

-define(SERVER, ?MODULE).


new_connection_id(Transport, Host, Port, ConnectOptions) ->
  ConnectionKey = {Transport, Host, Port, ConnectOptions},
  gen_server:call(?SERVER, {new_session_id, ConnectionKey}).


release_connection_id(Id) ->
  gen_server:cast(?SERVER, {release_connection_id, Id}).



start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    _ = ets:new(?CONNECTIONS, [set, named_table, public,
                               {read_concurrency, true},
                               {write_concurrency, true}]),
  {ok, {}}.


handle_call({new_session_id, ConnectionKey}, _From, State) ->
  case ets:lookup(?CONNECTIONS, ConnectionKey) of
    [{_, Id}] ->
      ets:update_counter(?CONNECTIONS, Id, {3, 1}),
      {reply, Id, State};
    [] ->
      Id = make_ref(),
      ets:insert(?CONNECTIONS, [{ConnectionKey, Id},
                                {Id, ConnectionKey, 1}]),
      {reply, Id, State}
  end;
handle_call(_Msg, _From, State) ->
  {reply, badarg, State}.


handle_cast({release_connection_id, Id},  State) ->
  RefCount = try ets:update_counter(?CONNECTIONS, Id, {3, -1})
             catch
               error:badarg -> -1
             end,
  if
    RefCount =< 0 ->
      case ets:take(?CONNECTIONS, Id) of
        [{_, ConnKey, _}] ->
          ets:delete(?CONNECTIONS, ConnKey);
        [] ->
          ok
      end;
    true ->
      ok
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


terminate(_Reason, _State) ->
  ok.

