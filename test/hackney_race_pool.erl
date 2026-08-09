%%% Test pool handler for issue #914.
%%%
%%% checkout_h2/4 and checkout_h3/4 return an already-terminated pid so the
%%% checkout get_state liveness probe races connection teardown. Every other
%%% callback delegates to hackney_pool, so the new-connection fallback behaves
%%% exactly as in production.
-module(hackney_race_pool).

-export([checkout/4,
         checkin/2,
         checkout_ssl/4,
         checkout_h2/4,
         register_h2/5,
         unregister_h2/2,
         unregister_h2_all/0,
         checkout_h3/4,
         register_h3/5,
         unregister_h3/2,
         get_h3_session/4,
         store_h3_session/5,
         delete_h3_session/4]).

%% The dead pid to hand out is stashed in application env by the test setup.
dead_pid() ->
    {ok, Pid} = application:get_env(hackney, race_dead_pid),
    Pid.

checkout_h2(_Host, _Port, _Transport, _Options) ->
    {ok, dead_pid()}.

checkout_h3(_Host, _Port, _Transport, _Options) ->
    {ok, dead_pid()}.

%% Everything else delegates unchanged.
checkout(Host, Port, Transport, Options) ->
    hackney_pool:checkout(Host, Port, Transport, Options).

checkin(Ref, Options) ->
    hackney_pool:checkin(Ref, Options).

checkout_ssl(Host, Port, Transport, Options) ->
    hackney_pool:checkout_ssl(Host, Port, Transport, Options).

register_h2(Host, Port, Transport, Pid, Options) ->
    hackney_pool:register_h2(Host, Port, Transport, Pid, Options).

unregister_h2(Pid, Options) ->
    hackney_pool:unregister_h2(Pid, Options).

unregister_h2_all() ->
    hackney_pool:unregister_h2_all().

register_h3(Host, Port, Transport, Pid, Options) ->
    hackney_pool:register_h3(Host, Port, Transport, Pid, Options).

unregister_h3(Pid, Options) ->
    hackney_pool:unregister_h3(Pid, Options).

get_h3_session(Host, Port, Transport, Options) ->
    hackney_pool:get_h3_session(Host, Port, Transport, Options).

store_h3_session(Host, Port, Transport, Session, Options) ->
    hackney_pool:store_h3_session(Host, Port, Transport, Session, Options).

delete_h3_session(Host, Port, Transport, Options) ->
    hackney_pool:delete_h3_session(Host, Port, Transport, Options).
