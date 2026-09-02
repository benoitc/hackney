%%% Test pool handler for issue #914.
%%%
%%% By default, checkout_h2/4 and checkout_h3/4 return an already-terminated
%%% pid so the checkout probe races connection teardown. Optional modes expose
%%% registration failures and checkout ordering
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

checkout_h2(Host, Port, Transport, Options) ->
    case application:get_env(hackney, race_h2_checkout_observer) of
        {ok, {Parent, Observed}} ->
            Result = hackney_pool:checkout_h2(Host, Port, Transport, Options),
            notify_h2_checkout(Parent, Observed, Result),
            Result;
        undefined ->
            case application:get_env(hackney, race_register_h2_error) of
                {ok, _} -> hackney_pool:checkout_h2(Host, Port, Transport, Options);
                undefined -> {ok, dead_pid()}
            end
    end.

notify_h2_checkout(Parent, Observed, Result) ->
    Key = {?MODULE, h2_checkout_observed},
    case self() =:= Observed andalso get(Key) =:= undefined of
        true ->
            put(Key, true),
            Parent ! {h2_checkout, self(), Result};
        false ->
            ok
    end.

checkout_h3(_Host, _Port, _Transport, _Options) ->
    {ok, dead_pid()}.

checkout(Host, Port, Transport, Options) ->
    case application:get_env(hackney, race_register_h2_error) of
        {ok, Parent} ->
            Pid = spawn(fun fake_h2_conn/0),
            _ = timer:kill_after(2000, Pid),
            Parent ! {h2_registration_candidate, Pid},
            {ok, undefined, Pid};
        undefined ->
            hackney_pool:checkout(Host, Port, Transport, Options)
    end.

checkin(Ref, Options) ->
    hackney_pool:checkin(Ref, Options).

checkout_ssl(Host, Port, Transport, Options) ->
    hackney_pool:checkout_ssl(Host, Port, Transport, Options).

register_h2(Host, Port, Transport, Pid, Options) ->
    case application:get_env(hackney, race_register_h2_error) of
        {ok, _Parent} ->
            {error, set_owner_failed};
        undefined ->
            hackney_pool:register_h2(Host, Port, Transport, Pid, Options)
    end.

fake_h2_conn() ->
    receive
        {'$gen_call', From, is_upgraded_ssl} ->
            gen_statem:reply(From, true),
            fake_h2_conn();
        {'$gen_call', From, get_protocol} ->
            gen_statem:reply(From, http2),
            fake_h2_conn()
    end.

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
