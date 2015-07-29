

# Module hackney_manager #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_response_pid-1">async_response_pid/1</a></td><td></td></tr><tr><td valign="top"><a href="#cancel_request-1">cancel_request/1</a></td><td></td></tr><tr><td valign="top"><a href="#close_request-1">close_request/1</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_state-1">get_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_state-2">get_state/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_error-1">handle_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_request-1">new_request/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_async_response-1">start_async_response/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop_async_response-1">stop_async_response/1</a></td><td></td></tr><tr><td valign="top"><a href="#store_state-1">store_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#store_state-2">store_state/2</a></td><td></td></tr><tr><td valign="top"><a href="#take_control-2">take_control/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_state-1">update_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#update_state-2">update_state/2</a></td><td></td></tr><tr><td valign="top"><a href="#with_async_response_pid-2">with_async_response_pid/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_response_pid-1"></a>

### async_response_pid/1 ###

`async_response_pid(Ref) -> any()`

<a name="cancel_request-1"></a>

### cancel_request/1 ###

`cancel_request(Client) -> any()`

<a name="close_request-1"></a>

### close_request/1 ###

`close_request(Client) -> any()`

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, Ring, Extra) -> any()`

<a name="controlling_process-2"></a>

### controlling_process/2 ###

`controlling_process(Ref, Pid) -> any()`

<a name="get_state-1"></a>

### get_state/1 ###

`get_state(Client) -> any()`

<a name="get_state-2"></a>

### get_state/2 ###

`get_state(Ref, Fun) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, Mstate) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_error-1"></a>

### handle_error/1 ###

`handle_error(Client) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="new_request-1"></a>

### new_request/1 ###

`new_request(Client) -> any()`

<a name="start_async_response-1"></a>

### start_async_response/1 ###

`start_async_response(Ref) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="stop_async_response-1"></a>

### stop_async_response/1 ###

`stop_async_response(Ref) -> any()`

<a name="store_state-1"></a>

### store_state/1 ###

`store_state(Client) -> any()`

<a name="store_state-2"></a>

### store_state/2 ###

`store_state(Ref, NState) -> any()`

<a name="take_control-2"></a>

### take_control/2 ###

`take_control(Ref, NState) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="update_state-1"></a>

### update_state/1 ###

`update_state(Client) -> any()`

<a name="update_state-2"></a>

### update_state/2 ###

`update_state(Ref, NState) -> any()`

<a name="with_async_response_pid-2"></a>

### with_async_response_pid/2 ###

`with_async_response_pid(Ref, Fun) -> any()`

