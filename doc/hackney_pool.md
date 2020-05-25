

# Module hackney_pool #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

pool of sockets connections.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#checkin-2">checkin/2</a></td><td>release a socket in the pool.</td></tr><tr><td valign="top"><a href="#checkout-4">checkout/4</a></td><td>fetch a socket from the pool.</td></tr><tr><td valign="top"><a href="#child_spec-2">child_spec/2</a></td><td>return a child spec suitable for embeding your pool in the
supervisor.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#count-1">count/1</a></td><td>get the number of connections in the pool.</td></tr><tr><td valign="top"><a href="#count-2">count/2</a></td><td>get the number of connections in the pool for <code>{Host0, Port, Transport}</code></td></tr><tr><td valign="top"><a href="#find_pool-1">find_pool/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_stats-1">get_stats/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#max_connections-1">max_connections/1</a></td><td>get max pool size.</td></tr><tr><td valign="top"><a href="#notify-2">notify/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_max_connections-2">set_max_connections/2</a></td><td>change the pool size.</td></tr><tr><td valign="top"><a href="#set_timeout-2">set_timeout/2</a></td><td>change the connection timeout.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_pool-2">start_pool/2</a></td><td>start a pool.</td></tr><tr><td valign="top"><a href="#stop_pool-1">stop_pool/1</a></td><td>stop a pool.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#timeout-1">timeout/1</a></td><td>get timeout.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="checkin-2"></a>

### checkin/2 ###

`checkin(X1, Socket) -> any()`

release a socket in the pool

<a name="checkout-4"></a>

### checkout/4 ###

`checkout(Host, Port, Transport, Client) -> any()`

fetch a socket from the pool

<a name="child_spec-2"></a>

### child_spec/2 ###

`child_spec(Name, Options0) -> any()`

return a child spec suitable for embeding your pool in the
supervisor

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="count-1"></a>

### count/1 ###

`count(Name) -> any()`

get the number of connections in the pool

<a name="count-2"></a>

### count/2 ###

`count(Name, X2) -> any()`

get the number of connections in the pool for `{Host0, Port, Transport}`

<a name="find_pool-1"></a>

### find_pool/1 ###

`find_pool(Name) -> any()`

<a name="get_stats-1"></a>

### get_stats/1 ###

`get_stats(Pool) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="max_connections-1"></a>

### max_connections/1 ###

`max_connections(Name) -> any()`

get max pool size

<a name="notify-2"></a>

### notify/2 ###

`notify(Pool, Msg) -> any()`

<a name="set_max_connections-2"></a>

### set_max_connections/2 ###

`set_max_connections(Name, NewSize) -> any()`

change the pool size

<a name="set_timeout-2"></a>

### set_timeout/2 ###

`set_timeout(Name, NewTimeout) -> any()`

change the connection timeout

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="start_link-2"></a>

### start_link/2 ###

`start_link(Name, Options0) -> any()`

<a name="start_pool-2"></a>

### start_pool/2 ###

`start_pool(Name, Options) -> any()`

start a pool

<a name="stop_pool-1"></a>

### stop_pool/1 ###

`stop_pool(Name) -> any()`

stop a pool

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="timeout-1"></a>

### timeout/1 ###

`timeout(Name) -> any()`

get timeout

