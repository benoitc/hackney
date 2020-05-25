

# Module hackney_connect #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_or_close-1">check_or_close/1</a></td><td></td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close the client.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#connect-5">connect/5</a></td><td></td></tr><tr><td valign="top"><a href="#create_connection-4">create_connection/4</a></td><td>create a connection and return a client state.</td></tr><tr><td valign="top"><a href="#create_connection-5">create_connection/5</a></td><td></td></tr><tr><td valign="top"><a href="#is_pool-1">is_pool/1</a></td><td>get current pool pid or name used by a client if needed.</td></tr><tr><td valign="top"><a href="#maybe_connect-1">maybe_connect/1</a></td><td>connect a socket and create a client state.</td></tr><tr><td valign="top"><a href="#peername-1">peername/1</a></td><td>get the address and port for the other end of current connection in the client.</td></tr><tr><td valign="top"><a href="#reconnect-4">reconnect/4</a></td><td></td></tr><tr><td valign="top"><a href="#set_sockopts-2">set_sockopts/2</a></td><td>add set sockets options in the client.</td></tr><tr><td valign="top"><a href="#sockname-1">sockname/1</a></td><td>the local address and port of current socket in the client.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check_or_close-1"></a>

### check_or_close/1 ###

`check_or_close(Client) -> any()`

<a name="close-1"></a>

### close/1 ###

`close(Client) -> any()`

close the client

<a name="connect-3"></a>

### connect/3 ###

`connect(Transport, Host, Port) -> any()`

<a name="connect-4"></a>

### connect/4 ###

`connect(Transport, Host, Port, Options) -> any()`

<a name="connect-5"></a>

### connect/5 ###

`connect(Transport, Host, Port, Options, Dynamic) -> any()`

<a name="create_connection-4"></a>

### create_connection/4 ###

`create_connection(Transport, Host, Port, Options) -> any()`

create a connection and return a client state

<a name="create_connection-5"></a>

### create_connection/5 ###

`create_connection(Transport, Host, Port, Options, Dynamic) -> any()`

<a name="is_pool-1"></a>

### is_pool/1 ###

`is_pool(Client) -> any()`

get current pool pid or name used by a client if needed

<a name="maybe_connect-1"></a>

### maybe_connect/1 ###

`maybe_connect(Client) -> any()`

connect a socket and create a client state.

<a name="peername-1"></a>

### peername/1 ###

`peername(Client) -> any()`

get the address and port for the other end of current connection in the client

<a name="reconnect-4"></a>

### reconnect/4 ###

`reconnect(Host, Port, Transport, State) -> any()`

<a name="set_sockopts-2"></a>

### set_sockopts/2 ###

`set_sockopts(Client, Options) -> any()`

add set sockets options in the client

<a name="sockname-1"></a>

### sockname/1 ###

`sockname(Client) -> any()`

the local address and port of current socket in the client

