

#Module hackney_pool#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


pool of sockets connections.

__Behaviours:__ [`gen_server`](gen_server.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#pool_size-1">pool_size/1</a></td><td></td></tr><tr><td valign="top"><a href="#pool_size-2">pool_size/2</a></td><td></td></tr><tr><td valign="top"><a href="#release-3">release/3</a></td><td></td></tr><tr><td valign="top"><a href="#socket-2">socket/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="code_change-3"></a>

###code_change/3##


`code_change(OldVsn, State, Extra) -> any()`

<a name="handle_call-3"></a>

###handle_call/3##


`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

###handle_cast/2##


`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

###handle_info/2##


`handle_info(X1, State) -> any()`

<a name="init-1"></a>

###init/1##


`init(Options) -> any()`

<a name="pool_size-1"></a>

###pool_size/1##


`pool_size(PidOrName) -> any()`

<a name="pool_size-2"></a>

###pool_size/2##


`pool_size(PidOrName, X2) -> any()`

<a name="release-3"></a>

###release/3##


`release(PidOrName, X2, Socket) -> any()`

<a name="socket-2"></a>

###socket/2##


`socket(PidOrName, X2) -> any()`

<a name="start_link-0"></a>

###start_link/0##


`start_link() -> any()`

<a name="start_link-1"></a>

###start_link/1##


`start_link(Options0) -> any()`

<a name="terminate-2"></a>

###terminate/2##


`terminate(Reason, State) -> any()`

