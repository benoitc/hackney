

# Module hackney_socks5 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

socks 5 transport.

<a name="types"></a>

## Data Types ##




### <a name="type-socks5_socket">socks5_socket()</a> ###


<pre><code>
socks5_socket() = {atom(), <a href="inet.md#type-socket">inet:socket()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close a socks5 socket.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Assign a new controlling process <em>Pid</em> to <em>Socket</em>.</td></tr><tr><td valign="top"><a href="#messages-1">messages/1</a></td><td>Atoms used to identify messages in {active, once | true} mode.</td></tr><tr><td valign="top"><a href="#peername-1">peername/1</a></td><td>Return the address and port for the other end of a connection.</td></tr><tr><td valign="top"><a href="#recv-2">recv/2</a></td><td></td></tr><tr><td valign="top"><a href="#recv-3">recv/3</a></td><td>Receive a packet from a socket in passive mode.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a packet on a socket.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>Set one or more options for a socket.</td></tr><tr><td valign="top"><a href="#shutdown-2">shutdown/2</a></td><td>Immediately close a socket in one or two directions.</td></tr><tr><td valign="top"><a href="#sockname-1">sockname/1</a></td><td>Get the local address and port of a socket.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(X1::<a href="#type-socks5_socket">socks5_socket()</a>) -&gt; ok
</code></pre>
<br />

Close a socks5 socket.

__See also:__ [gen_tcp:close/1](gen_tcp.md#close-1).

<a name="connect-3"></a>

### connect/3 ###

`connect(Host, Port, Opts) -> any()`

<a name="connect-4"></a>

### connect/4 ###

`connect(Host, Port, Opts, Timeout) -> any()`

<a name="controlling_process-2"></a>

### controlling_process/2 ###

<pre><code>
controlling_process(X1::<a href="#type-socks5_socket">socks5_socket()</a>, Pid::pid()) -&gt; ok | {error, closed | not_owner | atom()}
</code></pre>
<br />

Assign a new controlling process _Pid_ to _Socket_.

__See also:__ [gen_tcp:controlling_process/2](gen_tcp.md#controlling_process-2).

<a name="messages-1"></a>

### messages/1 ###

`messages(X1) -> any()`

Atoms used to identify messages in {active, once | true} mode.

<a name="peername-1"></a>

### peername/1 ###

<pre><code>
peername(X1::<a href="#type-socks5_socket">socks5_socket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>
<br />

Return the address and port for the other end of a connection.

__See also:__ [inet:peername/1](inet.md#peername-1).

<a name="recv-2"></a>

### recv/2 ###

`recv(Socket, Length) -> any()`

<a name="recv-3"></a>

### recv/3 ###

<pre><code>
recv(X1::<a href="#type-socks5_socket">socks5_socket()</a>, Length::non_neg_integer(), Timeout::timeout()) -&gt; {ok, any()} | {error, closed | atom()}
</code></pre>
<br />

Receive a packet from a socket in passive mode.

__See also:__ [gen_tcp:recv/3](gen_tcp.md#recv-3).

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(X1::<a href="#type-socks5_socket">socks5_socket()</a>, Packet::iolist()) -&gt; ok | {error, atom()}
</code></pre>
<br />

Send a packet on a socket.

__See also:__ [gen_tcp:send/2](gen_tcp.md#send-2).

<a name="setopts-2"></a>

### setopts/2 ###

<pre><code>
setopts(X1::<a href="#type-socks5_socket">socks5_socket()</a>, Opts::list()) -&gt; ok | {error, atom()}
</code></pre>
<br />

Set one or more options for a socket.

__See also:__ [inet:setopts/2](inet.md#setopts-2).

<a name="shutdown-2"></a>

### shutdown/2 ###

<pre><code>
shutdown(X1::<a href="#type-socks5_socket">socks5_socket()</a>, How::read | write | read_write) -&gt; ok
</code></pre>
<br />

Immediately close a socket in one or two directions.

__See also:__ [gen_tcp:shutdown/2](gen_tcp.md#shutdown-2).

<a name="sockname-1"></a>

### sockname/1 ###

<pre><code>
sockname(X1::<a href="#type-socks5_socket">socks5_socket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>
<br />

Get the local address and port of a socket

__See also:__ [inet:sockname/1](inet.md#sockname-1).

