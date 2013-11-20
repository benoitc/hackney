

# Module hackney #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-client">client()</a> ###


__abstract datatype__: `client()`




### <a name="type-url">url()</a> ###



<pre><code>
url() = #hackney_url{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the full body sent with the response.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>Return the full body sent with the response as long as the body
length doesn't go over MaxLength.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close the client.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td>connect a socket and create a client state.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Assign a new controlling process <em>Pid</em> to <em>Client</em>.</td></tr><tr><td valign="top"><a href="#end_stream_request_body-1">end_stream_request_body/1</a></td><td>end streaming the request body.</td></tr><tr><td valign="top"><a href="#pool-1">pool/1</a></td><td>get current pool pid or name used by a client if needed.</td></tr><tr><td valign="top"><a href="#request-1">request/1</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-2">request/2</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-4">request/4</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-5">request/5</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#send_request-2">send_request/2</a></td><td>send a request using the current client state.</td></tr><tr><td valign="top"><a href="#set_sockopts-2">set_sockopts/2</a></td><td>add set sockets options in the client.</td></tr><tr><td valign="top"><a href="#skip_body-1">skip_body/1</a></td><td>skip the full body.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the couchbeam process.</td></tr><tr><td valign="top"><a href="#start_pool-2">start_pool/2</a></td><td>start a pool.</td></tr><tr><td valign="top"><a href="#start_response-1">start_response/1</a></td><td>start a response.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop the couchbeam process.</td></tr><tr><td valign="top"><a href="#stop_pool-1">stop_pool/1</a></td><td>stop a pool.</td></tr><tr><td valign="top"><a href="#stream_body-1">stream_body/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#stream_multipart_request-2">stream_multipart_request/2</a></td><td>stream a multipart request until eof
Possible value are :
<ul>
<li><code>eof</code>: end the multipart request</li>
<li><code>{Id, {File, FileName}}</code>: to stream a file</li>
%% <li><code>{Id, {File, FileName, FileOptions}}</code>: to stream a file</li>
<li><code>{data, {start, Id, DileName, ContentType}}</code>: to start to stream
arbitrary binary content</li>
<li><code>{data, Bin}`: send a binary. Use it only after emitting a
**start**</li>
<li>`{data, eof}`: stop sending an arbitary content. It doesn</code>t stop
the multipart request</li>
<li><code>{Id, {file, Filename, Content}</code>: send a full content as a
boundary</li>
<li><code>{Id, Value}</code>: send an arbitrary value as a boundary. Filename and
Id are identique</li>
</ul>
File options can be:
<ul>
<li><code>{offset, Offset}</code>: start to send file from this offset</li>
<li><code>{bytes, Bytes}</code>: number of bytes to send</li>
<li><code>{chunk_size, ChunkSize}</code>: the size of the chunk to send</li>
</ul></td></tr><tr><td valign="top"><a href="#stream_request_body-2">stream_request_body/2</a></td><td>stream the request body.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="body-1"></a>

### body/1 ###


<pre><code>
body(Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}
</code></pre>

<br></br>


Return the full body sent with the response.
<a name="body-2"></a>

### body/2 ###


<pre><code>
body(MaxLength::non_neg_integer() | infinity, Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}
</code></pre>

<br></br>


Return the full body sent with the response as long as the body
length doesn't go over MaxLength.
<a name="close-1"></a>

### close/1 ###

`close(Client) -> any()`

close the client
<a name="connect-1"></a>

### connect/1 ###

`connect(Client) -> any()`

connect a socket and create a client state.
<a name="connect-3"></a>

### connect/3 ###

`connect(Transport, Host, Port) -> any()`


<a name="connect-4"></a>

### connect/4 ###

`connect(Transport, Host, Port, Client) -> any()`


<a name="controlling_process-2"></a>

### controlling_process/2 ###


<pre><code>
controlling_process(Client::#client{}, Pid::pid()) -&gt; ok | {error, closed | not_owner | atom()}
</code></pre>

<br></br>


Assign a new controlling process _Pid_ to _Client_.
<a name="end_stream_request_body-1"></a>

### end_stream_request_body/1 ###

`end_stream_request_body(Client) -> any()`

end streaming the request body.
<a name="pool-1"></a>

### pool/1 ###

`pool(Client) -> any()`

get current pool pid or name used by a client if needed
<a name="request-1"></a>

### request/1 ###


<pre><code>
request(URL::binary() | list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}
</code></pre>

<br></br>


make a request
<a name="request-2"></a>

### request/2 ###


<pre><code>
request(Method::term(), URL::binary() | list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}
</code></pre>

<br></br>


make a request
<a name="request-3"></a>

### request/3 ###


<pre><code>
request(Method::term(), URL::binary() | list(), Headers::list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}
</code></pre>

<br></br>


make a request
<a name="request-4"></a>

### request/4 ###


<pre><code>
request(Method::term(), URL::binary() | list(), Headers::list(), Body::term()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}
</code></pre>

<br></br>


make a request
<a name="request-5"></a>

### request/5 ###


<pre><code>
request(Method::term(), Hackney_url::<a href="#type-url">url()</a> | binary(), Headers0::list(), Body::term(), Options0::list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}
</code></pre>

<br></br>



make a request


Args:

* <strong>Method</strong>
>: method used for the request (get, post,
...)

* <strong>Url</strong>
: full url of the request

* <strong>Headers</strong>
 Proplists

* <strong>Body</strong>
:

* {form, [{K, V}, ...]}: send a form url encoded

* {multipart, [{K, V}, ...]}: send a form using multipart

* {file, "/path/to/file"}: to send a file

* Bin: binary or iolist



* <strong>Options:</strong>
 `[{connect_options, connect_options(),
{ssl_options, ssl_options()}, Others]`

* `connect_options()`: The default connect_options are
`[binary, {active, false}, {packet, raw}])` . Vor valid options
see the gen_tcp options.


* `ssl_options()`: See the ssl options from the ssl
module.


* _Others options are_:

* {follow_redirect, boolean}: false by default, follow a
redirection

* {max_redirect, integer}: 5 by default, the maximum of
redirection for a request

* {force_redirect, boolean}: false by default, to force the
redirection even on POST

* {proxy, proxy_options()}: to connect via a proxy.

* insecure: to perform "insecure" SSL connections and
transfers without checking the certificate

* {connect_timeout, infinity | integer()}: timeout used when
estabilishing a connection, in milliseconds. Default is 8000

* {recv_timeout, infinity | integer()}: timeout used when
receiving a connection. Default is infinity





* `proxy_options()`:  options to connect by a proxy:


* binary(): url to use for the proxy. Used for basic HTTP
proxy

* {Host::binary(), Port::binary}: Host and port to connect,
for HTTP proxy







<bloquote>Note: instead of doing `hackney:request(Method, ...)` you can
also do `hackney:Method(...)` if you prefer to use the REST
syntax.</bloquote>

<a name="send_request-2"></a>

### send_request/2 ###

`send_request(Client, Req) -> any()`

send a request using the current client state
<a name="set_sockopts-2"></a>

### set_sockopts/2 ###

`set_sockopts(Client, Options) -> any()`

add set sockets options in the client
<a name="skip_body-1"></a>

### skip_body/1 ###


<pre><code>
skip_body(Client::#client{}) -&gt; {ok, #client{}} | {error, atom()}
</code></pre>

<br></br>


skip the full body. (read all the body if needed).
<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Start the couchbeam process. Useful when testing using the shell.
<a name="start_pool-2"></a>

### start_pool/2 ###

`start_pool(Name, Options) -> any()`

start a pool
<a name="start_response-1"></a>

### start_response/1 ###


<pre><code>
start_response(Client::#client{}) -&gt; {ok, integer(), list(), #client{}} | {error, term()}
</code></pre>

<br></br>


start a response.
Useful if you stream the body by yourself. It will fetch the status
and headers of the response. and return
<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

Stop the couchbeam process. Useful when testing using the shell.
<a name="stop_pool-1"></a>

### stop_pool/1 ###

`stop_pool(Name) -> any()`

stop a pool
<a name="stream_body-1"></a>

### stream_body/1 ###

`stream_body(Client) -> any()`

Stream the response body.
<a name="stream_multipart_request-2"></a>

### stream_multipart_request/2 ###

`stream_multipart_request(Body, Client) -> any()`

stream a multipart request until eof
Possible value are :

* `eof`: end the multipart request

* `{Id, {File, FileName}}`: to stream a file

%% * `{Id, {File, FileName, FileOptions}}`: to stream a file

* `{data, {start, Id, DileName, ContentType}}`: to start to stream
arbitrary binary content

* `{data, Bin}`: send a binary. Use it only after emitting a
**start**</li>
<li>`{data, eof}`: stop sending an arbitary content. It doesn`t stop
the multipart request

* `{Id, {file, Filename, Content}`: send a full content as a
boundary

* `{Id, Value}`: send an arbitrary value as a boundary. Filename and
Id are identique


File options can be:

* `{offset, Offset}`: start to send file from this offset

* `{bytes, Bytes}`: number of bytes to send

* `{chunk_size, ChunkSize}`: the size of the chunk to send


<a name="stream_request_body-2"></a>

### stream_request_body/2 ###


<pre><code>
stream_request_body(Body::term(), Client::#client{}) -&gt; {ok, #client{}} | {error, term()}
</code></pre>

<br></br>


stream the request body. It isued after sending a request using
the `request` and `send_request` functions.
