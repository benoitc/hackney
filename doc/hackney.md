

# Module hackney #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-client">client()</a> ###


__abstract datatype__: `client()`




### <a name="type-client_ref">client_ref()</a> ###



<pre><code>
client_ref() = term()
</code></pre>





### <a name="type-url">url()</a> ###



<pre><code>
url() = #hackney_url{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the full body sent with the response.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>Return the full body sent with the response as long as the body
length doesn't go over MaxLength.</td></tr><tr><td valign="top"><a href="#cancel_request-1">cancel_request/1</a></td><td>Extract raw informations from the client context
This feature can be useful when you want to create a simple proxy, rerouting on the headers and the status line and continue to forward the connection for example.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close the client.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td></td></tr><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td></td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td>connect a socket and create a client state.</td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Assign a new controlling process <em>Pid</em> to <em>Client</em>.</td></tr><tr><td valign="top"><a href="#cookies-1">cookies/1</a></td><td></td></tr><tr><td valign="top"><a href="#finish_send_body-1">finish_send_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#pause_stream-1">pause_stream/1</a></td><td>pause a response stream, the stream process will hibernate and
be woken later by the resume function.</td></tr><tr><td valign="top"><a href="#redirect_location-1">redirect_location/1</a></td><td></td></tr><tr><td valign="top"><a href="#request-1">request/1</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-2">request/2</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-4">request/4</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-5">request/5</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#resume_stream-1">resume_stream/1</a></td><td>resume a paused response stream, the stream process will be
awoken.</td></tr><tr><td valign="top"><a href="#send_body-2">send_body/2</a></td><td>send the request body until eob.</td></tr><tr><td valign="top"><a href="#send_multipart_body-2">send_multipart_body/2</a></td><td>send a multipart body until eof
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
</ul></td></tr><tr><td valign="top"><a href="#send_request-2">send_request/2</a></td><td>send a request using the current client state.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>set client options.</td></tr><tr><td valign="top"><a href="#skip_body-1">skip_body/1</a></td><td>skip the full body.</td></tr><tr><td valign="top"><a href="#skip_multipart-1">skip_multipart/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the couchbeam process.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_response-1">start_response/1</a></td><td>start a response.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop the couchbeam process.</td></tr><tr><td valign="top"><a href="#stop_async-1">stop_async/1</a></td><td>stop to receive asynchronously.</td></tr><tr><td valign="top"><a href="#stream_body-1">stream_body/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#stream_multipart-1">stream_multipart/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#stream_next-1">stream_next/1</a></td><td>continue to the next stream message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="body-1"></a>

### body/1 ###


<pre><code>
body(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, binary()} | {error, atom()}
</code></pre>

<br></br>


Return the full body sent with the response.
<a name="body-2"></a>

### body/2 ###


<pre><code>
body(Ref::<a href="#type-client_ref">client_ref()</a>, MaxLength::non_neg_integer() | infinity) -&gt; {ok, binary()} | {error, atom()}
</code></pre>

<br></br>


Return the full body sent with the response as long as the body
length doesn't go over MaxLength.
<a name="cancel_request-1"></a>

### cancel_request/1 ###


<pre><code>
cancel_request(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {atom(), <a href="inet.md#type-socket">inet:socket()</a>, binary(), <a href="hackney_response.md#type-response_state">hackney_response:response_state()</a>} | {error, term()}
</code></pre>

<br></br>



Extract raw informations from the client context
This feature can be useful when you want to create a simple proxy, rerouting on the headers and the status line and continue to forward the connection for example.


return: `{ResponseState, Transport, Socket, Buffer} | {error, Reason}`

* `Response`: waiting_response, on_status, on_headers, on_body

* `Transport`: The current transport module

* `Socket`: the current socket

* `Buffer`: Data fetched but not yet processed


<a name="close-1"></a>

### close/1 ###

`close(Ref) -> any()`

close the client
<a name="connect-1"></a>

### connect/1 ###

`connect(URL) -> any()`


<a name="connect-2"></a>

### connect/2 ###

`connect(Hackney_url, Options) -> any()`


<a name="connect-3"></a>

### connect/3 ###

`connect(Transport, Host, Port) -> any()`

connect a socket and create a client state.
<a name="connect-4"></a>

### connect/4 ###

`connect(Transport, Host, Port, Options) -> any()`


<a name="controlling_process-2"></a>

### controlling_process/2 ###


<pre><code>
controlling_process(Ref::<a href="#type-client_ref">client_ref()</a>, Pid::pid()) -&gt; ok | {error, closed | not_owner | atom()}
</code></pre>

<br></br>


Assign a new controlling process _Pid_ to _Client_.
<a name="cookies-1"></a>

### cookies/1 ###


<pre><code>
cookies(Headers::list()) -&gt; list()
</code></pre>

<br></br>



<a name="finish_send_body-1"></a>

### finish_send_body/1 ###

`finish_send_body(Ref) -> any()`


<a name="pause_stream-1"></a>

### pause_stream/1 ###


<pre><code>
pause_stream(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, req_not_found}
</code></pre>

<br></br>


pause a response stream, the stream process will hibernate and
be woken later by the resume function
<a name="redirect_location-1"></a>

### redirect_location/1 ###

`redirect_location(Headers) -> any()`


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
request(Method::term(), Hackney_url::<a href="#type-url">url()</a> | binary(), Headers::list(), Body::term(), Options0::list()) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, <a href="#type-client_ref">client_ref()</a>} | {error, term()}
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


* `async`: receive the response asynchronously
The function return {ok, {response_stream, StreamRef}}.
When {async, once} is used the socket will receive only once. To
receive the other messages use the function
`hackney:stream_next/1`


* `{stream_to, pid()}`: If async is true or once, the response
messafes will be sent to this PID.


* `{cookie, list() | binary()}` : to set a cookie or a
list of cookies.


* _Others options are_:

* `{follow_redirect, boolean}`: false by default, follow a
redirection

* `{max_redirect, integer}`: 5 by default, the maximum of
redirection for a request

* `{force_redirect, boolean}`: false by default, to force the
redirection even on POST

* `{proxy, proxy_options()}`: to connect via a proxy.

* `insecure`: to perform "insecure" SSL connections and
transfers without checking the certificate

* `{connect_timeout, infinity | integer()}`: timeout used when
estabilishing a connection, in milliseconds. Default is 8000

* `{recv_timeout, infinity | integer()}`: timeout used when
receiving a connection. Default is infinity





<blocquote>Note: if the response is async, only
`follow_redirect` is take in consideration for the redirection.
If a valid redirection happen you receive the messages:

* `{redirect, To, Headers`}

* `{see_other, To, Headers}` for status 303 POST requests.

</blocquote>





* `proxy_options()`:  options to connect by a proxy:


* binary(): url to use for the proxy. Used for basic HTTP
proxy

* {Host::binary(), Port::binary}: Host and port to connect,
for HTTP proxy







<bloquote>Note: instead of doing `hackney:request(Method, ...)` you can
also do `hackney:Method(...)` if you prefer to use the REST
syntax.</bloquote>

<a name="resume_stream-1"></a>

### resume_stream/1 ###


<pre><code>
resume_stream(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, req_not_found}
</code></pre>

<br></br>


resume a paused response stream, the stream process will be
awoken
<a name="send_body-2"></a>

### send_body/2 ###


<pre><code>
send_body(Ref::<a href="#type-client_ref">client_ref()</a>, Body::term()) -&gt; ok | {error, term()}
</code></pre>

<br></br>


send the request body until eob. It's issued after sending a request using
the `request` and `send_request` functions.
<a name="send_multipart_body-2"></a>

### send_multipart_body/2 ###


<pre><code>
send_multipart_body(Ref::<a href="#type-client_ref">client_ref()</a>, Body::term()) -&gt; ok | {error, term()}
</code></pre>

<br></br>


send a multipart body until eof
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


<a name="send_request-2"></a>

### send_request/2 ###

`send_request(Ref, Req) -> any()`

send a request using the current client state
<a name="setopts-2"></a>

### setopts/2 ###


<pre><code>
setopts(Ref::<a href="#type-client_ref">client_ref()</a>, Options::list()) -&gt; ok
</code></pre>

<br></br>


set client options.
<a name="skip_body-1"></a>

### skip_body/1 ###


<pre><code>
skip_body(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, atom()}
</code></pre>

<br></br>


skip the full body. (read all the body if needed).
<a name="skip_multipart-1"></a>

### skip_multipart/1 ###


<pre><code>
skip_multipart(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, term()}
</code></pre>

<br></br>


Stream the response body.
<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Start the couchbeam process. Useful when testing using the shell.
<a name="start-1"></a>

### start/1 ###

`start(PoolHandler) -> any()`


<a name="start_response-1"></a>

### start_response/1 ###


<pre><code>
start_response(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, <a href="#type-client_ref">client_ref()</a>} | {error, term()}
</code></pre>

<br></br>


start a response.
Useful if you stream the body by yourself. It will fetch the status
and headers of the response. and return
<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

Stop the couchbeam process. Useful when testing using the shell.
<a name="stop_async-1"></a>

### stop_async/1 ###


<pre><code>
stop_async(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, req_not_found} | {error, term()}
</code></pre>

<br></br>


stop to receive asynchronously.
<a name="stream_body-1"></a>

### stream_body/1 ###


<pre><code>
stream_body(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, binary()} | done | {error, term()}
</code></pre>

<br></br>


Stream the response body.
<a name="stream_multipart-1"></a>

### stream_multipart/1 ###


<pre><code>
stream_multipart(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {headers, list()} | {body, binary()} | eof | end_of_part | {error, term()}
</code></pre>

<br></br>


Stream the response body.
<a name="stream_next-1"></a>

### stream_next/1 ###


<pre><code>
stream_next(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, req_not_found}
</code></pre>

<br></br>


continue to the next stream message. Only use it when
`{async, once}` is set in the client options.
