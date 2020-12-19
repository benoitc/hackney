

# Module hackney #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-client">client()</a> ###


<pre><code>
client() = #client{}
</code></pre>




### <a name="type-client_ref">client_ref()</a> ###


<pre><code>
client_ref() = term()
</code></pre>




### <a name="type-url">url()</a> ###


<pre><code>
url() = #hackney_url{} | binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the full body sent with the response.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>Return the full body sent with the response as long as the body
length doesn't go over MaxLength.</td></tr><tr><td valign="top"><a href="#cancel_request-1">cancel_request/1</a></td><td>Extract raw informations from the client context
This feature can be useful when you want to create a simple proxy, rerouting
on the headers and the status line and continue to forward the connection for example.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close the client.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td></td></tr><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td></td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td>connect a socket and create a client state.</td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Assign a new controlling process <em>Pid</em> to <em>Client</em>.</td></tr><tr><td valign="top"><a href="#cookies-1">cookies/1</a></td><td></td></tr><tr><td valign="top"><a href="#finish_send_body-1">finish_send_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#location-1">location/1</a></td><td>return the requested location.</td></tr><tr><td valign="top"><a href="#pause_stream-1">pause_stream/1</a></td><td>pause a response stream, the stream process will hibernate and
be woken later by the resume function.</td></tr><tr><td valign="top"><a href="#peername-1">peername/1</a></td><td>peername of the client.</td></tr><tr><td valign="top"><a href="#redirect_location-1">redirect_location/1</a></td><td></td></tr><tr><td valign="top"><a href="#request-1">request/1</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-2">request/2</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-4">request/4</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-5">request/5</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request_info-1">request_info/1</a></td><td>get request info.</td></tr><tr><td valign="top"><a href="#resume_stream-1">resume_stream/1</a></td><td>resume a paused response stream, the stream process will be
awoken.</td></tr><tr><td valign="top"><a href="#send_body-2">send_body/2</a></td><td>send the request body until eob.</td></tr><tr><td valign="top"><a href="#send_multipart_body-2">send_multipart_body/2</a></td><td>send a multipart body until eof
Possible value are :
<ul>
<li><code>eof</code>: end the multipart request</li>
<li><code>{file, Path}</code>: to stream a file</li>
<li><code>{file, Path, ExtraHeaders}</code>: to stream a file</li>
<li><code>{data, Name, Content}</code>: to send a full part</li>
<li><code>{data, Name, Content, ExtraHeaders}</code>: to send a full part</li>
<li><code>{part, Name, Len}</code>: to start sending a part with a known length in a streaming
fashion</li>
<li><code>{part, Name, Len, ExtraHeader}</code>: to start sending a part in a streaming
fashion</li>
<li><code>{part, Name}</code>: to start sending a part without length in a streaming
fashion</li>
<li><code>{part, Name, ExtraHeader}</code>: to start sending a part without
length in a streaming  fashion</li>
<li><code>{part_bin, Bin}</code>: To send part of part</li>
<li><code>{part, eof}</code>: To notify the end of the part </li>
<li><code>{mp_mixed, Name, MixedBoundary}</code>: To notify we start a part with a a mixed
multipart content</li>
<li><code>{mp_mixed_eof, MixedBoundary}</code>: To notify we end a part with a a mixed
multipart content</li>
</ul><p></p>Note: You can calculate the full length of a multipart stream using
the function <code>hackney_multipart:len_mp_stream/2</code> .</td></tr><tr><td valign="top"><a href="#send_request-2">send_request/2</a></td><td>send a request using the current client state.</td></tr><tr><td valign="top"><a href="#send_request-3">send_request/3</a></td><td>send a request using the current client state and pass new
options to it.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>set client options.</td></tr><tr><td valign="top"><a href="#skip_body-1">skip_body/1</a></td><td>skip the full body.</td></tr><tr><td valign="top"><a href="#skip_multipart-1">skip_multipart/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#sockname-1">sockname/1</a></td><td>sockname of the client.</td></tr><tr><td valign="top"><a href="#start_response-1">start_response/1</a></td><td>start a response.</td></tr><tr><td valign="top"><a href="#stop_async-1">stop_async/1</a></td><td>stop to receive asynchronously.</td></tr><tr><td valign="top"><a href="#stream_body-1">stream_body/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#stream_multipart-1">stream_multipart/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#stream_next-1">stream_next/1</a></td><td>continue to the next stream message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="body-1"></a>

### body/1 ###

<pre><code>
body(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, binary()} | {error, atom()} | {error, {closed, binary()}}
</code></pre>
<br />

Return the full body sent with the response.

<a name="body-2"></a>

### body/2 ###

<pre><code>
body(Ref::<a href="#type-client_ref">client_ref()</a>, MaxLength::non_neg_integer() | infinity) -&gt; {ok, binary()} | {error, atom()} | {error, {closed, binary()}}
</code></pre>
<br />

Return the full body sent with the response as long as the body
length doesn't go over MaxLength.

<a name="cancel_request-1"></a>

### cancel_request/1 ###

<pre><code>
cancel_request(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, {atom(), <a href="inet.md#type-socket">inet:socket()</a>, binary(), <a href="hackney_response.md#type-response_state">hackney_response:response_state()</a>}} | {error, term()}
</code></pre>
<br />

Extract raw informations from the client context
This feature can be useful when you want to create a simple proxy, rerouting
on the headers and the status line and continue to forward the connection for example.

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
<br />

Assign a new controlling process _Pid_ to _Client_.

<a name="cookies-1"></a>

### cookies/1 ###

<pre><code>
cookies(Headers::list()) -&gt; list()
</code></pre>
<br />

<a name="finish_send_body-1"></a>

### finish_send_body/1 ###

`finish_send_body(Ref) -> any()`

<a name="location-1"></a>

### location/1 ###

<pre><code>
location(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; binary()
</code></pre>
<br />

return the requested location

<a name="pause_stream-1"></a>

### pause_stream/1 ###

<pre><code>
pause_stream(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, req_not_found}
</code></pre>
<br />

pause a response stream, the stream process will hibernate and
be woken later by the resume function

<a name="peername-1"></a>

### peername/1 ###

`peername(Ref) -> any()`

peername of the client

<a name="redirect_location-1"></a>

### redirect_location/1 ###

`redirect_location(Headers) -> any()`

<a name="request-1"></a>

### request/1 ###

<pre><code>
request(URL::<a href="#type-url">url()</a> | binary() | list()) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, integer(), list()} | {error, term()}
</code></pre>
<br />

make a request

<a name="request-2"></a>

### request/2 ###

<pre><code>
request(Method::term(), URL::<a href="#type-url">url()</a> | binary() | list()) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, integer(), list()} | {error, term()}
</code></pre>
<br />

make a request

<a name="request-3"></a>

### request/3 ###

<pre><code>
request(Method::term(), URL::<a href="#type-url">url()</a> | binary() | list(), Headers::list()) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, integer(), list()} | {error, term()}
</code></pre>
<br />

make a request

<a name="request-4"></a>

### request/4 ###

<pre><code>
request(Method::term(), URL::<a href="#type-url">url()</a> | binary() | list(), Headers::list(), Body::term()) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, integer(), list()} | {error, term()}
</code></pre>
<br />

make a request

<a name="request-5"></a>

### request/5 ###

<pre><code>
request(Method::term(), Hackney_url::<a href="#type-url">url()</a> | binary() | list(), Headers0::list(), Body::term(), Options0::list()) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, integer(), list(), binary()} | {ok, integer(), list()} | {ok, <a href="#type-client_ref">client_ref()</a>} | {error, term()}
</code></pre>
<br />

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
`[binary, {active, false}, {packet, raw}])`. For valid options
see the gen_tcp options.

* `ssl_options()`: See the ssl options from the ssl
module.

* `with_body`: when this option is passed the body is returned
directly. The response is `{ok, Status, Headers, Body}`

* `max_body`: sets maximum allowed size of the body if
with_body is true

* `async`: receive the response asynchronously
The function return {ok, StreamRef}.
When {async, once} is used the response will be received only once. To
receive the other messages use the function
`hackney:stream_next/1`

* `{path_encode_fun, fun()}`: function used to encode the path. if
not set it will use `hackney_url:pathencode/1` the function takes the
binary path as entry and return a new encoded path.

* `{stream_to, pid()}`: If async is true or once, the response
messages will be sent to this PID.

* `{cookie, list() | binary()}` : to set a cookie or a
list of cookies.

* _Others options are_:

* `{follow_redirect, boolean}`: false by default, follow a
redirection

* `{max_redirect, integer}`: 5 by default, the maximum of
redirection for a request

* `{force_redirect, boolean}`: false by default, to force the
redirection even on POST

* `{basic_auth, {binary, binary}}`: HTTP basic auth username and password.</li>
<li>`{proxy, proxy_options()}`: to connect via a proxy.

* `insecure`: to perform "insecure" SSL connections and
transfers without checking the certificate

* `{checkout_timeout, infinity | integer()}`: timeout used when
checking out a socket from the pool, in milliseconds.
By default is equal to connect_timeout

* `{connect_timeout, infinity | integer()}`: timeout used when
establishing a connection, in milliseconds. Default is 8000

* `{recv_timeout, infinity | integer()}`: timeout used when
receiving data over a connection. Default is 5000


<blockquote>Note: if the response is async, only
`follow_redirect` is take in consideration for the redirection.
If a valid redirection happen you receive the messages:

* `{redirect, To, Headers`}

* `{see_other, To, Headers}` for status 303 POST requests.

</blockquote>

* `proxy_options()`:  options to connect by a proxy:

* binary(): url to use for the proxy. Used for basic HTTP
proxy

* {Host::binary(), Port::binary}: Host and port to connect,
for HTTP proxy

* {socks5, Host::binary(), Port::binary()}: Host and Port
to connect to a socks5 proxy.

* {connect, Host::binary(), Port::binary()}: Host and Port
to connect to an HTTP tunnel.




<blockquote>Note: instead of doing `hackney:request(Method, ...)` you can
also do `hackney:Method(...)` if you prefer to use the REST
syntax.</blockquote>

Return:

* `{ok, ResponseStatus, ResponseHeaders}`: On HEAD
request if the response succeeded.

* `{ok, ResponseStatus, ResponseHeaders, Ref}`: When
the response succeeded. The request reference is used later to
retrieve the body.

* `{ok, ResponseStatus, ResponseHeaders, Body}`: When the
option `with_body` is set to true and the response succeeded.

* `{ok, Ref}` Return the request reference when you
decide to stream the request. You can use the returned reference to
stream the request body and continue to handle the response.

* `{error, {closed, PartialBody}}` A body was expected but
instead the remote closed the response after sending the headers.
Equivalent to the curl  message `no chunk, no close, no size.
Assume close to signal end`.

* `{error, term()}` other errors.


<a name="request_info-1"></a>

### request_info/1 ###

<pre><code>
request_info(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; list()
</code></pre>
<br />

get request info

<a name="resume_stream-1"></a>

### resume_stream/1 ###

<pre><code>
resume_stream(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, req_not_found}
</code></pre>
<br />

resume a paused response stream, the stream process will be
awoken

<a name="send_body-2"></a>

### send_body/2 ###

<pre><code>
send_body(Ref::<a href="#type-client_ref">client_ref()</a>, Body::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

send the request body until eob. It's issued after sending a request using
the `request` and `send_request` functions.

<a name="send_multipart_body-2"></a>

### send_multipart_body/2 ###

<pre><code>
send_multipart_body(Ref::<a href="#type-client_ref">client_ref()</a>, Body::term()) -&gt; ok | {error, term()}
</code></pre>
<br />

send a multipart body until eof
Possible value are :

* `eof`: end the multipart request

* `{file, Path}`: to stream a file

* `{file, Path, ExtraHeaders}`: to stream a file

* `{data, Name, Content}`: to send a full part

* `{data, Name, Content, ExtraHeaders}`: to send a full part

* `{part, Name, Len}`: to start sending a part with a known length in a streaming
fashion

* `{part, Name, Len, ExtraHeader}`: to start sending a part in a streaming
fashion

* `{part, Name}`: to start sending a part without length in a streaming
fashion

* `{part, Name, ExtraHeader}`: to start sending a part without
length in a streaming  fashion

* `{part_bin, Bin}`: To send part of part

* `{part, eof}`: To notify the end of the part

* `{mp_mixed, Name, MixedBoundary}`: To notify we start a part with a a mixed
multipart content

* `{mp_mixed_eof, MixedBoundary}`: To notify we end a part with a a mixed
multipart content


Note: You can calculate the full length of a multipart stream using
the function `hackney_multipart:len_mp_stream/2` .

<a name="send_request-2"></a>

### send_request/2 ###

`send_request(Ref, Req) -> any()`

send a request using the current client state

<a name="send_request-3"></a>

### send_request/3 ###

`send_request(Ref, Req, Options) -> any()`

send a request using the current client state and pass new
options to it.

<a name="setopts-2"></a>

### setopts/2 ###

<pre><code>
setopts(Ref::<a href="#type-client_ref">client_ref()</a>, Options::list()) -&gt; ok
</code></pre>
<br />

set client options.
Options are:

* `async`: to fetch the response asynchronously

* `{async, once}`: to receive the response asynchronously one time.
To receive the next message use the function `hackney:stream_next/1`.

* `{stream_to, pid()}`: to set the pid where the messages of an
asynchronous response will be sent.

* `{follow_redirect, bool()}` : if true a redirection will be
followed when the response is received synchronously

* `{force_redirect, bool()}` : if true a 301/302 redirection will be
followed even on POST.

* `{max_redirect, integer()}` the maximum number of redirections that
will be followed


<a name="skip_body-1"></a>

### skip_body/1 ###

<pre><code>
skip_body(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, atom()}
</code></pre>
<br />

skip the full body. (read all the body if needed).

<a name="skip_multipart-1"></a>

### skip_multipart/1 ###

<pre><code>
skip_multipart(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Stream the response body.

<a name="sockname-1"></a>

### sockname/1 ###

`sockname(Ref) -> any()`

sockname of the client

<a name="start_response-1"></a>

### start_response/1 ###

<pre><code>
start_response(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, integer(), list(), <a href="#type-client_ref">client_ref()</a>} | {ok, <a href="#type-client_ref">client_ref()</a>} | {error, term()}
</code></pre>
<br />

start a response.
Useful if you stream the body by yourself. It will fetch the status
and headers of the response. and return

<a name="stop_async-1"></a>

### stop_async/1 ###

<pre><code>
stop_async(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, <a href="#type-client_ref">client_ref()</a>} | {error, req_not_found} | {error, term()}
</code></pre>
<br />

stop to receive asynchronously.

<a name="stream_body-1"></a>

### stream_body/1 ###

<pre><code>
stream_body(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {ok, binary()} | done | {error, term()}
</code></pre>
<br />

Stream the response body.

<a name="stream_multipart-1"></a>

### stream_multipart/1 ###

<pre><code>
stream_multipart(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; {headers, list()} | {body, binary()} | eof | end_of_part | {error, term()}
</code></pre>
<br />

Stream the response body.

Return:

* `{headers, Headers}`: the part headers

* `{body, Bin}`: part of the content

* `end_of_part` : end of part

* `mp_mixed`: notify the beginning of a mixed multipart part

* `mp_mixed_eof`: notify the end  of a mixed multipart part

* `eof`: notify the end of the multipart request


<a name="stream_next-1"></a>

### stream_next/1 ###

<pre><code>
stream_next(Ref::<a href="#type-client_ref">client_ref()</a>) -&gt; ok | {error, req_not_found}
</code></pre>
<br />

continue to the next stream message. Only use it when
`{async, once}` is set in the client options.

