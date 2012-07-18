

#Module hackney#
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the full body sent with the response.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>Return the full body sent with the response as long as the body
length doesn't go over MaxLength.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close the client.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td>connect a socket and create a client state.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td></td></tr><tr><td valign="top"><a href="#pool-1">pool/1</a></td><td>get current pool pid or name used by a client if needed.</td></tr><tr><td valign="top"><a href="#request-1">request/1</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-2">request/2</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-4">request/4</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#request-5">request/5</a></td><td>make a request.</td></tr><tr><td valign="top"><a href="#send_request-2">send_request/2</a></td><td>send a request using the current client state.</td></tr><tr><td valign="top"><a href="#set_sockopts-2">set_sockopts/2</a></td><td>add set sockets options in the client.</td></tr><tr><td valign="top"><a href="#skip_body-1">skip_body/1</a></td><td>skip the full body.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the couchbeam process.</td></tr><tr><td valign="top"><a href="#start_pool-2">start_pool/2</a></td><td>start a pool.</td></tr><tr><td valign="top"><a href="#start_response-1">start_response/1</a></td><td>start a response.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop the couchbeam process.</td></tr><tr><td valign="top"><a href="#stop_pool-1">stop_pool/1</a></td><td>stop a pool.</td></tr><tr><td valign="top"><a href="#stream_body-1">stream_body/1</a></td><td>Stream the response body.</td></tr><tr><td valign="top"><a href="#stream_request_body-2">stream_request_body/2</a></td><td>stream the request body.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="body-1"></a>

###body/1##


<pre>body(Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}</pre>
<br></br>


Return the full body sent with the response.<a name="body-2"></a>

###body/2##


<pre>body(MaxLength::non_neg_integer() | infinity, Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}</pre>
<br></br>


Return the full body sent with the response as long as the body
length doesn't go over MaxLength.<a name="close-1"></a>

###close/1##


`close(Client) -> any()`

close the client<a name="connect-1"></a>

###connect/1##


`connect(Client) -> any()`

connect a socket and create a client state.<a name="connect-3"></a>

###connect/3##


`connect(Transport, Host, Port) -> any()`

<a name="connect-4"></a>

###connect/4##


`connect(Transport, Host, Port, Client) -> any()`

<a name="pool-1"></a>

###pool/1##


`pool(Client) -> any()`

get current pool pid or name used by a client if needed<a name="request-1"></a>

###request/1##


<pre>request(URL::binary() | list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}</pre>
<br></br>


make a request<a name="request-2"></a>

###request/2##


<pre>request(Method::term(), URL::binary() | list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}</pre>
<br></br>


make a request<a name="request-3"></a>

###request/3##


<pre>request(Method::term(), URL::binary() | list(), Headers::list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}</pre>
<br></br>


make a request<a name="request-4"></a>

###request/4##


<pre>request(Method::term(), URL::binary() | list(), Headers::list(), Body::term()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}</pre>
<br></br>


make a request<a name="request-5"></a>

###request/5##


<pre>request(Method::term(), Hackney_url::binary(), Headers::list(), Body::term(), Options0::list()) -&gt; {ok, integer(), list(), #client{}} | {error, term()}</pre>
<br></br>




make a request

Args:

* _Method_: method used for the request (get, post,
...)

* _Url_: full url of the request

* _Headers_ Proplists

* _Body_:

* {form, [{K, V}, ...]}: send a form

* {file, "/path/to/file"}: to send a file

* Bin: binary or iolist



<a name="send_request-2"></a>

###send_request/2##


`send_request(Client, Req) -> any()`

send a request using the current client state<a name="set_sockopts-2"></a>

###set_sockopts/2##


`set_sockopts(Client, Options) -> any()`

add set sockets options in the client<a name="skip_body-1"></a>

###skip_body/1##


<pre>skip_body(Client::#client{}) -&gt; {ok, #client{}} | {error, atom()}</pre>
<br></br>


skip the full body. (read all the body if needed).<a name="start-0"></a>

###start/0##


`start() -> any()`

Start the couchbeam process. Useful when testing using the shell.<a name="start_pool-2"></a>

###start_pool/2##


`start_pool(Name, Options) -> any()`

start a pool<a name="start_response-1"></a>

###start_response/1##


<pre>start_response(Client::#client{}) -&gt; {ok, integer(), list(), #client{}} | {error, term()}</pre>
<br></br>


start a response.
Useful if you stream the body by yourself. It will fetch the status
and headers of the response. and return<a name="stop-0"></a>

###stop/0##


`stop() -> any()`

Stop the couchbeam process. Useful when testing using the shell.<a name="stop_pool-1"></a>

###stop_pool/1##


`stop_pool(Name) -> any()`

stop a pool<a name="stream_body-1"></a>

###stream_body/1##


`stream_body(Client) -> any()`

Stream the response body.<a name="stream_request_body-2"></a>

###stream_request_body/2##


<pre>stream_request_body(Body::term(), Client::#client{}) -&gt; {ok, #client{}} | {error, term()}</pre>
<br></br>


stream the request body. It isued after sending a request using
the `request` and `send_request` functions.