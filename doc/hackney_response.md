

#Module hackney_response#
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the full body sent with the request.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>Return the full body sent with the request as long as the body
length doesn't go over MaxLength.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>init response.</td></tr><tr><td valign="top"><a href="#skip_body-1">skip_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#stream_body-1">stream_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#stream_header-1">stream_header/1</a></td><td></td></tr><tr><td valign="top"><a href="#stream_headers-1">stream_headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#stream_status-1">stream_status/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="body-1"></a>

###body/1##


<pre>body(Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}</pre>
<br></br>


Return the full body sent with the request.<a name="body-2"></a>

###body/2##


<pre>body(MaxLength::non_neg_integer() | infinity, Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}</pre>
<br></br>




Return the full body sent with the request as long as the body
length doesn't go over MaxLength.

This is most useful to quickly be able to get the full body while
avoiding filling your memory with huge request bodies when you're
not expecting it.<a name="close-1"></a>

###close/1##


`close(Client) -> any()`

<a name="init-1"></a>

###init/1##


`init(Client) -> any()`

init response<a name="skip_body-1"></a>

###skip_body/1##


<pre>skip_body(Client::#client{}) -&gt; {ok, #client{}} | {error, atom()}</pre>
<br></br>


<a name="stream_body-1"></a>

###stream_body/1##


`stream_body(Client) -> any()`

<a name="stream_header-1"></a>

###stream_header/1##


`stream_header(Client) -> any()`

<a name="stream_headers-1"></a>

###stream_headers/1##


`stream_headers(Client) -> any()`

<a name="stream_status-1"></a>

###stream_status/1##


`stream_status(Client) -> any()`

