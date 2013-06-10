

# Module hackney_response #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


module handling the response.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the full body sent with the request.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>Return the full body sent with the request as long as the body
length doesn't go over MaxLength.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#skip_body-1">skip_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_response-1">start_response/1</a></td><td>Start the response It parse the request lines and headers.</td></tr><tr><td valign="top"><a href="#stream_body-1">stream_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#stream_header-1">stream_header/1</a></td><td></td></tr><tr><td valign="top"><a href="#stream_headers-1">stream_headers/1</a></td><td>fetch all headers.</td></tr><tr><td valign="top"><a href="#stream_status-1">stream_status/1</a></td><td>parse the status line.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="body-1"></a>

### body/1 ###


<pre><code>
body(Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}
</code></pre>

<br></br>


Return the full body sent with the request.
<a name="body-2"></a>

### body/2 ###


<pre><code>
body(MaxLength::non_neg_integer() | infinity, Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}
</code></pre>

<br></br>



Return the full body sent with the request as long as the body
length doesn't go over MaxLength.



This is most useful to quickly be able to get the full body while
avoiding filling your memory with huge request bodies when you're
not expecting it.


When the response is larger than MaxLength, this function will return
the body it received up to the last chunk, which might be a bit more than MaxLength.
<a name="close-1"></a>

### close/1 ###

`close(Client) -> any()`


<a name="skip_body-1"></a>

### skip_body/1 ###


<pre><code>
skip_body(Client::#client{}) -&gt; {ok, #client{}} | {error, atom()}
</code></pre>

<br></br>



<a name="start_response-1"></a>

### start_response/1 ###

`start_response(Client) -> any()`

Start the response It parse the request lines and headers.
<a name="stream_body-1"></a>

### stream_body/1 ###

`stream_body(Client) -> any()`


<a name="stream_header-1"></a>

### stream_header/1 ###

`stream_header(Client) -> any()`


<a name="stream_headers-1"></a>

### stream_headers/1 ###

`stream_headers(Client) -> any()`

fetch all headers
<a name="stream_status-1"></a>

### stream_status/1 ###

`stream_status(Client) -> any()`

parse the status line
