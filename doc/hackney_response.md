

# Module hackney_response #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

module handling the response.

<a name="types"></a>

## Data Types ##




### <a name="type-response_state">response_state()</a> ###


<pre><code>
response_state() = start | waiting | on_status | on_headers | on_body
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>Return the full body sent with the request.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>Return the full body sent with the request as long as the body
length doesn't go over MaxLength.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#expect_response-1">expect_response/1</a></td><td>handle Expect header.</td></tr><tr><td valign="top"><a href="#maybe_close-1">maybe_close/1</a></td><td></td></tr><tr><td valign="top"><a href="#skip_body-1">skip_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#skip_multipart-1">skip_multipart/1</a></td><td>Skip a part returned by the multipart parser.</td></tr><tr><td valign="top"><a href="#start_response-1">start_response/1</a></td><td>Start the response It parse the request lines and headers.</td></tr><tr><td valign="top"><a href="#stream_body-1">stream_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#stream_multipart-1">stream_multipart/1</a></td><td>stream a multipart response.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="body-1"></a>

### body/1 ###

<pre><code>
body(Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}
</code></pre>
<br />

Return the full body sent with the request.

<a name="body-2"></a>

### body/2 ###

<pre><code>
body(MaxLength::non_neg_integer() | infinity, Client::#client{}) -&gt; {ok, binary(), #client{}} | {error, atom()}
</code></pre>
<br />

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

<a name="expect_response-1"></a>

### expect_response/1 ###

`expect_response(Client) -> any()`

handle Expect header

<a name="maybe_close-1"></a>

### maybe_close/1 ###

`maybe_close(Client) -> any()`

<a name="skip_body-1"></a>

### skip_body/1 ###

<pre><code>
skip_body(Client::#client{}) -&gt; {ok, #client{}} | {skip, #client{}} | {error, atom()}
</code></pre>
<br />

<a name="skip_multipart-1"></a>

### skip_multipart/1 ###

<pre><code>
skip_multipart(Client) -&gt; {ok, Client}
</code></pre>

<ul class="definitions"><li><code>Client = #client{}</code></li></ul>

Skip a part returned by the multipart parser.

This function repeatedly calls _multipart_data/1_ until
_{end_of_part, Req}_ or _{eof, Req}_ is parsed.

<a name="start_response-1"></a>

### start_response/1 ###

`start_response(Client) -> any()`

Start the response It parse the request lines and headers.

<a name="stream_body-1"></a>

### stream_body/1 ###

`stream_body(Client) -> any()`

<a name="stream_multipart-1"></a>

### stream_multipart/1 ###

<pre><code>
stream_multipart(Client::#client{}) -&gt; {headers, list(), #client{}} | {body, binary(), #client{}} | {eof | end_of_part | mp_mixed | mp_mixed_eof, #client{}}
</code></pre>
<br />

stream a multipart response

Use this function for multipart streaming. For each part in the
response, this function returns _{headers, Headers, Req}_ followed by a sequence of
_{body, Data, Req}_ tuples and finally _{end_of_part, Req}_. When there
is no part to parse anymore, _{eof, Req}_ is returned.

