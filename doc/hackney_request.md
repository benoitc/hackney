

# Module hackney_request #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

module handling the request.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_ua-0">default_ua/0</a></td><td></td></tr><tr><td valign="top"><a href="#encode_form-1">encode_form/1</a></td><td>encode a list of properties in a form.</td></tr><tr><td valign="top"><a href="#end_stream_body-1">end_stream_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_default_port-1">is_default_port/1</a></td><td></td></tr><tr><td valign="top"><a href="#location-1">location/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_multipart_stream-2">make_multipart_stream/2</a></td><td></td></tr><tr><td valign="top"><a href="#perform-2">perform/2</a></td><td></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td></td></tr><tr><td valign="top"><a href="#send_chunk-2">send_chunk/2</a></td><td></td></tr><tr><td valign="top"><a href="#sendfile-3">sendfile/3</a></td><td></td></tr><tr><td valign="top"><a href="#stream_body-2">stream_body/2</a></td><td></td></tr><tr><td valign="top"><a href="#stream_multipart-2">stream_multipart/2</a></td><td>stream multipart.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_ua-0"></a>

### default_ua/0 ###

`default_ua() -> any()`

<a name="encode_form-1"></a>

### encode_form/1 ###

`encode_form(KVs) -> any()`

encode a list of properties in a form.

<a name="end_stream_body-1"></a>

### end_stream_body/1 ###

`end_stream_body(Client) -> any()`

<a name="is_default_port-1"></a>

### is_default_port/1 ###

`is_default_port(Client) -> any()`

<a name="location-1"></a>

### location/1 ###

`location(Client) -> any()`

<a name="make_multipart_stream-2"></a>

### make_multipart_stream/2 ###

<pre><code>
make_multipart_stream(Parts::list(), Boundary::binary()) -&gt; {function(), list()}
</code></pre>
<br />

<a name="perform-2"></a>

### perform/2 ###

`perform(Client0, X2) -> any()`

<a name="send-2"></a>

### send/2 ###

`send(Client, Data) -> any()`

<a name="send_chunk-2"></a>

### send_chunk/2 ###

`send_chunk(Client, Data) -> any()`

<a name="sendfile-3"></a>

### sendfile/3 ###

`sendfile(FileName, Opts, Client) -> any()`

<a name="stream_body-2"></a>

### stream_body/2 ###

`stream_body(Msg, Client) -> any()`

<a name="stream_multipart-2"></a>

### stream_multipart/2 ###

`stream_multipart(File, Client) -> any()`

stream multipart

