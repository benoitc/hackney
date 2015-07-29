

# Module hackney_headers #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

module to manipulate HTTP headers.

<a name="types"></a>

## Data Types ##




### <a name="type-disposition">disposition()</a> ###


<pre><code>
disposition() = {binary(), [{binary(), binary()}]}
</code></pre>




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#content_disposition-1">content_disposition/1</a></td><td>Parse a content disposition.</td></tr><tr><td valign="top"><a href="#content_type-1">content_type/1</a></td><td>Parse a content type.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Delete the header corresponding to key if it is present.</td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td>fold the list of headers.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>get the value of the header.</td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#header_value-2">header_value/2</a></td><td>join value and params in a binary.</td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td>Insert the pair into the headers, merging with any pre-existing key.</td></tr><tr><td valign="top"><a href="#insert-4">insert/4</a></td><td>same as <code>insert/3</code> but allows to add params to the header value.</td></tr><tr><td valign="top"><a href="#make_header-2">make_header/2</a></td><td>Create a binary header.</td></tr><tr><td valign="top"><a href="#make_header-3">make_header/3</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>initialise an header dict.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Semantically parse headers.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td>store the pair into the headers, replacing any pre-existing key.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td>return all the headers as a binary that can be sent over the
wire.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>extend the headers with a new list of <code>{Key, Value}</code> pair.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="content_disposition-1"></a>

### content_disposition/1 ###

<pre><code>
content_disposition(Data::binary()) -&gt; <a href="#type-disposition">disposition()</a>
</code></pre>
<br />

Parse a content disposition.

<a name="content_type-1"></a>

### content_type/1 ###

<pre><code>
content_type(Data::binary()) -&gt; any()
</code></pre>
<br />

Parse a content type.

We lowercase the charset header as we know it's case insensitive.

<a name="delete-2"></a>

### delete/2 ###

`delete(Key, Headers) -> any()`

Delete the header corresponding to key if it is present.

<a name="fold-3"></a>

### fold/3 ###

`fold(Fun, Acc0, Headers) -> any()`

fold the list of headers

<a name="get_value-2"></a>

### get_value/2 ###

`get_value(Key, Headers) -> any()`

get the value of the header

<a name="get_value-3"></a>

### get_value/3 ###

`get_value(Key, Headers, Default) -> any()`

<a name="header_value-2"></a>

### header_value/2 ###

`header_value(Value, Params) -> any()`

join value and params in a binary

<a name="insert-3"></a>

### insert/3 ###

`insert(Key, Value, Headers) -> any()`

Insert the pair into the headers, merging with any pre-existing key.
A merge is done with Value = V0 ++ ", " ++ V1.

<a name="insert-4"></a>

### insert/4 ###

`insert(Key, Value, Params, Headers) -> any()`

same as `insert/3` but allows to add params to the header value.

<a name="make_header-2"></a>

### make_header/2 ###

`make_header(Name, Value) -> any()`

Create a binary header

<a name="make_header-3"></a>

### make_header/3 ###

`make_header(Name, Value, Params) -> any()`

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

initialise an header dict

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(D::list()) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(Name::binary(), Headers::list() | <a href="#type-headers">headers()</a>) -&gt; any() | undefined | {error, badarg}
</code></pre>
<br />

Semantically parse headers.

When the value isn't found, a proper default value for the type
returned is used as a return value.

__See also:__ [parse/3](#parse-3).

<a name="store-3"></a>

### store/3 ###

`store(Key, Value, Headers) -> any()`

store the pair into the headers, replacing any pre-existing key.

<a name="to_binary-1"></a>

### to_binary/1 ###

`to_binary(Headers) -> any()`

return all the headers as a binary that can be sent over the
wire.

<a name="to_list-1"></a>

### to_list/1 ###

`to_list(Headers) -> any()`

<a name="update-2"></a>

### update/2 ###

`update(Headers, KVs) -> any()`

extend the headers with a new list of `{Key, Value}` pair.

