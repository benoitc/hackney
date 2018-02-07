

# Module hackney_headers_new #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = term()
</code></pre>




### <a name="type-headers_list">headers_list()</a> ###


<pre><code>
headers_list() = [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]
</code></pre>




### <a name="type-key">key()</a> ###


<pre><code>
key() = binary() | string()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = binary() | {binary() | [{binary(), binary()} | binary()]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#append-3">append/3</a></td><td>append a new value to the list of value for the the header field
if the key has not been recorded the list will be created witht eh value as the first item.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>delete a field from headers.</td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>create headers from a list.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>get the first value of an headers or return undefined.</td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td>get the first value of an headers or return the default.</td></tr><tr><td valign="top"><a href="#is_key-2">is_key/2</a></td><td>is the header field exists or no.</td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr><tr><td valign="top"><a href="#merge-2">merge/2</a></td><td>merge 2 headers objects.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>initialize an empty headers objecy.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_content_type-1">parse_content_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_media_type-2">parse_media_type/2</a></td><td>Parse a media type.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>return the number of headers fields.</td></tr><tr><td valign="top"><a href="#store-2">store/2</a></td><td>store a list of headers.</td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td>replace the content of the header field with the value or the list of values.</td></tr><tr><td valign="top"><a href="#store_new-3">store_new/3</a></td><td>only store a vakue if the key exist.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td>transform headers to a binary that can be used to construct a request.</td></tr><tr><td valign="top"><a href="#to_iolist-1">to_iolist/1</a></td><td>conver headers to an iolist.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>convert headers to a list.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="append-3"></a>

### append/3 ###

<pre><code>
append(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, X3::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

append a new value to the list of value for the the header field
if the key has not been recorded the list will be created witht eh value as the first item.

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Key::<a href="#type-key">key()</a>, H::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

delete a field from headers.

<a name="fold-3"></a>

### fold/3 ###

`fold(Fun, Acc, X3) -> any()`

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(HeadersList::<a href="#type-headers_list">headers_list()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

create headers from a list

<a name="get_value-2"></a>

### get_value/2 ###

<pre><code>
get_value(Key::<a href="#type-key">key()</a>, Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-value">value()</a> | undefined
</code></pre>
<br />

get the first value of an headers or return undefined

<a name="get_value-3"></a>

### get_value/3 ###

<pre><code>
get_value(Key::<a href="#type-key">key()</a>, Headers::<a href="#type-headers">headers()</a>, Default::any()) -&gt; <a href="#type-value">value()</a> | any()
</code></pre>
<br />

get the first value of an headers or return the default

<a name="is_key-2"></a>

### is_key/2 ###

<pre><code>
is_key(Key::<a href="#type-key">key()</a>, X2::<a href="#type-headers">headers()</a>) -&gt; true | false
</code></pre>
<br />

is the header field exists or no

<a name="lookup-2"></a>

### lookup/2 ###

`lookup(Key, X2) -> any()`

<a name="merge-2"></a>

### merge/2 ###

<pre><code>
merge(Headers1::<a href="#type-headers">headers()</a>, X2::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

merge 2 headers objects. If a key is already exising in HEader1, it will be kept.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

initialize an empty headers objecy

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(H::<a href="#type-headers_list">headers_list()</a> | <a href="#type-headers">headers()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

<a name="parse_content_type-1"></a>

### parse_content_type/1 ###

<pre><code>
parse_content_type(Data::binary()) -&gt; any()
</code></pre>
<br />

<a name="parse_media_type-2"></a>

### parse_media_type/2 ###

<pre><code>
parse_media_type(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

Parse a media type.

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(X1::<a href="#type-headers">headers()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

return the number of headers fields

<a name="store-2"></a>

### store/2 ###

<pre><code>
store(KVs::<a href="#type-headers_list">headers_list()</a>, Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

store a list of headers. Replacing oldest

<a name="store-3"></a>

### store/3 ###

<pre><code>
store(Key::<a href="#type-key">key()</a>, Values::<a href="#type-value">value()</a> | [<a href="#type-value">value()</a>], X3::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-headers">headers()</a>
</code></pre>
<br />

replace the content of the header field with the value or the list of values.

<a name="store_new-3"></a>

### store_new/3 ###

<pre><code>
store_new(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Headers::<a href="#type-headers">headers()</a>) -&gt; {boolean(), <a href="#type-headers">headers()</a>}
</code></pre>
<br />

only store a vakue if the key exist.

<a name="to_binary-1"></a>

### to_binary/1 ###

<pre><code>
to_binary(Headers::<a href="#type-headers">headers()</a>) -&gt; binary()
</code></pre>
<br />

transform headers to a binary that can be used to construct a request

<a name="to_iolist-1"></a>

### to_iolist/1 ###

<pre><code>
to_iolist(Headers::<a href="#type-headers">headers()</a>) -&gt; iolist()
</code></pre>
<br />

conver headers to an iolist. Useful to send them over the wire.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Headers::<a href="#type-headers">headers()</a>) -&gt; <a href="#type-headers_list">headers_list()</a>
</code></pre>
<br />

convert headers to a list

