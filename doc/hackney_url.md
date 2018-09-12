

# Module hackney_url #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

module to manage urls.

<a name="types"></a>

## Data Types ##




### <a name="type-qs_opt">qs_opt()</a> ###


<pre><code>
qs_opt() = noplus | upper
</code></pre>




### <a name="type-qs_vals">qs_vals()</a> ###


<pre><code>
qs_vals() = [{binary(), binary() | true}]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#fix_path-1">fix_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_url-3">make_url/3</a></td><td> construct an url from a base url, a path and a list of
properties to give to the url.</td></tr><tr><td valign="top"><a href="#normalize-1">normalize/1</a></td><td>Normalizes the encoding of a Url
use the hackney_url:pathencode/1 to encode an url.</td></tr><tr><td valign="top"><a href="#normalize-2">normalize/2</a></td><td>Normalizes the encoding of a Url.</td></tr><tr><td valign="top"><a href="#parse_qs-1">parse_qs/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_url-1">parse_url/1</a></td><td>Parse an url and return a #hackney_url record.</td></tr><tr><td valign="top"><a href="#pathencode-1">pathencode/1</a></td><td>encode a URL path.</td></tr><tr><td valign="top"><a href="#qs-1">qs/1</a></td><td>encode query properties to binary.</td></tr><tr><td valign="top"><a href="#qs-2">qs/2</a></td><td>encode query properties to binary
Opts are passed to urlencode.</td></tr><tr><td valign="top"><a href="#transport_scheme-1">transport_scheme/1</a></td><td></td></tr><tr><td valign="top"><a href="#unparse_url-1">unparse_url/1</a></td><td></td></tr><tr><td valign="top"><a href="#urldecode-1">urldecode/1</a></td><td>Decode a URL encoded binary.</td></tr><tr><td valign="top"><a href="#urldecode-2">urldecode/2</a></td><td>Decode a URL encoded binary.</td></tr><tr><td valign="top"><a href="#urlencode-1">urlencode/1</a></td><td>URL encode a string binary.</td></tr><tr><td valign="top"><a href="#urlencode-2">urlencode/2</a></td><td>URL encode a string binary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="fix_path-1"></a>

### fix_path/1 ###

`fix_path(Path) -> any()`

<a name="make_url-3"></a>

### make_url/3 ###

<pre><code>
make_url(Url::binary(), Path::binary() | [binary()], Query::binary() | <a href="#type-qs_vals">qs_vals()</a>) -&gt; binary()
</code></pre>
<br />

construct an url from a base url, a path and a list of
properties to give to the url.

<a name="normalize-1"></a>

### normalize/1 ###

<pre><code>
normalize(URL) -&gt; NormalizedUrl
</code></pre>

<ul class="definitions"><li><code>URL = binary() | list() | <a href="#type-hackney_url">hackney_url()</a></code></li><li><code>NormalizedUrl = <a href="#type-hackney_url">hackney_url()</a></code></li></ul>

Normalizes the encoding of a Url
use the hackney_url:pathencode/1 to encode an url

<a name="normalize-2"></a>

### normalize/2 ###

<pre><code>
normalize(URL, Fun) -&gt; NormalizedUrl
</code></pre>

<ul class="definitions"><li><code>URL = binary() | list() | <a href="#type-hackney_url">hackney_url()</a></code></li><li><code>Fun = function()</code></li><li><code>NormalizedUrl = <a href="#type-hackney_url">hackney_url()</a></code></li></ul>

Normalizes the encoding of a Url

<a name="parse_qs-1"></a>

### parse_qs/1 ###

<pre><code>
parse_qs(Bin::binary()) -&gt; <a href="#type-qs_vals">qs_vals()</a>
</code></pre>
<br />

<a name="parse_url-1"></a>

### parse_url/1 ###

<pre><code>
parse_url(URL::binary() | list()) -&gt; <a href="#type-hackney_url">hackney_url()</a>
</code></pre>
<br />

Parse an url and return a #hackney_url record.

<a name="pathencode-1"></a>

### pathencode/1 ###

<pre><code>
pathencode(Bin::binary()) -&gt; binary()
</code></pre>
<br />

Equivalent to [`pathencode(Bin, [])`](#pathencode-2).

encode a URL path

<a name="qs-1"></a>

### qs/1 ###

<pre><code>
qs(KVs::<a href="#type-qs_vals">qs_vals()</a>) -&gt; binary()
</code></pre>
<br />

encode query properties to binary

<a name="qs-2"></a>

### qs/2 ###

<pre><code>
qs(KVs::<a href="#type-qs_vals">qs_vals()</a>, Opts::[<a href="#type-qs_opt">qs_opt()</a>]) -&gt; binary()
</code></pre>
<br />

encode query properties to binary
Opts are passed to urlencode.

<a name="transport_scheme-1"></a>

### transport_scheme/1 ###

`transport_scheme(X1) -> any()`

<a name="unparse_url-1"></a>

### unparse_url/1 ###

`unparse_url(Hackney_url) -> any()`

<a name="urldecode-1"></a>

### urldecode/1 ###

<pre><code>
urldecode(Bin::binary()) -&gt; binary()
</code></pre>
<br />

Equivalent to [`urldecode(Bin, crash)`](#urldecode-2).

Decode a URL encoded binary.

<a name="urldecode-2"></a>

### urldecode/2 ###

<pre><code>
urldecode(Bin::binary(), OnError::crash | skip) -&gt; binary()
</code></pre>
<br />

Decode a URL encoded binary.
The second argument specifies how to handle percent characters that are not
followed by two valid hex characters. Use `skip` to ignore such errors,
if `crash` is used the function will fail with the reason `badarg`.

<a name="urlencode-1"></a>

### urlencode/1 ###

<pre><code>
urlencode(Bin::binary() | string()) -&gt; binary()
</code></pre>
<br />

URL encode a string binary.

<a name="urlencode-2"></a>

### urlencode/2 ###

<pre><code>
urlencode(Bin::binary() | string(), Opts::[<a href="#type-qs_opt">qs_opt()</a>]) -&gt; binary()
</code></pre>
<br />

URL encode a string binary.
The `noplus` option disables the default behaviour of quoting space
characters, `\s`, as `+`. The `upper` option overrides the default behaviour
of writing hex numbers using lowecase letters to using uppercase letters
instead.

