

#Module hackney_url#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


module to manage urls.

<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_url-1">parse_url/1</a></td><td>Parse an url and return a #hackney_url record.</td></tr><tr><td valign="top"><a href="#unparse_url-1">unparse_url/1</a></td><td></td></tr><tr><td valign="top"><a href="#urldecode-1">urldecode/1</a></td><td>Decode a URL encoded binary.</td></tr><tr><td valign="top"><a href="#urldecode-2">urldecode/2</a></td><td>Decode a URL encoded binary.</td></tr><tr><td valign="top"><a href="#urlencode-1">urlencode/1</a></td><td>URL encode a string binary.</td></tr><tr><td valign="top"><a href="#urlencode-2">urlencode/2</a></td><td>URL encode a string binary.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="parse_url-1"></a>

###parse_url/1##


<pre>parse_url(URL::binary() | list()) -> <a href="#type-hackney_url">hackney_url()</a></pre>
<br></br>


Parse an url and return a #hackney_url record.<a name="unparse_url-1"></a>

###unparse_url/1##


`unparse_url(Hackney_url) -> any()`

<a name="urldecode-1"></a>

###urldecode/1##


<pre>urldecode(Bin::binary()) -&gt; binary()</pre>
<br></br>


Equivalent to [`urldecode(Bin, crash)`](#urldecode-2).

Decode a URL encoded binary.<a name="urldecode-2"></a>

###urldecode/2##


<pre>urldecode(Bin::binary(), OnError::crash | skip) -&gt; binary()</pre>
<br></br>


Decode a URL encoded binary.
The second argument specifies how to handle percent characters that are not
followed by two valid hex characters. Use `skip` to ignore such errors,
if `crash` is used the function will fail with the reason `badarg`.<a name="urlencode-1"></a>

###urlencode/1##


<pre>urlencode(Bin::binary()) -&gt; binary()</pre>
<br></br>


Equivalent to [`urlencode(Bin, [])`](#urlencode-2).

URL encode a string binary.<a name="urlencode-2"></a>

###urlencode/2##


<pre>urlencode(Bin::binary(), Opts::[noplus | upper]) -&gt; binary()</pre>
<br></br>


URL encode a string binary.
The `noplus` option disables the default behaviour of quoting space
characters, `\s`, as `+`. The `upper` option overrides the default behaviour
of writing hex numbers using lowecase letters to using uppercase letters
instead.