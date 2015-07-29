

# Module hackney_cookie #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-cookie_option">cookie_option()</a> ###


<pre><code>
cookie_option() = {max_age, non_neg_integer()} | {domain, binary()} | {path, binary()} | {secure, boolean()} | {http_only, boolean()}
</code></pre>




### <a name="type-cookie_opts">cookie_opts()</a> ###


<pre><code>
cookie_opts() = [<a href="#type-cookie_option">cookie_option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_cookie-1">parse_cookie/1</a></td><td>Parse a cookie header string and return a list of key/values.</td></tr><tr><td valign="top"><a href="#setcookie-3">setcookie/3</a></td><td>Convert a cookie name, value and options to its iodata form.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_cookie-1"></a>

### parse_cookie/1 ###

<pre><code>
parse_cookie(Cookie::binary()) -&gt; [{binary(), binary()}] | {error, badarg}
</code></pre>
<br />

Parse a cookie header string and return a list of key/values.

<a name="setcookie-3"></a>

### setcookie/3 ###

<pre><code>
setcookie(Name::iodata(), Value::iodata(), Opts::<a href="#type-cookie_opts">cookie_opts()</a>) -&gt; binary()
</code></pre>
<br />

Convert a cookie name, value and options to its iodata form.

