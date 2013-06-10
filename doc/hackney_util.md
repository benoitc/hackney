

# Module hackney_util #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#char_to_lower-1">char_to_lower/1</a></td><td>Convert [A-Z] characters to lowercase.</td></tr><tr><td valign="top"><a href="#char_to_upper-1">char_to_upper/1</a></td><td>Convert [a-z] characters to uppercase.</td></tr><tr><td valign="top"><a href="#content_type-1">content_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_ipv6-1">is_ipv6/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_hex-1">to_hex/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td>Convert a binary string to lowercase.</td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#token-2">token/2</a></td><td>Parse a token.</td></tr><tr><td valign="top"><a href="#token_ci-2">token_ci/2</a></td><td>Parse a case-insensitive token.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="char_to_lower-1"></a>

### char_to_lower/1 ###


<pre><code>
char_to_lower(Ch::char()) -&gt; char()
</code></pre>

<br></br>


Convert [A-Z] characters to lowercase.
<a name="char_to_upper-1"></a>

### char_to_upper/1 ###


<pre><code>
char_to_upper(Ch::char()) -&gt; char()
</code></pre>

<br></br>


Convert [a-z] characters to uppercase.
<a name="content_type-1"></a>

### content_type/1 ###

`content_type(Name) -> any()`


<a name="is_ipv6-1"></a>

### is_ipv6/1 ###

`is_ipv6(Host) -> any()`


<a name="join-2"></a>

### join/2 ###

`join(L, Separator) -> any()`


<a name="to_binary-1"></a>

### to_binary/1 ###

`to_binary(V) -> any()`


<a name="to_hex-1"></a>

### to_hex/1 ###

`to_hex(Bin) -> any()`


<a name="to_lower-1"></a>

### to_lower/1 ###


<pre><code>
to_lower(L::binary()) -&gt; binary()
</code></pre>

<br></br>


Convert a binary string to lowercase.
<a name="to_upper-1"></a>

### to_upper/1 ###

`to_upper(U) -> any()`


<a name="token-2"></a>

### token/2 ###


<pre><code>
token(Data::binary(), Fun::function()) -&gt; any()
</code></pre>

<br></br>


Parse a token.
<a name="token_ci-2"></a>

### token_ci/2 ###


<pre><code>
token_ci(Data::binary(), Fun::function()) -&gt; any()
</code></pre>

<br></br>



Parse a case-insensitive token.


Changes all characters to lowercase.
