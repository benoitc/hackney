

# Module hackney_bstr #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-cp">cp()</a> ###


__abstract datatype__: `cp()`




### <a name="type-part">part()</a> ###


<pre><code>
part() = {Start::non_neg_integer(), Length::integer()}
</code></pre>

 END: Remove when OTP 17 not officially supported

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alpha-2">alpha/2</a></td><td>Parse a list of case-insensitive alpha characters.</td></tr><tr><td valign="top"><a href="#char_to_lower-1">char_to_lower/1</a></td><td>Convert [A-Z] characters to lowercase.</td></tr><tr><td valign="top"><a href="#char_to_upper-1">char_to_upper/1</a></td><td>Convert [a-z] characters to uppercase.</td></tr><tr><td valign="top"><a href="#digits-1">digits/1</a></td><td>Parse a list of digits as a non negative integer.</td></tr><tr><td valign="top"><a href="#digits-2">digits/2</a></td><td></td></tr><tr><td valign="top"><a href="#digits-3">digits/3</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td></td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>Parse a list of the given type.</td></tr><tr><td valign="top"><a href="#nonempty_list-2">nonempty_list/2</a></td><td>Parse a non-empty list of the given type.</td></tr><tr><td valign="top"><a href="#parameterized_tokens-1">parameterized_tokens/1</a></td><td>Parse a non empty list of tokens followed with optional parameters.</td></tr><tr><td valign="top"><a href="#params-2">params/2</a></td><td>Parse a list of parameters (a=b;c=d).</td></tr><tr><td valign="top"><a href="#quoted_string-2">quoted_string/2</a></td><td></td></tr><tr><td valign="top"><a href="#split-3">split/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_hex-1">to_hex/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td>Convert a binary string to lowercase.</td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#token-2">token/2</a></td><td>Parse a token.</td></tr><tr><td valign="top"><a href="#token_ci-2">token_ci/2</a></td><td>Parse a case-insensitive token.</td></tr><tr><td valign="top"><a href="#trim-1">trim/1</a></td><td></td></tr><tr><td valign="top"><a href="#whitespace-2">whitespace/2</a></td><td>Skip whitespace.</td></tr><tr><td valign="top"><a href="#word-2">word/2</a></td><td>Parse either a token or a quoted string.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alpha-2"></a>

### alpha/2 ###

<pre><code>
alpha(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

Parse a list of case-insensitive alpha characters.

Changes all characters to lowercase.

<a name="char_to_lower-1"></a>

### char_to_lower/1 ###

<pre><code>
char_to_lower(Ch::char()) -&gt; char()
</code></pre>
<br />

Convert [A-Z] characters to lowercase.

<a name="char_to_upper-1"></a>

### char_to_upper/1 ###

<pre><code>
char_to_upper(Ch::char()) -&gt; char()
</code></pre>
<br />

Convert [a-z] characters to uppercase.

<a name="digits-1"></a>

### digits/1 ###

<pre><code>
digits(Data::binary()) -&gt; non_neg_integer() | {error, badarg}
</code></pre>
<br />

Parse a list of digits as a non negative integer.

<a name="digits-2"></a>

### digits/2 ###

<pre><code>
digits(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

<a name="digits-3"></a>

### digits/3 ###

<pre><code>
digits(Data::binary(), Fun::function(), Acc::non_neg_integer()) -&gt; any()
</code></pre>
<br />

<a name="join-2"></a>

### join/2 ###

`join(L, Separator) -> any()`

<a name="list-2"></a>

### list/2 ###

<pre><code>
list(Data::binary(), Fun::function()) -&gt; list() | {error, badarg}
</code></pre>
<br />

Parse a list of the given type.

<a name="nonempty_list-2"></a>

### nonempty_list/2 ###

<pre><code>
nonempty_list(Data::binary(), Fun::function()) -&gt; [any(), ...] | {error, badarg}
</code></pre>
<br />

Parse a non-empty list of the given type.

<a name="parameterized_tokens-1"></a>

### parameterized_tokens/1 ###

<pre><code>
parameterized_tokens(Data::binary()) -&gt; any()
</code></pre>
<br />

Parse a non empty list of tokens followed with optional parameters.

<a name="params-2"></a>

### params/2 ###

<pre><code>
params(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

Parse a list of parameters (a=b;c=d).

<a name="quoted_string-2"></a>

### quoted_string/2 ###

<pre><code>
quoted_string(X1::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

<a name="split-3"></a>

### split/3 ###

<pre><code>
split(Subject, Pattern, Options) -&gt; Parts
</code></pre>

<ul class="definitions"><li><code>Subject = binary()</code></li><li><code>Pattern = binary() | [binary()] | <a href="#type-cp">cp()</a></code></li><li><code>Options = [Option]</code></li><li><code>Option = {scope, <a href="#type-part">part()</a>} | trim | global | trim_all</code></li><li><code>Parts = [binary()]</code></li></ul>

<a name="to_binary-1"></a>

### to_binary/1 ###

`to_binary(V) -> any()`

<a name="to_hex-1"></a>

### to_hex/1 ###

`to_hex(Bin) -> any()`

<a name="to_lower-1"></a>

### to_lower/1 ###

<pre><code>
to_lower(L::binary() | atom() | list()) -&gt; binary()
</code></pre>
<br />

Convert a binary string to lowercase.

<a name="to_upper-1"></a>

### to_upper/1 ###

<pre><code>
to_upper(U::binary() | atom() | list()) -&gt; binary()
</code></pre>
<br />

<a name="token-2"></a>

### token/2 ###

<pre><code>
token(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

Parse a token.

<a name="token_ci-2"></a>

### token_ci/2 ###

<pre><code>
token_ci(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

Parse a case-insensitive token.

Changes all characters to lowercase.

<a name="trim-1"></a>

### trim/1 ###

<pre><code>
trim(Data::binary()) -&gt; binary()
</code></pre>
<br />

<a name="whitespace-2"></a>

### whitespace/2 ###

<pre><code>
whitespace(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

Skip whitespace.

<a name="word-2"></a>

### word/2 ###

<pre><code>
word(Data::binary(), Fun::function()) -&gt; any()
</code></pre>
<br />

Parse either a token or a quoted string.

