

# Module hackney_util #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#filter_options-3">filter_options/3</a></td><td>filter a proplists and only keep allowed keys.</td></tr><tr><td valign="top"><a href="#is_ipv6-1">is_ipv6/1</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_apply_defaults-2">maybe_apply_defaults/2</a></td><td></td></tr><tr><td valign="top"><a href="#require-1">require/1</a></td><td>Start the given applications if they were not already started.</td></tr><tr><td valign="top"><a href="#set_option_default-3">set_option_default/3</a></td><td>set the default options in a proplists if not defined.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="filter_options-3"></a>

### filter_options/3 ###


<pre><code>
filter_options(Tail::[{atom(), any()} | {raw, any(), any(), any()}], AllowedKeys::[atom()], Acc) -&gt; Acc
</code></pre>

<ul class="definitions"><li><code>Acc = [any()]</code></li></ul>

filter a proplists and only keep allowed keys
<a name="is_ipv6-1"></a>

### is_ipv6/1 ###

`is_ipv6(Host) -> any()`


<a name="maybe_apply_defaults-2"></a>

### maybe_apply_defaults/2 ###

`maybe_apply_defaults(Rest, Options) -> any()`


<a name="require-1"></a>

### require/1 ###


<pre><code>
require(Rest::[module()]) -&gt; ok
</code></pre>
<br />

Start the given applications if they were not already started.
<a name="set_option_default-3"></a>

### set_option_default/3 ###


<pre><code>
set_option_default(Opts, Key::atom(), Value::any()) -&gt; Opts
</code></pre>

<ul class="definitions"><li><code>Opts = [{atom(), any()}]</code></li></ul>

set the default options in a proplists if not defined
