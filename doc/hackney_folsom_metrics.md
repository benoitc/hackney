

# Module hackney_folsom_metrics #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decrement_counter-1">decrement_counter/1</a></td><td></td></tr><tr><td valign="top"><a href="#decrement_counter-2">decrement_counter/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#increment_counter-1">increment_counter/1</a></td><td></td></tr><tr><td valign="top"><a href="#increment_counter-2">increment_counter/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_gauge-2">update_gauge/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_histogram-2">update_histogram/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_meter-2">update_meter/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decrement_counter-1"></a>

### decrement_counter/1 ###

<pre><code>
decrement_counter(Name::any()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="decrement_counter-2"></a>

### decrement_counter/2 ###

<pre><code>
decrement_counter(Name::any(), Value::pos_integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="delete-1"></a>

### delete/1 ###

`delete(Name) -> any()`

<a name="increment_counter-1"></a>

### increment_counter/1 ###

<pre><code>
increment_counter(Name::any()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="increment_counter-2"></a>

### increment_counter/2 ###

<pre><code>
increment_counter(Name::any(), Value::pos_integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(X1::atom(), Name::any()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="update_gauge-2"></a>

### update_gauge/2 ###

<pre><code>
update_gauge(Name::any(), Value::number()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="update_histogram-2"></a>

### update_histogram/2 ###

<pre><code>
update_histogram(Name::any(), Fun::number()) -&gt; ok | {error, term()}
</code></pre>
<br />

<a name="update_meter-2"></a>

### update_meter/2 ###

<pre><code>
update_meter(Name::any(), Value::number()) -&gt; ok | {error, term()}
</code></pre>
<br />

