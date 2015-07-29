

# Module hackney_date #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#asctime_to_date-1">asctime_to_date/1</a></td><td>Parse an asctime date.</td></tr><tr><td valign="top"><a href="#date_to_rfc2109-1">date_to_rfc2109/1</a></td><td>Return the date formatted according to RFC2109.</td></tr><tr><td valign="top"><a href="#parse_http_date-1">parse_http_date/1</a></td><td>Parse an HTTP date (RFC1123, RFC850 or asctime date).</td></tr><tr><td valign="top"><a href="#rfc1123_to_date-1">rfc1123_to_date/1</a></td><td>Parse an RFC1123 date.</td></tr><tr><td valign="top"><a href="#rfc2109_to_date-1">rfc2109_to_date/1</a></td><td>Parse an RFC2109 date.</td></tr><tr><td valign="top"><a href="#rfc850_to_date-1">rfc850_to_date/1</a></td><td>Parse an RFC850 date.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="asctime_to_date-1"></a>

### asctime_to_date/1 ###

<pre><code>
asctime_to_date(Data::binary()) -&gt; any()
</code></pre>
<br />

Parse an asctime date.

<a name="date_to_rfc2109-1"></a>

### date_to_rfc2109/1 ###

<pre><code>
date_to_rfc2109(X1::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; binary()
</code></pre>
<br />

Return the date formatted according to RFC2109.

<a name="parse_http_date-1"></a>

### parse_http_date/1 ###

<pre><code>
parse_http_date(Data::binary()) -&gt; any()
</code></pre>
<br />

Parse an HTTP date (RFC1123, RFC850 or asctime date).

<a name="rfc1123_to_date-1"></a>

### rfc1123_to_date/1 ###

<pre><code>
rfc1123_to_date(Data::binary()) -&gt; any()
</code></pre>
<br />

Parse an RFC1123 date.

<a name="rfc2109_to_date-1"></a>

### rfc2109_to_date/1 ###

<pre><code>
rfc2109_to_date(Data::binary()) -&gt; any()
</code></pre>
<br />

Parse an RFC2109 date.

<a name="rfc850_to_date-1"></a>

### rfc850_to_date/1 ###

<pre><code>
rfc850_to_date(Data::binary()) -&gt; any()
</code></pre>
<br />

Parse an RFC850 date.

