

# Module hackney_multipart #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


module to encode/decode forms.


<a name="types"></a>

## Data Types ##




### <a name="type-body_cont">body_cont()</a> ###



<pre><code>
body_cont() = <a href="#type-cont">cont</a>(<a href="#type-more">more</a>(<a href="#type-body_result">body_result()</a>))
</code></pre>





### <a name="type-body_result">body_result()</a> ###



<pre><code>
body_result() = {body, binary(), <a href="#type-body_cont">body_cont()</a>} | <a href="#type-end_of_part">end_of_part()</a>
</code></pre>





### <a name="type-cont">cont()</a> ###



<pre><code>
cont(T) = fun(() -&gt; T)
</code></pre>





### <a name="type-end_of_part">end_of_part()</a> ###



<pre><code>
end_of_part() = {end_of_part, <a href="#type-cont">cont</a>(<a href="#type-more">more</a>(<a href="#type-part_result">part_result()</a>))}
</code></pre>





### <a name="type-headers">headers()</a> ###



<pre><code>
headers() = {headers, <a href="#type-http_headers">http_headers()</a>, <a href="#type-body_cont">body_cont()</a>}
</code></pre>





### <a name="type-http_headers">http_headers()</a> ###



<pre><code>
http_headers() = [{binary(), binary()}]
</code></pre>





### <a name="type-more">more()</a> ###



<pre><code>
more(T) = T | {more, <a href="#type-parser">parser</a>(T)}
</code></pre>





### <a name="type-parser">parser()</a> ###



<pre><code>
parser(T) = fun((binary()) -&gt; T)
</code></pre>





### <a name="type-part_parser">part_parser()</a> ###



<pre><code>
part_parser() = <a href="#type-parser">parser</a>(<a href="#type-more">more</a>(<a href="#type-part_result">part_result()</a>))
</code></pre>





### <a name="type-part_result">part_result()</a> ###



<pre><code>
part_result() = <a href="#type-headers">headers()</a> | eof
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#boundary-0">boundary/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode_form-2">decode_form/2</a></td><td>decode a multipart form.</td></tr><tr><td valign="top"><a href="#decode_form-3">decode_form/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode_form-1">encode_form/1</a></td><td>encode a list of properties in a form.</td></tr><tr><td valign="top"><a href="#field-1">field/1</a></td><td></td></tr><tr><td valign="top"><a href="#mp_header-4">mp_header/4</a></td><td></td></tr><tr><td valign="top"><a href="#parser-1">parser/1</a></td><td>Return a multipart parser for the given boundary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="boundary-0"></a>

### boundary/0 ###

`boundary() -> any()`


<a name="decode_form-2"></a>

### decode_form/2 ###


<pre><code>
decode_form(Boundary::binary(), Body::binary()) -&gt; {ok, list()} | {error, term()}
</code></pre>

<br></br>


decode a multipart form.
<a name="decode_form-3"></a>

### decode_form/3 ###

`decode_form(Boundary, Body, Acc) -> any()`


<a name="encode-2"></a>

### encode/2 ###

`encode(X1, Boundary) -> any()`


<a name="encode_form-1"></a>

### encode_form/1 ###

`encode_form(KVs) -> any()`

encode a list of properties in a form.
<a name="field-1"></a>

### field/1 ###

`field(V) -> any()`


<a name="mp_header-4"></a>

### mp_header/4 ###

`mp_header(Field, FileName, CType, Boundary) -> any()`


<a name="parser-1"></a>

### parser/1 ###


<pre><code>
parser(Boundary::binary()) -&gt; <a href="#type-part_parser">part_parser()</a>
</code></pre>

<br></br>


Return a multipart parser for the given boundary.
