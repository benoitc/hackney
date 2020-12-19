

# Module hackney_multipart #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

module to encode/decode multipart.

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#boundary-0">boundary/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode_form-2">decode_form/2</a></td><td>decode a multipart form.</td></tr><tr><td valign="top"><a href="#encode_form-1">encode_form/1</a></td><td>encode a list of parts a multipart form.</td></tr><tr><td valign="top"><a href="#encode_form-2">encode_form/2</a></td><td></td></tr><tr><td valign="top"><a href="#len_mp_stream-2">len_mp_stream/2</a></td><td>get the size of a mp stream.</td></tr><tr><td valign="top"><a href="#mp_data_header-2">mp_data_header/2</a></td><td>return the multipart header for a data.</td></tr><tr><td valign="top"><a href="#mp_eof-1">mp_eof/1</a></td><td>return the boundary ending a multipart.</td></tr><tr><td valign="top"><a href="#mp_file_header-2">mp_file_header/2</a></td><td>return the multipart header for a file that will be sent later.</td></tr><tr><td valign="top"><a href="#mp_header-2">mp_header/2</a></td><td>create a generic multipart header.</td></tr><tr><td valign="top"><a href="#mp_mixed_header-2">mp_mixed_header/2</a></td><td>return the mixed multipart header.</td></tr><tr><td valign="top"><a href="#parser-1">parser/1</a></td><td>Return a multipart parser for the given boundary.</td></tr><tr><td valign="top"><a href="#part-3">part/3</a></td><td>create a part.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="boundary-0"></a>

### boundary/0 ###

<pre><code>
boundary() -&gt; binary()
</code></pre>
<br />

<a name="decode_form-2"></a>

### decode_form/2 ###

<pre><code>
decode_form(Boundary::binary(), Body::binary()) -&gt; {ok, list()} | {error, term()}
</code></pre>
<br />

decode a multipart form.

<a name="encode_form-1"></a>

### encode_form/1 ###

`encode_form(Parts) -> any()`

encode a list of parts a multipart form.
Parts can be under the form:

* `{file, Path}` : to send a file

* `{file, Path, ExtraHeaders}` : to send a file with extra headers

* `{file, Path, Name, ExtraHeaders}`: to send a file with DOM element name and extra headers

* `{mp_mixed, Name, Boundary}` to send a mixed multipart.

* `{mp_mixed_eof, Boundary}`: to signal the end of the mixed
multipart boundary.

* `{Name, Data}`: to send a custom content as a part

* `{Name, Data, ExtraHeaders}`: the same as above but with extra
headers.


<a name="encode_form-2"></a>

### encode_form/2 ###

<pre><code>
encode_form(Parts::list(), Boundary::binary()) -&gt; {binary(), integer()}
</code></pre>
<br />

<a name="len_mp_stream-2"></a>

### len_mp_stream/2 ###

`len_mp_stream(Parts, Boundary) -> any()`

get the size of a mp stream. Useful to calculate the
content-length of a full multipart stream and send it as an identity
transfer-encoding instead of chunked so any server can handle it.

Calculated Parts can be under the form:

* `{file, Path}` : to send a file

* `{file, Path, ExtraHeaders}` : to send a file with extra headers

* `{file, Path, Name, ExtraHeaders}` : to send a file with DOM element name and extra headers

* `{mp_mixed, Name, Boundary}` to send a mixed multipart.
multipart boundary.

* `{Name, DataLen}`: to send a custom content as a part

* `{Name, DataLen, ExtraHeaders}`: the same as above but with extra
headers.


<a name="mp_data_header-2"></a>

### mp_data_header/2 ###

<pre><code>
mp_data_header(X1::{Name::binary(), DataLen::integer()} | {Name::binary(), DataLen::integer(), ExtraHeaders::[{binary(), binary()}]} | {Name::binary(), DataLen::integer(), {Disposition::binary(), Params::[{binary(), binary()}]}, ExtraHeaders::[{binary(), binary()}]}, Boundary::binary()) -&gt; {binary(), DataLen::integer()}
</code></pre>
<br />

return the multipart header for a data

<a name="mp_eof-1"></a>

### mp_eof/1 ###

`mp_eof(Boundary) -> any()`

return the boundary ending a multipart

<a name="mp_file_header-2"></a>

### mp_file_header/2 ###

<pre><code>
mp_file_header(X1::{file, Path::binary()} | {file, Path::binary(), ExtraHeaders::[{binary(), binary()}]} | {file, Path::binary(), Name::binary(), ExtraHeaders::[{binary(), binary()}]} | {file, Path::binary(), {Disposition::binary(), Params::[{binary(), binary()}]}, ExtraHeaders::[{binary(), binary()}]}, Boundary::binary()) -&gt; {binary(), FileSize::integer()}
</code></pre>
<br />

return the multipart header for a file that will be sent later

<a name="mp_header-2"></a>

### mp_header/2 ###

`mp_header(Headers, Boundary) -> any()`

create a generic multipart header

<a name="mp_mixed_header-2"></a>

### mp_mixed_header/2 ###

<pre><code>
mp_mixed_header(X1::{Name::binary(), MixedBoundary::binary()}, Boundary::binary()) -&gt; {binary(), 0}
</code></pre>
<br />

return the mixed multipart header

<a name="parser-1"></a>

### parser/1 ###

<pre><code>
parser(Boundary::binary()) -&gt; <a href="#type-part_parser">part_parser()</a>
</code></pre>
<br />

Return a multipart parser for the given boundary.

<a name="part-3"></a>

### part/3 ###

`part(Content, Headers, Boundary) -> any()`

create a part

