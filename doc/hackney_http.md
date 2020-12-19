

# Module hackney_http #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

HTTP parser in pure Erlang
This parser is able to parse HTTP responses and requests in a
streaming fashion.

<a name="description"></a>

## Description ##

If not set it will be autodetect the type of
binary parsed, if it's a request or a response.

Internally it is keeping a buffer for intermediary steps but don't
keep any state in memory.

The first time you initialise a parser using `hackney_http:parser/0`
or `hackney_http:parser/1` you will receive an opaque record You can
then process it using the function `hackney_http:execute/2`.

Each steps will return the status, some data and the new parser that
you can process later with `hackney_http:execute/2` when
`{more, ...}`  is returnned or `hackney_http:execute/1` in other
cases:

* `{response, http_version(), status(), http_reason(), parser()}`:
when the first line of a response is parsed

* `{request, http_version(), http_method(), uri(), parser()}`:
when the first line of a request (on servers) is parsed

* `{more, parser()}`: when the parser need more
data. The new data should be passed to `hackney_http:execute/2` with
the new parser() state received.

* `{header, {Name :: binary(), Value :: binary()}, parser()}`:
when an header has been parsed. To continue the parsing you must
call the given `parser()` with `hackney_http:execute/1`.

* `{headers_complete, parser()}` : when all headers have been parsed.
To continue the parsing you must call the given `parser()` state
with `hackney_http:execute/1`.

* `{more, parser(), binary()}`: on body, when
the parser need more data. The new data should be passed to
`hackney_http:execute/2` (with `parser()` ) when received. The binary at the end of the
tuple correspond to the actual buffer of the parser. It may be used
for other purpose, like start to parse a new request on pipeline
connections, for a proxy...

* `{ok, binary(), parser()}`: on body, when a chunk has been
parsed. To continue the parsing you must call
`hackney_http:execute/1` with the given `parser()`.

* `{done, binary()}`: when the parsing is done. The binary
given correpond to the non parsed part of the internal buffer.

* `{error, term{}}`: when an error happen


<a name="types"></a>

## Data Types ##




### <a name="type-body_result">body_result()</a> ###


<pre><code>
body_result() = {more, <a href="#type-parser">parser()</a>, binary()} | {ok, binary(), <a href="#type-parser">parser()</a>} | {done, binary()} | done
</code></pre>




### <a name="type-header_result">header_result()</a> ###


<pre><code>
header_result() = {headers_complete, <a href="#type-parser">parser()</a>} | {header, {binary(), binary()}, <a href="#type-parser">parser()</a>}
</code></pre>




### <a name="type-http_method">http_method()</a> ###


<pre><code>
http_method() = binary()
</code></pre>




### <a name="type-http_reason">http_reason()</a> ###


<pre><code>
http_reason() = binary()
</code></pre>




### <a name="type-http_version">http_version()</a> ###


<pre><code>
http_version() = {integer(), integer()}
</code></pre>




### <a name="type-parser">parser()</a> ###


<pre><code>
parser() = #hparser{}
</code></pre>




### <a name="type-parser_option">parser_option()</a> ###


<pre><code>
parser_option() = request | response | auto | {max_empty_lines, integer()} | {max_line_length, integer()}
</code></pre>




### <a name="type-parser_options">parser_options()</a> ###


<pre><code>
parser_options() = [<a href="#type-parser_option">parser_option()</a>]
</code></pre>




### <a name="type-parser_result">parser_result()</a> ###


<pre><code>
parser_result() = {response, <a href="#type-http_version">http_version()</a>, <a href="#type-status">status()</a>, <a href="#type-http_reason">http_reason()</a>, <a href="#type-parser">parser()</a>} | {request, <a href="#type-http_method">http_method()</a>, <a href="#type-uri">uri()</a>, <a href="#type-http_version">http_version()</a>, <a href="#type-parser">parser()</a>} | {more, <a href="#type-parser">parser()</a>} | <a href="#type-header_result">header_result()</a> | <a href="#type-body_result">body_result()</a> | {error, term()}
</code></pre>




### <a name="type-status">status()</a> ###


<pre><code>
status() = integer()
</code></pre>




### <a name="type-uri">uri()</a> ###


<pre><code>
uri() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute-1">execute/1</a></td><td>Execute the parser with the current buffer.</td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td>Execute the parser with the new buffer.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>retrieve a parser property.</td></tr><tr><td valign="top"><a href="#parse_response_version-2">parse_response_version/2</a></td><td></td></tr><tr><td valign="top"><a href="#parser-0">parser/0</a></td><td>Create a new HTTP parser.</td></tr><tr><td valign="top"><a href="#parser-1">parser/1</a></td><td>create a new HTTP parser with options.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute-1"></a>

### execute/1 ###

<pre><code>
execute(Hparser::#hparser{}) -&gt; <a href="#type-parser_result">parser_result()</a>
</code></pre>
<br />

Execute the parser with the current buffer.

<a name="execute-2"></a>

### execute/2 ###

<pre><code>
execute(Hparser::#hparser{}, Bin::binary()) -&gt; <a href="#type-parser_result">parser_result()</a>
</code></pre>
<br />

Execute the parser with the new buffer

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Parser::<a href="#type-parser">parser()</a>, Props::atom() | [atom()]) -&gt; any()
</code></pre>
<br />

retrieve a parser property.
Properties are:

* `buffer`: internal buffer of the parser (non parsed)

* `state`: the current state (on_status, on_header, on_body, done)

* `version`: HTTP version

* `content_length`: content length header if any

* `transfer_encoding`: transfer encoding header if any

* `content_type`: content type header if any

* `location`: location header if any

* `connection`: connection header if any.


<a name="parse_response_version-2"></a>

### parse_response_version/2 ###

`parse_response_version(X1, St) -> any()`

<a name="parser-0"></a>

### parser/0 ###

<pre><code>
parser() -&gt; <a href="#type-parser">parser()</a>
</code></pre>
<br />

Create a new HTTP parser. The parser will autodetect if the parded
binary is a response or a request.

<a name="parser-1"></a>

### parser/1 ###

<pre><code>
parser(Options::<a href="#type-parser_options">parser_options()</a>) -&gt; <a href="#type-parser">parser()</a>
</code></pre>
<br />

create a new HTTP parser with options. By default the type of
parsed binary will be detected.

Available options:

* `auto` : autodetect if the binary parsed is a response or a
request (default).

* `response`: set the parser to parse a response

* `request`: set the parser to parse a request (server)

* `{max_line_lenght, Max}`: set the maximum size of a line parsed
before we give up.

* `{max_lines_empty, Max}`: the maximum number of empty line we
accept before the first line happen


