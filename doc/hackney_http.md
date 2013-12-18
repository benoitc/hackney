

# Module hackney_http #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


HTTP parser in pure Erlang
This parser is able to parse HTTP responses and requests in a
streaming manner.

<a name="description"></a>

## Description ##

If not set it will be autodetect the type of
binary parsed, if it's a request or a response.



Internally it is keeping a buffer for intermediary steps but don't
keep any state in memory.

The first time you initialise a parser using `hackney_http:parser/0`
or `hackney_http:parser/1` you will receive a Function accepting a
binary. This binary will start to be parsed.



Each steps will return the status, sonme data and the next function
to execute:


- `{response, http_version(), status(), http_reason(), continue()}`:
when the first line of a response is parsed
- `{request, http_version(), http_method(), uri(), continue()}`:
when the first line of a request (on servers) is parsed
- `{more, incomplete_handler(binary())}`: when the parser need more
data. The new data should be passed to the given function (the
incomplete handler) when received.
- `{header, {Name :: binary(), Value :: binary()}, continue()}`:
when an header has been parsed. To continue the parsing you must
call the given `continue` function.
- `{headers_complete, continue()}` : when all headers have been parsed.
To continue the parsing you must call the given `continue` function.
- `{more, incomplete_handler(binary()), binary()}`: on body, when
the parser need more data. The new data should be passed to the given function (the
incomplete handler) when received. The binary at the end of the
tuple correspond to the actual buffer of the parser. It may be used
for other purpose, like start to parse a new request on pipeline
connections, for a proxy...
- `{ok, binary(), continue()}`: on body, when a chunk has been
parsed. To continue the parsing you must call the given `continue`
function.
- `done`: when the parsing is done
- `{done, binary()}`: on body, when no more need to be parsing. The binary
given correpond to the non parsed part of the internal buffer.
- `{error, term{}}`: when an error happen
<a name="types"></a>

## Data Types ##




### <a name="type-incomplete_handler">incomplete_handler()</a> ###



<pre><code>
incomplete_handler(T) = fun((binary()) -&gt; T)
</code></pre>





### <a name="type-parser_option">parser_option()</a> ###



<pre><code>
parser_option() = request | response | auto | {max_empty_lines, integer()} | {max_line_length, integer()}
</code></pre>





### <a name="type-parser_options">parser_options()</a> ###



<pre><code>
parser_options() = [<a href="#type-parser_option">parser_option()</a>]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parser-0">parser/0</a></td><td>Create a new HTTP parser.</td></tr><tr><td valign="top"><a href="#parser-1">parser/1</a></td><td>create a new HTTP parser with options.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parser-0"></a>

### parser/0 ###


<pre><code>
parser() -&gt; <a href="#type-incomplete_handler">incomplete_handler</a>(binary())
</code></pre>

<br></br>


Create a new HTTP parser. The parser will autodetect if the parded
binary is a response or a request.
<a name="parser-1"></a>

### parser/1 ###


<pre><code>
parser(Options::<a href="#type-parser_options">parser_options()</a>) -&gt; <a href="#type-incomplete_handler">incomplete_handler</a>(binary())
</code></pre>

<br></br>



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


