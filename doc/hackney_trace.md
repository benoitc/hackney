

# Module hackney_trace #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-filename">filename()</a> ###


<pre><code>
filename() = string()
</code></pre>




### <a name="type-trace_level">trace_level()</a> ###


<pre><code>
trace_level() = max | min | integer()
</code></pre>




### <a name="type-trace_type">trace_type()</a> ###


<pre><code>
trace_type() = io | <a href="#type-filename">filename()</a> | port() | {function(), any()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#disable-0">disable/0</a></td><td>stop tracing.</td></tr><tr><td valign="top"><a href="#enable-2">enable/2</a></td><td>start tracing
start tracing at level Level and send the result either to the file File,
the port Port or to a  trace handler.</td></tr><tr><td valign="top"><a href="#report_event-4">report_event/4</a></td><td></td></tr><tr><td valign="top"><a href="#set_level-1">set_level/1</a></td><td>change the trace level when tracing has already started.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="disable-0"></a>

### disable/0 ###

<pre><code>
disable() -&gt; ok
</code></pre>
<br />

stop tracing

<a name="enable-2"></a>

### enable/2 ###

<pre><code>
enable(Level::<a href="#type-trace_level">trace_level()</a>, File::<a href="#type-trace_type">trace_type()</a>) -&gt; ok
</code></pre>
<br />

start tracing
start tracing at level Level and send the result either to the file File,
the port Port or to a  trace handler.

Note: that it starts a tracer server.
When Destination is the atom io (or the tuple {io, Verbosity}),
%% all (printable) inets trace events (trace_ts events which has
%% Severity within Limit) will be written to stdout using io:format.

<a name="report_event-4"></a>

### report_event/4 ###

`report_event(Severity, Label, Service, Content) -> any()`

<a name="set_level-1"></a>

### set_level/1 ###

<pre><code>
set_level(Level::<a href="#type-trace_level">trace_level()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

change the trace level when tracing has already started.

