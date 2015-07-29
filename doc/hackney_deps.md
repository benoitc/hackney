

# Module hackney_deps #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deps_on_path-0">deps_on_path/0</a></td><td>List of project dependencies on the path.</td></tr><tr><td valign="top"><a href="#ensure-0">ensure/0</a></td><td>Ensure that the ebin and include paths for dependencies of
this application are on the code path.</td></tr><tr><td valign="top"><a href="#ensure-1">ensure/1</a></td><td>Ensure that all ebin and include paths for dependencies
of the application for Module are on the code path.</td></tr><tr><td valign="top"><a href="#get_base_dir-0">get_base_dir/0</a></td><td>Return the application directory for this application.</td></tr><tr><td valign="top"><a href="#get_base_dir-1">get_base_dir/1</a></td><td>Return the application directory for Module.</td></tr><tr><td valign="top"><a href="#local_path-1">local_path/1</a></td><td>Return an application-relative directory for this application.</td></tr><tr><td valign="top"><a href="#local_path-2">local_path/2</a></td><td>Return an application-relative directory from Module's application.</td></tr><tr><td valign="top"><a href="#new_siblings-1">new_siblings/1</a></td><td>Find new siblings paths relative to Module that aren't already on the
code path.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deps_on_path-0"></a>

### deps_on_path/0 ###

<pre><code>
deps_on_path() -&gt; [ProjNameAndVers]
</code></pre>
<br />

List of project dependencies on the path.

<a name="ensure-0"></a>

### ensure/0 ###

<pre><code>
ensure() -&gt; ok
</code></pre>
<br />

Ensure that the ebin and include paths for dependencies of
this application are on the code path. Equivalent to
ensure(?Module).

<a name="ensure-1"></a>

### ensure/1 ###

<pre><code>
ensure(Module) -&gt; ok
</code></pre>
<br />

Ensure that all ebin and include paths for dependencies
of the application for Module are on the code path.

<a name="get_base_dir-0"></a>

### get_base_dir/0 ###

<pre><code>
get_base_dir() -&gt; string()
</code></pre>
<br />

Return the application directory for this application. Equivalent to
get_base_dir(?MODULE).

<a name="get_base_dir-1"></a>

### get_base_dir/1 ###

<pre><code>
get_base_dir(Module) -&gt; string()
</code></pre>
<br />

Return the application directory for Module. It assumes Module is in
a standard OTP layout application in the ebin or src directory.

<a name="local_path-1"></a>

### local_path/1 ###

<pre><code>
local_path(Components) -&gt; string()
</code></pre>
<br />

Return an application-relative directory for this application.
Equivalent to local_path(Components, ?MODULE).

<a name="local_path-2"></a>

### local_path/2 ###

<pre><code>
local_path(Components::[string()], Module) -&gt; string()
</code></pre>
<br />

Return an application-relative directory from Module's application.

<a name="new_siblings-1"></a>

### new_siblings/1 ###

<pre><code>
new_siblings(Module) -&gt; [Dir]
</code></pre>
<br />

Find new siblings paths relative to Module that aren't already on the
code path.

