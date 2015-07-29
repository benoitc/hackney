

# Module hackney_mimetypes #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#extension-1">extension/1</a></td><td>Transform an extension to a mimetype.</td></tr><tr><td valign="top"><a href="#filename-1">filename/1</a></td><td>Return the mimetype for any file by looking at its extension.</td></tr><tr><td valign="top"><a href="#mime_to_exts-1">mime_to_exts/1</a></td><td>Return the list of extensions for a mimetype.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="extension-1"></a>

### extension/1 ###

<pre><code>
extension(Ext::binary()) -&gt; binary()
</code></pre>
<br />

Transform an extension to a mimetype

Example:

```
  1> mimetypes:extension(<<"c">>).
  <<"text/x-c">>
```

<a name="filename-1"></a>

### filename/1 ###

<pre><code>
filename(Path::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; binary()
</code></pre>
<br />

Return the mimetype for any file by looking at its extension.
Example:

```
  1> hackney_mimetypes:filename(<<"test.cpp">>).
  <<"text/x-c">>
```

<a name="mime_to_exts-1"></a>

### mime_to_exts/1 ###

<pre><code>
mime_to_exts(Mimetype::binary()) -&gt; [binary()]
</code></pre>
<br />

Return the list of extensions for a mimetype.
Example:

```
  1> hackney_mimetypes:mime_to_exts(<<"text/plain">>).
  [<<"txt">>,<<"text">>,<<"conf">>,<<"def">>,<<"list">>,<<"log">>,<<"in">>]
```

