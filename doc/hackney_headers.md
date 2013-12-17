

# Module hackney_headers #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Delete the header corresponding to key if it is present.</td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td>fold the list of headers.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>get the value of the header.</td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td>Insert the pair into the headers, merging with any pre-existing key.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>initialise an header dict.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td>store the pair into the headers, replacing any pre-existing key.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-2"></a>

### delete/2 ###

`delete(Key, Headers) -> any()`

Delete the header corresponding to key if it is present.
<a name="fold-3"></a>

### fold/3 ###

`fold(Fun, Acc0, Headers) -> any()`

fold the list of headers
<a name="get_value-2"></a>

### get_value/2 ###

`get_value(Key, Headers) -> any()`

get the value of the header
<a name="get_value-3"></a>

### get_value/3 ###

`get_value(Key, Headers, Default) -> any()`


<a name="insert-3"></a>

### insert/3 ###

`insert(Key, Value, Headers) -> any()`

Insert the pair into the headers, merging with any pre-existing key.
A merge is done with Value = V0 ++ ", " ++ V1.
<a name="new-0"></a>

### new/0 ###

`new() -> any()`

initialise an header dict
<a name="new-1"></a>

### new/1 ###

`new(D) -> any()`


<a name="store-3"></a>

### store/3 ###

`store(Key, Value, Headers) -> any()`

store the pair into the headers, replacing any pre-existing key.
<a name="to_list-1"></a>

### to_list/1 ###

`to_list(Headers) -> any()`


<a name="update-2"></a>

### update/2 ###

`update(Headers, KVs) -> any()`


