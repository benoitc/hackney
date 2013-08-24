

# hackney - HTTP client library in Erlang #

Copyright (c) 2012-2013 Benoît Chesneau.

__Version:__ 0.4.3

# hackney

**hackney** is an HTTP client library for Erlang.

Main features:

- no message passing: response is directly streamed to the current
  process and state is kept in a `#client{}` record.
- binary streams
- SSL support
- Keepalive handling
- basic authentication
- stream the response and the requests
- multipart support (streamed or not)
- chunked encoding support
- Can send files using the sendfile API
- Chunked encoding support
- Optional socket pool
- REST syntax: `hackney:Method(URL)` (where a method can be get, post, put, delete, ...)

Note: This is a work in progress, see the
[TODO](http://github.com/benoitc/hackney/blob/master/TODO.md) for more
informations on what still need to be done.

## Installation

Download the sources from our [Github
repository](http://github.com/benoitc/hackney)

To build the application simply run 'make'. This should build .beam, .app
files and documentation.

To run tests run 'make test'.
To generate doc, run 'make doc'.

Or add it to your rebar config

```
{deps, [
    ....
    {hackney, ".*", {git, "git://github.com/benoitc/hackney.git", {branch, "master"}}}
]}.
```

## Basic usage

The basic usage of hackney is:

### Start hackney

hackney is an
[OTP](http://www.erlang.org/doc/design_principles/users_guide.html)
application. You have to start it first before using any of the functions.
The hackney application will start the default socket pool for you.

To start in the console run:

```
$ erl -pa ebin
1>> hackney:start().
ok
```

It will start hackney and all of the application it depends on:

```
application:start(crypto),
application:start(public_key),
application:start(ssl),
application:start(hackney).
```

Or add hackney to the applications property of your .app in a release

### Simple request without pool

Do a simple requet that will return a client state:

```
Method = get,
URL = <<"https://friendpaste.com">>,
Headers = [],
Payload = <<>>,
Options = [],
{ok, StatusCode, RespHeaders, Client} = hackney:request(Method, URL,
                                                        Headers, Payload,
                                                        Options).
```

The request method return the tuple `{ok, StatusCode, Headers, Client}`
or `{error, Reason}`.

If you prefer the REST syntax, you can also do:

```
hackney:get(URL, Headers, Payload, Options)
```

### Read the body

```
{ok, Body, Client1} = hackney:body(Client).
```

`hackney:body/1` fetch the body. To fetch it by chunk you can use the
`hackney:stream_body/1` function:

```
read_body(MaxLength, Client, Acc) when MaxLength > byte_size(Acc) ->
	case stream_body(Client) of
		{ok, Data, Client2} ->
			read_body(MaxLength, Client2, << Acc/binary, Data/binary >>);
		{done, Client2} ->
			{ok, Acc, Client2};
		{error, Reason} ->
			{error, Reason}
	end.
```

### Reuse the client object

If your connection supports keepalive you can reuse the Client
record using the `hackney:send_request/2` function:

```
ReqBody = << "{
      \"id\": \"some_paste_id\",
      \"rev\": \"some_revision_id\",
      \"changeset\": \"changeset in unidiff format\"
}" >>,
ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
NextPath = <<"/">>,
NextMethod = post,
NextReq = {NextMethod, NextPath, ReqHeaders, ReqBody}
{ok, _, _, Client2} = hackney:send_request(Client1, NextReq).
{ok, Body1, Client3} = hackney:body(Client2),
hackney:close(Client3).
```

Here we are posting a JSON payload to '/' on the friendpaste service to
create a paste. Then we close the client connection.

### Send a body

hackney helps you send different payloads by passing different terms as
the request body:

- `{form, PropList}` : To send a form
- `{multipart, KVs}` : to send you body using the multipart API. KVs can
  be formatted as `{file, Name, Content}` or `Value`
- `{file, File}` : To send a file
- Bin: To send a binary or an iolist

> Note: to send a chunked request, just add the `Transfer-Encoding: chunked`
> header to your headers. Binary and Iolist bodies will be then sent using
> the chunked encoding.

#### Send the body by yourself

While the default is to directly send the request and fetch the status
and headers, if the body is set as the atom `stream` the request and
send_request function will return {ok, Client}. Then you can use the
function `hackney:stream_request_body/2` to stream the request body and
`hackney:start_response/1` to initialize the response.

> Note: The function `hackney:start_response/1` will only accept
> a Client that is waiting for a response (with a response state
> equal to the atom `waiting`).

ex:

```
ReqBody = << "{
      \"id\": \"some_paste_id2\",
      \"rev\": \"some_revision_id\",
      \"changeset\": \"changeset in unidiff format\"
}" >>,
ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
Path = <<"https://friendpaste.com/">>,
Method = post,
{ok, Client} = hackney:request(Method, Path, ReqHeaders, stream,
                               []),

{ok, Client1} = hackney:stream_request_body(ReqBody, Client),
{ok, _Status, _Headers, Client2} = hackney:start_response(Client1),
{ok, Body, Client3} = hackney:body(Client2),
hackney:close(Client3).
```

### Use a pool

To reuse a connection globally in your application you can also use a
socket pool. On startup, hackney launcesh a pool named default. To use it
do the following:

```
Method = get,
URL = <<"https://friendpaste.com">>,
Headers = [],
Payload = <<>>,
Options = [{pool, default}],
{ok, StatusCode, RespHeaders, Client} = hackney:request(Method, URL, Headers,
                                                        Payload, Options).
```

By adding the tuple `{pool, default}` to the options, hackney will use
the connections stored in that pool.

You can also use different pools in your application which allows
you to maintain a group of connections.

```
PoolName = mypool,
Options = [{timeout, 150000}, {pool_size, 100}],
{ok, Pid} = hackney:start_pool(PoolName, Options),
```

`timeout` is the time we keep the conneciton alive in the pool,
`pool_size` is the number of connections maintained in the pool. Each
connection in a pool is monitored and closed connections are removed
automatically.

To close a pool do:

```
hackney:stop_pool(PoolName).
```

> Note: Sometimes you want to always use the default pool in your app
> without having to set the client option each time. You can now do this
> by setting the hackney application environment key `use_default_pool`
> to true.

### Automatically follow a redirection

If the option `{follow_redirect, true}` is given to the request, the
client will be able to automatically follow the redirection and
retrieve the body. The maximum number of connections can be set using the
`{max_redirect, Max}` option. Default is 5.

The client will follow redirects on 301, 302 & 307 if the method is
get or head. If another method is used the tuple
`{ok, maybe_redirect, Status, Headers, Client}` will be returned. It
only follow 303 redirects (see other) if the method is a POST.

Last Location is stored in the `location` property of the client state.

ex:

```
Method = get,
URL = "http://friendpaste.com/",
ReqHeaders = [{<<"accept-encoding">>, <<"identity">>}],
ReqBody = <<>>,
Options = [{follow_redirect, true}, {max_redirect, true}],
{ok, S, H, Client} = hackney:request(Method, URL, ReqHeaders,
                                     ReqBody, Options),
{ok, Body, Client1} = hackney:body(Client).
```

### Proxy a connection

For now only HTTP tunneling is supported. To use an HTTP tunnel add the
option `{proxy, ProxyUrl}` where `ProxyUrl` can be a simple url or an
`{Host, Port}` tuple. If you need to authetnicate set the option
`{proxy_auth, {User, Password}}`.

## Contribute

For issues, comments or feedback please [create an
issue](http://github.com/benoitc/hackney/issues).

### Notes for developers

If you want to contribute patches or improve the docs, you will need to
build hackney using the `rebar_dev.config`  file. It can also be built
using the **Makefile**:

```
$ make dev ; # compile & get deps
$ make devclean ; # clean all files
```


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney.md" class="module">hackney</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_app.md" class="module">hackney_app</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_deps.md" class="module">hackney_deps</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_form.md" class="module">hackney_form</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_headers.md" class="module">hackney_headers</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_multipart.md" class="module">hackney_multipart</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_pool.md" class="module">hackney_pool</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_request.md" class="module">hackney_request</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_response.md" class="module">hackney_response</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_ssl_transport.md" class="module">hackney_ssl_transport</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_sup.md" class="module">hackney_sup</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_tcp_transport.md" class="module">hackney_tcp_transport</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_url.md" class="module">hackney_url</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_util.md" class="module">hackney_util</a></td></tr></table>

