

# hackney - HTTP client library in Erlang #

Copyright (c) 2012-2013 Benoît Chesneau.

__Version:__ 0.7.0

# hackney

**hackney** is an HTTP client library for Erlang.

Main features:

- no message passing (except for asynchronous responses): response is
  directly streamed to the current process and state is kept in a `#client{}` record.
- binary streams
- SSL support
- Keepalive handling
- basic authentication
- stream the response and the requests
- fetch a response asynchronously
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

Do a simple request that will return a client state:

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
hackney:Method(URL, Headers, Payload, Options)
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

Note: asynchronous responses automatically checkout the socket at the
end.

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

Ex:

```
ReqBody = << "{
      \"id\": \"some_paste_id2\",
      \"rev\": \"some_revision_id\",
      \"changeset\": \"changeset in unidiff format\"
}" >>,
ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
Path = <<"https://friendpaste.com/">>,
Method = post,
{ok, Client} = hackney:request(Method, Path, ReqHeaders, stream, []),
{ok, Client1} = hackney:stream_request_body(ReqBody, Client),
{ok, _Status, _Headers, Client2} = hackney:start_response(Client1),
{ok, Body, Client3} = hackney:body(Client2),
hackney:close(Client3).
```

### Get a response asynchronously

Since the 0.6 version, hackney is able to fetch the response
asynchrnously using the `async` option:

```
Url = <<"https://friendpaste.com/_all_languages">>,
Opts = [async],
LoopFun = fun(Loop, Ref) ->
        receive
            {Ref, {status, StatusInt, Reason}} ->
                io:format("got status: ~p with reason ~p~n", [StatusInt,
                                                              Reason]),
                Loop(Loop, Ref);
            {Ref, {headers, Headers}} ->
                io:format("got headers: ~p~n", [Headers]),
                Loop(Loop, Ref);
            {Ref, done} ->
                ok;
            {Ref, Bin} ->
                io:format("got chunk: ~p~n", [Bin]),
                Loop(Loop, Ref);

            Else ->
                io:format("else ~p~n", [Else]),
                ok
        end
    end.

{ok, {response_stream, StreamRef}} = hackney:get(Url, [], <<>>, Opts),
LoopFun(LoopFun, StreamRef).
```

### Use the default pool

To reuse a connection globally in your application you can also use a
socket pool. On startup, hackney launches a pool named default. To use it
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
{ok, Pid} = hackney_pool:start_pool(PoolName, Options),
```

`timeout` is the time we keep the connection alive in the pool,
`pool_size` is the number of connections maintained in the pool. Each
connection in a pool is monitored and closed connections are removed
automatically.

To close a pool do:

```
hackney_pool:stop_pool(PoolName).
```

> Note: Sometimes you want to always use the default pool in your app
> without having to set the client option each time. You can now do this
> by setting the hackney application environment key `use_default_pool`
> to true.

### Use the Load-balanced Pool dispatcher

Like the default pool handler hackney_pool, but with the difference that
for each endpoint (domain/ip + port + ssl) of requests, a load balancer
is started allowing as many connections as mentioned in the
configuration.

Each load balancer has N workers that will connect on-demand to each
client.

The load balancer/pools/dispatcher mechanism is based on
[dispcount](https://github.com/ferd/dispcount), which will randomly
contact workers. This means that even though few connections might be
required, the nondeterministic dispatching may make all connections open
at some point. As such, it should be used in cases where the load
is somewhat predictable in terms of base levels.

#### When to use it?

Whenever the HTTP client you're currently using happens to block trying
to access resources that are too scarce for the load required, you may
experience something similar to bufferbloat, where the queuing up of
requests ends up ruining latency for everyone, making the overall
response time terribly slow.

In the case of Erlang, this may happen over pools (like the default)
that dispatch resources through message passing. Then the process'
mailbox ends up as a bottleneck that makes the application too slow.
Dispcount was developed to solve similar issues by avoiding all message
passing on busy workers.

> **WARNING**: use with caution, this pool handler is considered as
> experimental. It's for now nased on the code from the
> [dlhttpc](https://github.com/ferd/dlhttpc) project and adapted to
> hackney.

#### How to use it?

In your application config set the `pool_handler` property to
`hackney_disp`:

```
{hackney, [
    {pool_handler, hackney_disp},
    {restart, permanent},
    {shutdown, 10000},
    {maxr, 10},
    {maxt, 1}
    ...
]}
```

and hackney will automatically use this pool.

The restart, shutdown, maxr, and maxt values allow to configure the
supervisor that will take care of that dispatcher. You can set the
maximum number of connections with the options passed to the client:
`[{max_connections, 200}]' .

> Note: for now you can't force the pool handler / client.
### Automatically follow a redirection

If the option `{follow_redirect, true}' is given to the request, the
client will be able to automatically follow the redirection and
retrieve the body. The maximum number of connections can be set using the
`{max_redirect, Max}' option. Default is 5.

The client will follow redirects on 301, 302 &amp; 307 if the method is
get or head. If another method is used the tuple
`{ok, maybe_redirect, Status, Headers, Client}' will be returned. It
only follow 303 redirects (see other) if the method is a POST.

Last Location is stored in the `location' property of the client state.

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
`{Host, Port}` tuple. If you need to authenticate set the option
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
<tr><td><a href="hackney.md" class="module">hackney</a></td></tr>
<tr><td><a href="hackney_app.md" class="module">hackney_app</a></td></tr>
<tr><td><a href="hackney_deps.md" class="module">hackney_deps</a></td></tr>
<tr><td><a href="hackney_disp.md" class="module">hackney_disp</a></td></tr>
<tr><td><a href="hackney_disp_handler.md" class="module">hackney_disp_handler</a></td></tr>
<tr><td><a href="hackney_form.md" class="module">hackney_form</a></td></tr>
<tr><td><a href="hackney_headers.md" class="module">hackney_headers</a></td></tr>
<tr><td><a href="hackney_multipart.md" class="module">hackney_multipart</a></td></tr>
<tr><td><a href="hackney_pool.md" class="module">hackney_pool</a></td></tr>
<tr><td><a href="hackney_request.md" class="module">hackney_request</a></td></tr>
<tr><td><a href="hackney_response.md" class="module">hackney_response</a></td></tr>
<tr><td><a href="hackney_ssl_transport.md" class="module">hackney_ssl_transport</a></td></tr>
<tr><td><a href="hackney_sup.md" class="module">hackney_sup</a></td></tr>
<tr><td><a href="hackney_tcp_transport.md" class="module">hackney_tcp_transport</a></td></tr>
<tr><td><a href="hackney_url.md" class="module">hackney_url</a></td></tr>
<tr><td><a href="hackney_util.md" class="module">hackney_util</a></td></tr></table>

