

#hackney - simple HTTP client in Erlang#


Copyright (c) 2012 Benoît Chesneau.

__Version:__ 0.1

# hackney

**hackney** is a simple HTTP client.

Main features:

- no message passing: response is directly streamed to the current
  process and a state is kept in a `#client{}` record.
- binary streams
- SSL support
- Keepalive handling
- basic authentication
- stream the response
- Can send files using the sendfile API
- Chunked encoding support
- Optionnal socket pool
- Used parse transform for shorcut methods calls:`hackney:get("https://friendpaste.com")`

Note: This is a work in progress, see the
[TODO](http://github.com/benoitc/hackney/blob/master/TODO.md) for more
informations on what still need to be done.

## Installation

Download the sources from our [Github
repository](http://github.com/benoitc/hackney)

To buildd the application simply run 'make'. This should build .beam, .app
files and documentation.

To run tests run 'make test'.
To generate doc, run 'make doc'.

Or add it to your rebar config<pre>{deps, [
    ....
    {hackney, ".*", {git, "git://github.com/benoitc/hackney.git", {branch, "master"}}}
]}.</pre>

## Basic usage

The basic usage of hackney is:

### Start hackney

hackney is an
[OTP](http://www.erlang.org/doc/design_principles/users_guide.html)
application. You have to start it first before using all the functions.
The hackney applications will start for you the default socket pool.

To start in the console run :<pre>$ erl -pa ebin
1>> hackney:start().
ok</pre>

It will start hackney and all the application it depends:<pre>application:start(crypto),
application:start(public_key),
application:start(ssl),
application:start(hackney).</pre>

Or add hackney to the applications membe rof your relase an app

### Simple request without pool<pre>Method = get,
URL = <<"https://friendpaste.com">>,
Headers = [],
Payload = <<>>,
Options = [],
{ok, StatusCode, RespHeaders, Client} = hackney:request(Method, URL, Headers,
                                                        Payload, Options).</pre>

The request method return the tupple `{ok, StatusCode, Headers, Client}`
or `{error, Reason}`.

If you enable the **parse_transform**, you can also do:<pre>hackney:get(URL, Headers, Payload, Options)</pre>

To enable parse transform add the following option to the erlang
compiler flags:<pre>{parse_transform, hackney_transform}</pre>

Alternately, you can add it to the module you wish to compile:<pre>-compile([{parse_transform, hackney_transform}]).</pre>

### Read the body<pre>{ok, Body, Client1} = hackney:body(Client).</pre>

`hackney:body/1` fetch the body. To fetch it by chunk you can use the
`hackney:stream/body/1` function:<pre>read_body(MaxLength, Client, Acc) when MaxLength > byte_size(Acc) ->
	case stream_body(Client) of
		{ok, Data, Client2} ->
			read_body(MaxLength, Client2, << Acc/binary, Data/binary >>);
		{done, Client2} ->
			{ok, Acc, Client2};
		{error, Reason} ->
			{error, Reason}
	end.</pre>

### Reuse the client object

If your connection support the keepalive you can reuse the Client
record using the `hackney:send_request/2` function:<pre>ReqBody = << "{
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
hackney:close(Client3).</pre>

Here we are posting a JSON paylod to '/' on the service friendpaste to
create a paste. Then we close the client connection.

### Send a body

hackney helps you send different payload by passing different terms as
the request body:

- `{form, PropList}` : To send a form
- `{file, File}` : To send a file
- Bin: To send a binary

### Use a pool

To reuse a connection globally in your application you can also use a
socket pool. On startup, hackney launch a pool named default. To use it
do the following:<pre>Method = get,
URL = <<"https://friendpaste.com">>,
Headers = [],
Payload = <<>>,
Options = [{pool, default}],
{ok, StatusCode, RespHeaders, Client} = hackney:request(Method, URL, Headers,
                                                        Payload, Options).</pre>

By adding the tuple `{pool, default}` to the options, hackney will use
the connections stored in that pool.

You can also use different pools in your application which will allows
you to maintain some kind of group of connections.<pre>PoolName = mypool,
Options = [{timeout, 150000}, {pool_size, 100}],
{ok, Pid} = hackney:start_pool(PoolName, Options),</pre>

`timeout` is the time we keep alive the conneciton in the pool,
`pool_size` is the number of connections maintained in the pool. Each
connection in a pool is monitored and closed connections are removed
automatically.

To close a pool do:<pre>hackney:stop_pool(PoolName).</pre>

Contribute
----------
For issues, comments or feedback please [create an issue!] [1][1]: http://github.com/benoitc/hackney/issues "hackney issues"


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="hackney.md" class="module">hackney</a></td></tr>
<tr><td><a href="hackney_app.md" class="module">hackney_app</a></td></tr>
<tr><td><a href="hackney_deps.md" class="module">hackney_deps</a></td></tr>
<tr><td><a href="hackney_form.md" class="module">hackney_form</a></td></tr>
<tr><td><a href="hackney_headers.md" class="module">hackney_headers</a></td></tr>
<tr><td><a href="hackney_pool.md" class="module">hackney_pool</a></td></tr>
<tr><td><a href="hackney_request.md" class="module">hackney_request</a></td></tr>
<tr><td><a href="hackney_response.md" class="module">hackney_response</a></td></tr>
<tr><td><a href="hackney_ssl_transport.md" class="module">hackney_ssl_transport</a></td></tr>
<tr><td><a href="hackney_sup.md" class="module">hackney_sup</a></td></tr>
<tr><td><a href="hackney_tcp_transport.md" class="module">hackney_tcp_transport</a></td></tr>
<tr><td><a href="hackney_transform.md" class="module">hackney_transform</a></td></tr>
<tr><td><a href="hackney_url.md" class="module">hackney_url</a></td></tr>
<tr><td><a href="hackney_util.md" class="module">hackney_util</a></td></tr></table>

