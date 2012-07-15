

#hackney - simple HTTP client in Erlang#


Copyright (c) 2012 Benoît Chesneau.

__Version:__ 0.1

# hackney

**hackney** is a simple HTTP client.

Main features:

- no message passing: response is directly streamed to the current
  process and a state is kept in a `#client{}` record.
- Keepalive handling
- basic authentication
- stream binary bodies
- Can send files using the sendfile API
- Chunked encoding support
- Used parse transform for shorcut methods calls:

ex:<pre>hackney:get("https://friendpaste.com")</pre>

hackney is based on some code from
[cowboy](http://github.com/extend/cownboy) for the HTTP parsing. This is
a work in progress, see the
[TODO](http://github.com/benoitc/hackney/blob/master/TODO.md) for more
informations on what still need to be done.

## Examples

Quick usage example without pool:<pre>application:start(crypto),
application:start(public_key),
application:start(ssl),

{ok, _, _, Client} = hackney:request(<<"https://friendpaste.com">>),
{ok, Body, Client1} = hackney:body(Client),

io:format("body: ~p~n~n", [Body]),

{ok, _, _, Client2} = hackney:send_request(Client1, {get,
                                                     <<"/_all_languages">>,
                                                     [],
                                                     <<>>}),

{ok, Body1, Client3} = hackney:body(Client2),

io:format("body: ~p~n~n", [Body1]),
ReqBody = << "{
     \"id\": \"some_paste_id\",
     \"rev\": \"some_revision_id\",
     \"changeset\": \"changeset in unidiff format\"
}" >>,

ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],

{ok, _, _, Client4} = hackney:send_request(Client3, {post, <<"/">>,
                                                     ReqHeaders,
                                                     ReqBody}),
{ok, Body2, Client5} = hackney:body(Client4),
io:format("body: ~p~n~n", [Body2]),

ReqBody1 = {file, "./examples/test.json"},

{ok, _, _, Client6} = hackney:send_request(Client5, {post, <<"/">>,
                                                     ReqHeaders,
                                                     ReqBody1}),
{ok, Body3, Client7} = hackney:body(Client6),
io:format("body: ~p~n~n", [Body3]),

hackney:close(Client7).</pre>

See the [example][Contribute
----------
For issues, comments or feedback please [create an issue!](http://github.com/benoitc/hackney/blob/master/examples/test.ebin).) [1][1]: http://github.com/benoitc/hackney/issues "hackney issues"


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="hackney.md" class="module">hackney</a></td></tr>
<tr><td><a href="hackney_app.md" class="module">hackney_app</a></td></tr>
<tr><td><a href="hackney_form.md" class="module">hackney_form</a></td></tr>
<tr><td><a href="hackney_headers.md" class="module">hackney_headers</a></td></tr>
<tr><td><a href="hackney_request.md" class="module">hackney_request</a></td></tr>
<tr><td><a href="hackney_response.md" class="module">hackney_response</a></td></tr>
<tr><td><a href="hackney_ssl_transport.md" class="module">hackney_ssl_transport</a></td></tr>
<tr><td><a href="hackney_sup.md" class="module">hackney_sup</a></td></tr>
<tr><td><a href="hackney_tcp_transport.md" class="module">hackney_tcp_transport</a></td></tr>
<tr><td><a href="hackney_transform.md" class="module">hackney_transform</a></td></tr>
<tr><td><a href="hackney_url.md" class="module">hackney_url</a></td></tr>
<tr><td><a href="hackney_util.md" class="module">hackney_util</a></td></tr></table>

