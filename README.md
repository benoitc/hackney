

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
- stream the response
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

Make a simple request without pool:<pre>1> hackney:start().
ok
2> {ok, StatusCode, Headers, Client} = hackney:request(<<"https://friendpaste.com">>).
{ok,200,
    [{<<"Server">>,<<"nginx/0.7.62">>},
     {<<"Date">>,<<"Mon, 16 Jul 2012 08:12:39 GMT">>},
     {<<"Content-Type">>,<<"text/html; charset=utf-8">>},
     {<<"Transfer-Encoding">>,<<"chunked">>},
     {<<"Connection">>,<<"keep-alive">>}],
    {client,hackney_ssl_transport,"friendpaste.com",443,netloc,
            [],
            {sslsocket,new_ssl,<0.52.0>},
            infinity,connected,on_body,waiting,4096,
            <<"eee\r\n<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n   "...>>,
            {1,1},
            nil,<<"chunked">>,<<"keep-alive">>,
            <<"text/html; charset=utf-8">>}}
3>  {ok, Body, Client1} = hackney:body(Client).
{ok,<<"<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n    <meta charset=\"utf-8\"/>\n    <title>Friendpaste - Welcome</title>"...>>,
    {client,hackney_ssl_transport,"friendpaste.com",443,netloc,
            [],
            {sslsocket,new_ssl,<0.52.0>},
            infinity,connected,done,done,4096,<<>>,
            {1,1},
            nil,<<"chunked">>,<<"keep-alive">>,
            <<"text/html; charset=utf-8">>}}
4> hackney:close(Client1).
{client,hackney_ssl_transport,"friendpaste.com",443,netloc,
        [],nil,infinity,closed,done,done,4096,<<>>,
        {1,1},
        nil,<<"chunked">>,<<"keep-alive">>,
        <<"text/html; charset=utf-8">>}</pre>

Quick usage example with keepalive:<pre>1> hackney:start().
ok
2> {ok, _, _, Client} = hackney:request(<<"https://friendpaste.com/_all_languages">>),
2> {ok, Body, Client1} = hackney:body(Client).
{ok,<<"[[\"Cucumber\", \"Gherkin\"], [\"abap\", \"ABAP\"], [\"ada\", \"Ada\"], [\"ahk\", \"autohotkey\"], [\"antlr\", \"ANTLR\"], [\"ant"...>>,
    {client,hackney_ssl_transport,"friendpaste.com",443,netloc,
            [],
            {sslsocket,new_ssl,<0.52.0>},
            infinity,connected,done,done,4096,<<>>,
            {1,1},
            nil,<<"chunked">>,<<"keep-alive">>,<<"application/json">>}}
3>
3> ReqBody = << "{
3>      \"id\": \"some_paste_id\",
3>      \"rev\": \"some_revision_id\",
3>      \"changeset\": \"changeset in unidiff format\"
3> }" >>,
3> ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
3> {ok, _, _, Client2} = hackney:send_request(Client1, {post, <<"/">>, ReqHeaders, ReqBody}).
{ok,200,
    [{<<"Server">>,<<"nginx/0.7.62">>},
     {<<"Date">>,<<"Mon, 16 Jul 2012 08:18:52 GMT">>},
     {<<"Content-Type">>,<<"application/json">>},
     {<<"Transfer-Encoding">>,<<"chunked">>},
     {<<"Connection">>,<<"keep-alive">>}],
    {client,hackney_ssl_transport,"friendpaste.com",443,netloc,
            [],
            {sslsocket,new_ssl,<0.52.0>},
            infinity,connected,on_body,waiting,4096,
            <<"7c\r\n{\"url\": \"https://friendpaste.com/2kF2g0nQpVE"...>>,
            {1,1},
            nil,<<"chunked">>,<<"keep-alive">>,<<"application/json">>}}
4> {ok, Body2, Client3} = hackney:body(Client2).
{ok,<<"{\"url\": \"https://friendpaste.com/2kF2g0nQpVE0GzrKaEubiY\", \"rev\": \"333732656130\", \"ok\": true, \"id\": \"2kF2g0nQ"...>>,
    {client,hackney_ssl_transport,"friendpaste.com",443,netloc,
            [],
            {sslsocket,new_ssl,<0.52.0>},
            infinity,connected,done,done,4096,<<>>,
            {1,1},
            nil,<<"chunked">>,<<"keep-alive">>,<<"application/json">>}}
5> hackney:close(Client3).
{client,hackney_ssl_transport,"friendpaste.com",443,netloc,
        [],nil,infinity,closed,done,done,4096,<<>>,
        {1,1},
        nil,<<"chunked">>,<<"keep-alive">>,<<"application/json">>}</pre>

Contribute
----------
For issues, comments or feedback please [create an issue!] [1][1]: http://github.com/benoitc/hackney/issues "hackney issues"


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney.md" class="module">hackney</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_app.md" class="module">hackney_app</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_deps.md" class="module">hackney_deps</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_form.md" class="module">hackney_form</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_headers.md" class="module">hackney_headers</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_pool_sup.md" class="module">hackney_pool_sup</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_request.md" class="module">hackney_request</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_response.md" class="module">hackney_response</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_ssl_transport.md" class="module">hackney_ssl_transport</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_sup.md" class="module">hackney_sup</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_tcp_transport.md" class="module">hackney_tcp_transport</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_transform.md" class="module">hackney_transform</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_url.md" class="module">hackney_url</a></td></tr>
<tr><td><a href="http://github.com/benoitc/hackney/blob/master/doc/hackney_util.md" class="module">hackney_util</a></td></tr></table>

