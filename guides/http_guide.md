# HTTP Guide

This guide covers hackney's HTTP features in depth.

## Request Anatomy

```erlang
hackney:request(Method, URL, Headers, Body, Options) ->
    {ok, StatusCode, RespHeaders, Ref} | {error, Reason}
```

## Request Bodies

### Binary Body

```erlang
hackney:post(URL,
    [{<<"content-type">>, <<"application/json">>}],
    <<"{\"key\": \"value\"}">>
).
```

### Form-Encoded Body

```erlang
hackney:post(URL, [], {form, [{<<"key">>, <<"value">>}]}).
```

### Multipart Body

```erlang
hackney:post(URL, [], {multipart, [
    {<<"field">>, <<"value">>},
    {file, <<"/path/to/file.txt">>}
]}).
```

### Streaming Body

```erlang
{ok, Ref} = hackney:post(URL, Headers, stream),
ok = hackney:send_body(Ref, <<"chunk1">>),
ok = hackney:send_body(Ref, <<"chunk2">>),
ok = hackney:finish_send_body(Ref),
{ok, Status, RespHeaders, Ref} = hackney:start_response(Ref).
```

## Response Handling

### Read Full Body

```erlang
{ok, 200, Headers, Ref} = hackney:get(URL),
{ok, Body} = hackney:body(Ref).
```

### Stream Response Body

```erlang
{ok, 200, Headers, Ref} = hackney:get(URL),
stream_body(Ref).

stream_body(Ref) ->
    case hackney:stream_body(Ref) of
        {ok, Data} -> process(Data), stream_body(Ref);
        done -> ok
    end.
```

## HTTP/2 Support

Hackney automatically negotiates HTTP/2 for HTTPS connections via ALPN.

### Automatic HTTP/2

```erlang
%% HTTP/2 used automatically when server supports it
{ok, 200, Headers, Body} = hackney:get(
    <<"https://nghttp2.org/">>,
    [],
    <<>>,
    [with_body]
).
```

### Force Protocol

```erlang
%% HTTP/2 only
hackney:get(URL, [], <<>>, [{protocols, [http2]}]).

%% HTTP/1.1 only
hackney:get(URL, [], <<>>, [{protocols, [http1]}]).
```

### Detect Protocol

HTTP/2 responses have lowercase header names:

```erlang
case hd(Headers) of
    {<<"date">>, _} -> http2;
    {<<"Date">>, _} -> http1
end.
```

For details on multiplexing, server push, and architecture, see the [HTTP/2 Guide](http2_guide.md).

## Async Responses

```erlang
{ok, Ref} = hackney:get(URL, [], <<>>, [async]),
receive
    {hackney_response, Ref, {status, Status, _}} -> ok
end,
receive
    {hackney_response, Ref, {headers, Headers}} -> ok
end,
receive
    {hackney_response, Ref, done} -> ok;
    {hackney_response, Ref, Bin} -> ok
end.
```

### Async Once

```erlang
{ok, Ref} = hackney:get(URL, [], <<>>, [{async, once}]),
receive {hackney_response, Ref, Msg} -> ok end,
hackney:stream_next(Ref).  %% Request next message
```

## Connection Pooling

### Default Pool

```erlang
hackney:get(URL).  %% Uses default pool
```

### Named Pools

```erlang
hackney_pool:start_pool(my_api, [
    {max_connections, 100},
    {timeout, 150000}
]),
hackney:get(URL, [], <<>>, [{pool, my_api}]).
```

## Redirects

```erlang
{ok, 200, Headers, Ref} = hackney:get(URL, [], <<>>, [
    {follow_redirect, true},
    {max_redirect, 5}
]).
```

## Proxies

### HTTP Proxy

```erlang
hackney:get(URL, [], <<>>, [
    {proxy, <<"http://proxy:8080">>}
]).
```

### Environment Variables

hackney reads `HTTP_PROXY`, `HTTPS_PROXY`, `NO_PROXY` automatically.

## SSL/TLS

### Custom CA Certificate

```erlang
hackney:get(URL, [], <<>>, [
    {ssl_options, [{cacertfile, "/path/to/ca.crt"}]}
]).
```

### Skip Verification

```erlang
hackney:get(URL, [], <<>>, [insecure]).
```

## Timeouts

```erlang
hackney:get(URL, [], <<>>, [
    {connect_timeout, 5000},
    {recv_timeout, 30000}
]).
```

## Cookies

```erlang
hackney:get(URL, [], <<>>, [{cookie, <<"session=abc">>}]).

%% Parse response cookies
{ok, 200, Headers, _} = hackney:get(URL),
Cookies = hackney:cookies(Headers).
```

## Basic Authentication

```erlang
hackney:get(URL, [], <<>>, [
    {basic_auth, {<<"user">>, <<"pass">>}}
]).
```

## Next Steps

- [HTTP/2 Guide](http2_guide.md) - Multiplexing, server push, architecture
- [WebSocket Guide](websocket_guide.md)
- [Migration Guide](MIGRATION.md)
