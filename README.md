# hackney

An HTTP client for Erlang. Simple, reliable, fast.

[![Build Status](https://github.com/benoitc/hackney/workflows/build/badge.svg)](https://github.com/benoitc/hackney/actions?query=workflow%3Abuild)
[![Hex pm](http://img.shields.io/hexpm/v/hackney.svg?style=flat)](https://hex.pm/packages/hackney)

## Why hackney?

- **HTTP/3 support** - Experimental QUIC/HTTP3 via lsquic. Opt-in with `{protocols, [http3, http2, http1]}`.
- **HTTP/2 support** - Automatic protocol negotiation via ALPN. Multiplexing, header compression, flow control.
- **Process per connection** - Each connection runs in its own `gen_statem` process. Clean isolation, automatic cleanup on crashes.
- **Connection pooling** - Reuse connections automatically. Configure pools per host or globally.
- **Streaming** - Stream request bodies, response bodies, or both. Handle large files without loading them in memory.
- **Async responses** - Get response chunks as messages. Process other work while waiting.
- **WebSocket support** - Full WebSocket client with the same process-per-connection model.
- **IPv6 first** - Happy Eyeballs algorithm tries IPv6 before IPv4 for faster connections on modern networks.
- **SSL by default** - Secure connections with certificate verification using Mozilla's CA bundle.

## Quick Start

```erlang
%% Start hackney
application:ensure_all_started(hackney).

%% Simple GET
{ok, 200, _Headers, Body} = hackney:get(<<"https://httpbin.org/get">>, [], <<>>, [with_body]).

%% POST JSON
Headers = [{<<"content-type">>, <<"application/json">>}],
Payload = <<"{\"key\": \"value\"}">>,
{ok, 201, _, _} = hackney:post(<<"https://httpbin.org/post">>, Headers, Payload, [with_body]).

%% Stream large response
{ok, 200, _, Ref} = hackney:get(<<"https://example.com/large-file">>),
stream_body(Ref).

stream_body(Ref) ->
    case hackney:stream_body(Ref) of
        {ok, Chunk} -> io:format("~p bytes~n", [byte_size(Chunk)]), stream_body(Ref);
        done -> ok
    end.
```

## Installation

### Rebar3

```erlang
{deps, [hackney]}.
```

### Mix

```elixir
{:hackney, "~> 2.0"}
```

## Documentation

| Guide | Description |
|-------|-------------|
| [Getting Started](GETTING_STARTED.md) | Installation, first requests, basic patterns |
| [HTTP Guide](guides/http_guide.md) | Requests, responses, streaming, async, pools |
| [HTTP/2 Guide](guides/http2_guide.md) | HTTP/2 protocol, ALPN, multiplexing, server push |
| [HTTP/3 Guide](guides/http3_guide.md) | HTTP/3 over QUIC, opt-in configuration, Alt-Svc |
| [WebSocket Guide](guides/websocket_guide.md) | Connect, send, receive, active mode |
| [Design Guide](guides/design.md) | Architecture, pooling, load regulation internals |
| [Migration Guide](guides/MIGRATION.md) | Upgrading from hackney 1.x |
| [API Reference](https://hexdocs.pm/hackney) | Full module documentation |
| [Changelog](NEWS.md) | Version history |

## Features

### HTTP Methods

All standard HTTP methods as convenient functions:

```erlang
hackney:get(URL).
hackney:post(URL, Headers, Body).
hackney:put(URL, Headers, Body).
hackney:delete(URL).
hackney:head(URL).
hackney:options(URL).
hackney:patch(URL, Headers, Body).
```

### Connection Pooling

Connections are pooled by default. Configure pools for different use cases:

```erlang
%% Use default pool
hackney:get(URL).

%% Named pool with custom settings
hackney_pool:start_pool(api_pool, [{max_connections, 100}]),
hackney:get(URL, [], <<>>, [{pool, api_pool}]).

%% No pooling for one-off requests
hackney:get(URL, [], <<>>, [{pool, false}]).
```

### Streaming

Stream request bodies for uploads:

```erlang
{ok, Ref} = hackney:post(URL, Headers, stream),
hackney:send_body(Ref, <<"chunk 1">>),
hackney:send_body(Ref, <<"chunk 2">>),
hackney:finish_send_body(Ref),
{ok, Status, _, Ref} = hackney:start_response(Ref).
```

Stream response bodies for downloads:

```erlang
{ok, 200, _, Ref} = hackney:get(URL),
read_chunks(Ref).

read_chunks(Ref) ->
    case hackney:stream_body(Ref) of
        {ok, Data} -> process(Data), read_chunks(Ref);
        done -> ok
    end.
```

### Async Responses

Receive response data as messages:

```erlang
{ok, Ref} = hackney:get(URL, [], <<>>, [async]),
receive
    {hackney_response, Ref, {status, 200, _}} -> ok
end,
receive
    {hackney_response, Ref, {headers, Headers}} -> ok
end,
receive_body(Ref).

receive_body(Ref) ->
    receive
        {hackney_response, Ref, done} -> ok;
        {hackney_response, Ref, Bin} -> receive_body(Ref)
    end.
```

### WebSocket

```erlang
{ok, Conn} = hackney:ws_connect(<<"wss://echo.websocket.org">>),
ok = hackney:ws_send(Conn, {text, <<"hello">>}),
{ok, {text, <<"hello">>}} = hackney:ws_recv(Conn),
hackney:ws_close(Conn).
```

### HTTP/2

HTTP/2 is used automatically when the server supports it:

```erlang
%% Automatic HTTP/2 via ALPN negotiation
{ok, 200, Headers, Body} = hackney:get(<<"https://nghttp2.org/">>, [], <<>>, [with_body]).

%% Force HTTP/1.1 only
hackney:get(URL, [], <<>>, [{protocols, [http1]}]).

%% Force HTTP/2 only
hackney:get(URL, [], <<>>, [{protocols, [http2]}]).
```

### HTTP/3 (Experimental)

HTTP/3 support is **opt-in**. Enable it per-request or globally:

```erlang
%% Enable HTTP/3 for a single request
hackney:get(URL, [], <<>>, [{protocols, [http3, http2, http1]}]).

%% Enable HTTP/3 globally (application-wide)
application:set_env(hackney, default_protocols, [http3, http2, http1]).
```

**Note:** HTTP/3 uses QUIC (UDP transport). Some networks may block UDP traffic.

### Multipart

Upload files and form data:

```erlang
Multipart = {multipart, [
    {<<"field">>, <<"value">>},
    {file, <<"/path/to/file.txt">>},
    {file, <<"/path/to/image.png">>, <<"image.png">>, [{<<"content-type">>, <<"image/png">>}]}
]},
hackney:post(URL, [], Multipart).
```

### Proxy Support

```erlang
%% HTTP proxy
hackney:get(URL, [], <<>>, [{proxy, <<"http://proxy:8080">>}]).

%% With authentication
hackney:get(URL, [], <<>>, [{proxy, <<"http://user:pass@proxy:8080">>}]).

%% Environment variables work automatically
%% HTTP_PROXY, HTTPS_PROXY, NO_PROXY
```

### Redirects

```erlang
%% Follow redirects automatically
hackney:get(URL, [], <<>>, [{follow_redirect, true}, {max_redirect, 5}]).
```

### Timeouts

```erlang
hackney:get(URL, [], <<>>, [
    {connect_timeout, 5000},  %% Connection timeout
    {recv_timeout, 30000}     %% Response timeout
]).
```

### SSL Options

```erlang
%% Custom CA certificate
hackney:get(URL, [], <<>>, [
    {ssl_options, [{cacertfile, "/path/to/ca.pem"}]}
]).

%% Skip verification (development only)
hackney:get(URL, [], <<>>, [insecure]).
```

## Modules

| Module | Purpose |
|--------|---------|
| `hackney` | Main API - requests, connections, WebSocket |
| `hackney_pool` | Connection pool management |
| `hackney_url` | URL parsing and encoding |
| `hackney_headers` | Header manipulation |
| `hackney_multipart` | Multipart encoding |
| `hackney_cookie` | Cookie parsing |
| `hackney_http` | HTTP protocol parser |

## Requirements

Erlang/OTP 27+

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on pull requests and development setup.

Issues and pull requests welcome at https://github.com/benoitc/hackney

## Support

Professional support is available via Enki Multimedia. Contact [sales@enki-multimedia.eu](mailto:sales@enki-multimedia.eu).

## License

Apache 2.0 - See [LICENSE](LICENSE) and [NOTICE](NOTICE)

Copyright (c) 2012-2026 Benoit Chesneau
