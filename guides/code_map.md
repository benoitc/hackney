# Where Things Live

This page maps each part of hackney to the modules that implement it and the
tests that cover it. Use it when you want to change hackney or follow a bug
into the source: find the concept, open the module, run the tests listed next
to it. For how the parts work together, read the [Design Guide](design.md)
first.

## How a request travels

1. `hackney:request/5` (`src/hackney.erl`) parses the URL and options, runs
   the middleware chain, and asks for a connection.
2. `hackney_load_regulation` takes a per-host slot, then `hackney_pool` hands
   out a connection process.
3. `hackney_conn` owns the socket. It sends the request and reads the response
   for HTTP/1.1 itself, through the `h2` dependency for HTTP/2, and through
   `hackney_h3` and the `quic` dependency for HTTP/3.
4. When the response is read, an HTTP/1.1 connection goes back to the pool or
   closes. HTTP/2 and HTTP/3 connections stay registered in the pool and are
   shared by every caller for that host.

## Map

Source modules are in `src/`. Test modules are in `test/` and end in `_tests`,
so `hackney_pool` below means `test/hackney_pool_tests.erl`.

| Concept | Modules | Tests |
|---------|---------|-------|
| Public API | `hackney` | `hackney_integration`, `hackney_query`, `hackney_redirect`, `hackney_send_request` |
| Connection process | `hackney_conn`, `hackney_conn_sup` | `hackney_conn`, `hackney_direct_owner`, `hackney_close_hang`, `hackney_recv_timeout` |
| Pool and concurrency | `hackney_pool`, `hackney_load_regulation`, `hackney_keepalive` | `hackney_pool`, `hackney_pool_fault`, `hackney_pool_h2h3_fault`, `hackney_pool_chaos`, `hackney_pool_safety`, `hackney_load_regulation` |
| HTTP/1.1 wire format | `hackney_http`, `hackney_headers`, `hackney_multipart`, `hackney_cookie`, `hackney_deflate` | `hackney_http`, `hackney_header_limits`, `hackney_chunk_limit`, `hackney_max_body_size`, `hackney_multipart` |
| HTTP/2 | `hackney_conn` (functions prefixed `h2_`), `hackney_h2_stream` | `hackney_http2_*`, `hackney_h2_bidi` |
| HTTP/3 | `hackney_h3`, `hackney_altsvc`, `hackney_conn` (functions prefixed `h3_`) | `hackney_http3_*`, `hackney_h3_*`, `hackney_altsvc`, `hackney_pool_h3` |
| TLS | `hackney_ssl`, `hackney_ssl_certificate` | `hackney_ssl`, `hackney_ssl_alpn`, `hackney_insecure`, `hackney_tls_resumption_race` |
| Connecting | `hackney_happy`, `hackney_tcp`, `hackney_local_tcp`, `hackney_cidr` | `hackney_happy`, `hackney_connect_race`, `hackney_pool_unix_socket` |
| Proxies | `hackney_http_connect`, `hackney_socks5` | `hackney_proxy`, `hackney_https_proxy`, `hackney_http_connect`, `hackney_socks5` |
| WebSocket | `hackney_ws`, `hackney_ws_proto` | `hackney_ws`, `hackney_ws_proto` |
| WebTransport | `hackney_wt` | `hackney_wt` |
| Middleware | `hackney_middleware` | `hackney_middleware`, `hackney_middleware_integration` |
| URLs and helpers | `hackney_url`, `hackney_bstr`, `hackney_date`, `hackney_util`, `hackney_trace` | `hackney_url`, `hackney_bstr`, `hackney_date`, `hackney_util`, `hackney_trace` |

Run one test module with:

```sh
rebar3 eunit --module=hackney_pool_tests
```

## What is public and what is internal

You can rely on `hackney`, `hackney_pool`, `hackney_url`, `hackney_headers`,
`hackney_multipart`, `hackney_cookie` and the `hackney_middleware` behaviour.
The other modules are internal and can change between releases.

## Notes

- `hackney_conn` is the largest module. Its states are separated by `State:`
  banner comments; the HTTP/2 and HTTP/3 code is in functions prefixed `h2_`
  and `h3_`. Read the state machine section of the Design Guide before
  changing it.
- HTTP/2 framing, QUIC and WebTransport are not implemented here. They come
  from the `h2`, `quic` and `webtransport` dependencies. If a bug is in
  framing or flow control, it probably belongs there.
- `test/` also holds test servers (`hackney_h3_test_server`,
  `mock_proxy_server`), the fault harness described in the Development guide,
  and reproduction harnesses (`repro_h1_hang`, `repro_h2_hang`).
