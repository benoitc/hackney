# Proxy Testing Guide

This document describes how to test hackney's proxy support locally and in CI.

## Automated Tests

The test suite includes:
- **Unit tests**: Test proxy configuration parsing
- **Mock proxy tests**: Use mock proxy servers (in `test/mock_proxy_server.erl`) for integration tests

Run all tests:
```bash
rebar3 eunit
```

Run only proxy tests:
```bash
rebar3 eunit --module=hackney_proxy_tests
```

## Manual Testing with Real Proxies

### Setting Up a SOCKS5 Proxy

#### macOS (Homebrew)

```bash
# Install microsocks (lightweight)
brew install microsocks
microsocks -p 1080

# Or use SSH as SOCKS5 proxy
ssh -D 1080 -N localhost
```

#### macOS (MacPorts)

```bash
# Install dante
sudo port install dante

# Create config file /opt/local/etc/sockd.conf
sudo tee /opt/local/etc/sockd.conf << 'EOF'
logoutput: stderr
internal: 127.0.0.1 port = 1080
external: en0
socksmethod: none
clientmethod: none
client pass {
    from: 0.0.0.0/0 to: 0.0.0.0/0
}
socks pass {
    from: 0.0.0.0/0 to: 0.0.0.0/0
}
EOF

# Run sockd
sudo /opt/local/sbin/sockd -f /opt/local/etc/sockd.conf
```

#### Linux (Ubuntu/Debian)

```bash
# Install dante
sudo apt-get install dante-server

# Create config file /etc/danted.conf
sudo tee /etc/danted.conf << 'EOF'
logoutput: stderr
internal: 127.0.0.1 port = 1080
external: eth0
socksmethod: none
clientmethod: none
client pass {
    from: 0.0.0.0/0 to: 0.0.0.0/0
}
socks pass {
    from: 0.0.0.0/0 to: 0.0.0.0/0
}
EOF

# Run
sudo danted
```

### Setting Up an HTTP Proxy

#### macOS (Homebrew)

```bash
# Install tinyproxy
brew install tinyproxy

# Run with default config
tinyproxy -d -c /opt/homebrew/etc/tinyproxy/tinyproxy.conf
```

#### macOS (MacPorts)

```bash
# Install tinyproxy
sudo port install tinyproxy

# Run
sudo /opt/local/sbin/tinyproxy -c /opt/local/etc/tinyproxy/tinyproxy.conf
```

#### Linux

```bash
# Install tinyproxy
sudo apt-get install tinyproxy

# Run
sudo tinyproxy
```

### Testing in Erlang Shell

```erlang
% Start hackney
application:ensure_all_started(hackney).

% Test SOCKS5 proxy (assuming proxy on port 1080)
hackney:request(get, <<"https://httpbin.org/get">>, [], <<>>,
    [{proxy, "socks5://127.0.0.1:1080"}]).

% Test SOCKS5 with tuple config
hackney:request(get, <<"https://httpbin.org/get">>, [], <<>>,
    [{proxy, {socks5, "127.0.0.1", 1080}}]).

% Test HTTP CONNECT proxy (for HTTPS targets)
hackney:request(get, <<"https://httpbin.org/get">>, [], <<>>,
    [{proxy, "http://127.0.0.1:8888"}]).

% Test HTTP proxy (for HTTP targets)
hackney:request(get, <<"http://httpbin.org/get">>, [], <<>>,
    [{proxy, {connect, "127.0.0.1", 8888}}]).
```

## CI Configuration

### GitHub Actions

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    services:
      socks5:
        image: serjs/go-socks5-proxy
        ports:
          - 1080:1080
      http-proxy:
        image: vimagick/tinyproxy
        ports:
          - 8888:8888

    steps:
      - uses: actions/checkout@v4

      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.23'

      - name: Run tests
        run: rebar3 eunit

      - name: Test with real SOCKS5 proxy
        run: |
          rebar3 shell --eval "
            application:ensure_all_started(hackney),
            {ok, _, _, C} = hackney:request(get, <<\"http://httpbin.org/get\">>, [], <<>>,
                [{proxy, \"socks5://127.0.0.1:1080\"}]),
            hackney:close(C),
            init:stop(0).
          "
```

### Docker-based Testing

```bash
# Start SOCKS5 proxy
docker run -d --name socks5 -p 1080:1080 serjs/go-socks5-proxy

# Start HTTP proxy
docker run -d --name http-proxy -p 8888:8888 vimagick/tinyproxy

# Run tests
rebar3 eunit

# Cleanup
docker stop socks5 http-proxy
docker rm socks5 http-proxy
```

## Proxy Configuration Options

| Option | Format | Description |
|--------|--------|-------------|
| `{proxy, "http://host:port"}` | URL string | HTTP proxy, auto-detects CONNECT for HTTPS |
| `{proxy, "socks5://host:port"}` | URL string | SOCKS5 proxy |
| `{proxy, {Host, Port}}` | Tuple | Simple HTTP proxy |
| `{proxy, {connect, Host, Port}}` | Tuple | Explicit HTTP CONNECT tunnel |
| `{proxy, {socks5, Host, Port}}` | Tuple | SOCKS5 proxy |
| `{proxy_auth, {User, Pass}}` | Tuple | HTTP proxy authentication |
| `{socks5_user, User}` | Binary | SOCKS5 username |
| `{socks5_pass, Pass}` | Binary | SOCKS5 password |
| `{socks5_resolve, local|remote}` | Atom | DNS resolution (default: remote) |

## Environment Variables

hackney supports proxy auto-detection from environment variables:

- `HTTP_PROXY` / `http_proxy` - Proxy for HTTP requests
- `HTTPS_PROXY` / `https_proxy` - Proxy for HTTPS requests
- `ALL_PROXY` / `all_proxy` - Fallback proxy for all requests
- `NO_PROXY` / `no_proxy` - Comma-separated list of hosts to bypass
