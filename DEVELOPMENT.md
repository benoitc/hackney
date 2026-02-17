# Hackney Development Guide

This guide covers development setup, testing, and contributing to hackney.

## Prerequisites

- Erlang/OTP 27 or later
- rebar3 3.24.0 or later

### Platform-specific requirements

**macOS:**
```bash
brew install erlang
```

**Ubuntu/Debian:**
```bash
sudo apt-get install erlang build-essential
```

**FreeBSD:**
```bash
pkg install erlang-runtime28 rebar3
```

## Building

Clone the repository:

```bash
git clone https://github.com/benoitc/hackney.git
cd hackney
```

Build the project:

```bash
rebar3 compile
```

This will compile all Erlang source files and fetch dependencies (including the pure Erlang QUIC library for HTTP/3 support).

## Running Tests

Run all tests:

```bash
rebar3 eunit
```

Run specific test modules:

```bash
rebar3 eunit --module=hackney_quic_tests
rebar3 eunit --module=hackney_http3_tests
```

### Running tests with httpbin

Some tests require the httpbin server. Start it before running tests:

```bash
pip3 install httpbin gunicorn
gunicorn -b 127.0.0.1:8000 httpbin:app &
rebar3 eunit
```

## Local Docker Testing

A Dockerfile is provided for testing on Linux locally, which mirrors the GitHub CI environment.

### Building the Docker image

```bash
docker build -f Dockerfile.test -t hackney-test .
```

### Running tests in Docker

Run all tests:

```bash
docker run --rm hackney-test
```

Run specific test modules:

```bash
docker run --rm hackney-test bash -c "rebar3 eunit --module=hackney_quic_tests"
```

### Interactive debugging in Docker

Start an interactive shell:

```bash
docker run --rm -it hackney-test bash
```

Then you can:
- Run tests manually: `rebar3 eunit`
- Start an Erlang shell: `rebar3 shell`

## QUIC/HTTP3 Development

HTTP/3 support uses a pure Erlang QUIC implementation from the `quic` dependency.

### Source Files

- `src/hackney_quic.erl` - QUIC/HTTP3 transport wrapper
- `src/hackney_qpack.erl` - QPACK header compression
- `src/hackney_h3.erl` - HTTP/3 high-level API

The underlying QUIC implementation is in the `quic` dependency which provides:
- TLS 1.3 handshake
- QUIC packet encoding/decoding
- Congestion control
- Loss recovery

## Code Style

### Erlang

- Follow standard Erlang conventions
- Use edoc for function documentation
- Keep lines under 100 characters

## Submitting Changes

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make your changes
4. Run tests locally and in Docker
5. Commit with clear messages
6. Push and create a pull request

### Commit Message Format

```
type: short description

Longer description if needed.
```

Types: `fix`, `feat`, `docs`, `test`, `refactor`, `ci`, `chore`

## Continuous Integration

CI runs on:
- Linux x86_64 (OTP 27.2, 28.0)
- Linux ARM64 (OTP 27.2)
- macOS ARM64 (OTP 27)
- FreeBSD 14.2 (OTP 28)

All CI jobs must pass before merging.
