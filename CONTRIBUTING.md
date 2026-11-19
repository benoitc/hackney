# Contributing to hackney

Thank you for your interest in contributing to hackney!

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/hackney.git`
3. Create a branch: `git checkout -b my-feature`
4. Make your changes
5. Run tests: `rebar3 eunit`
6. Push and open a pull request

## Pull Request Guidelines

### Title Format

We use [Conventional Commits](https://www.conventionalcommits.org/) for PR titles:

```
<type>: <description>
```

**Types:**

| Type | Description |
|------|-------------|
| `feat` | New feature |
| `fix` | Bug fix |
| `docs` | Documentation only |
| `refactor` | Code change that neither fixes a bug nor adds a feature |
| `perf` | Performance improvement |
| `test` | Adding or updating tests |
| `chore` | Maintenance tasks, dependencies |
| `security` | Security fix |

**Examples:**

- `feat: add HTTP/3 connection pooling`
- `fix: handle timeout in async responses`
- `docs: update WebSocket guide`
- `refactor: simplify connection state machine`
- `perf: reduce memory allocation in header parsing`
- `security: update SSL certificate bundle`

### Description

Include in your PR description:

- **What** the change does
- **Why** the change is needed
- **How** to test it (if applicable)

## Development Setup

### Requirements

- Erlang/OTP 27+
- rebar3

### Building

```sh
rebar3 compile
```

### Building with QUIC support

QUIC/HTTP3 support is included automatically via the pure Erlang `quic` dependency - no additional build steps required.

### Running Tests

```sh
# All tests
rebar3 eunit

# Specific test module
rebar3 eunit --module=hackney_conn_tests

# With verbose output
rebar3 eunit --verbose
```

### Running Dialyzer

```sh
rebar3 dialyzer
```

## Finding Your Way

[Where Things Live](guides/code_map.md) maps each concept to its modules and
tests. The table below tells you where to start for common changes.

| If you want to | Start in | Check with |
|----------------|----------|------------|
| Add a request option | `src/hackney.erl`, then `src/hackney_conn.erl` | `hackney_integration_tests`, plus the HTTP/2 and HTTP/3 tests if the option applies there |
| Change pool or checkout behaviour | `src/hackney_pool.erl`, `src/hackney_load_regulation.erl` | `hackney_pool_tests` and the fault and chaos suites in [DEVELOPMENT.md](DEVELOPMENT.md) |
| Change HTTP/2 or HTTP/3 behaviour | `src/hackney_conn.erl`, `src/hackney_h3.erl`. Check first whether the fix belongs in the `h2` or `quic` dependency | `hackney_http2_*_tests`, `hackney_http3_*_tests`, `hackney_h3_*_tests` |
| Change the HTTP/1.1 parser | `src/hackney_http.erl` | `hackney_http_tests`, `hackney_header_limits_tests`, `hackney_chunk_limit_tests` |
| Add a proxy type or transport | `src/hackney_http_connect.erl`, `src/hackney_socks5.erl`, `src/hackney_tcp.erl` | `hackney_proxy_tests`, `hackney_socks5_tests` |
| Add a function to the public API | `src/hackney.erl` | A test in `test/`, and the matching guide in `guides/` |
| Bump a dependency | `rebar.config`, then `rebar3 upgrade --all` | `rebar3 eunit`, `rebar3 xref`, `rebar3 dialyzer` |

Put tests in `test/`. Do not export functions from `src/` only for tests.

## Code Style

- Follow existing code conventions
- Keep functions short and focused
- Add specs for exported functions
- Update documentation for API changes

## Reporting Issues

When reporting bugs, include:

- Erlang/OTP version (`erl -version`)
- hackney version
- Minimal reproduction case
- Expected vs actual behavior

## Questions?

Open an issue or reach out to the maintainers.
