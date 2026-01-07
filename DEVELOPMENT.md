# Hackney Development Guide

This guide covers development setup, testing, and contributing to hackney.

## Prerequisites

- Erlang/OTP 27 or later
- rebar3 3.24.0 or later
- CMake 3.14 or later
- Go (for building BoringSSL)
- zlib development headers

### Platform-specific requirements

**macOS:**
```bash
brew install erlang cmake go zlib
```

**Ubuntu/Debian:**
```bash
sudo apt-get install erlang cmake golang zlib1g-dev build-essential
```

**FreeBSD:**
```bash
pkg install erlang-runtime28 rebar3 cmake git gmake go llvm18
```

## Building

Clone the repository with submodules:

```bash
git clone --recursive https://github.com/benoitc/hackney.git
cd hackney
```

Build the project:

```bash
rebar3 compile
```

This will:
1. Compile all Erlang source files
2. Build the QUIC NIF (BoringSSL + lsquic + hackney_quic)

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
- Inspect the build: `ls -la priv/`

### Debugging with core dumps

Enable core dumps for debugging segfaults:

```bash
docker run --rm --ulimit core=-1 hackney-test bash -c "
    ulimit -c unlimited
    rebar3 eunit || (ls -la core* 2>/dev/null; gdb -batch -ex bt /usr/local/bin/erl core*)
"
```

## QUIC/HTTP3 Development

The QUIC implementation uses:
- **BoringSSL** - Google's fork of OpenSSL (required for QUIC TLS 1.3)
- **lsquic** - LiteSpeed QUIC library

### NIF Source Files

- `c_src/hackney_quic_nif.c` - NIF entry points
- `c_src/quic_conn.c` - Connection management
- `c_src/quic_conn.h` - Connection structures and declarations
- `c_src/atoms.h` - Atom definitions

### Building the NIF

The NIF is built automatically by rebar3 using CMake. To rebuild from scratch:

```bash
rm -rf _build/cmake priv/*.so
rebar3 compile
```

### CMake Configuration

The CMake build is configured in `c_src/CMakeLists.txt`. Key options:

- `CMAKE_BUILD_TYPE` - Release (default) or Debug
- `CMAKE_POSITION_INDEPENDENT_CODE` - Always ON for NIF shared library

### Debugging the NIF

Build with debug symbols:

```bash
rm -rf _build/cmake
CMAKE_BUILD_TYPE=Debug rebar3 compile
```

Use LLDB/GDB to debug:

```bash
lldb -- erl -pa _build/default/lib/*/ebin
```

## Code Style

### Erlang

- Follow standard Erlang conventions
- Use edoc for function documentation
- Keep lines under 100 characters

### C

- Use C17 standard
- Use `enif_alloc`/`enif_free` for memory allocation (not malloc/free)
- Always check return values
- Use atomic operations for thread-safe flags

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
