# Migrating from hackney 1.x to 2.x

## Quick Summary

- **Simple requests**: No changes needed
- **Streaming/async**: Same API, `ClientRef` is now a PID
- **Pool**: Per-host limits replace global pool size, SSL not pooled
- **Proxy**: URL-based config, env vars work automatically

## Architecture: Before and After

### 1.x Design

```
- State stored in ETS tables
- Multiple modules coordinate via hackney_manager
- Socket ownership transferred between processes
- Complex cleanup on errors
- Global pool with max_connections limit
- SSL connections pooled
```

### 2.x Design

```
- One gen_statem process per connection
- Process owns its socket and state
- Clean OTP supervision
- Automatic cleanup on process exit
- Per-host connection limits via load regulation
- TCP-only pooling (SSL connections never pooled)
- Connection prewarm for low-latency reuse
```

See [Design Guide](design.md) for detailed architecture documentation.

### Key Differences

| Aspect | 1.x | 2.x |
|--------|-----|-----|
| State storage | ETS tables | Process state |
| Connection handle | Opaque reference | PID |
| Socket ownership | Transferred between processes | Owned by connection process |
| Error cleanup | Manual via manager | Automatic via process exit |
| Supervision | Custom tracking | OTP supervisor |
| Pool scope | Global max_connections | Per-host limits |
| SSL pooling | Yes | No (security) |
| Prewarm | No | Yes (default 4 per host) |

## What Changed

### Connection Handle

```erlang
%% 1.x - opaque reference
{ok, StatusCode, Headers, Ref} = hackney:get(URL).

%% 2.x - pid
{ok, StatusCode, Headers, ConnPid} = hackney:get(URL).
```

Code works unchanged - you pass the handle to other hackney functions.

### Pool Behavior

The most significant change is how connection pooling works:

**1.x**: Single global pool with `max_connections` limit shared across all hosts.

**2.x**: Per-host connection limits. Each host gets up to `max_per_host` concurrent connections (default 50). TCP connections are pooled; SSL connections are never pooled.

```erlang
%% 1.x - global pool limit
hackney_pool:start_pool(mypool, [{max_connections, 100}]).  %% 100 total

%% 2.x - per-host limit (100 connections per host)
hackney_pool:start_pool(mypool, [{max_connections, 100}]).
%% Plus request option:
hackney:get(URL, [], <<>>, [{pool, mypool}, {max_per_host, 100}]).
```

### SSL Connections

**1.x**: SSL connections were pooled and reused.

**2.x**: SSL connections are never pooled. Each HTTPS request either:
- Gets a TCP connection from pool and upgrades to SSL
- Creates a new connection with SSL

This is a security improvement - SSL session state is never shared.

### Connection Prewarm

**2.x only**: After first use of a host, the pool maintains warm TCP connections:

```erlang
%% Automatic after first request to host
%% Or explicit:
hackney_pool:prewarm(default, "api.example.com", 443, 4).
```

### Load Regulation

**2.x only**: Per-host backpressure when connection limit reached:

```erlang
%% Request waits if api.example.com has 50 active connections
hackney:get("https://api.example.com/data", [], <<>>, [
    {max_per_host, 50},
    {checkout_timeout, 5000}  %% Wait up to 5s for slot
]).

%% Returns {error, checkout_timeout} if slot not available in time
```

### Removed Functions

| 1.x | 2.x |
|-----|-----|
| `hackney:cancel_request/1` | `hackney:close/1` |
| `hackney:controlling_process/2` | Not needed |
| `hackney:send_multipart_body/2` | `hackney:send_body/2` |

### Removed Modules

Merged into `hackney_conn`:
- `hackney_connect`
- `hackney_connection`
- `hackney_request`
- `hackney_response`
- `hackney_stream`

## Migration Patterns

### Simple Request

No changes:

```erlang
{ok, 200, Headers, Ref} = hackney:get(URL),
{ok, Body} = hackney:body(Ref).
```

### Streaming Request

No changes:

```erlang
{ok, Ref} = hackney:request(post, URL, Headers, stream, []),
ok = hackney:send_body(Ref, Chunk),
ok = hackney:finish_send_body(Ref),
{ok, Status, RespHeaders, Ref} = hackney:start_response(Ref).
```

### Async Response

No changes:

```erlang
{ok, Ref} = hackney:get(URL, [], <<>>, [async]),
receive
    {hackney_response, Ref, {status, Status, _}} -> ok
end.
```

### Cancel Request

```erlang
%% 1.x
hackney:cancel_request(Ref).

%% 2.x
hackney:close(Ref).
```

### Pool Configuration

```erlang
%% 1.x - global pool limit
hackney_pool:start_pool(mypool, [{max_connections, 50}]),
hackney:get(URL, [], <<>>, [{pool, mypool}]).

%% 2.x - same API works, but behavior differs:
%% - max_connections is now per pool, not global limit
%% - Add max_per_host for per-host limiting
hackney_pool:start_pool(mypool, [
    {max_connections, 100},    %% Pool capacity
    {prewarm_count, 4},        %% Warm connections per host
    {timeout, 2000}            %% Keepalive timeout (max 2s)
]),
hackney:get(URL, [], <<>>, [
    {pool, mypool},
    {max_per_host, 50},        %% Per-host limit
    {checkout_timeout, 5000}   %% Wait time for slot
]).
```

### High-Concurrency Scenarios

If you were using a large global pool for high concurrency:

```erlang
%% 1.x - 1000 connections shared across all hosts
hackney_pool:start_pool(bigpool, [{max_connections, 1000}]).

%% 2.x - 100 connections per host (better isolation)
hackney_pool:start_pool(bigpool, [{max_connections, 1000}]),
hackney:get(URL, [], <<>>, [
    {pool, bigpool},
    {max_per_host, 100}  %% Each host gets up to 100
]).
```

### Monitoring Pool Stats

```erlang
%% 1.x
hackney_pool:get_stats(mypool).

%% 2.x - same, plus per-host stats
hackney_pool:get_stats(mypool).
hackney_pool:host_stats(mypool, "api.example.com", 443).
%% Returns: [{active, N}, {in_use, N}, {free, N}]
```

## Configuration Changes

### Application Environment

```erlang
%% 1.x
{hackney, [
    {max_connections, 50},
    {timeout, 150000}  %% Could be any value
]}.

%% 2.x
{hackney, [
    {max_connections, 50},
    {timeout, 2000},       %% Capped at 2000ms
    {prewarm_count, 4}     %% New option
]}.
```

### Timeout Capping

Keepalive timeout is now capped at 2000ms (2 seconds). This prevents issues with stale connections and aligns with common server defaults.

```erlang
%% 1.x - any timeout value
hackney_pool:start_pool(p, [{timeout, 300000}]).  %% 5 minutes

%% 2.x - capped at 2000ms
hackney_pool:start_pool(p, [{timeout, 300000}]).  %% Becomes 2000ms
```

## Requirements

Erlang/OTP 27+

## Help

https://github.com/benoitc/hackney/issues
