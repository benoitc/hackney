# Migrating from hackney 1.x to 2.x

## Quick Summary

- **Simple requests**: No changes needed
- **Streaming/async**: Same API, `ClientRef` is now a PID
- **Proxy**: URL-based config, env vars work automatically

## Architecture: Before and After

### 1.x Design

```
- State stored in ETS tables
- Multiple modules coordinate via hackney_manager
- Socket ownership transferred between processes
- Complex cleanup on errors
```

### 2.x Design

```
- One gen_statem process per connection
- Process owns its socket and state
- Clean OTP supervision
- Automatic cleanup on process exit
```

### Key Differences

| Aspect | 1.x | 2.x |
|--------|-----|-----|
| State storage | ETS tables | Process state |
| Connection handle | Opaque reference | PID |
| Socket ownership | Transferred between processes | Owned by connection process |
| Error cleanup | Manual via manager | Automatic via process exit |
| Supervision | Custom tracking | OTP supervisor |

## What Changed

### Connection Handle

```erlang
%% 1.x - opaque reference
{ok, StatusCode, Headers, Ref} = hackney:get(URL).

%% 2.x - pid
{ok, StatusCode, Headers, ConnPid} = hackney:get(URL).
```

Code works unchanged - you pass the handle to other hackney functions.

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

### Pool

No changes:

```erlang
hackney_pool:start_pool(mypool, [{max_connections, 50}]),
hackney:get(URL, [], <<>>, [{pool, mypool}]).
```

## Requirements

Erlang/OTP 27+

## Help

https://github.com/benoitc/hackney/issues
