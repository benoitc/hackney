/**
 * hackney_quic_nif.c - NIF entry point for hackney QUIC support
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2026 Benoit Chesneau
 */

#include "hackney_quic_nif.h"
#include "atoms.h"
#include "quic_conn.h"
#include <arpa/inet.h>

/* Atom definitions - declared extern in atoms.h */
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_UNDEFINED;
ERL_NIF_TERM ATOM_QUIC;
ERL_NIF_TERM ATOM_CONNECTED;
ERL_NIF_TERM ATOM_CLOSED;
ERL_NIF_TERM ATOM_TRANSPORT_ERROR;
ERL_NIF_TERM ATOM_STREAM_HEADERS;
ERL_NIF_TERM ATOM_STREAM_DATA;
ERL_NIF_TERM ATOM_STREAM_OPENED;
ERL_NIF_TERM ATOM_STREAM_RESET;
ERL_NIF_TERM ATOM_STOP_SENDING;
ERL_NIF_TERM ATOM_GOAWAY;
ERL_NIF_TERM ATOM_SESSION_TICKET;
ERL_NIF_TERM ATOM_SEND_READY;
ERL_NIF_TERM ATOM_TIMER;
ERL_NIF_TERM ATOM_BLOCKED;
ERL_NIF_TERM ATOM_NOT_CONNECTED;
ERL_NIF_TERM ATOM_BADARG;
ERL_NIF_TERM ATOM_ENOMEM;
ERL_NIF_TERM ATOM_TIMEOUT;
ERL_NIF_TERM ATOM_CLOSED_BY_PEER;
ERL_NIF_TERM ATOM_CONNECTION_REFUSED;
ERL_NIF_TERM ATOM_TLS_ERROR;
ERL_NIF_TERM ATOM_PROTOCOL_ERROR;
ERL_NIF_TERM ATOM_IDLE;
ERL_NIF_TERM ATOM_HANDSHAKING;
ERL_NIF_TERM ATOM_DRAINING;

/* Helper to convert sockaddr to Erlang {IP, Port} tuple */
static ERL_NIF_TERM
make_address(ErlNifEnv *env, const struct sockaddr_storage *addr, socklen_t addrlen)
{
    UNUSED(addrlen);

    if (addr->ss_family == AF_INET) {
        const struct sockaddr_in *sin = (const struct sockaddr_in *)addr;
        unsigned char *ip = (unsigned char *)&sin->sin_addr.s_addr;

        ERL_NIF_TERM ip_tuple = enif_make_tuple4(env,
            enif_make_int(env, ip[0]),
            enif_make_int(env, ip[1]),
            enif_make_int(env, ip[2]),
            enif_make_int(env, ip[3]));

        return enif_make_tuple2(env, ip_tuple, enif_make_int(env, ntohs(sin->sin_port)));

    } else if (addr->ss_family == AF_INET6) {
        const struct sockaddr_in6 *sin6 = (const struct sockaddr_in6 *)addr;
        const uint16_t *ip = (const uint16_t *)&sin6->sin6_addr;

        ERL_NIF_TERM ip_tuple = enif_make_tuple8(env,
            enif_make_int(env, ntohs(ip[0])),
            enif_make_int(env, ntohs(ip[1])),
            enif_make_int(env, ntohs(ip[2])),
            enif_make_int(env, ntohs(ip[3])),
            enif_make_int(env, ntohs(ip[4])),
            enif_make_int(env, ntohs(ip[5])),
            enif_make_int(env, ntohs(ip[6])),
            enif_make_int(env, ntohs(ip[7])));

        return enif_make_tuple2(env, ip_tuple, enif_make_int(env, ntohs(sin6->sin6_port)));
    }

    return enif_make_badarg(env);
}

/*
 * NIF: connect(Host, Port, Opts, OwnerPid) -> {ok, ConnRef} | {error, Reason}
 *
 * Initiates a QUIC connection to the specified host:port.
 * This is a dirty NIF that performs the connection asynchronously.
 *
 * Opts is a map that may contain:
 *   - socket_fd: integer() - Existing UDP socket file descriptor to use
 *   - verify: boolean() - Whether to verify server certificate (default: false)
 */
static ERL_NIF_TERM
nif_connect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    /* Extract host */
    ErlNifBinary host_bin;
    if (!enif_inspect_binary(env, argv[0], &host_bin) &&
        !enif_inspect_iolist_as_binary(env, argv[0], &host_bin)) {
        return enif_make_badarg(env);
    }

    /* Null-terminate the hostname */
    char *hostname = enif_alloc(host_bin.size + 1);
    if (!hostname) {
        return make_error(env, ATOM_ENOMEM);
    }
    memcpy(hostname, host_bin.data, host_bin.size);
    hostname[host_bin.size] = '\0';

    /* Extract port */
    unsigned int port;
    if (!enif_get_uint(env, argv[1], &port) || port > 65535) {
        enif_free(hostname);
        return enif_make_badarg(env);
    }

    /* Parse options map */
    int socket_fd = -1;  /* -1 means create new socket */

    if (enif_is_map(env, argv[2])) {
        ERL_NIF_TERM key, value;

        /* Check for socket_fd option */
        key = enif_make_atom(env, "socket_fd");
        if (enif_get_map_value(env, argv[2], key, &value)) {
            if (!enif_get_int(env, value, &socket_fd)) {
                enif_free(hostname);
                return enif_make_badarg(env);
            }
        }
    }

    /* Extract owner pid */
    ErlNifPid owner_pid;
    if (!enif_get_local_pid(env, argv[3], &owner_pid)) {
        enif_free(hostname);
        return enif_make_badarg(env);
    }

    /* Create connection resource */
    QuicConn *conn = quic_conn_create(env, owner_pid);
    if (!conn) {
        enif_free(hostname);
        return make_error(env, ATOM_ENOMEM);
    }

    /* Initiate connection */
    int ret = quic_conn_connect(conn, hostname, (uint16_t)port, socket_fd, NULL, 0);
    enif_free(hostname);

    if (ret < 0) {
        enif_release_resource(conn);
        return make_error_str(env, "connect_failed");
    }

    /* Create a reference for select messages */
    conn->select_ref = enif_make_ref(env);

    /* Arm enif_select for incoming packets */
    if (conn->sockfd >= 0) {
        int sel_ret = enif_select(env, (ErlNifEvent)conn->sockfd,
                                  ERL_NIF_SELECT_READ,
                                  conn,     /* resource object */
                                  NULL,     /* pid - use calling process */
                                  conn->select_ref);
        if (sel_ret >= 0) {
            conn->select_armed = true;
        }
    }

    ERL_NIF_TERM conn_ref = enif_make_resource(env, conn);
    enif_release_resource(conn); /* Erlang now owns it */

    return make_ok(env, conn_ref);
}

/*
 * NIF: close(ConnRef, Reason) -> ok
 *
 * Closes a QUIC connection gracefully.
 */
static ERL_NIF_TERM
nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    quic_conn_close(conn);

    return ATOM_OK;
}

/*
 * NIF: open_stream(ConnRef) -> {ok, StreamId} | {error, Reason}
 *
 * Opens a new bidirectional stream on the connection.
 */
static ERL_NIF_TERM
nif_open_stream(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    int64_t stream_id = quic_conn_open_stream(conn);
    if (stream_id < 0) {
        return make_error(env, ATOM_NOT_CONNECTED);
    }

    /* Stream ID 0 means the request was queued; actual ID will come via callback */
    return make_ok(env, enif_make_int64(env, stream_id));
}

/*
 * NIF: send_headers(ConnRef, StreamId, Headers, Fin) -> ok | {error, Reason}
 *
 * Sends HTTP/3 headers on a stream.
 */
static ERL_NIF_TERM
nif_send_headers(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    /* Extract stream ID */
    ErlNifSInt64 stream_id;
    if (!enif_get_int64(env, argv[1], &stream_id)) {
        return enif_make_badarg(env);
    }

    /* Extract fin flag */
    char fin_str[6];
    int fin = 0;
    if (enif_get_atom(env, argv[3], fin_str, sizeof(fin_str), ERL_NIF_LATIN1)) {
        fin = (strcmp(fin_str, "true") == 0);
    } else {
        return enif_make_badarg(env);
    }

    /* Send headers */
    int ret = quic_conn_send_headers(conn, stream_id, env, argv[2], fin);
    if (ret < 0) {
        return make_error(env, ATOM_NOT_CONNECTED);
    }

    return ATOM_OK;
}

/*
 * NIF: send_data(ConnRef, StreamId, Data, Fin) -> ok | {error, Reason}
 *
 * Sends data on a stream.
 */
static ERL_NIF_TERM
nif_send_data(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    /* Extract stream ID */
    ErlNifSInt64 stream_id;
    if (!enif_get_int64(env, argv[1], &stream_id)) {
        return enif_make_badarg(env);
    }

    /* Extract data */
    ErlNifBinary data_bin;
    if (!enif_inspect_iolist_as_binary(env, argv[2], &data_bin)) {
        return enif_make_badarg(env);
    }

    /* Extract fin flag */
    char fin_str[6];
    int fin = 0;
    if (enif_get_atom(env, argv[3], fin_str, sizeof(fin_str), ERL_NIF_LATIN1)) {
        fin = (strcmp(fin_str, "true") == 0);
    } else {
        return enif_make_badarg(env);
    }

    /* Send data */
    int ret = quic_conn_send_data(conn, stream_id, data_bin.data, data_bin.size, fin);
    if (ret < 0) {
        return make_error(env, ATOM_NOT_CONNECTED);
    }

    return ATOM_OK;
}

/*
 * NIF: reset_stream(ConnRef, StreamId, ErrorCode) -> ok | {error, Reason}
 *
 * Resets a stream with the given error code.
 */
static ERL_NIF_TERM
nif_reset_stream(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    /* Extract stream ID */
    ErlNifSInt64 stream_id;
    if (!enif_get_int64(env, argv[1], &stream_id)) {
        return enif_make_badarg(env);
    }

    /* Extract error code */
    unsigned long error_code;
    if (!enif_get_ulong(env, argv[2], &error_code)) {
        return enif_make_badarg(env);
    }

    /* Reset stream */
    int ret = quic_conn_reset_stream(conn, stream_id, error_code);
    if (ret < 0) {
        return make_error(env, ATOM_NOT_CONNECTED);
    }

    return ATOM_OK;
}

/*
 * NIF: handle_timeout(ConnRef, NowMs) -> NextTimeoutMs | infinity
 *
 * Handles connection timeouts for retransmission.
 */
static ERL_NIF_TERM
nif_handle_timeout(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    int64_t next_timeout = quic_conn_handle_timeout(conn);
    if (next_timeout < 0) {
        return enif_make_atom(env, "infinity");
    }

    return enif_make_int64(env, next_timeout);
}

/*
 * NIF: peername(ConnRef) -> {ok, {IP, Port}} | {error, Reason}
 *
 * Returns the remote address of the connection.
 */
static ERL_NIF_TERM
nif_peername(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    struct sockaddr_storage addr;
    socklen_t addrlen;

    if (quic_conn_peername(conn, &addr, &addrlen) < 0) {
        return make_error_str(env, "not_connected");
    }

    return make_ok(env, make_address(env, &addr, addrlen));
}

/*
 * NIF: sockname(ConnRef) -> {ok, {IP, Port}} | {error, Reason}
 *
 * Returns the local address of the connection.
 */
static ERL_NIF_TERM
nif_sockname(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    struct sockaddr_storage addr;
    socklen_t addrlen;

    if (quic_conn_sockname(conn, &addr, &addrlen) < 0) {
        return make_error_str(env, "not_connected");
    }

    return make_ok(env, make_address(env, &addr, addrlen));
}

/*
 * NIF: setopts(ConnRef, Opts) -> ok | {error, Reason}
 *
 * Sets connection options.
 */
static ERL_NIF_TERM
nif_setopts(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    /* TODO: Implement option setting */
    UNUSED(argv);
    return ATOM_OK;
}

/*
 * NIF: process(ConnRef) -> NextTimeoutMs | infinity
 *
 * Processes pending QUIC events (reads packets, handles timeouts).
 * This is a dirty I/O NIF that should be called when:
 *   1. The socket has data ready (after receiving {select, _, _, ready_input})
 *   2. A timer has expired
 *
 * Returns the next timeout in milliseconds, or 'infinity' if no timeout needed.
 * The caller should re-arm enif_select after this returns.
 */
static ERL_NIF_TERM
nif_process(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    QuicConn *conn;
    if (!enif_get_resource(env, argv[0], QUIC_CONN_RESOURCE, (void **)&conn)) {
        return enif_make_badarg(env);
    }

    /* Process pending I/O */
    int timeout_ms = quic_conn_process(conn, env);

    /* Re-arm enif_select for next packet (if socket still valid) */
    if (conn->sockfd >= 0 &&
        !__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) {
        int sel_ret = enif_select(env, (ErlNifEvent)conn->sockfd,
                                  ERL_NIF_SELECT_READ,
                                  conn,
                                  NULL,
                                  conn->select_ref);
        if (sel_ret >= 0) {
            conn->select_armed = true;
        }
    }

    if (timeout_ms < 0) {
        return enif_make_atom(env, "infinity");
    }
    return enif_make_int(env, timeout_ms);
}

/* NIF function table - names must match Erlang _nif function names */
static ErlNifFunc nif_funcs[] = {
    /* {name, arity, function, flags} */
    {"connect_nif", 4, nif_connect, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_nif", 2, nif_close, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"open_stream_nif", 1, nif_open_stream, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"send_headers_nif", 4, nif_send_headers, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"send_data_nif", 4, nif_send_data, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"reset_stream_nif", 3, nif_reset_stream, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"handle_timeout_nif", 2, nif_handle_timeout, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"process_nif", 1, nif_process, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"peername_nif", 1, nif_peername, 0},
    {"sockname_nif", 1, nif_sockname, 0},
    {"setopts_nif", 2, nif_setopts, 0}
};

/* NIF load callback */
static int
nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(priv_data);
    UNUSED(load_info);

    /* Initialize atoms */
    init_atoms(env);

    /* Initialize resource types */
    if (!quic_conn_resource_init(env)) {
        return -1;
    }

    return 0;
}

/* NIF upgrade callback */
static int
nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(old_priv_data);
    return nif_load(env, priv_data, load_info);
}

/* NIF unload callback */
static void
nif_unload(ErlNifEnv *env, void *priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);

    /* Cleanup lsquic global state */
    quic_global_cleanup();
}

/* NIF initialization */
ERL_NIF_INIT(hackney_quic, nif_funcs, nif_load, NULL, nif_upgrade, nif_unload)
