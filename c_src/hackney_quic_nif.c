/**
 * hackney_quic_nif.c - NIF entry point for hackney QUIC support
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 */

#include "hackney_quic_nif.h"
#include "atoms.h"
#include "quic_conn.h"

/*
 * NIF: connect(Host, Port, Opts, OwnerPid) -> {ok, ConnRef} | {error, Reason}
 *
 * Initiates a QUIC connection to the specified host:port.
 * This is a dirty NIF that performs the connection asynchronously.
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

    /* Extract port */
    unsigned int port;
    if (!enif_get_uint(env, argv[1], &port) || port > 65535) {
        return enif_make_badarg(env);
    }

    /* argv[2] is options map - TODO: parse in later step */

    /* Extract owner pid */
    ErlNifPid owner_pid;
    if (!enif_get_local_pid(env, argv[3], &owner_pid)) {
        return enif_make_badarg(env);
    }

    /* Create connection resource */
    QuicConn *conn = quic_conn_create(env, owner_pid);
    if (!conn) {
        return make_error(env, ATOM_ENOMEM);
    }

    /* TODO: Step 3 will implement actual QUIC connection using ngtcp2 */
    /* For now, just return the resource reference */

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

    /* TODO: Step 3 will implement graceful shutdown */
    enif_mutex_lock(conn->mutex);
    conn->state = QUIC_CONN_CLOSED;
    enif_mutex_unlock(conn->mutex);

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

    enif_mutex_lock(conn->mutex);
    if (conn->state != QUIC_CONN_CONNECTED) {
        enif_mutex_unlock(conn->mutex);
        return make_error(env, ATOM_NOT_CONNECTED);
    }
    enif_mutex_unlock(conn->mutex);

    /* TODO: Step 4 will implement stream opening via nghttp3 */
    return make_error_str(env, "not_implemented");
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

    /* TODO: Step 4 will implement header sending */
    UNUSED(argv);
    return make_error_str(env, "not_implemented");
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

    /* TODO: Step 4 will implement data sending */
    UNUSED(argv);
    return make_error_str(env, "not_implemented");
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

    /* TODO: Step 4 will implement stream reset */
    UNUSED(argv);
    return make_error_str(env, "not_implemented");
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

    /* TODO: Step 3 will implement timeout handling */
    UNUSED(argv);
    return enif_make_atom(env, "infinity");
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

    /* TODO: Implement in Step 3 */
    return make_error_str(env, "not_implemented");
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

    /* TODO: Implement in Step 3 */
    return make_error_str(env, "not_implemented");
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

/* NIF function table */
static ErlNifFunc nif_funcs[] = {
    /* {name, arity, function, flags} */
    {"connect", 4, nif_connect, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close", 2, nif_close, 0},
    {"open_stream", 1, nif_open_stream, 0},
    {"send_headers", 4, nif_send_headers, 0},
    {"send_data", 4, nif_send_data, 0},
    {"reset_stream", 3, nif_reset_stream, 0},
    {"handle_timeout", 2, nif_handle_timeout, 0},
    {"peername", 1, nif_peername, 0},
    {"sockname", 1, nif_sockname, 0},
    {"setopts", 2, nif_setopts, 0}
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
    /* Resource types are automatically cleaned up */
}

/* NIF initialization */
ERL_NIF_INIT(hackney_quic, nif_funcs, nif_load, NULL, nif_upgrade, nif_unload)
