/**
 * quic_conn.c - QUIC connection resource management
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 */

#include "quic_conn.h"
#include "atoms.h"
#include <stdlib.h>

/* Global resource type for QUIC connections */
ErlNifResourceType *QUIC_CONN_RESOURCE = NULL;

/* Resource destructor - called when Erlang GC collects the resource */
void quic_conn_resource_dtor(ErlNifEnv *env, void *obj) {
    UNUSED(env);
    QuicConn *conn = (QuicConn *)obj;
    quic_conn_destroy(conn);
}

/* Initialize the QUIC connection resource type */
int quic_conn_resource_init(ErlNifEnv *env) {
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    QUIC_CONN_RESOURCE = enif_open_resource_type(
        env,
        NULL,
        "quic_conn",
        quic_conn_resource_dtor,
        flags,
        NULL
    );
    return QUIC_CONN_RESOURCE != NULL;
}

/* Create a new QUIC connection */
QuicConn *quic_conn_create(ErlNifEnv *env, ErlNifPid owner_pid) {
    QuicConn *conn = enif_alloc_resource(QUIC_CONN_RESOURCE, sizeof(QuicConn));
    if (!conn) {
        return NULL;
    }

    /* Initialize all fields to safe defaults */
    memset(conn, 0, sizeof(QuicConn));

    conn->sockfd = -1;
    conn->owner_pid = owner_pid;
    conn->state = QUIC_CONN_IDLE;
    conn->ref_count = 1;

    /* Create message environment for async sends to owner */
    conn->msg_env = enif_alloc_env();
    if (!conn->msg_env) {
        enif_release_resource(conn);
        return NULL;
    }

    /* Create mutex for thread safety */
    conn->mutex = enif_mutex_create("quic_conn_mutex");
    if (!conn->mutex) {
        enif_free_env(conn->msg_env);
        enif_release_resource(conn);
        return NULL;
    }

    return conn;
}

/* Destroy a QUIC connection (internal cleanup) */
void quic_conn_destroy(QuicConn *conn) {
    if (!conn) return;

    /* Close socket */
    if (conn->sockfd >= 0) {
        close(conn->sockfd);
        conn->sockfd = -1;
    }

    /* Free nghttp3 connection */
    if (conn->h3conn) {
        nghttp3_conn_del(conn->h3conn);
        conn->h3conn = NULL;
    }

    /* Free ngtcp2 connection */
    if (conn->conn) {
        ngtcp2_conn_del(conn->conn);
        conn->conn = NULL;
    }

    /* Free SSL */
    if (conn->ssl) {
        SSL_free(conn->ssl);
        conn->ssl = NULL;
    }

    /* Free SSL context */
    if (conn->ssl_ctx) {
        SSL_CTX_free(conn->ssl_ctx);
        conn->ssl_ctx = NULL;
    }

    /* Free session ticket */
    if (conn->session_ticket) {
        free(conn->session_ticket);
        conn->session_ticket = NULL;
    }

    /* Free message environment */
    if (conn->msg_env) {
        enif_free_env(conn->msg_env);
        conn->msg_env = NULL;
    }

    /* Free mutex */
    if (conn->mutex) {
        enif_mutex_destroy(conn->mutex);
        conn->mutex = NULL;
    }
}
