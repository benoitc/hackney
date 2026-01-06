/**
 * quic_conn.h - QUIC connection resource management
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 */

#ifndef QUIC_CONN_H
#define QUIC_CONN_H

#include "hackney_quic_nif.h"
#include <ngtcp2/ngtcp2.h>
#include <nghttp3/nghttp3.h>
#include <openssl/ssl.h>

/* Connection states */
typedef enum {
    QUIC_CONN_IDLE,
    QUIC_CONN_HANDSHAKING,
    QUIC_CONN_CONNECTED,
    QUIC_CONN_DRAINING,
    QUIC_CONN_CLOSED
} QuicConnState;

/* QUIC connection resource */
struct QuicConn {
    /* ngtcp2 QUIC connection */
    ngtcp2_conn *conn;

    /* nghttp3 HTTP/3 connection */
    nghttp3_conn *h3conn;

    /* OpenSSL TLS context and connection */
    SSL_CTX *ssl_ctx;
    SSL *ssl;

    /* UDP socket */
    int sockfd;

    /* Owner Erlang process */
    ErlNifPid owner_pid;

    /* Environment for sending messages to owner */
    ErlNifEnv *msg_env;

    /* Mutex for thread safety */
    ErlNifMutex *mutex;

    /* Connection state */
    QuicConnState state;

    /* Remote address info */
    struct sockaddr_storage remote_addr;
    socklen_t remote_addrlen;

    /* Local address info */
    struct sockaddr_storage local_addr;
    socklen_t local_addrlen;

    /* Connection ID */
    ngtcp2_cid scid;  /* Source connection ID */
    ngtcp2_cid dcid;  /* Destination connection ID */

    /* Timer for retransmission/keep-alive */
    uint64_t next_timeout;

    /* Session ticket for 0-RTT (future use) */
    uint8_t *session_ticket;
    size_t session_ticket_len;

    /* Reference count for cleanup */
    int ref_count;
};

/* Initialize the QUIC connection resource type */
int quic_conn_resource_init(ErlNifEnv *env);

/* Create a new QUIC connection */
QuicConn *quic_conn_create(ErlNifEnv *env, ErlNifPid owner_pid);

/* Destroy a QUIC connection */
void quic_conn_destroy(QuicConn *conn);

/* Resource destructor callback */
void quic_conn_resource_dtor(ErlNifEnv *env, void *obj);

#endif /* QUIC_CONN_H */
