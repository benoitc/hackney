/**
 * quic_conn.h - QUIC connection resource management using lsquic
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 */

#ifndef QUIC_CONN_H
#define QUIC_CONN_H

#include "hackney_quic_nif.h"
#include <lsquic.h>
#include <openssl/ssl.h>
#include <sys/socket.h>
#include <netinet/in.h>

/* Connection states */
typedef enum {
    QUIC_CONN_IDLE,
    QUIC_CONN_HANDSHAKING,
    QUIC_CONN_CONNECTED,
    QUIC_CONN_DRAINING,
    QUIC_CONN_CLOSED
} QuicConnState;

/* Stream context for tracking individual HTTP/3 streams */
typedef struct QuicStream {
    int64_t stream_id;
    struct QuicConn *conn;
    bool headers_sent;
    bool headers_received;
    bool fin_sent;
    bool fin_received;
    struct QuicStream *next;  /* Linked list */
} QuicStream;

/* Header set for receiving headers */
typedef struct QuicHeaderSet {
    ERL_NIF_TERM headers_list;  /* Erlang list of {Name, Value} tuples */
    ErlNifEnv *env;             /* Environment for building the list */
    struct QuicConn *conn;
    int64_t stream_id;
} QuicHeaderSet;

/* QUIC connection resource */
struct QuicConn {
    /* lsquic engine (one per connection for simplicity) */
    lsquic_engine_t *engine;

    /* lsquic connection handle */
    lsquic_conn_t *conn;

    /* OpenSSL TLS context */
    SSL_CTX *ssl_ctx;

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

    /* Hostname for SNI */
    char *hostname;

    /* Port number */
    uint16_t port;

    /* Timer for retransmission/keep-alive (microseconds) */
    uint64_t next_timeout_us;

    /* Session ticket for 0-RTT (future use) */
    uint8_t *session_ticket;
    size_t session_ticket_len;

    /* Reference count for cleanup */
    int ref_count;

    /* Active streams (linked list) */
    QuicStream *streams;

    /* Self-reference for lsquic callbacks */
    ERL_NIF_TERM self_ref;

    /* Flag for whether we're in the event loop */
    bool processing;

    /* Thread for packet I/O */
    ErlNifTid io_thread;
    bool io_thread_running;
    bool should_stop;
};

/* Global initialization (call once at NIF load) */
int quic_global_init(void);

/* Global cleanup (call once at NIF unload) */
void quic_global_cleanup(void);

/* Initialize the QUIC connection resource type */
int quic_conn_resource_init(ErlNifEnv *env);

/* Create a new QUIC connection */
QuicConn *quic_conn_create(ErlNifEnv *env, ErlNifPid owner_pid);

/* Start QUIC connection to host:port
 * If sockfd >= 0, use the provided socket instead of creating a new one.
 * The socket should be a bound, non-blocking UDP socket.
 * Ownership of the socket is transferred to QuicConn.
 */
int quic_conn_connect(QuicConn *conn, const char *hostname, uint16_t port,
                      int sockfd, const struct sockaddr *local_addr, socklen_t local_addrlen);

/* Increment reference count */
void quic_conn_keep(QuicConn *conn);

/* Decrement reference count, destroy if zero */
void quic_conn_release(QuicConn *conn);

/* Destroy a QUIC connection */
void quic_conn_destroy(QuicConn *conn);

/* Resource destructor callback */
void quic_conn_resource_dtor(ErlNifEnv *env, void *obj);

/* Stream operations */
int64_t quic_conn_open_stream(QuicConn *conn);
int quic_conn_send_headers(QuicConn *conn, int64_t stream_id,
                           ERL_NIF_TERM headers_list, bool fin);
int quic_conn_send_data(QuicConn *conn, int64_t stream_id,
                        const uint8_t *data, size_t len, bool fin);
int quic_conn_reset_stream(QuicConn *conn, int64_t stream_id, uint64_t error_code);

/* Close connection gracefully */
int quic_conn_close(QuicConn *conn);

/* Process timeouts - returns next timeout in ms or -1 for infinity */
int64_t quic_conn_handle_timeout(QuicConn *conn);

/* Get peer/local addresses */
int quic_conn_peername(QuicConn *conn, struct sockaddr_storage *addr, socklen_t *addrlen);
int quic_conn_sockname(QuicConn *conn, struct sockaddr_storage *addr, socklen_t *addrlen);

/* Send message to owner process */
void quic_conn_send_to_owner(QuicConn *conn, ERL_NIF_TERM msg);

#endif /* QUIC_CONN_H */
