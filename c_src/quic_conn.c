/**
 * quic_conn.c - QUIC connection resource management using lsquic
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 */

#include "quic_conn.h"
#include "atoms.h"
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sys/select.h>
#include <lsxpack_header.h>

/* Maximum UDP packet size */
#define MAX_UDP_PAYLOAD 1500

/* Buffer for incoming packets */
#define RECV_BUF_SIZE 65536

/* Global resource type for QUIC connections */
ErlNifResourceType *QUIC_CONN_RESOURCE = NULL;

/* Global initialization flag - use atomic to prevent race conditions */
static volatile int g_lsquic_initialized = 0;
static ErlNifMutex *g_init_mutex = NULL;

/* Forward declarations for lsquic callbacks */
static lsquic_conn_ctx_t *on_new_conn(void *stream_if_ctx, lsquic_conn_t *conn);
static void on_conn_closed(lsquic_conn_t *conn);
static lsquic_stream_ctx_t *on_new_stream(void *stream_if_ctx, lsquic_stream_t *s);
static void on_read(lsquic_stream_t *s, lsquic_stream_ctx_t *h);
static void on_write(lsquic_stream_t *s, lsquic_stream_ctx_t *h);
static void on_close(lsquic_stream_t *s, lsquic_stream_ctx_t *h);
static void on_hsk_done(lsquic_conn_t *conn, enum lsquic_hsk_status status);
static void on_goaway_received(lsquic_conn_t *conn);

/* Packet output callback */
static int packets_out(void *ctx, const struct lsquic_out_spec *specs, unsigned n_specs);

/* SSL context callback */
static SSL_CTX *get_ssl_ctx(void *peer_ctx, const struct sockaddr *local);

/* Header set interface callbacks */
static void *hsi_create_header_set(void *ctx, lsquic_stream_t *s, int is_push);
static struct lsxpack_header *hsi_prepare_decode(void *hdr_set,
                                                  struct lsxpack_header *hdr,
                                                  size_t space);
static int hsi_process_header(void *hdr_set, struct lsxpack_header *hdr);
static void hsi_discard_header_set(void *hdr_set);

/* Stream interface for lsquic */
static const struct lsquic_stream_if stream_if = {
    .on_new_conn        = on_new_conn,
    .on_conn_closed     = on_conn_closed,
    .on_new_stream      = on_new_stream,
    .on_read            = on_read,
    .on_write           = on_write,
    .on_close           = on_close,
    .on_hsk_done        = on_hsk_done,
    .on_goaway_received = on_goaway_received,
};

/* Header set interface */
static const struct lsquic_hset_if hset_if = {
    .hsi_create_header_set = hsi_create_header_set,
    .hsi_prepare_decode    = hsi_prepare_decode,
    .hsi_process_header    = hsi_process_header,
    .hsi_discard_header_set = hsi_discard_header_set,
    .hsi_flags             = 0,
};

/*===========================================================================
 * Utility functions
 *===========================================================================*/

/* Get current time in microseconds */
static uint64_t get_time_us(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)tv.tv_sec * 1000000 + tv.tv_usec;
}

/* Set socket to non-blocking mode */
static int set_nonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) return -1;
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

/* Create UDP socket and bind */
static int create_udp_socket(int family, struct sockaddr_storage *local_addr,
                             socklen_t *local_addrlen) {
    int fd = socket(family, SOCK_DGRAM, 0);
    if (fd < 0) return -1;

    if (set_nonblocking(fd) < 0) {
        close(fd);
        return -1;
    }

    /* Enable address reuse */
    int on = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

    /* Bind to any available port */
    if (family == AF_INET) {
        struct sockaddr_in addr = {0};
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = INADDR_ANY;
        addr.sin_port = 0;
        if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
            close(fd);
            return -1;
        }
    } else {
        struct sockaddr_in6 addr = {0};
        addr.sin6_family = AF_INET6;
        addr.sin6_addr = in6addr_any;
        addr.sin6_port = 0;
        if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
            close(fd);
            return -1;
        }
    }

    /* Get the bound address */
    *local_addrlen = sizeof(*local_addr);
    if (getsockname(fd, (struct sockaddr *)local_addr, local_addrlen) < 0) {
        close(fd);
        return -1;
    }

    return fd;
}

/* Resolve hostname to sockaddr */
static int resolve_hostname(const char *hostname, uint16_t port,
                           struct sockaddr_storage *addr, socklen_t *addrlen) {
    struct addrinfo hints = {0};
    struct addrinfo *res;
    char port_str[6];

    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;

    snprintf(port_str, sizeof(port_str), "%u", port);

    int ret = getaddrinfo(hostname, port_str, &hints, &res);
    if (ret != 0) {
        return -1;
    }

    memcpy(addr, res->ai_addr, res->ai_addrlen);
    *addrlen = res->ai_addrlen;

    freeaddrinfo(res);
    return 0;
}

/*===========================================================================
 * Global initialization
 *===========================================================================*/

int quic_global_init(void) {
    /* Fast path - already initialized */
    if (__atomic_load_n(&g_lsquic_initialized, __ATOMIC_ACQUIRE)) return 0;

    /* Create mutex on first call - this is safe because NIF load is single-threaded */
    if (!g_init_mutex) {
        g_init_mutex = enif_mutex_create("quic_global_init_mutex");
        if (!g_init_mutex) return -1;
    }

    /* Double-checked locking */
    enif_mutex_lock(g_init_mutex);
    if (__atomic_load_n(&g_lsquic_initialized, __ATOMIC_RELAXED)) {
        enif_mutex_unlock(g_init_mutex);
        return 0;
    }

    if (lsquic_global_init(LSQUIC_GLOBAL_CLIENT) != 0) {
        enif_mutex_unlock(g_init_mutex);
        return -1;
    }

    __atomic_store_n(&g_lsquic_initialized, 1, __ATOMIC_RELEASE);
    enif_mutex_unlock(g_init_mutex);
    return 0;
}

void quic_global_cleanup(void) {
    if (__atomic_load_n(&g_lsquic_initialized, __ATOMIC_ACQUIRE)) {
        if (g_init_mutex) {
            enif_mutex_lock(g_init_mutex);
        }
        if (__atomic_load_n(&g_lsquic_initialized, __ATOMIC_RELAXED)) {
            lsquic_global_cleanup();
            __atomic_store_n(&g_lsquic_initialized, 0, __ATOMIC_RELEASE);
        }
        if (g_init_mutex) {
            enif_mutex_unlock(g_init_mutex);
            enif_mutex_destroy(g_init_mutex);
            g_init_mutex = NULL;
        }
    }
}

/*===========================================================================
 * SSL context
 *===========================================================================*/

static SSL_CTX *create_ssl_ctx(void) {
    SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
    if (!ctx) return NULL;

    /* Set minimum TLS version to 1.3 (required for QUIC) */
    SSL_CTX_set_min_proto_version(ctx, TLS1_3_VERSION);
    SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION);

    /* Set default verify paths */
    SSL_CTX_set_default_verify_paths(ctx);

    /* For now, skip verification (TODO: make configurable) */
    SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);

    return ctx;
}

static SSL_CTX *get_ssl_ctx(void *peer_ctx, const struct sockaddr *local) {
    UNUSED(local);
    QuicConn *conn = (QuicConn *)peer_ctx;
    return conn->ssl_ctx;
}

/*===========================================================================
 * lsquic stream interface callbacks
 *===========================================================================*/

static lsquic_conn_ctx_t *on_new_conn(void *stream_if_ctx, lsquic_conn_t *c) {
    QuicConn *conn = (QuicConn *)stream_if_ctx;
    conn->conn = c;
    return (lsquic_conn_ctx_t *)conn;
}

static void on_conn_closed(lsquic_conn_t *c) {
    lsquic_conn_ctx_t *ctx = lsquic_conn_get_ctx(c);
    if (!ctx) return;

    QuicConn *conn = (QuicConn *)ctx;

    /* Note: Don't use mutex here - called from I/O thread which may hold it */
    conn->state = QUIC_CONN_CLOSED;

    /* Use a fresh environment for thread-safe message sending */
    ErlNifEnv *env = enif_alloc_env();
    if (!env) return;

    ERL_NIF_TERM msg = enif_make_tuple3(env,
        ATOM_QUIC,
        enif_make_resource(env, conn),
        enif_make_tuple2(env, ATOM_CLOSED, enif_make_atom(env, "peer_closed")));

    enif_send(NULL, &conn->owner_pid, env, msg);
    enif_free_env(env);
}

static void on_hsk_done(lsquic_conn_t *c, enum lsquic_hsk_status status) {
    lsquic_conn_ctx_t *ctx = lsquic_conn_get_ctx(c);
    if (!ctx) return;

    QuicConn *conn = (QuicConn *)ctx;

    /* Use a fresh environment for thread-safe message sending */
    ErlNifEnv *env = enif_alloc_env();
    if (!env) return;

    if (status == LSQ_HSK_OK || status == LSQ_HSK_RESUMED_OK) {
        /* Note: Don't use mutex here - we're called from I/O thread which holds it */
        conn->state = QUIC_CONN_CONNECTED;

        /* Notify owner of successful connection */
        ERL_NIF_TERM info = enif_make_new_map(env);
        enif_make_map_put(env, info,
            enif_make_atom(env, "resumed"),
            status == LSQ_HSK_RESUMED_OK ? ATOM_TRUE : ATOM_FALSE,
            &info);

        ERL_NIF_TERM msg = enif_make_tuple3(env,
            ATOM_QUIC,
            enif_make_resource(env, conn),
            enif_make_tuple2(env, ATOM_CONNECTED, info));

        enif_send(NULL, &conn->owner_pid, env, msg);
    } else {
        /* Handshake failed */
        /* Note: Don't use mutex here - we're called from I/O thread which holds it */
        conn->state = QUIC_CONN_CLOSED;

        ERL_NIF_TERM msg = enif_make_tuple3(env,
            ATOM_QUIC,
            enif_make_resource(env, conn),
            enif_make_tuple2(env, ATOM_CLOSED,
                enif_make_atom(env, "handshake_failed")));

        enif_send(NULL, &conn->owner_pid, env, msg);
    }

    enif_free_env(env);
}

static void on_goaway_received(lsquic_conn_t *c) {
    lsquic_conn_ctx_t *ctx = lsquic_conn_get_ctx(c);
    if (!ctx) return;

    QuicConn *conn = (QuicConn *)ctx;

    /* Note: Don't use mutex here - called from I/O thread which may hold it */
    conn->state = QUIC_CONN_DRAINING;

    /* Use a fresh environment for thread-safe message sending */
    ErlNifEnv *env = enif_alloc_env();
    if (!env) return;

    ErlNifBinary empty_bin;
    enif_alloc_binary(0, &empty_bin);

    ERL_NIF_TERM msg = enif_make_tuple3(env,
        ATOM_QUIC,
        enif_make_resource(env, conn),
        enif_make_tuple4(env, ATOM_GOAWAY,
            enif_make_int64(env, 0),  /* LastStreamId - not available in callback */
            enif_make_int64(env, 0),  /* ErrorCode */
            enif_make_binary(env, &empty_bin)));

    enif_send(NULL, &conn->owner_pid, env, msg);
    enif_free_env(env);
}

static lsquic_stream_ctx_t *on_new_stream(void *stream_if_ctx, lsquic_stream_t *s) {
    QuicConn *conn = (QuicConn *)stream_if_ctx;
    if (!s) return NULL;

    /* Create stream context */
    QuicStream *stream = enif_alloc(sizeof(QuicStream));
    if (!stream) return NULL;

    memset(stream, 0, sizeof(QuicStream));
    stream->stream_id = lsquic_stream_id(s);
    stream->stream = s;  /* Store lsquic stream handle */
    stream->conn = conn;

    /* Add to connection's stream list (no mutex - called from I/O thread) */
    stream->next = conn->streams;
    conn->streams = stream;

    /* Want to read headers/data */
    lsquic_stream_wantread(s, 1);

    /* Notify Erlang of the new stream */
    ErlNifEnv *env = enif_alloc_env();
    if (env) {
        ERL_NIF_TERM msg = enif_make_tuple3(env,
            ATOM_QUIC,
            enif_make_resource(env, conn),
            enif_make_tuple2(env, ATOM_STREAM_OPENED,
                enif_make_int64(env, stream->stream_id)));

        enif_send(NULL, &conn->owner_pid, env, msg);
        enif_free_env(env);
    }

    return (lsquic_stream_ctx_t *)stream;
}

static void on_read(lsquic_stream_t *s, lsquic_stream_ctx_t *h) {
    if (!h) return;

    QuicStream *stream = (QuicStream *)h;
    QuicConn *conn = stream->conn;

    /* Check if we need to read headers first */
    if (!stream->headers_received) {
        void *hset = lsquic_stream_get_hset(s);
        if (hset) {
            QuicHeaderSet *header_set = (QuicHeaderSet *)hset;

            /* Use a fresh environment for thread-safe message sending */
            ErlNifEnv *env = enif_alloc_env();
            if (env) {
                /* Copy headers list from header_set env to new env */
                ERL_NIF_TERM headers = enif_make_copy(env, header_set->headers_list);

                ERL_NIF_TERM msg = enif_make_tuple3(env,
                    ATOM_QUIC,
                    enif_make_resource(env, conn),
                    enif_make_tuple4(env, ATOM_STREAM_HEADERS,
                        enif_make_int64(env, stream->stream_id),
                        headers,
                        ATOM_FALSE));  /* fin flag */

                enif_send(NULL, &conn->owner_pid, env, msg);
                enif_free_env(env);
            }

            /* Clean up header set */
            enif_free_env(header_set->env);
            enif_free(header_set);

            stream->headers_received = true;
        }
    }

    /* Read data */
    unsigned char buf[16384];
    ssize_t nread;

    while ((nread = lsquic_stream_read(s, buf, sizeof(buf))) > 0) {
        ErlNifEnv *env = enif_alloc_env();
        if (!env) continue;

        ErlNifBinary bin;
        enif_alloc_binary(nread, &bin);
        memcpy(bin.data, buf, nread);

        ERL_NIF_TERM msg = enif_make_tuple3(env,
            ATOM_QUIC,
            enif_make_resource(env, conn),
            enif_make_tuple4(env, ATOM_STREAM_DATA,
                enif_make_int64(env, stream->stream_id),
                enif_make_binary(env, &bin),
                ATOM_FALSE));

        enif_send(NULL, &conn->owner_pid, env, msg);
        enif_free_env(env);
    }

    if (nread == 0) {
        /* EOF - FIN received */
        stream->fin_received = true;

        ErlNifEnv *env = enif_alloc_env();
        if (env) {
            ErlNifBinary empty_bin;
            enif_alloc_binary(0, &empty_bin);

            ERL_NIF_TERM msg = enif_make_tuple3(env,
                ATOM_QUIC,
                enif_make_resource(env, conn),
                enif_make_tuple4(env, ATOM_STREAM_DATA,
                    enif_make_int64(env, stream->stream_id),
                    enif_make_binary(env, &empty_bin),
                    ATOM_TRUE));

            enif_send(NULL, &conn->owner_pid, env, msg);
            enif_free_env(env);
        }

        lsquic_stream_wantread(s, 0);
    } else if (nread < 0 && errno != EWOULDBLOCK) {
        /* Error */
        lsquic_stream_wantread(s, 0);
    }
}

static void on_write(lsquic_stream_t *s, lsquic_stream_ctx_t *h) {
    if (!h) return;

    QuicStream *stream = (QuicStream *)h;
    QuicConn *conn = stream->conn;

    /* Use a fresh environment for thread-safe message sending */
    ErlNifEnv *env = enif_alloc_env();
    if (env) {
        ERL_NIF_TERM msg = enif_make_tuple3(env,
            ATOM_QUIC,
            enif_make_resource(env, conn),
            enif_make_tuple2(env, ATOM_SEND_READY,
                enif_make_int64(env, stream->stream_id)));

        enif_send(NULL, &conn->owner_pid, env, msg);
        enif_free_env(env);
    }

    /* Don't want write events until explicitly requested */
    lsquic_stream_wantwrite(s, 0);
}

static void on_close(lsquic_stream_t *s, lsquic_stream_ctx_t *h) {
    if (!h) return;

    QuicStream *stream = (QuicStream *)h;
    QuicConn *conn = stream->conn;

    /* Remove from connection's stream list (no mutex - called from I/O thread) */
    QuicStream **pp = &conn->streams;
    while (*pp) {
        if (*pp == stream) {
            *pp = stream->next;
            break;
        }
        pp = &(*pp)->next;
    }

    enif_free(stream);
}

/*===========================================================================
 * Header set interface callbacks
 *===========================================================================*/

static void *hsi_create_header_set(void *ctx, lsquic_stream_t *s, int is_push) {
    UNUSED(is_push);

    QuicConn *conn = (QuicConn *)ctx;

    QuicHeaderSet *hset = enif_alloc(sizeof(QuicHeaderSet));
    if (!hset) return NULL;

    hset->env = enif_alloc_env();
    if (!hset->env) {
        enif_free(hset);
        return NULL;
    }

    hset->headers_list = enif_make_list(hset->env, 0);
    hset->conn = conn;
    hset->stream_id = s ? lsquic_stream_id(s) : -1;

    return hset;
}

static struct lsxpack_header *hsi_prepare_decode(void *hdr_set,
                                                  struct lsxpack_header *hdr,
                                                  size_t space) {
    UNUSED(hdr_set);

    if (hdr) {
        /* Resize existing header - use enif_alloc since we can't realloc */
        if (space > LSXPACK_MAX_STRLEN) return NULL;
        char *new_buf = enif_alloc(space);
        if (!new_buf) return NULL;
        /* Copy existing data if any */
        if (hdr->buf && hdr->val_len > 0) {
            size_t copy_len = hdr->val_len < space ? hdr->val_len : space;
            memcpy(new_buf, hdr->buf, copy_len);
            enif_free(hdr->buf);
        }
        hdr->buf = new_buf;
        hdr->val_len = space;
        return hdr;
    }

    /* Create new header */
    struct lsxpack_header *new_hdr = enif_alloc(sizeof(struct lsxpack_header));
    if (!new_hdr) return NULL;

    memset(new_hdr, 0, sizeof(*new_hdr));
    new_hdr->buf = enif_alloc(space);
    if (!new_hdr->buf) {
        enif_free(new_hdr);
        return NULL;
    }
    lsxpack_header_prepare_decode(new_hdr, new_hdr->buf, 0, space);

    return new_hdr;
}

static int hsi_process_header(void *hdr_set, struct lsxpack_header *hdr) {
    if (!hdr) {
        return 0;  /* End of headers */
    }

    QuicHeaderSet *hs = (QuicHeaderSet *)hdr_set;

    /* Extract name and value */
    const char *name = lsxpack_header_get_name(hdr);
    size_t name_len = hdr->name_len;
    const char *value = lsxpack_header_get_value(hdr);
    size_t value_len = hdr->val_len;

    /* Create Erlang binaries */
    ErlNifBinary name_bin, value_bin;
    enif_alloc_binary(name_len, &name_bin);
    enif_alloc_binary(value_len, &value_bin);
    memcpy(name_bin.data, name, name_len);
    memcpy(value_bin.data, value, value_len);

    /* Create {Name, Value} tuple and prepend to list */
    ERL_NIF_TERM tuple = enif_make_tuple2(hs->env,
        enif_make_binary(hs->env, &name_bin),
        enif_make_binary(hs->env, &value_bin));

    hs->headers_list = enif_make_list_cell(hs->env, tuple, hs->headers_list);

    /* Free the header buffer */
    enif_free(hdr->buf);
    enif_free(hdr);

    return 0;
}

static void hsi_discard_header_set(void *hdr_set) {
    if (!hdr_set) return;

    QuicHeaderSet *hs = (QuicHeaderSet *)hdr_set;
    if (hs->env) enif_free_env(hs->env);
    enif_free(hs);
}

/*===========================================================================
 * Packet output
 *===========================================================================*/

static int packets_out(void *ctx, const struct lsquic_out_spec *specs,
                       unsigned n_specs) {
    QuicConn *conn = (QuicConn *)ctx;
    unsigned n;

    for (n = 0; n < n_specs; n++) {
        struct msghdr msg = {0};
        msg.msg_name = (void *)specs[n].dest_sa;
        msg.msg_namelen = specs[n].dest_sa->sa_family == AF_INET ?
                          sizeof(struct sockaddr_in) : sizeof(struct sockaddr_in6);
        msg.msg_iov = specs[n].iov;
        msg.msg_iovlen = specs[n].iovlen;

        ssize_t sent = sendmsg(conn->sockfd, &msg, 0);
        if (sent < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                return n;
            }
            return -1;
        }
    }

    return n;
}

/*===========================================================================
 * I/O thread
 *===========================================================================*/

static void *io_thread_func(void *arg) {
    QuicConn *conn = (QuicConn *)arg;
    unsigned char buf[RECV_BUF_SIZE];
    struct sockaddr_storage peer_addr;
    socklen_t peer_addrlen;

    while (!__atomic_load_n(&conn->should_stop, __ATOMIC_RELAXED) &&
           !__atomic_load_n(&conn->destroyed, __ATOMIC_RELAXED)) {
        /* Calculate timeout */
        int timeout_ms = 50;  /* Default poll interval */

        enif_mutex_lock(conn->mutex);
        if (conn->next_timeout_us > 0) {
            uint64_t now = get_time_us();
            if (conn->next_timeout_us > now) {
                uint64_t delta = conn->next_timeout_us - now;
                timeout_ms = (int)(delta / 1000);
                if (timeout_ms > 1000) timeout_ms = 1000;
                if (timeout_ms < 1) timeout_ms = 1;
            } else {
                timeout_ms = 0;
            }
        }
        enif_mutex_unlock(conn->mutex);

        /* Wait for socket to be readable */
        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(conn->sockfd, &rfds);

        struct timeval tv;
        tv.tv_sec = timeout_ms / 1000;
        tv.tv_usec = (timeout_ms % 1000) * 1000;

        int ret = select(conn->sockfd + 1, &rfds, NULL, NULL, &tv);

        if (__atomic_load_n(&conn->should_stop, __ATOMIC_RELAXED) ||
            __atomic_load_n(&conn->destroyed, __ATOMIC_RELAXED)) break;

        enif_mutex_lock(conn->mutex);

        /* Verify engine is still valid before processing */
        if (!conn->engine) {
            enif_mutex_unlock(conn->mutex);
            break;
        }

        if (ret > 0 && FD_ISSET(conn->sockfd, &rfds)) {
            /* Receive packet */
            peer_addrlen = sizeof(peer_addr);
            ssize_t nread = recvfrom(conn->sockfd, buf, sizeof(buf), 0,
                                     (struct sockaddr *)&peer_addr, &peer_addrlen);

            if (nread > 0) {
                /* Feed to lsquic */
                lsquic_engine_packet_in(conn->engine, buf, nread,
                    (struct sockaddr *)&conn->local_addr,
                    (struct sockaddr *)&peer_addr,
                    conn, 0);
            }
        }

        /* Process connections (handles timeouts too) */
        lsquic_engine_process_conns(conn->engine);


        /* Update next timeout */
        int diff = lsquic_engine_earliest_adv_tick(conn->engine, &ret);
        if (ret) {
            conn->next_timeout_us = get_time_us() + (uint64_t)diff;
        } else {
            conn->next_timeout_us = 0;
        }

        /* Send any pending packets */
        if (lsquic_engine_has_unsent_packets(conn->engine)) {
            lsquic_engine_send_unsent_packets(conn->engine);
        }

        enif_mutex_unlock(conn->mutex);
    }

    return NULL;
}

/*===========================================================================
 * Resource management
 *===========================================================================*/

void quic_conn_resource_dtor(ErlNifEnv *env, void *obj) {
    UNUSED(env);
    QuicConn *conn = (QuicConn *)obj;
    quic_conn_destroy(conn);
}

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

    if (!QUIC_CONN_RESOURCE) return 0;

    /* Initialize lsquic globally */
    return quic_global_init() == 0;
}

QuicConn *quic_conn_create(ErlNifEnv *env, ErlNifPid owner_pid) {
    UNUSED(env);

    QuicConn *conn = enif_alloc_resource(QUIC_CONN_RESOURCE, sizeof(QuicConn));
    if (!conn) return NULL;

    memset(conn, 0, sizeof(QuicConn));

    conn->sockfd = -1;
    conn->owner_pid = owner_pid;
    conn->state = QUIC_CONN_IDLE;
    conn->ref_count = 1;

    conn->mutex = enif_mutex_create("quic_conn_mutex");
    if (!conn->mutex) {
        enif_release_resource(conn);
        return NULL;
    }

    return conn;
}

int quic_conn_connect(QuicConn *conn, const char *hostname, uint16_t port,
                      int sockfd, const struct sockaddr *local_addr, socklen_t local_addrlen) {
    if (!conn || !hostname) return -1;

    /* Store hostname */
    conn->hostname = strdup(hostname);
    if (!conn->hostname) return -1;
    conn->port = port;

    /* Resolve hostname */
    if (resolve_hostname(hostname, port, &conn->remote_addr, &conn->remote_addrlen) < 0) {
        return -1;
    }

    /* Use provided socket or create new one */
    if (sockfd >= 0) {
        /* Use external socket - ensure non-blocking */
        set_nonblocking(sockfd);
        conn->sockfd = sockfd;

        /* Use provided local address or get from socket */
        if (local_addr && local_addrlen > 0) {
            memcpy(&conn->local_addr, local_addr, local_addrlen);
            conn->local_addrlen = local_addrlen;
        } else {
            conn->local_addrlen = sizeof(conn->local_addr);
            if (getsockname(sockfd, (struct sockaddr *)&conn->local_addr,
                           &conn->local_addrlen) < 0) {
                return -1;
            }
        }
    } else {
        /* Create new UDP socket */
        int family = conn->remote_addr.ss_family;
        conn->sockfd = create_udp_socket(family, &conn->local_addr, &conn->local_addrlen);
        if (conn->sockfd < 0) {
            return -1;
        }
    }

    /* Create SSL context */
    conn->ssl_ctx = create_ssl_ctx();
    if (!conn->ssl_ctx) {
        close(conn->sockfd);
        conn->sockfd = -1;
        return -1;
    }

    /* Create lsquic engine */
    struct lsquic_engine_settings settings;
    lsquic_engine_init_settings(&settings, LSENG_HTTP);

    /* Use only IETF QUIC v1 */
    settings.es_versions = (1 << LSQVER_I001);

    struct lsquic_engine_api api = {0};
    api.ea_settings = &settings;
    api.ea_stream_if = &stream_if;
    api.ea_stream_if_ctx = conn;
    api.ea_packets_out = packets_out;
    api.ea_packets_out_ctx = conn;
    api.ea_get_ssl_ctx = get_ssl_ctx;
    api.ea_hsi_if = &hset_if;
    api.ea_hsi_ctx = conn;

    conn->engine = lsquic_engine_new(LSENG_HTTP, &api);
    if (!conn->engine) {
        SSL_CTX_free(conn->ssl_ctx);
        conn->ssl_ctx = NULL;
        close(conn->sockfd);
        conn->sockfd = -1;
        return -1;
    }

    /* Initiate connection */
    conn->state = QUIC_CONN_HANDSHAKING;

    lsquic_conn_t *lconn = lsquic_engine_connect(
        conn->engine,
        LSQVER_I001,  /* QUIC v1 (RFC 9000) */
        (struct sockaddr *)&conn->local_addr,
        (struct sockaddr *)&conn->remote_addr,
        conn,       /* peer_ctx */
        (lsquic_conn_ctx_t *)conn,  /* conn_ctx */
        hostname,   /* SNI */
        0,          /* base_plpmtu - let engine decide */
        NULL, 0,    /* session resumption */
        NULL, 0     /* token */
    );

    if (!lconn) {
        lsquic_engine_destroy(conn->engine);
        conn->engine = NULL;
        SSL_CTX_free(conn->ssl_ctx);
        conn->ssl_ctx = NULL;
        close(conn->sockfd);
        conn->sockfd = -1;
        conn->state = QUIC_CONN_IDLE;
        return -1;
    }

    conn->conn = lconn;

    /* Process to send initial packets */
    lsquic_engine_process_conns(conn->engine);

    /* Start I/O thread */
    __atomic_store_n(&conn->should_stop, 0, __ATOMIC_RELAXED);
    if (enif_thread_create("quic_io", &conn->io_thread, io_thread_func, conn, NULL) != 0) {
        lsquic_engine_destroy(conn->engine);
        conn->engine = NULL;
        SSL_CTX_free(conn->ssl_ctx);
        conn->ssl_ctx = NULL;
        close(conn->sockfd);
        conn->sockfd = -1;
        conn->state = QUIC_CONN_IDLE;
        return -1;
    }
    conn->io_thread_running = true;

    return 0;
}

void quic_conn_keep(QuicConn *conn) {
    if (!conn) return;

    /* Don't keep a destroyed connection */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) {
        return;
    }

    enif_mutex_lock(conn->mutex);
    conn->ref_count++;
    enif_mutex_unlock(conn->mutex);
    enif_keep_resource(conn);
}

void quic_conn_release(QuicConn *conn) {
    if (!conn) return;

    /* Check if already destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) {
        enif_release_resource(conn);
        return;
    }

    enif_mutex_lock(conn->mutex);
    int ref = --conn->ref_count;
    enif_mutex_unlock(conn->mutex);

    if (ref <= 0) {
        /* Don't call destroy here - let the Erlang GC call the destructor */
        /* The destructor will call quic_conn_destroy */
    }
    enif_release_resource(conn);
}

void quic_conn_destroy(QuicConn *conn) {
    if (!conn) return;

    /* Atomically check and set destroyed flag to prevent double-free */
    int expected = 0;
    if (!__atomic_compare_exchange_n(&conn->destroyed, &expected, 1,
                                      0, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {
        /* Already destroyed or being destroyed */
        return;
    }

    /* Stop I/O thread */
    if (conn->io_thread_running) {
        __atomic_store_n(&conn->should_stop, 1, __ATOMIC_RELAXED);
        enif_thread_join(conn->io_thread, NULL);
        conn->io_thread_running = false;
    }

    /* Close lsquic connection */
    if (conn->conn && conn->engine) {
        lsquic_conn_close(conn->conn);
        lsquic_engine_process_conns(conn->engine);
        conn->conn = NULL;
    }

    /* Destroy lsquic engine */
    if (conn->engine) {
        lsquic_engine_destroy(conn->engine);
        conn->engine = NULL;
    }

    /* Free SSL context */
    if (conn->ssl_ctx) {
        SSL_CTX_free(conn->ssl_ctx);
        conn->ssl_ctx = NULL;
    }

    /* Close socket */
    if (conn->sockfd >= 0) {
        close(conn->sockfd);
        conn->sockfd = -1;
    }

    /* Free hostname */
    if (conn->hostname) {
        free(conn->hostname);
        conn->hostname = NULL;
    }

    /* Free session ticket */
    if (conn->session_ticket) {
        free(conn->session_ticket);
        conn->session_ticket = NULL;
    }

    /* Free streams */
    QuicStream *stream = conn->streams;
    while (stream) {
        QuicStream *next = stream->next;
        enif_free(stream);
        stream = next;
    }
    conn->streams = NULL;

    /* Free mutex */
    if (conn->mutex) {
        enif_mutex_destroy(conn->mutex);
        conn->mutex = NULL;
    }
}

/*===========================================================================
 * Stream operations
 *===========================================================================*/

/* Find a stream by its ID */
static QuicStream *find_stream(QuicConn *conn, int64_t stream_id) {
    QuicStream *stream = conn->streams;
    while (stream) {
        if (stream->stream_id == stream_id) {
            return stream;
        }
        stream = stream->next;
    }
    return NULL;
}

int64_t quic_conn_open_stream(QuicConn *conn) {
    if (!conn || !conn->conn) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);
    if (conn->state != QUIC_CONN_CONNECTED || !conn->engine) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    /* Remember the current head of streams list */
    QuicStream *prev_head = conn->streams;

    /* Request a new stream - callback will be triggered synchronously */
    lsquic_conn_make_stream(conn->conn);
    lsquic_engine_process_conns(conn->engine);

    /* on_new_stream adds new stream at head of list */
    int64_t stream_id = -1;
    if (conn->streams && conn->streams != prev_head) {
        stream_id = conn->streams->stream_id;
    }

    enif_mutex_unlock(conn->mutex);
    return stream_id;
}

int quic_conn_close(QuicConn *conn) {
    if (!conn) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);
    if (conn->conn && conn->engine && conn->state != QUIC_CONN_CLOSED) {
        conn->state = QUIC_CONN_DRAINING;
        lsquic_conn_close(conn->conn);
        lsquic_engine_process_conns(conn->engine);
    }
    enif_mutex_unlock(conn->mutex);

    return 0;
}

/* Maximum headers we support in a single send */
#define MAX_HEADERS 64

int quic_conn_send_headers(QuicConn *conn, int64_t stream_id,
                           ErlNifEnv *env, ERL_NIF_TERM headers_list, bool fin) {
    if (!conn || !conn->conn) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);
    if (conn->state != QUIC_CONN_CONNECTED || !conn->engine) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    QuicStream *stream = find_stream(conn, stream_id);
    if (!stream || !stream->stream) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    if (stream->headers_sent) {
        enif_mutex_unlock(conn->mutex);
        return -1;  /* Headers already sent */
    }

    /* Count headers and calculate buffer size */
    unsigned count = 0;
    size_t total_buf_size = 0;
    ERL_NIF_TERM list = headers_list;
    ERL_NIF_TERM head;

    while (enif_get_list_cell(env, list, &head, &list)) {
        int arity;
        const ERL_NIF_TERM *tuple;
        if (!enif_get_tuple(env, head, &arity, &tuple) || arity != 2) {
            enif_mutex_unlock(conn->mutex);
            return -1;
        }

        ErlNifBinary name_bin, value_bin;
        if (!enif_inspect_binary(env, tuple[0], &name_bin) &&
            !enif_inspect_iolist_as_binary(env, tuple[0], &name_bin)) {
            enif_mutex_unlock(conn->mutex);
            return -1;
        }
        if (!enif_inspect_binary(env, tuple[1], &value_bin) &&
            !enif_inspect_iolist_as_binary(env, tuple[1], &value_bin)) {
            enif_mutex_unlock(conn->mutex);
            return -1;
        }

        total_buf_size += name_bin.size + value_bin.size;
        count++;
        if (count >= MAX_HEADERS) break;
    }

    if (count == 0) {
        enif_mutex_unlock(conn->mutex);
        return -1;  /* Empty headers */
    }

    /* Allocate buffer and headers array */
    char *buf = enif_alloc(total_buf_size);
    struct lsxpack_header *headers = enif_alloc(count * sizeof(struct lsxpack_header));
    if (!buf || !headers) {
        if (buf) enif_free(buf);
        if (headers) enif_free(headers);
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    /* Fill in headers */
    list = headers_list;
    size_t offset = 0;
    unsigned i = 0;

    while (enif_get_list_cell(env, list, &head, &list) && i < count) {
        int arity;
        const ERL_NIF_TERM *tuple;
        enif_get_tuple(env, head, &arity, &tuple);

        ErlNifBinary name_bin, value_bin;
        enif_inspect_iolist_as_binary(env, tuple[0], &name_bin);
        enif_inspect_iolist_as_binary(env, tuple[1], &value_bin);

        /* Copy name and value to buffer */
        size_t name_offset = offset;
        memcpy(buf + offset, name_bin.data, name_bin.size);
        offset += name_bin.size;

        size_t val_offset = offset;
        memcpy(buf + offset, value_bin.data, value_bin.size);
        offset += value_bin.size;

        /* Set up the header */
        lsxpack_header_set_offset2(&headers[i], buf,
                                   name_offset, name_bin.size,
                                   val_offset, value_bin.size);
        i++;
    }

    /* Create HTTP headers struct */
    lsquic_http_headers_t http_headers = {
        .count = count,
        .headers = headers
    };

    /* Send headers */
    int ret = lsquic_stream_send_headers(stream->stream, &http_headers, fin);

    enif_free(buf);
    enif_free(headers);

    if (ret == 0) {
        stream->headers_sent = true;
        if (fin) {
            stream->fin_sent = true;
            /* Shutdown write side to send FIN */
            lsquic_stream_shutdown(stream->stream, 1);  /* 1 = write side */
        }
        /* Flush the stream to ensure headers are sent */
        lsquic_stream_flush(stream->stream);

        /* Re-enable read after sending request headers */
        lsquic_stream_wantread(stream->stream, 1);
    }

    /* Process to send the headers packet */
    lsquic_engine_process_conns(conn->engine);

    /* Send any packets immediately */
    if (lsquic_engine_has_unsent_packets(conn->engine)) {
        lsquic_engine_send_unsent_packets(conn->engine);
    }

    enif_mutex_unlock(conn->mutex);

    return ret;
}

int quic_conn_send_data(QuicConn *conn, int64_t stream_id,
                        const uint8_t *data, size_t len, bool fin) {
    if (!conn || !conn->conn) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);
    if (conn->state != QUIC_CONN_CONNECTED || !conn->engine) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    QuicStream *stream = find_stream(conn, stream_id);
    if (!stream || !stream->stream) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    ssize_t written = lsquic_stream_write(stream->stream, data, len);
    if (written < 0) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    if (fin) {
        lsquic_stream_shutdown(stream->stream, 1);  /* Shutdown write side */
        stream->fin_sent = true;
    }

    lsquic_engine_process_conns(conn->engine);
    enif_mutex_unlock(conn->mutex);

    return (int)written;
}

int quic_conn_reset_stream(QuicConn *conn, int64_t stream_id, uint64_t error_code) {
    UNUSED(error_code);  /* lsquic doesn't have a reset with error code API */
    if (!conn || !conn->conn) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);
    if (conn->state != QUIC_CONN_CONNECTED || !conn->engine) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    QuicStream *stream = find_stream(conn, stream_id);
    if (!stream || !stream->stream) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    /* Close the stream (lsquic doesn't have a reset with error code) */
    lsquic_stream_close(stream->stream);
    lsquic_engine_process_conns(conn->engine);

    enif_mutex_unlock(conn->mutex);
    return 0;
}

int64_t quic_conn_handle_timeout(QuicConn *conn) {
    if (!conn || !conn->engine) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);

    /* Double-check engine after acquiring mutex */
    if (!conn->engine) {
        enif_mutex_unlock(conn->mutex);
        return -1;
    }

    lsquic_engine_process_conns(conn->engine);

    int diff;
    int has_tick = lsquic_engine_earliest_adv_tick(conn->engine, &diff);

    int64_t result = has_tick ? diff / 1000 : -1;  /* Convert us to ms */
    enif_mutex_unlock(conn->mutex);

    return result;
}

int quic_conn_peername(QuicConn *conn, struct sockaddr_storage *addr, socklen_t *addrlen) {
    if (!conn) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);
    memcpy(addr, &conn->remote_addr, conn->remote_addrlen);
    *addrlen = conn->remote_addrlen;
    enif_mutex_unlock(conn->mutex);

    return 0;
}

int quic_conn_sockname(QuicConn *conn, struct sockaddr_storage *addr, socklen_t *addrlen) {
    if (!conn) return -1;

    /* Check if connection is destroyed */
    if (__atomic_load_n(&conn->destroyed, __ATOMIC_ACQUIRE)) return -1;

    enif_mutex_lock(conn->mutex);
    memcpy(addr, &conn->local_addr, conn->local_addrlen);
    *addrlen = conn->local_addrlen;
    enif_mutex_unlock(conn->mutex);

    return 0;
}
