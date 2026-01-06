/**
 * hackney_quic_nif.h - Common headers for hackney QUIC NIF
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 */

#ifndef HACKNEY_QUIC_NIF_H
#define HACKNEY_QUIC_NIF_H

#include <erl_nif.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

/* Forward declarations */
typedef struct QuicConn QuicConn;

/* Resource types (initialized in nif_load) */
extern ErlNifResourceType *QUIC_CONN_RESOURCE;

/* Common atoms - defined in atoms.h, initialized in nif_load */
extern ERL_NIF_TERM ATOM_OK;
extern ERL_NIF_TERM ATOM_ERROR;
extern ERL_NIF_TERM ATOM_TRUE;
extern ERL_NIF_TERM ATOM_FALSE;
extern ERL_NIF_TERM ATOM_UNDEFINED;
extern ERL_NIF_TERM ATOM_QUIC;
extern ERL_NIF_TERM ATOM_CONNECTED;
extern ERL_NIF_TERM ATOM_CLOSED;
extern ERL_NIF_TERM ATOM_TRANSPORT_ERROR;
extern ERL_NIF_TERM ATOM_STREAM_HEADERS;
extern ERL_NIF_TERM ATOM_STREAM_DATA;
extern ERL_NIF_TERM ATOM_STREAM_OPENED;
extern ERL_NIF_TERM ATOM_STREAM_RESET;
extern ERL_NIF_TERM ATOM_STOP_SENDING;
extern ERL_NIF_TERM ATOM_GOAWAY;
extern ERL_NIF_TERM ATOM_SESSION_TICKET;
extern ERL_NIF_TERM ATOM_SEND_READY;
extern ERL_NIF_TERM ATOM_TIMER;
extern ERL_NIF_TERM ATOM_BLOCKED;
extern ERL_NIF_TERM ATOM_NOT_CONNECTED;
extern ERL_NIF_TERM ATOM_BADARG;
extern ERL_NIF_TERM ATOM_ENOMEM;

/* Utility macros */
#define UNUSED(x) (void)(x)

/* Make a {ok, Value} tuple */
static inline ERL_NIF_TERM make_ok(ErlNifEnv *env, ERL_NIF_TERM value) {
    return enif_make_tuple2(env, ATOM_OK, value);
}

/* Make a {error, Reason} tuple */
static inline ERL_NIF_TERM make_error(ErlNifEnv *env, ERL_NIF_TERM reason) {
    return enif_make_tuple2(env, ATOM_ERROR, reason);
}

/* Make a {error, Atom} tuple from string */
static inline ERL_NIF_TERM make_error_str(ErlNifEnv *env, const char *reason) {
    return enif_make_tuple2(env, ATOM_ERROR, enif_make_atom(env, reason));
}

#endif /* HACKNEY_QUIC_NIF_H */
