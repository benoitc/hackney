/**
 * atoms.h - Atom definitions for hackney QUIC NIF
 *
 * This file is part of hackney released under the Apache 2 license.
 * See the NOTICE for more information.
 *
 * Copyright (c) 2024-2025 Benoit Chesneau
 */

#ifndef HACKNEY_QUIC_ATOMS_H
#define HACKNEY_QUIC_ATOMS_H

#include <erl_nif.h>

/* Atom declarations */
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_UNDEFINED;

/* QUIC-specific atoms */
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

/* Error atoms */
ERL_NIF_TERM ATOM_BLOCKED;
ERL_NIF_TERM ATOM_NOT_CONNECTED;
ERL_NIF_TERM ATOM_BADARG;
ERL_NIF_TERM ATOM_ENOMEM;
ERL_NIF_TERM ATOM_TIMEOUT;
ERL_NIF_TERM ATOM_CLOSED_BY_PEER;
ERL_NIF_TERM ATOM_CONNECTION_REFUSED;
ERL_NIF_TERM ATOM_TLS_ERROR;
ERL_NIF_TERM ATOM_PROTOCOL_ERROR;

/* Connection state atoms */
ERL_NIF_TERM ATOM_IDLE;
ERL_NIF_TERM ATOM_HANDSHAKING;
ERL_NIF_TERM ATOM_DRAINING;

/* Initialize all atoms - call from nif_load */
static inline void init_atoms(ErlNifEnv *env) {
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_UNDEFINED = enif_make_atom(env, "undefined");

    ATOM_QUIC = enif_make_atom(env, "quic");
    ATOM_CONNECTED = enif_make_atom(env, "connected");
    ATOM_CLOSED = enif_make_atom(env, "closed");
    ATOM_TRANSPORT_ERROR = enif_make_atom(env, "transport_error");
    ATOM_STREAM_HEADERS = enif_make_atom(env, "stream_headers");
    ATOM_STREAM_DATA = enif_make_atom(env, "stream_data");
    ATOM_STREAM_OPENED = enif_make_atom(env, "stream_opened");
    ATOM_STREAM_RESET = enif_make_atom(env, "stream_reset");
    ATOM_STOP_SENDING = enif_make_atom(env, "stop_sending");
    ATOM_GOAWAY = enif_make_atom(env, "goaway");
    ATOM_SESSION_TICKET = enif_make_atom(env, "session_ticket");
    ATOM_SEND_READY = enif_make_atom(env, "send_ready");
    ATOM_TIMER = enif_make_atom(env, "timer");

    ATOM_BLOCKED = enif_make_atom(env, "blocked");
    ATOM_NOT_CONNECTED = enif_make_atom(env, "not_connected");
    ATOM_BADARG = enif_make_atom(env, "badarg");
    ATOM_ENOMEM = enif_make_atom(env, "enomem");
    ATOM_TIMEOUT = enif_make_atom(env, "timeout");
    ATOM_CLOSED_BY_PEER = enif_make_atom(env, "closed_by_peer");
    ATOM_CONNECTION_REFUSED = enif_make_atom(env, "connection_refused");
    ATOM_TLS_ERROR = enif_make_atom(env, "tls_error");
    ATOM_PROTOCOL_ERROR = enif_make_atom(env, "protocol_error");

    ATOM_IDLE = enif_make_atom(env, "idle");
    ATOM_HANDSHAKING = enif_make_atom(env, "handshaking");
    ATOM_DRAINING = enif_make_atom(env, "draining");
}

#endif /* HACKNEY_QUIC_ATOMS_H */
