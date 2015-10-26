-define(RECV_TIMEOUT, 5000).

-record(client, {
        start_time,
        mod_metrics = nil,
        transport,
        host,
        port,
        netloc,
        options = [],
        socket = nil,
        socket_ref = nil,
        request_ref = nil,
        dynamic = true,
        pool_handler = hackney_pool,
        recv_timeout = ?RECV_TIMEOUT,
        follow_redirect = false,
        max_redirect = 5,
        force_redirect = false,
        retries = 0,
        redirect = nil,
        location=nil,
        parser=nil,
        state,
        response_state = start,
        mp_boundary = nil,
        req_type = normal,
        expect = false,
        async = false,
        with_body = false,
        max_body,
        stream_to,
        send_fun=nil,
        body_state=waiting,
        multipart=nil,
        req_chunk_size=4096,
        buffer = <<>>,
        partial_headers = [],
        version,
        clen = nil,
        te = nil,
        connection = nil,
        method = nil,
        path,
        ctype = nil}).



-define(DEFAULT_CACHE_SIZE, 1000).
-define(TAB, hackney_server).
-define(LOOKUP_CACHE, hackney_lookup).

%% default pool info
-define(DEFAULT_IDLE_TIMEOUT, 150000). %% default time until a connectino is forced to closed
-define(DEFAULT_GROUP_LIMIT, 6). %% max number of connections kept for a group
-define(DEFAULT_PROXY_LIMIT, 20). %% max number of connections cached / proxy
-define(DEFAULT_MAX_CONNS, 200). %% maximum number of connections kept

%% connectors options
-define(DEFAULT_NB_CONNECTORS, 20).
-define(DEFAULT_FALLBACK_TIME, 250).
