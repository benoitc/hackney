-define(METHODS, [
        delete,
        get,
        head,
        post,
        put,

        connect,
        options,
        trace,

        %% WEBDAV
        copy,
        lock,
        mkcol,
        move,
        propfind,
        proppatch,
        search,
        unlock,

        %% SUBVERSION
        report,
        mkactivity,
        checkout,
        merge,

        %% UPNP
        msearch,
        notify,
        subscribe,
        unsubscribe,

        %% RFC-5789
        patch,
        purge]).

-record(client, {
        transport,
        host,
        port,
        netloc = netloc,
        options = [],
        socket = nil,
        recv_timeout = infinity,
        follow_redirect = false,
        max_redirect = 5,
        force_redirect = false,
        redirect = nil,
        location,
        state,
        response_state = start,
        mp_boundary = nil,
        req_type = normal,
        send_fun=nil,
        body_state=waiting,
        req_chunk_size=4096,
        buffer = <<>>,
        version,
        clen = nil,
        te = nil,
        connection = nil,
        ctype = nil}).

-record(hackney_url, {
        transport,
        scheme,
        netloc,
        raw_path,
        path,
        qs = <<>>,
        fragment = <<>>,
        host,
        port,
        user = nil,
        password = <<>> }).

%% common types
-type hackney_url() :: #hackney_url{}.
