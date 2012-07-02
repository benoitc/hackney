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
        options = [],
        socket = nil,
        timeout = infinity,
        state,
        response_state=on_status,
        body_state=waiting,
        req_chunk_size=4096,
        buffer = <<>>,
        version,
        clen = nil,
        te = nil}).

-record(hackney_url, {
        transport,
        scheme,
        netloc,
        path,
        host,
        port,
        user = nil,
        password = <<>> }).

%% common types
-type hackney_url() :: #hackney_url{}.
