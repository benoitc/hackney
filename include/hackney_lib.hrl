-record(hackney_url, {
        transport,
        scheme,
        netloc,
        raw_path,
        path = <<>>,
        qs = <<>>,
        fragment = <<>>,
        host,
        port,
        user = <<>>,
        password = <<>> }).

%% common types
-type hackney_url() :: #hackney_url{}.


-record(hparser, {type=auto,
                  max_line_length=4096,
                  max_empty_lines=10,
                  empty_lines=0,
                  state=on_first_line,
                  buffer = <<>>,
                  version,
                  method,
                  partial_headers=[],
                  clen,
                  te,
                  connection,
                  ctype,
                  location,
                  body_state=waiting}).