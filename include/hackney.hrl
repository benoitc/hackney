-define(RECV_TIMEOUT, 5000).

-record(connection, {transport,
                     host,
                     port,
                     id,
                     tunnel = false}).

-record(client,  {
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
  headers=hackney_headers_new:new(),
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
  ctype = nil
}).


-define(CONFIG, hackney_config).

-define(CONNECTIONS, hackney_connections).

-define(PROXY_ENV_VARS, ["http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY"]).
