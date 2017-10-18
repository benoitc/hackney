# NEWS

1.9.0 - 2017-07-30
------------------

- security: certifi 2.0.0
- dependency: update idna 5.1.0 (fix windows build and usage with elixir)
- doc: fix typo hackney_multipart doc (#422)

1.8.6 - 2017-06-09
--------------------

- fix: cleanup socket in async request (#411) 

1.8.5 - 2017-05-30
------------------

- fix: dialyzer

1.8.4 - 2017-05-28
------------------

- fix: tests
- dependency: update idna  5.0.2 (fix compatibility with erlang R20)

1.8.3 - 2017-05-22
------------------

- security: certifi 1.2.1
- dependency: update idna  5.0.1

1.8.2 - 2017-05-20
------------------

- fix: race condition in controlling process (#407)
- fix: spec of #hackney_url{} (#404)
- fix: make sure to not lost a message during hiberntion in async request
- security: certifi 1.2.0
- dependency: update idna  5.0.0

1.8.0 - 2017-04-20
-----------------

- fix: undefined function (#393)
- fix: close connection if proxy handshake failed (#392)
- fix: handle all headers with the new datastructure introduced in 1.7.0 (#395)
- fix: host header when redirect (#400)
- fix: use connect timeout when retrieving from the pool (#402)
- security: new certifi version

1.7.1 - 2017-03-02
------------------

- fix: regression in headers handling (handle different key types)

1.7.0 - 2017-03-01
------------------

- fix: new datastructure to handle headers (#390)
- security: new certifi version

1.6.6 - 2017-02-26
------------------

- fix: fix header appending
- fix: Url encode host header for unix domain sockets (#382)
- security: new certifi version
- doc: fix few typos

1.6.4 - 2016-12-22
------------------

- add: optional urlencode options to qs (#368)
- fix: handle continuation lines in HTTP headers correctly (#366)
- doc: Fix a few documentation typos

1.6.3 - 2016-10-27
------------------

- fix: handle trailing whitespace in header values

1.6.2 - 2016-10-22
------------------

- add: unix sockets support on Erlang > 19
- fix: `hackney_multiprart` for Erlang < 17
- add: new `socks5_resolver` function
- fix: `hackney_util:merge_opts/2`
- improvements: inet6 support in socks5 sockets
- doc: miscenaleous docs fixes
- security: being more strict in ssl support
- security: bump to certifi 0.7

1.6.1 - 2016-07-10
------------------
- fix: close socket on error (#308)
- improvement: handle errors in `hackney_response:wait_status` (#313)
- improvement: make pathencode faster (#317)
- fix: typo (#321)
- fix: elixir 1.4 warnings (#325)

1.6.0 - 2016-03-25
------------------

- add `path_encode_fun` option to request.
- add: allow force non-POST 303 redirects
- use `ssl_verif_fun` dependency to replace `ssl_verify_hostname`
- fix: 	move included_applications to applications
- fix: mix packaging

1.5.4 - 2016-03-18
------------------

- fix support of rebar 3 stable
- add mix package

1.5.0 - 2016-03-02
------------------

- refactor: one flat source
- replace hackney_metrics_* by [metrics](https://github.com/benoitc/erlang-metrics) library
- fix: hackney_pool (#286)
- security: bump to [erlang-certifi](https//github.com/certifi/erlang-certifi) 0.4.0

1.4.10 - 2016/02/27
------------------

- bump to idna 1.1.0
- fix: don't encode @ in urls
- fix: header stream multipart

1.4.7 - 2015/12/07
------------------

- bump to mimerl 1.0.2

1.4.6 - 2015/11/24
------------------

- fix build with mix

1.4.5 - 2015/11/23
------------------

- fix multipart/form parsing (#258)
- TRAVIS-CI build with rebar3

1.4.4 - 2015/11/04
------------------

- fix rebar3 detection

1.4.3 - 2015/11/04
------------------

- fix header value parsing (#256)

1.4.2 - 2015/11/03
------------------

- fix build with rebar2 and Erlang < 17

1.4.1 - 2015/11/03
------------------

- fix build with mix (#255)

1.4.0 - 2015/10/27
------------------

- build using hex.pm & small refactoring
- fix multipart (#245)
- fix redirection (#237)
- fix url parsing (#236)
- close connection when max body length is reached (#248)

1.3.2 - 2015/08/27
------------------

- fix `connect_time metric` (#227)
- fix redirection when `with_body`  is enabled (#228)
- close half-closed socket to avoid leak (#231)
- fix undexpected message in `hackney_stream` (#223)
- fix receive/error in hackney_manager (#232)

1.3.1 - 2015/07/28
------------------

- fix: set default `recv_timeout` to 5s. (#219)
- fix: socks5 fix auth: handle not required case (#218)

1.3.0 - 2015/07/23
------------------

- new add `max_body` setting
- fix: handle partial chains during handshake in HTTPS (#196)

1.2.0 - 2015/06/25
------------------

- new: add `with_body` option to return the body directly (#184)
- fix: rely on ssl version to validate certificates securely using hostname
  verification
- fix: fix redirection when transport change (#177)
- new: build is now using rebar3
- new: updated root certificates
- fix: ignore comma in set-cookie attributes (#193)
- fix: status line parsing when reason phrase is missing entirely (#190)
- fix: make sure sure the response is done during async streaming (#186)
- fix metrics (#186)
- new: bump latest version of `ssl_verify_hostname` (#175)
- fix: parse server headers
- fix: really honor max redirection (#170)
- fix: handle path parameters in URL (#176)
-

1.1.1 - 2015/03/20
------------------

- fix: fix max redirection (#170)
- fix: don't encode path parameters and unreserved chars. (#176)


1.1.0 - 2015/03/04
------------------

- fix: honor max_redirect.
- fix: socket checkout in the pool: close the socket if something happen while
  passing the control to the client
- fix: put back the waiter in the queue of the pool if no socket can be
  delivered
- fix: make sure we don't release a closed typo
- add: shutdown method to transports
- add: hackney_trace module to trace a request
- add: reuse/new connection metrics
- fix: guard binary in `hackney_multipart:len_mp_stream/2`
- improvement: pass the socket to `hackney:request_info/1`
- dependency: update ssl_verify_hostname
- fix: make sure to pass the Host header to the request
- fix: HTTP basic authentication
- fix content-type case
- improvement: tests

1.0.6 - 2015/01/21
------------------

- improvement: handle {error, closed} for HTTP 1.1 when no content-length is given.
- improvement: handle 204 and 304 status
- fix keep-alive handling
- remove expm package
- build under R18

1.0.5 - 2014/12/12
------------------

- improvement: Do not wait to cancel a request
- improvement: do not control the request premptively

1.0.4 - 2014/12/8
-----------------

- fix client leaks on error
- fix monitor counters

1.0.3 - 2014/12/5
-----------------

- fix SSL validation under R15 and R14 Erlang versions.
- Apply SSL certificate validation to SOCKS5 and HTTP proxies.

1.0.2 - 2014/12/02
------------------

- fix redirection: rewrite Host header

1.0.1 - 2014/12/01
------------------

- update default certification authority file. Make sure we can validate all SSL
  connections even on the AWS platform.
- fix typo

1.0.0 - 2014/11/30
------------------

hackney 1.0.0 has been released. This is the first stable and long term
supported release of hackney.

- add [metrics](https://github.com/benoitc/hackney#metrics)
- add SSL certificate verification by default.
- fix: Pool handling


0.15.2 - 2014/11/27
-------------------

- fix: handle strings in headers
- fix; convert User/Password as string if needed
- fix: handle body given as an empty list

0.15.1 - 2014/11/26
-------------------

- export `find_pool/1` and allows any poolname.

0.15.0 - 2014/11/11
-------------------

- improve hackney performance and concurrency
- fix pool handling: make sure to reuse the connections

0.14.3 - 2014/10/28
-------------------

- fix `hackney:stop_async/1`

0.14.2 - 2014/10/27
-------------------

- fix memory leak (#77): some requests were not cleaned correctly in
  hackney_manager.
- fix ssl race condition (#130)
- fix: check if relative url contains a forward slash
- refactor integration tests and add more tests
- fix socket pool: make sure to close all sockets when the pool is terminated,
  and do not store closed sockets if we know it.

0.14.1 - 2014/09/24
-------------------

- fix redirect location: make sure we use absolute urls
- fix redirection: make sure to follow redirections
- fix hackney_response:read_body/3 spec
- trim response headers when needed
- add redirection basic tests

0.14.0 - 2014/09/18
-------------------

- fix: validate if the redirection url is absolute.
- fix: return location from headers when possible in
  `hackney:location/1`.
- fix HEAD request. Remove the need to call the body method
- fix: remove undefined function references
- tests: start to handle tests with httpbin

### Breaking change:

When doing an HEAD request, the signature of the response when it
succeded is now `{ok, Status, ResponseHeaders}` and do not contains a
client reference any more.


0.13.0 - 2014/07/08
-------------------

- put hackney_lib back in the source code and refactor the source repository
- fix: handle bad socks5 proxy response
  [#113](http://github.com/benoitc/hackney/issues/113)
- fix: handle timeout in hackney_socks4:connect/5
  [#112](http://github.com/benoitc/hackney/issues/112)
- fix: Accept inet6 tcp option for ssl
- fix redirection
- fix: add versions option for ssl

0.12.1 - 2014/04/18
-------------------

- fix: return the full body on closed connections.
- fix: make sure to always pass the Host header.

0.12.0 - 2014/04/18
-------------------

- improvement: URI encoding is now fully normalized.
- improvement: TCP_NODELAY is now available by default for all transports
- improvements: IDNA parsing is only done during the normalization which
  makes all the connections faster..
- fix: connections options are now correctly passed to the transports.
- fix: HTTP proxying. make sure we reuse the connection
- fix: HTTP proxying, only resolve the proxy domain.
- bump [hackney_lib](https://github.com/benoitc/hackney_lib) to 0.3.0

### Breaking change:

the [mimetypes](https://github.com/spawngrid/mimetypes) has been
replaced by the
[hackney_mimetypes](https://github.com/benoitc/hackney_lib/blob/master/doc/hackney_mimetypes.md)
module. It makes content-type detection a little more efficient. In the
process the functions `hackney_util:content_type/1` and
`hackney_bstr:content_type/1` has been removed. You should now use the
function `hackney_mimetypes:filename/1` .



0.11.2 - 2014/04/15
-------------------

- new improved and more performant IDNA support
- make sure the socket is closed when we skip the body if needed
- fix multipart EOF parsing
- make sure we finish a multipart stream
- bump hackney_lib to 0.2.5
- enable TCP_NODELAY by default. (To disable, pass the option
  `{nodelay, false} to `connect_options`.

0.11.1 - 2014/03/03
-------------------

- improvement: speed IDNA domains handing
- fix http proxy via CONNECT
- fix: encode the path
- bump to [hackney_lib 0.2.4](https://github.com/benoitc/hackney_lib/releases/tag/0.2.4)

0.11.0 - 2014/03/02
-------------------

- add `hackney:location/1` to get the final location
- make `hackney_request:send/2` more efficient
- fix socket removing in the pool
- fix [HTTP proxying](https://github.com/benoitc/hackney/commit/a21e8802e1dc91c25d863ac6fc5b23a79196efcd)
- support IDNA hostnames

0.10.1 - 2013/12/30
-------------------

- fix multipart file header
- improve the performance when sending a `{multipart, Parts}` body. Send
  it as a stream.
- bump hackney_lib version to 0.2.2

0.10.0 - 2013/12/29
-------------------

- improve multipart handling: With this change, we can now calculate the
  full multipart stream content-length using `hackney_multipart:len_mp_stream/2` .
- add `hackney:setopts/2` to set options to a request when reusing it.
- add `hackney:send_reques/3` to pass new options to a request.
- add the `{stream_to, Pid}` setting to a request to send the messages
  from an asynchronous response to another PID.
- fix `Host` header: some server do not comply well with the spec and
  fail to parse the port when they are listening on 80 or 443. This
change fix it.
- fix: make sure we are re-using connections with asynchronous
  responses.

### Breaking changes:

- All messages from an async response are now under the
  format `{hackney_response, Ref, ... }` to distinct hackney messages
from others in a process easily.
- You can only make an async response at a time. Ie if you are are doing
  a persistent request (reusing the same reference) you will need to
pass the async option again to the request. For that purpose the
functions hackney:send_request/3 and hackney:setopts/2 have been
added.
- multipart messages have changed. See the documentation for more
  information.

0.9.1 - 2013/12/20
------------------

- fix response multipart processing

0.9.0 - 2013/12/19
------------------

- add support for multipart responses
- add support for cookies: There is now a `cookie`
option that can be passed to the request. It can be a sinle cookie or a
list of cookies. To parse cookies from the response a function `hackney:cookies/1` has
been added. It return all the cookies as a list of [{Key, Value}].
- breaking change: use [hackney_lib](http://github.com/benoitc/hackney_lib)  a web toolkit to handle the HTTP protocol and other manipulations.
- optimization: send body and headers together when it is possible
- fix release handling

0.8.3 - 2013/12/07
------------------

- add: support redirection in async responses
- improve
  [hackney_url:make_url/3](https://github.com/benoitc/hackney/commit/a545d266106c0557374a8b9b13caa63ce89e86f2)
- fix: handle case where the response is already done in async responses

0.8.2 - 2013/12/05
------------------

- fix: trap exits in hackney_manager

0.8.1 - 2013/12/04
------------------

service release with a new feature and some minor improvements

- added the support for [socks5
  proxies](https://github.com/benoitc/hackney#socks5-proxy)
- improvment: integer and atom can now be passed in url params or forms
  values.
- breaking change: differentiate connect/recv timeout, now connect
  timeout return `{error, connect_timeout}`

0.8.0 - 2013/12/02
------------------

major release. With this release the API will not evolve much until the
1.0 release sometimes in january.

- breaking change: hackney now return a reference instead of an opaque record. The
  information is maintained in an ETS table. The same reference is now
used for async response requests.
- breaking change: `stream_body_request/2` and `stream_multipart_request/2` functions has
  been renamed to `send_body/2` and `send_multipart_body/2` .
- breaking change: remove `hackney:close_stream/1` function. You only need to
  use `hackney:close/1` now.
- breaking change: rename `hackney:raw/1` function to
  `hackney:cancel_request/1`.
- breaking change: the hakney pool handler based on dispcount is now
  available in its [own repository](https://github.com/benoitc/hackney_disp) so hackney doe  not depends on dispcount.
- fix: canceling and closing a request now make sure the async response
  process is killed.
- fix: make sure we pass a `Transfer-Encoding: chunked` header when we
  send a body without content-length.
- fix: make sure the client is correcly reconnected when we reuse a
  reference.

0.7.0 - 2013/11/22
------------------

- add new Loadbalance pool handler based on dispcount
- allows to set the pool handler
- breaking change: remove `hackney:start_pool/2` and
  `hackney:stop_pool/1`, use instead `hackney_pool:start_pool/2` and
  `hackney_pool:stop_pool/1`
- breaking change: A pool is now used by default
- breaking change: The `hackney_form` module has been removed. You can
  now encode/parse a form using the functions in the `hackney_url` module.
- deprecate `pool_size` and replace it by `max_connections`
- fix: apply applications defaults to the pool


0.6.1 - 2013/11/21
------------------

- doc: Fix the asynchronous response example in the readme
- add hackney_url:make_url/3, hackney_url:qs/1, hackney_url:parse_qs/1 functions

0.6.0 - 2013/11/21
------------------

- add the possibility to get an asynchronous response
- add support for the "Expect: 100-continue" header
- add hackney:controlling_process/2 to pass the control of an hackney context to another process

0.5.0 - 2013/11/06
------------------

- fix: proxied connections
- fix: correct the path passed to a request
- fix: multipart forms
- fix: Make sure that the controller process of the socket is the pool process when the socket is in the pool
- fix: auth header when when the user is not given

0.4.4 - 2013/08/25
------------------

- fix: doc typos
- fix: dialyzer errors
- fix: add mimetypes to the list of loaded applications
- fix: test.ebin example

0.4.3 - 2013/08/04
------------------

- removed parse_transform, the REST API is now available at the compilation.
fix: fix file upload content type
- doc: fix typos

0.4.2 - 2013/06/10
------------------

- handle `identity` transfert encoding. When the connection close return
  latest buffer.

0.4.1 - 2013/06/10
------------------

- Body can be passed as a
  [function](https://github.com/benoitc/hackney/commit/efd877f52733ccecf0ba1b5ed10783fe29d49b74)
- Add recv_timeout option
- Fix HEAD request (don't stream the body)
- Don't pass the Port to the Host header if it's default (http, https)
- Set the connection timeout
- Make sure sendfile correctly handle chunked encoding
- Add support for partial file uploads
- Return received buffer when no content length is given (http 1.0)
- Instead of returning `{error, closed}`, return `{error, {closed,
  Buffer}}` when you receive the body, so you can figure what happened
and maybe use the partial body.

0.4.0 - 2012/10/26
------------------

- Allows to stream a multipart request
- Add `insecure` option to connect via ssl without verifying an SSL
  certificate
- Handle empty headers values
- Add `force_redirect` option
- Add expm support
- Fix body streaming
- Fix SSL handling
- Fix hackney:request/3 (no more loop)


0.3.0 - 2012/09/26
------------------

- Add Multipart support
- Add HTTP Proxy tunneling support
- Fix Chuncked Response decoding

0.2.0 - 2012/07/18
------------------

- Allows the user to use a custom function to stream the body
- Add the possibility to send chunked requests
- Add an option to automatically follow a redirection
- Allows the user to force hackney to use the default pool

0.1.0 - 2012/07/16
------------------

- initial release
