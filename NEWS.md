# NEWS

0.14.1 - unreleased

- fix redirection: make sure to follow redirections

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
- Return received buffer when no content lenght is given (http 1.0)
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
