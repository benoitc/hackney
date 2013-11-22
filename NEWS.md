# NEWS

0.7.0 - current

- add new Loadbalance pool handler based on dispcount
- allows to set the pool handler
- breaking change: remove `hackney:start_pool/2` and
  `hackney:stop_pool/1`, use instead `hackney_pool:start_pool/2` and
  `hackney_pool:stop_pool/1`
- breaking change: A pool is now used by default

0.6.1 - 2013/11/21

- doc: Fix the asynchronous response example in the readme
- add d hackney_url:make_url/3, hackney_url:qs/1, hackney_url:parse_qs/1 functions

0.6.0 - 2013/11/21

- add the possibility to get an asynchronous response
- add support for the "Expect: 100-continue" header
- add hackney:controlling_process/2 to pass the control of an hackney context to another process

0.5.0 - 2013/11/06

- fix: proxied connections
- fix: correct the path passed to a request
- fix: multipart forms
- fix: Make sure that the controller process of the socket is the pool process when the socket is in the pool
- fix: auth header when when the user is not given

0.4.4 - 2013/08/25

- fix: doc typos
- fix: dialyzer errors
- fix: add mimetypes to the list of loaded applications
- fix: test.ebin example

0.4.3 - 2013/08/04

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
