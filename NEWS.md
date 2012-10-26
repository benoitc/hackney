# NEWS

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
