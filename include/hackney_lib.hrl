-record(hackney_url, {
 transport        :: atom(),
 scheme           :: atom() | binary(),
 netloc   = <<>>  :: binary(),
 raw_path         :: binary() | undefined,
 path = <<>>      :: binary() | undefined | nil,
 qs = <<>>        :: binary(),
 fragment = <<>>  :: binary(),
 host = ""        :: string(),
 port             :: integer() | undefined,
 user = <<>>      :: binary(),
 password = <<>>  :: binary()
}).

-type hackney_url() :: #hackney_url{}.

-record(hparser, {
 type = auto            :: atom(),
 max_line_length = 4096 :: integer(),
 max_empty_lines = 10   :: integer(),
 max_header_size = 1048576 :: non_neg_integer() | infinity,
 max_headers = 1000     :: non_neg_integer() | infinity,
 header_size = 0        :: non_neg_integer(),
 header_count = 0       :: non_neg_integer(),
 scan_from = 0          :: non_neg_integer(),
 empty_lines = 0        :: integer(),
 state = on_first_line  :: atom(),
 buffer = <<>>          :: binary(),
 version                :: {integer(), integer()} | undefined,
 method = <<>>          :: binary(),
 partial_headers = []   :: list(),
 clen = undefined        :: integer() | undefined | bad_int,
 te = <<>>              :: binary(),
 connection = <<>>      :: binary(),
 ctype = <<>>           :: binary(),
 location = <<>>        :: binary(),
 content_encoding = <<>> :: binary(),
 body_state = waiting   :: atom() | tuple()
}).
