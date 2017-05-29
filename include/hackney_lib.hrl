-record(hackney_url, {
  transport        :: atom() | undefined,
     scheme        :: atom() | undefined,
     netloc        :: binary() | undefined,
   raw_path        :: binary() | undefined,
       path = <<>> :: binary() | undefined | nil,
         qs = <<>> :: binary() | undefined,
   fragment = <<>> :: binary() | undefined,
       host        :: string() | undefined,
       port        :: integer() | undefined,
       user = <<>> :: binary() | undefined,
   password = <<>> :: binary() | undefined
}).

-type hackney_url() :: #hackney_url{}.

-record(hparser, {
             type = auto          :: atom(),
  max_line_length = 4096          :: integer(),
  max_empty_lines = 10            :: integer(),
      empty_lines = 0             :: integer(),
            state = on_first_line :: atom(),
           buffer = <<>>          :: binary(),
          version                 :: binary(),
           method                 :: binary(),
  partial_headers = []            :: list(),
             clen                 :: integer(),
               te                 :: binary(),
       connection                 :: binary(),
            ctype                 :: binary(),
         location                 :: binary(),
       body_state = waiting       :: atom()
}).
