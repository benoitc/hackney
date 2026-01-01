%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(upload_resource).

-export([init/2]).

init(Req, State) ->
    {Terms, Req2} = from_multipart(Req, []),
    Bin = term_to_binary(Terms),
    Req3 = cowboy_req:reply(200, #{<<"Content-Type">> => <<"application/x-erlang-term">>}, Bin, Req2),
    {ok, Req3, State}.

from_multipart(Req, Bodies) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            {Req4, N, B} = case cow_multipart:form_data(Headers) of
              {data, FieldName} ->
                  {ok, Body, Req3} = cowboy_req:read_part_body(Req2),
                  {Req3, FieldName, Body}
            end,
            from_multipart(Req4, [{N, B} | Bodies]);
        {done, Req2} ->
            {lists:reverse(Bodies), Req2}
    end.
