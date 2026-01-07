%% @doc Generic test HTTP resource for hackney tests.
%% Replaces dependency on external httpbin server.
%%
%% Supported routes:
%%   GET  /get              - return request info as JSON
%%   POST /post             - echo request info with body as JSON
%%   HEAD /*                - return headers only
%%   GET  /status/:code     - return specific status code
%%   GET  /redirect-to      - redirect to ?url= (optional ?status_code=)
%%   GET  /basic-auth/:u/:p - require basic auth
%%   GET  /cookies/set      - set cookies from query params
%%   GET  /cookies          - return cookies as JSON
%%   GET  /robots.txt       - return fixed robots.txt content

-module(test_http_resource).

-export([init/2]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    handle_request(Method, Path, Req, State).

%% GET /get - return request info as JSON
handle_request(<<"GET">>, <<"/get">>, Req, State) ->
    reply_json(200, request_info(Req), Req, State);

%% POST /post - echo request info with body
handle_request(<<"POST">>, <<"/post">>, Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Info = request_info(Req),
    Info2 = Info#{<<"data">> => Body},
    reply_json(200, Info2, Req, State);

%% HEAD requests - return headers only (no body)
handle_request(<<"HEAD">>, _Path, Req, State) ->
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Req),
    {ok, Req2, State};

%% GET /status/:code - return specific status code
handle_request(<<"GET">>, <<"/status/", CodeBin/binary>>, Req, State) ->
    Code = binary_to_integer(CodeBin),
    %% For 204 and 304, no body should be sent
    case Code of
        204 ->
            Req2 = cowboy_req:reply(204, #{}, Req),
            {ok, Req2, State};
        304 ->
            Req2 = cowboy_req:reply(304, #{}, Req),
            {ok, Req2, State};
        _ ->
            reply_json(Code, #{<<"status">> => Code}, Req, State)
    end;

%% GET /redirect-to?url=X&status_code=Y - redirect to URL
handle_request(<<"GET">>, <<"/redirect-to">>, Req, State) ->
    QS = cowboy_req:parse_qs(Req),
    Url = proplists:get_value(<<"url">>, QS, <<"/">>),
    StatusCode = case proplists:get_value(<<"status_code">>, QS) of
        undefined -> 302;
        SC -> binary_to_integer(SC)
    end,
    Req2 = cowboy_req:reply(StatusCode, #{
        <<"location">> => Url
    }, Req),
    {ok, Req2, State};

%% GET /basic-auth/:user/:pass - require basic auth
handle_request(<<"GET">>, <<"/basic-auth/", Rest/binary>>, Req, State) ->
    [ExpectedUser, ExpectedPass] = binary:split(Rest, <<"/">>),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User, Pass} when User =:= ExpectedUser, Pass =:= ExpectedPass ->
            reply_json(200, #{
                <<"authenticated">> => true,
                <<"user">> => User
            }, Req, State);
        _ ->
            Req2 = cowboy_req:reply(401, #{
                <<"www-authenticate">> => <<"Basic realm=\"Fake Realm\"">>
            }, <<"{\"authenticated\": false}">>, Req),
            {ok, Req2, State}
    end;

%% GET /cookies/set?k=v - set cookies
handle_request(<<"GET">>, <<"/cookies/set">>, Req, State) ->
    QS = cowboy_req:parse_qs(Req),
    %% Set each query param as a cookie
    Req2 = lists:foldl(fun({Name, Value}, ReqAcc) ->
        cowboy_req:set_resp_cookie(Name, Value, ReqAcc, #{path => <<"/">>})
    end, Req, QS),
    Req3 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, <<"{\"cookies\": \"set\"}">>, Req2),
    {ok, Req3, State};

%% GET /cookies - return cookies as JSON
handle_request(<<"GET">>, <<"/cookies">>, Req, State) ->
    Cookies = cowboy_req:parse_cookies(Req),
    CookieMap = maps:from_list(Cookies),
    reply_json(200, #{<<"cookies">> => CookieMap}, Req, State);

%% GET /robots.txt - fixed content
handle_request(<<"GET">>, <<"/robots.txt">>, Req, State) ->
    Body = <<"User-agent: *\nDisallow: /deny\n">>,
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, Body, Req),
    {ok, Req2, State};

%% GET /connection-close - return with Connection: close header
handle_request(<<"GET">>, <<"/connection-close">>, Req, State) ->
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>,
        <<"connection">> => <<"close">>
    }, <<"{\"connection\": \"close\"}">>, Req),
    {ok, Req2, State};

%% Fallback - return 404
handle_request(_Method, _Path, Req, State) ->
    Req2 = cowboy_req:reply(404, #{
        <<"content-type">> => <<"application/json">>
    }, <<"{\"error\": \"not found\"}">>, Req),
    {ok, Req2, State}.

%% Helper: build request info as a map
request_info(Req) ->
    Headers = cowboy_req:headers(Req),
    #{
        <<"headers">> => Headers,
        <<"method">> => cowboy_req:method(Req),
        <<"path">> => cowboy_req:path(Req),
        <<"url">> => cowboy_req:uri(Req)
    }.

%% Helper: reply with JSON (accepts map)
reply_json(Status, Data, Req, State) when is_map(Data) ->
    Body = jsx:encode(Data),
    Req2 = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req),
    {ok, Req2, State}.
