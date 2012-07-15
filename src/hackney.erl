-module(hackney).

-export([connect/3,
         connect/4,
         close/1,
         setopts/2,
         request/2, request/3, request/4, request/6]).

-include("hackney.hrl").

-define(USER_AGENT, <<"hackney/0.1">>).

connect(Transport, Host, Port) ->
    connect(Transport, Host, Port, #client{options=[]}).

connect(_Transport, _Host, _Port, #client{state=connected}=Client) ->
    {ok, Client};
connect(Transport, Host, Port, #client{options=Options0,
                                       socket=Skt0}=Client)
        when is_list(Host), is_integer(Port), Skt0 =:= nil ->

    %% handle ipv6
    Options = case hackney_util:is_ipv6(Host) of
        true ->
            [inet6 | Options0];
        false ->
            Options0
    end,

    case Transport:connect(Host, Port, Options) of
        {ok, Skt} ->
            {ok, Client#client{transport=Transport,
                               socket=Skt,
                               state = connected}};
        Error ->
            Error
    end;
connect(Transport, Host, Port, Options) when is_list(Options) ->
    connect(Transport, Host, Port, #client{options=Options}).

close(#client{transport=Transport, socket=Skt}=Client) ->
    Transport:close(Skt),
    Client#client{state = closed, socket=nil}.

setopts(#client{transport=Transport, socket=Skt}, Options) ->
    Transport:setopts(Skt, Options).

request(Method, URL) ->
    request(Method, URL, [], <<>>, [], #client{}).

request(Method, URL, Client) ->
    request(Method, URL, [], <<>>, [], Client).

request(Method, URL, Headers, Client) ->
    request(Method, URL, Headers, <<>>, [], Client).

request(Method, #hackney_url{}=URL, Headers, Body, Options, Client) ->
    io:format("method ~p~n, url:~p~n", [Method, URL]),

    #hackney_url{transport=Transport,
                 host = Host,
                 port = Port} = URL,

    case connect(Transport, Host, Port, Client) of
        {ok, Client1} ->
            perform(Method, URL, Headers, Body, Options, Client1);
        Error ->
            Error
    end;
request(Method, URL, Headers, Body, Options, Client)
        when is_binary(URL) orelse is_list(URL) ->
    request(Method, hackney_url:parse_url(URL), Headers, Body, Options,
            Client).



%% @private

perform(Method0, URL, Headers0, Body0, _Options, Client) ->
    #hackney_url{host =Host,
                 path = Path,
                 user = User,
                 password = Pwd} = URL,

    Method = hackney_util:to_upper(hackney_util:to_binary(Method0)),

    %% make header dict
    DefaultHeaders0 = [{<<"Host">>, list_to_binary(Host)},
                      {<<"User-Agent">>, ?USER_AGENT}],

    %% basic authorization handling
    DefaultHeaders = case User of
        nil ->
            DefaultHeaders0;
        _ ->
            Credentials = base64:encode(<< User/binary, ":", Pwd/binary >>),
            DefaultHeaders0 ++ [{<<"Authorization">>, Credentials}]
    end,

    HeadersDict = hackney_headers:update(hackney_headers:new(DefaultHeaders),
                                         Headers0),
    %% build headers with the body.
    {HeaderDict1, Body} = case Body0 of
        <<>> when Method =:= <<"POST">> orelse Method =:= <<"PUT">> ->
            hackney_request:handle_body(HeadersDict, Body0);
        <<>> ->
            {HeadersDict, Body0};
        _ ->
            hackney_request:handle_body(HeadersDict, Body0)
    end,

    HeadersLines = hackney_headers:fold(fun({K, V}, Lines) ->
                    V1 = hackney_util:to_binary(V),
                    [ << K/binary, ": ", V1/binary, "\r\n" >> | Lines]
            end, [], HeaderDict1),

    HeadersData = iolist_to_binary([
                << Method/binary, " ", Path/binary, " HTTP/1.1", "\r\n" >>,
                HeadersLines,
                <<"\r\n">>]),

    %% send headers data
    case hackney_request:send(Client, HeadersData) of
        ok ->
            %% send body
            Result = hackney_request:stream_body(Body, Client),

            case Result of
                {error, _Reason}=E ->
                    E;
                _ ->
                    hackney_response:init(Client)
            end;
        Error ->
            Error
    end.
