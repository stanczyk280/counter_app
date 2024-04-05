-module(get_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([get_to_html/2]).
-export([get_to_json/2]).


init(Req, Opts) ->
    api:init(),
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, get_to_html},
        {<<"application/json">>, get_to_json},
        {<<"text/plain">>, get_to_text}
        ], Req, State}.

%curl -X GET -H "Content-Type: text/html" "http://localhost:8080/users/usertest"
get_to_html(Req, State) ->
    UserId = cowboy_req:binding(id, Req),
    logger:debug("User ID: ~p", [UserId]),
    Response = api:get_user(binary_to_list(UserId)),
    logger:debug("Response: ~p", [Response]),
    case Response of
        [] ->
            cowboy_req:reply(404, #{}, <<"User not found.">>, Req);
        [{_Type, _, Counter, Value}] ->
            Body = io_lib:format("<html><body><p>User ID: ~s</p><p>Counter: ~p</p><p>Value: ~p</p></body></html>", [binary_to_list(UserId), Counter, Value]),
            {Body, Req, State}
    end.

%curl -X GET -H "Accept: application/json" "http://localhost:8080/users/usertest"
get_to_json(Req, State) ->
    UserId = cowboy_req:binding(id, Req),
    logger:debug("User ID: ~p", [UserId]),
    Response = api:get_user(binary_to_list(UserId)),
    logger:debug("Response: ~p", [Response]),
    case Response of
        [] ->
            cowboy_req:reply(404, #{}, <<"User not found.">>, Req);
        [{_Type, _, Counter, Value}] ->
            DataJson = jiffy:encode({[{<<"UserId">>, UserId}, {<<"Counter">>, list_to_binary(Counter)}, {<<"Value">>, Value}]}),
            {DataJson, Req, State}
    end.