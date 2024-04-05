-module(post_put_handler).
-export([init/2, allowed_methods/2, content_types_accepted/2, post_to_json/2, put_to_json/2, from_json/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    api:init(),
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"POST">> -> 
            logger:info("POST request received."),
            post_to_json(Req, State);
        <<"PUT">> -> 
            logger:info("PUT request received."),
            put_to_json(Req, State);
        _ -> {halt, Req, State}
    end.

%POST -H "Content-Type: application/json" -d '{"UserId":"user123", "Counter":"C1", "Value":10}' "http://localhost:8080/user"
post_to_json(Req, State) ->
    case cowboy_req:has_body(Req) of
        false ->
            logger:info("Missing body."),
            cowboy_req:reply(400, #{}, <<"Missing body.">>, Req);
        true ->

            {ok, Body, Req1} = cowboy_req:read_body(Req),
            Data = jiffy:decode(Body, [return_maps]),
            #{<<"UserId">> := UserId, <<"Counter">> := Counter, <<"Value">> := Value} = Data,
            case api:get_user(binary_to_list(UserId)) of
                [] ->
                    logger:info("Attempting to post user: ~p, ~p, ~p", [binary_to_list(UserId), Counter, Value]),
                    api:post_user(binary_to_list(UserId), Counter, Value),
                    {true, Req1, State};
                [_User] ->
                    logger:info("User with ID ~p already exists.", [UserId]),
                    cowboy_req:reply(409, #{}, <<"User already exists.">>, Req)
                end
        end.

%curl -X PUT -H "Content-Type: application/json" -d '{"UserId": "user123","Counter":"C2" ,"Value": 40}' "http://localhost:8080/user"
put_to_json(Req, State) ->

    case cowboy_req:has_body(Req) of
        false ->
            logger:info("Missing body."),
            cowboy_req:reply(400, #{}, <<"Missing body.">>, Req);
        true ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            Data = jiffy:decode(Body, [return_maps]),
            Proplist = maps:to_list(Data),
            UserId = proplists:get_value(<<"UserId">>, Proplist),
            case api:get_user(binary_to_list(UserId)) of
                [] ->
                    logger:info("User with ID ~p not found.", [UserId]),
                    cowboy_req:reply(404, #{}, <<"User not found.">>, Req);
                [_User] ->
                    logger:info("Attempting to put user: ~p, ~p", [UserId, Proplist]),
                    api:put_user(binary_to_list(UserId), Proplist),
                    {true, Req1, State}
            end
    end.