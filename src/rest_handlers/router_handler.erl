-module(router_handler).
-include_lib("kernel/include/logger.hrl").

-export([init/2]).

init(Req0, Opts) ->
    api:init(),
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0),
    {cowboy_rest, Req, Opts}.

handle_request(<<"GET">>, Req) ->
    #{user_id := UserId} = cowboy_req:match_qs([{user_id, [], undefined}], Req),
    case UserId of
    undefined ->
        logger:info("Missing user_id parameter."),
        cowboy_req:reply(400, #{}, <<"Missing user_id parameter.">>, Req);
    _ ->
        Response = api:get_user(binary_to_list(UserId)),
        case Response of
            [] ->
                logger:info("User with ID ~p not found.", [UserId]),
                cowboy_req:reply(404, #{}, <<"User not found.">>, Req);
            _ -> 
                [{_Type, _, Counter, Value}] = Response,
            DataJson = jiffy:encode({[{<<"UserId">>, UserId}, 
                                    {<<"Counter">>, list_to_binary(Counter)}, 
                                    {<<"Value">>, Value}]}),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, DataJson, Req)
        end
    end;

handle_request(<<"POST">>, Req0) ->
    case cowboy_req:has_body(Req0) of
    false ->
        logger:info("Missing body."),
        cowboy_req:reply(400, #{}, <<"Missing body.">>, Req0);
    true -> 
        {ok, Body, Req} = cowboy_req:read_urlencoded_body(Req0),
        [{DataBin, _}] = Body,
        DecodedData = jiffy:decode(DataBin),
        {[ {<<"UserId">>, UserIdBin},{<<"Counter">>, CounterBin},{<<"Value">>, Value}]} = DecodedData,
        UserId = binary_to_list(UserIdBin),
        Counter = binary_to_list(CounterBin),
        api:post_user(UserId, Counter, Value),
        cowboy_req:reply(201,#{},<<"User created">> ,Req)
    end;

handle_request(<<"PUT">>, Req0) ->
    case cowboy_req:has_body(Req0) of
    false ->
        logger:info("Missing body."),
        cowboy_req:reply(400, #{}, <<"Missing body.">>, Req0);
    true -> 
        {ok, Body, Req} = cowboy_req:read_urlencoded_body(Req0),
        [{DataBin, _}] = Body,
        {DecodedData} = jiffy:decode(DataBin),
        UserId = proplists:get_value(<<"UserId">>, DecodedData),
        logger:info("Attempting to update user with ID ~p and Data: ~p.", [UserId, DecodedData]),
        api:put_user(binary_to_list(UserId), DecodedData),
        logger:info("User with ID ~p updated.", [UserId]),
        cowboy_req:reply(201,#{},<<"User updated">> ,Req)
    end;

handle_request(<<"DELETE">>, Req) ->
    #{user_id := UserId} = cowboy_req:match_qs([{user_id, [], undefined}], Req),
    case UserId of
    undefined ->
        logger:info("Missing user_id parameter."),
        cowboy_req:reply(400, #{}, <<"Missing user_id parameter.">>, Req);
    _ ->
        Response= api:get_user(binary_to_list(UserId)),
        case Response of
            [] ->
                logger:info("User with ID ~p not found.", [UserId]),
                cowboy_req:reply(404, #{}, <<"User not found.">>, Req);
            _ -> 
                api:delete_user(binary_to_list(UserId)),
                cowboy_req:reply(200, #{}, <<"User deleted.">>, Req)
        end
    end;
handle_request(_,Req) ->
  cowboy_req:reply(405, Req).

