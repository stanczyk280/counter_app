-module(post_put_handler).
-export([init/2, allowed_methods/2, content_types_accepted/2, post_to_json/2, put_to_json/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    api:init(),
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, post_to_json}, {<<"application/json">>, put_to_json}], Req, State}.

post_to_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Data = jiffy:decode(Body, [return_maps]),
    logger:debug("Data: ~p", [Data]),
    {true, Req1, State}.

put_to_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Data = jiffy:decode(Body, [return_maps]),
    logger:debug("Data: ~p", [Data]),
    {true, Req1, State}.