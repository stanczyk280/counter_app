-module(ws_handler).

-export([init/2, websocket_init/1]).
-export([websocket_handle/2, websocket_info/2, websocket_terminate/3]).

init(Req, State) ->
    Opts = #{idle_timeout => 60000},
    {cowboy_websocket, Req, State, Opts}.

websocket_init(_TransportName) ->
    api:init(),
    {ok, undefined}.

websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~p~n", [Msg]),
    DataDecoded = jiffy:decode(Msg),
    handle_command(DataDecoded, State);

websocket_handle(_Data, State) ->
    {ok, State}.

%command = {"command":"get_user", "UserId":"user1"}
handle_command({[{<<"command">>, <<"get_user">>}, 
                 {<<"UserId">>, UserId}]}, State) ->
    UserIdStr = binary_to_list(UserId),
    io:format("Finding user with ID: ~p~n", [UserIdStr]),
    {atomic, Response} = api:get_user(UserIdStr),
    io:format("Response: ~p~n", [Response]),
    [{_Type, _, Counter, Value}] = Response,
    DataJson = jiffy:encode({[{<<"UserId">>, UserIdStr},{<<"Counter">>, Counter}, {<<"Value">>, Value}]}),
    {[{text, DataJson}], State};

handle_command({[{<<"command">>, <<"get_users">>}]},State) ->
    io:format("Finding all users"),
    {atomic, Response} = api:get_users(),
    io:format("~n~p",[Response]),
    {ok, State};

%%TO DO%%
%rewrite rest of the methods from the old ws handler

handle_command(_, State) ->
    io:format("Unknown command~n"),
    {ok, State}.


websocket_info({text, _Msg} = Frame, State)->
     {[Frame],State};

websocket_info(_Info, State)->
     {ok,State}.

websocket_terminate(_Reason,_Req,_State)->
     ok.
