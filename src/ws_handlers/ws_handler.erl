-module(ws_handler).
-include_lib("kernel/include/logger.hrl").

-export([init/2, websocket_init/1]).
-export([websocket_handle/2, websocket_info/2, websocket_terminate/3]).

init(Req, State) ->
    Opts = #{idle_timeout => 360000},
    {cowboy_websocket, Req, State, Opts}.

websocket_init(_TransportName) ->
    api:init(),
    {ok, undefined}.

websocket_handle({text, Msg}, State) ->
    logger:info("Received message: ~p~n", [Msg]),
    JsonDataDecoded = jiffy:decode(Msg),
    handle_command(JsonDataDecoded, State);


websocket_handle(_Data, State) ->
    {ok, State}.

%command = {"jsonrpc":"2.0", "method":"get_user", "params":{"UserId":"usertest"}, "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"get_user">>}, 
                 {<<"params">>, {[{<<"UserId">>, UserId}]}},
                 {<<"id">>, Id}]}, State) ->
    UserIdStr = binary_to_list(UserId),
    Response = api:get_user(binary_to_list(UserId)),
    [{_Type, _, Counter, Value}] = Response,
    Result = {[{<<"UserId">>, UserId},{<<"Counter">>, list_to_binary(Counter)}, {<<"Value">>, Value}]},
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, Result}, {<<"id">>, Id}]}),
    {reply, {text, DataJson}, State};

%command = {"jsonrpc":"2.0", "method":"get_users", "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"get_users">>}, 
                 {<<"id">>, Id}]}, State) ->
    {atomic, Response} = api:get_users(),
    Result = lists:map(fun(User) -> 
        {[{<<"UserId">>, element(2, User)}, 
          {<<"Counter">>, element(3, User)}, 
          {<<"Value">>, element(4, User)}]} 
    end, Response),
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, Result}, {<<"id">>, Id}]}),
    {[{text, DataJson}], State};

%command = {"jsonrpc":"2.0", "method":"list_users", "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"list_users">>}, 
                 {<<"id">>, Id}]}, State) ->
    Users = api:list_users(),
    Result = lists:map(fun(User) -> 
        {[{<<"UserId">>, User}]} 
    end, Users),
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, Result}, {<<"id">>, Id}]}),
    {[{text, DataJson}], State};

%command = {"jsonrpc":"2.0", "method":"post_user", "params":{"UserId":"usertest", "Counter":"C2", "Value":15}, "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"post_user">>}, 
                 {<<"params">>, {[{<<"UserId">>, UserId}, {<<"Counter">>, Counter}, {<<"Value">>, Value}]}},
                 {<<"id">>, Id}]}, State) ->
    api:post_user(binary_to_list(UserId), Counter, Value),
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, <<"User created">>}, {<<"id">>, Id}]}),
    {[{text, DataJson}], State};

%command = {"jsonrpc":"2.0", "method":"delete_user", "params":{"UserId":"user1"}, "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"delete_user">>}, 
                 {<<"params">>, {[{<<"UserId">>, UserId}]}},
                 {<<"id">>, Id}]}, State) ->
    api:delete_user(binary_to_list(UserId)),
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, <<"User deleted">>}, {<<"id">>, Id}]}),
    {[{text, DataJson}], State};

%command = {"jsonrpc":"2.0", "method":"put_user", "params":{"UserId":"usertest", "Value":1}, "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"put_user">>}, 
                 {<<"params">>, {[{<<"UserId">>, UserId}, {<<"Value">>, Value}]}},
                 {<<"id">>, Id}]}, State) ->
    api:put_user(binary_to_list(UserId), [{<<"UserId">>, UserId}, {<<"Value">>, Value}]),
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, <<"User updated">>}, {<<"id">>, Id}]}),
    {[{text, DataJson}], State};

%command = {"jsonrpc":"2.0", "method":"put_user", "params":{"UserId":"usertest", "Counter":"C12"}, "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"put_user">>}, 
                 {<<"params">>, {[{<<"UserId">>, UserId}, {<<"Counter">>, Counter}]}},
                 {<<"id">>, Id}]}, State) ->
    api:put_user_user(binary_to_list(UserId), [{<<"UserId">>, UserId}, {<<"Counter">>, Counter}]),
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, <<"User updated">>}, {<<"id">>, Id}]}),
    {[{text, DataJson}], State};

%command = {"jsonrpc":"2.0", "method":"put_user", "params":{"UserId":"usertest", "Counter":"C12", "Value":1}, "id":1}
handle_command({[{<<"jsonrpc">>, <<"2.0">>}, 
                 {<<"method">>, <<"put_user">>}, 
                 {<<"params">>, {[{<<"UserId">>, UserId}, {<<"Counter">>, Counter}, {<<"Value">>, Value}]}},
                 {<<"id">>, Id}]}, State) ->
    api:put_user(binary_to_list(UserId), [{<<"UserId">>, UserId}, {<<"Counter">>, Counter}, {<<"Value">>, Value}]),
    DataJson = jiffy:encode({[{<<"jsonrpc">>, <<"2.0">>}, {<<"result">>, <<"User updated">>}, {<<"id">>, Id}]}),
    {[{text, DataJson}], State};

handle_command(_, State) ->
    io:format("Unknown command~n"),
    {ok, State}.

websocket_info({text, _Msg} = Frame, State)->
     {[Frame],State};

websocket_info(_Info, State)->
     {ok,State}.

websocket_terminate(_Reason,_Req,_State)->
     io:format("Terminating websocket connection~n~p", [_Reason]),
     ok.
