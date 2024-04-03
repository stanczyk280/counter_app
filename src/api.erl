-module(api).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/0, post_user/3, get_user/1, get_users/0, list_users/0, put_user/2, delete_user/1, parse_update_fields/1]).

-record(user_data, {
    user_id :: any(),
    counter :: any(),
    value :: integer()
}).

init() ->
    logger:set_primary_config(level, debug),
    application:set_env(mnesia, dir, "../db_counter"),
    case catch mnesia:create_schema([node()]) of
        {error,{nonode@nohost,{already_exists,nonode@nohost}}} -> 
            logger:info("Node already exists, starting without creating.")
    end,

    mnesia:start(),

    case catch mnesia:create_table(user_data, [{disc_copies, [node()]}, {attributes, record_info(fields, user_data)}]) of
         {'aborted',{already_exists,user_data}} -> 
             logger:info("Table already exists, starting without creating.");
         {'aborted',Error} -> 
             logger:error("Failed to create table: ~p", [Error])
    end.

post_user(UserId, Counter, Value) ->
    Record = #user_data{
        user_id = UserId,
        counter = binary_to_list(Counter),
        value = Value
        },
        logger:info("Attempting to create user with ID ~p, Counter ~p and Value ~p.", [UserId, binary_to_list(Counter), Value]),
        mnesia:dirty_write(Record),
        logger:info("User created: UserId: ~p, Counter: ~p, Value: ~p", [UserId, binary_to_list(Counter), Value]).

get_user(UserId) ->
    logger:info("Attempting to find user with ID: ~p", [UserId]),
    Result = mnesia:dirty_read(user_data, UserId),
    logger:info("User found: ~p", [Result]),
    Result.

get_users() -> 
    Query = fun() ->
        logger:info("Attempting to get all users"),
        mnesia:select(user_data, ets:fun2ms(fun(#user_data{} = Record) -> Record end))
        end,
    logger:info("Users found"),
    {atomic, _Results} = mnesia:transaction(Query).

list_users() -> 
    mnesia:dirty_all_keys(user_data).

put_user(UserId, DataToUpdate) ->
    case get_user(UserId) of
        [] -> 
            logger:info("User not found: ~p", [UserId]);
        [User] -> 
            logger:info("Attempting to parse fields for update"),
            UpdateFields = parse_update_fields(DataToUpdate),
            case UpdateFields of
                {ok, Counter, Value} ->
                    Query = fun() ->
                        {user_data, _, OldCounter, OldValue} = User,
                        NewCounter = case Counter of
                            undefined -> OldCounter;
                            _ -> binary_to_list(Counter)
                        end,
                        NewValue = case Value of
                            undefined -> OldValue;
                            _ -> Value
                        end,
                        logger:info("Attempting to update user with ID ~p, Counter ~p and Value ~p", [UserId, NewCounter, NewValue]),
                        mnesia:dirty_write({user_data, UserId, NewCounter, NewValue})
                    end,
                    case mnesia:transaction(Query) of
                        {atomic, ok} -> 
                            logger:info("User data updated successfully");
                        {aborted, Error} -> 
                            logger:error("Failed to put user: ~p", [Error])
                    end;
                {aborted, Error} -> logger:error("Failed to put user: ~p", [Error])
            end
    end.

delete_user(UserId) ->
    case get_user(UserId) of
        [] ->
             logger:info("User not found: ~p", [UserId]);
        _User ->
            mnesia:dirty_delete({user_data, UserId}),
            logger:info("User of id: ~p, deleted", [UserId])
        end.

%======HELPERS=======%
parse_update_fields(DataToUpdate) ->
    Counter = proplists:get_value(<<"Counter">>, DataToUpdate, undefined),
    Value = proplists:get_value(<<"Value">>, DataToUpdate, undefined),
    case {Counter, Value} of
        {undefined, undefined} ->
            logger:info("No fields provided for update"),
            {error, "No fields provided for update"};
        _ ->
            logger:info("Fields parsed successfully"),
            {ok, Counter, Value}
    end.

