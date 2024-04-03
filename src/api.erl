-module(api).
-export([init/0, post_user/3, get_user/1, get_users/0, list_users/0, put_user/2, delete_user/1]).

-record(user_data, {
    user_id :: any(),
    counter :: any(),
    value :: integer()
}).

init() ->
    application:set_env(mnesia, dir, "../db_counter"),
    mnesia:create_schema([node()]),
    mnesia:start(),

    case catch mnesia:create_table(user_data, [{disc_copies, [node()]}, {attributes, record_info(fields, user_data)}]) of
         {'aborted',{already_exists,user_data}} -> "Table already exists, starting without creating.";
         {'aborted',Error} -> {error, Error};
    end.

post_user(UserId, Counter, Value) ->
    Record = #user_data{
        user_id = UserId,
        counter = Counter,
        value = Value
        },

    mnesia:transaction(
        fun() ->
            mnesia:dirty_write(Record)
        end
    ).

get_user(UserId) ->
    Query = fun() -> 
        mnesia:dirty_match_object({user_data, UserId, '_', '_'}) 
        end,
    {atomic, _Results} = mnesia:transaction(Query).

get_users() -> 
     Query = fun() ->
        mnesia:dirty_match_object({user_data,'_','_','_'})
        end,
    {atomic, _Results} = mnesia:transaction(Query).

list_users() -> 
    mnesia:dirty_all_keys(user_data).

put_user(UserId, DataToUpdate) ->
    case get_user(UserId) of
        {atomic, []} -> 
            {error, user_not_found};
        {atomic, [User]} -> 
            UpdateFields = parse_update_fields(DataToUpdate),
            case UpdateFields of
                {ok, Counter, Value} ->
                    Query = fun() ->
                        {user_data, _, OldCounter, OldValue} = User,
                        NewCounter = case Counter of
                            undefined -> OldCounter;
                            _ -> Counter
                        end,
                        NewValue = case Value of
                            undefined -> OldValue;
                            _ -> Value
                        end,
                        mnesia:dirty_write({user_data, UserId, NewCounter, NewValue})
                    end,
                    case mnesia:transaction(Query) of
                        {atomic, ok} -> {ok, "User data updated successfully"};
                        {aborted, Reason} -> {error, Reason}
                    end;
                {error, Reason} -> {error, Reason}
            end
    end.

delete_user(UserId) ->
    case get_user(UserId) of
        {atomic,[]} -> {
            error, user_not_found};
        {atomic, _User} ->
            Query = fun() ->
                mnesia:dirty_delete({user_data, UserId})
                end,
            {atomic, _Results} = mnesia:transaction(Query)
        end.

%======HELPERS=======%
parse_update_fields(DataToUpdate) ->
    Counter = proplists:get_value(<<"Counter">>, DataToUpdate, undefined),
    Value = proplists:get_value(<<"Value">>, DataToUpdate, undefined),
    case {Counter, Value} of
        {undefined, undefined} ->
            {error, "No fields provided for update"};
        _ ->
            {ok, Counter, Value}
    end.

