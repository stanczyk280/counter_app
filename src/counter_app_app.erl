%%%-------------------------------------------------------------------
%% @doc counter_app public API
%% @end
%%%-------------------------------------------------------------------

-module(counter_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = cowboy_router:compile([
        {'_', [
            {"/router", router_handler, []},
            {"/users/:id", get_handler, []},
            {"/users", post_put_handler, []},
            {"/ws", ws_handler, []}
        ]}
    ]),
    cowboy:start_clear(websockets_cowboy_listener, [{port, 8080}], #{ env => #{ dispatch => Routes }}),
    counter_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
