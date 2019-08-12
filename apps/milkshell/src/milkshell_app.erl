%%%-------------------------------------------------------------------
%% @doc milkshell public API
%% @end
%%%-------------------------------------------------------------------

-module(milkshell_app).

%% internal functions
%
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", milkshell_handlers, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    milkshell_sup:start_link().

stop(_State) ->
    ok.
