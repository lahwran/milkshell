%%%-------------------------------------------------------------------
%% @doc milkshell public API
%% @end
%%%-------------------------------------------------------------------

-module(milkshell_app).

%% internal functions
%
%
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_',
         [
          {"/", test07a_handler, [{op, help}]}
         ]}
                                     ]),
    {ok, _} = cowboy:start_clear(test07a_http_listener, 100,
                                 [{port, 8002}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    db_sup:start_link(),
    test07a_sup:start_link().

stop(_State) ->
    ok.
