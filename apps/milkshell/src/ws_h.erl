-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	erlang:start_timer(1000, self(), jiffy:encode(#{<<"hello">> => <<"world">>})),
	{ok, State}.

websocket_handle({text, Msg}, State) ->
    % TODO: how do I handle json decode errors correctly?
    Decoded = jiffy:decode(Msg, [return_maps]),
	{reply, {text, jiffy:encode(#{<<"you_sent">> => Decoded})}, State};
websocket_handle(_Data, State) ->
	{ok, State}.

% `websocket_info` is like `info`: https://ninenines.eu/docs/en/cowboy/2.6/guide/loop_handlers/
% it's called by cowboy to handle messages to this process from the erlang side.
% start_timer queues a message to be sent to this process.
websocket_info({timeout, _Ref, Msg}, State) ->
	%erlang:start_timer(1000, self(), jiffy:encode(#{<<"greeting">> => <<"How' you doin'?">>})),
	{reply, {text, Msg}, State};
websocket_info(_Info, State) ->
	{ok, State}.
