-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	erlang:start_timer(1000, self(), jiffy:encode(#{<<"type">> => <<"hello">>})),
	{ok, State}.

format_parsed({ok, Parsed}) -> #{
    <<"type">> => <<"parsed">>, <<"value">> => Parsed, <<"warnings">> => []};
format_parsed({ok, Parsed, Warnings}) -> #{<<"type">> => <<"parsed">>, <<"value">> => Parsed, <<"warnings">> => Warnings};
format_parsed({error, Errors, Warnings}) -> #{<<"type">> => <<"parsefailed">>, <<"errors">> => Errors, <<"warnings">> => Warnings};
format_parsed(_Info) -> #{<<"type">> => <<"mismatch">>}.

websocket_handle({text, Msg}, State) ->
    % TODO: how do I handle json decode errors correctly?
    Decoded = jiffy:decode(Msg, [return_maps]),
    ParseResult = milkshell_misc:parse(binary_to_list(Decoded)),
    Formatted = format_parsed(ParseResult),
    Encoded = jiffy:encode(Formatted),
	{reply, {text, Encoded}, State};
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
