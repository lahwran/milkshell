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

format_parsed({ok, Parsed}, Tokens) -> #{
    <<"type">> => <<"parsed">>,
    <<"value">> => Parsed,
    <<"warnings">> => [],
    <<"tokens">> => Tokens};
format_parsed({ok, Parsed, Warnings}, Tokens) -> #{
    <<"type">> => <<"parsed">>,
    <<"value">> => Parsed,
    <<"warnings">> => Warnings,
    <<"tokens">> => Tokens};
format_parsed({error, Errors, Warnings}, Tokens) -> #{
    <<"type">> => <<"parsefailed">>,
    <<"errors">> => Errors,
    <<"warnings">> => Warnings,
    <<"tokens">> => Tokens};
format_parsed(_Info, Tokens) -> #{
    <<"type">> => <<"mismatch">>,
    <<"tokens">> => Tokens}.

format_token({Name, Lineno, Data}) -> #{<<"type">> => atom_to_binary(Name, utf8), <<"line">> => Lineno, <<"data">> => list_to_binary(Data)};
format_token({Name, Lineno}) -> #{<<"type">> => atom_to_binary(Name, utf8), <<"line">> => Lineno}.
format_tokens([]) -> [];
format_tokens([Head | Tokens]) -> [format_token(Head) | format_tokens(Tokens)].


websocket_handle({text, Msg}, State) ->
    % TODO: how do I handle json decode errors correctly?
    Decoded = jiffy:decode(Msg, [return_maps]),
    {ParseResult, Tokens} = milkshell_misc:parse(binary_to_list(Decoded)),
    FormattedTokens = format_tokens(Tokens),
    Formatted = format_parsed(ParseResult, FormattedTokens),
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
