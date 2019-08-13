
-module(milkshell_misc).
-export([parse/1]).

example() -> parse("[]").

parse(S) ->
    {ok, Tokens, _} = milkshell_lexer:string(S),
    milkshell_parser:parse(Tokens).
