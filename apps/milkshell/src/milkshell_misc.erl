
-module(milkshell_misc).
-export([parse/1]).

example() -> parse("[]").

parse(S) ->
    LexerRes = milkshell_lexer:string(S),
    case LexerRes of
        {ok, Tokens, _} -> {milkshell_parser:parse(Tokens), Tokens};
        {ErrorLine, _, Error} -> {{error, [ErrorLine, Error], []}, []}
    end.
