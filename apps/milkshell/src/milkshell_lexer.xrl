Definitions.

WS = [\s\t]
WSNL = [\s\t\n\r]

Rules.

[^\\|\s\t\n\r]+ : {token, {word, TokenLine, TokenChars}}.
'[^']*' : {token, {word, TokenLine, string:sub_string(TokenChars,1,length(TokenChars)-1)}}.

{WSNL}*\|{WSNL}*          : {token, {'|',  TokenLine}}.
\n          : {token, {'\n',  TokenLine}}.
{WS}+ : skip_token.

Erlang code.
