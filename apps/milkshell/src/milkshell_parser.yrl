Nonterminals pipelines pipeline command.
Terminals '\n' '|' word.
Rootsymbol pipelines.


pipelines -> pipeline : ['$1'].
pipelines -> pipeline '\n' pipelines : ['$1'|'$3'].

pipeline -> command : ['$1'].
pipeline -> command '|' pipeline : ['$1'|'$3'].

command -> word : [extract_token('$1')].
command -> word command : [extract_token('$1')|'$2'].

Erlang code.

extract_token({_Token, _Line, Value}) -> list_to_binary(Value).

