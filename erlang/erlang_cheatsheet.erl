% erlang cheatsheet

% ========  ways to run erlang  ========
% http://erlang.org/doc/reference_manual/users_guide.html
% http://erlang.org/doc/getting_started/users_guide.html

% installed binaries:
% - erl: erlang runtime system, http://erlang.org/doc/man/erl.html
% - erlc: erlang compiler, http://erlang.org/doc/man/erlc.html
% - escript: shebang line runner, http://erlang.org/doc/man/escript.html

% $ escript <modulename.erl> <args>
%   - run modulename:main([args...])
%   - exit on main() exit
%   - allow exiting with a return code
%   - closest to `python x.py` or `node x.js`
% $ erlc <modulename.erl>
% $ erl -s modulename
%   - run modulename:start()
%   - continue with erl startup
% $ erl -s modulename func
%   - run modulename:func()
%   - continue with erl startup
% $ erl -s modulename func arg1 arg2 arg3
%   - run modulename:func([arg1, arg2, arg3])
%   - passes arguments as *atoms*, not strings!
% $ erl -run modulename func arg1 arg2 arg3
%   - same as -s, but passes "charlists"
% $ erl <other stuff>
%   - run erlang shell, most tutorials act like this is
%     all that exists for some reason
%
% relevant docs:
% http://erlang.org/doc/man/init.html
%
% slave nodes allow automatically launching nodes remotely via ssh. see
% http://erlang.org/doc/man/slave.html
%
% IPC/FFI options:
% http://erlang.org/doc/tutorial/overview.html
%
% for any erl invocation, these parameters can be added to configure distributed communication and daemonization:
%  -name dilbert \  % enable distributed mode: long node names, eg 'node@host.fqdn.tld' - not compatible/mutually exclusive with
%  -sname dilbert \  % enable distributed mode: short node names, eg 'node@host'
%       launching erl with a node name also launches epmd to track local names.
%
%  -connect_all false  % disable fully connected networking
%  -hidden  % disable fully connected networking *to* this node, but not *from* it?
%  -setcookie <shared auth secret>  % authorization token - shared between all nodes unfortunately
%  -detached  % daemonize
%  -noshell  % don't attach the shell to stdio by default
%  -noinput  % kill stdin
%  -mode embedded \  % disable code autoloading, bootscript must load everything needed
%       see http://erlang.org/doc/man/code.html#code-path and
%       http://erlang.org/doc/reference_manual/code_loading.html#loading




% ========  SYNTAX & LANGUAGE  ========

% `Variables` are always first letter capitalized. lowercase is like an atom, see also :ruby :atoms
% Variables_with_underscores are allowed, the first letter just has to be capitalized

% blocks are terminated with period, ".", instead of semicolon, ";"
% statements are terminated with comma, ",", instead of semicolon, ";"
% comma-separated statements are executed sequentially

% module annotation syntax: "-Tag(Value)."
% http://erlang.org/doc/reference_manual/modules.html
-module(erlang_cheatsheet).
-mode(compile). % increase performance when running long scripts or large programs in escript
% modules can run code on load:
% http://erlang.org/doc/reference_manual/code_loading.html#running-a-function-when-a-module-is-loaded
-on_load(example/0).
% must export start to run it
-export([example/0, start/0, main/1]).
% alternate way to do the same thing:
% -compile(export_all).

% http://www.cheat-sheets.org/saved-copy/Erlang.CheatSheet(1.0).pdf

% functions are defined as `name(params) -> value.` note the period ending the line.
% io:format("charlist", [vals...]) writes to stdio.
example() -> io:format("test, ~w~n", [derp]).

start() -> io:format("start run successfully\n", []).

main([String]) ->
    try
        N = list_to_integer(String),
        F = fac(N),
        io:format("factorial ~w = ~w\n", [N,F]),
        io:format("factorial ~w = ~w\n", [N,F])
    catch
        _:_ ->
            usage()
    end,
    erlang:halt(0);

main(_) ->
    usage().

usage() ->
    io:format("usage: factorial integer\n"),
    halt(1).

fac(0) -> 1;
fac(N) -> N * fac(N-1).


% http://erlang.org/doc/reference_manual/functions.html
% functions can be defined multiple times in the same module
% a definition that matches by pattern matching assignment will be used
% function definitions might need to end with a semicolon to allow more entries for the function?

% No output for an empty list
