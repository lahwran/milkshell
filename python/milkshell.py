#!/usr/bin/env python3
import functools
import glob
import itertools
import sys

import ometa
import parsley
import re

import pytest

commands = {}
commands_by_target = {}


# bash commands we want
# - if
# - history
# - exit
# - for {}
# - while {}
# - pushd
# - popd
# - pwd
# - echo
# - time
# - function {}
# - unset
# - break
# - [command] &
# - kill
# - jobs
# - read?
# - return
# - export
# - cd
# - ctrl+z, ctrl+c, etc
# - bg/fg
# - argument parsing?
# - clear
# - env

# coreutils/etc:
# - ls
# - cat
# - chmod
# - cp

# things to support?
# - tig
# - sed
# - sudo
# - ssh
# - sqlite3
# - sort
# - sleep
# - sha256sum
# - wget
# - whereis
# - which
# - whois
# - yarn
# - youtube-dl
# - adb
# - asana
# - aws
# - bash
# - dotfiles-install
# - black
# - brew
# - cmake
# - convert
# - create-react-app
# - base64
# - crontab
# - curl
# - date
# - df
# -diff
# - display
# - dropdb
# - elixir
# - erl
# - erlc
# - file
# - escript
# - ffmpeg
# - find
# - git
# - go
# - grep
# - head
# - host
# - identify
# - instanttype
# - ifconfig
# - iex
# - ipython?
# -  lerna
# - make
# - lsof
# - manual
# - mkdir
# - mosh
# mount
# mplayer
# mtr
# openssl
# osascript
# pathprepend
# pbpaste
# ping
# pip
# pipenv
# ponyc
# - py.test
# psql
# ps
#

# render to jsx
# need jsx embed
# | jsx { <XYPlot></XYPlot> }
# | <XYPlot></XYPlot>
# | @py.whatever
# |

# core functionality:
# rerun on cell change
# => mark commands as pure
#

# language nesting rules:
# - python:
#   {}: allowed, mean dicts
#   comments: # to end of line, unless quoted
#   quoting: f[stringblock] allows escaping back to python with {} (unless ({{|}})), but is otherwise a stringblock. stringblock can be started with ("""|'''|"|'), and ends with the matching thing.
# - js:
#   {}: allowed, mean either objects or blocks
#   comments: // to end of line, /* to */
#   quoting: three types: `x`, "x"/'x', /x/. `` allows ${js}, escaped by backslash. ""/'' allow escaped "/', by backslash. /x/ allows escaped /, by backslash or character class.
# - erlang:
#   {}: allowed, mean tuples or types
#   comments: % to end of line
#   quoting: '' or "", and $char for in-code char escaping. backslash followed by anything, including quote. backslash backslash consumes first.


def command(targets):
    def inner(cls):
        commands[cls.__name__] = cls
        compilers = set(
            [
                name.replace("compile_", "")
                for name, val in vars(cls).items()
                if name.startswith("compile_") and type(val) == staticmethod
            ]
        )
        assert compilers == set(targets)

        for target in targets:
            commands_by_target.setdefault(target, {})[cls.__name__] = cls

        # TODO: exception guards
        # TODO: vm hang guards
        # TODO: function argument info
        # TODO: function argument type info
        return cls


@command
async def stdin(*a):
    return sys.stdin


@command
async def subprocess(*a):
    pass


@command
async def echo(inputs, *args):
    yield


@functools.lru_cache(50)
def recache(r):
    return re.compile(r, re.VERBOSE | re.DOTALL)


def tokenize_python(string):
    offset = recache("[ ]*{").match(string).end()
    start = offset
    end = offset
    depth = 1
    python_tokens = []
    ranges = []
    while offset < len(string):
        match = recache(
            r"""
            (
                \#[^\n]*\n
                | \"""(?:\\\\|\\"|(?!\""").)*\"""
                | '''(?:\\\\|\\'|(?!''').)*'''
                | "(?:\\\\|\\"|[^"])*"
                | '(?:\\\\|\\'|[^'])*'
                | [{}]
                
            )
            """
        ).search(string, offset)
        if not match:
            break
        token = match.group(1)
        offset = match.end()
        if token == '{':
            depth += 1
        if token == '}':
            depth -= 1
        if depth == 0:
            end = match.start()
            break
        ranges.append((match.start(), match.end()))
        python_tokens.append(token)
    return ["{", string[start:end], "}"], string[offset:], python_tokens, ranges


def parse_python_block(string):
    milkshell_tokens, remainder, python_tokens, python_token_ranges = tokenize_python(string)
    return milkshell_tokens, remainder


def check_tokenize_python(code):
    prefix = "   {    " + code + "    }"
    target_milkshell_tokens = ["{", "    " + code + "    ", "}"]

    s = prefix + code[::-1]
    milkshell_tokens, remainder, python_tokens, python_token_ranges = tokenize_python(s)

    try:
        assert milkshell_tokens == target_milkshell_tokens
        assert remainder == code[::-1]

        s = "        {    " + code + "    }" + code
        milkshell_tokens, remainder, python_tokens, python_token_ranges = tokenize_python(s)
        assert milkshell_tokens == target_milkshell_tokens
        assert remainder == code
    except AssertionError:

        lastend = 0
        import sys
        sys.stdout.write("START")
        for start, end in python_token_ranges:
            sys.stdout.write("\033[m" + s[lastend:start])
            sys.stdout.write("\033[m\033[32m<\033[m\033[42m" + s[start:end].replace("\n", "\033[m\n\033[42m") + "\033[m\033[32m>\033[m")
            lastend = end
        sys.stdout.write("\033[m" + s[lastend:])
        sys.stdout.write("END")
        sys.stdout.flush()
        raise
    assert tokenize("hello |    @py " + prefix) == ["hello", "|", "@py"] + target_milkshell_tokens
    assert tokenize("@py" + prefix.strip()) == ["@py"] + target_milkshell_tokens


    return python_tokens


def test_tokenize_python():
    assert check_tokenize_python("") == []
    assert check_tokenize_python("1") == []
    q = check_tokenize_python(
        """   
     {
     eggbert \""" "hello" there! \""" def derp(hello:) completely! invalid! syntax!
      r'''\n ' ' '\\n ' r' ' \n' \\n''' # this is a comment, }}}}} and should not  be interpreted " as anything else \"""
      {} {{} } {{{}{}}}

     } """
    )
    assert q == [
        '{',
        '""" "hello" there! """',
        "'''\n ' ' '\\n ' r' ' \n' \\n'''",
        '# this is a comment, }}}}} and should not  be interpreted " as anything else """\n',
        '{',
        '}',
        '{',
        '{',
        '}',
        '}',
        '{',
        '{',
        '{',
        '}',
        '{',
        '}',
        '}',
        '}',
        '}',
    ]
    q = check_tokenize_python(open(__file__).read() + "\n")
    assert q is not None

def test_tokenize_python_slow():
    for x in set(itertools.chain.from_iterable(glob.glob(x + "**.py") + glob.glob(x + "/**/*.py") for x in sys.path if x and not x.endswith(".zip"))):
        try:
            check_tokenize_python(open(x).read() + "\n")
        except UnicodeDecodeError: continue





languages = {"py": parse_python_block}


def tokenize(string):
    tokens = []
    while len(string):
        match = re.search(
            r"""
            (
                [|\n]
                | "(?:\\"|[^"])*"
                | '[^']*'
                | @[a-zA-Z]+
                | [^ |\n]+
            )
            """,
            string,
            re.VERBOSE,
        )
        if not match:
            return tokens
        token = match.group(1)
        string = string[match.end() :]
        tokens.append(token)
        if token.startswith("@"):
            language = languages[token[1:]]
            ms_tokens, string = language(string)
            tokens.extend(ms_tokens)
            continue
    return tokens


grammar = parsley.makeGrammar(
    r"""
    pipe = '|'
    newline = '\n'
    word = ~pipe ~newline :x -> x
    command = word+
    comment = '#' (~newline :x)*
    pipeline = (command:first ((comment? newline)? pipe (comment? newline)? command)*:rest -> [first] + rest)
    logical_line = newline* pipeline?:p comment? -> p
    document = logical_line:first (newline logical_line)*:rest newline* -> [x for x in [first] + rest if x]
    """,
    {},
)


def parse(s):
    tokens = tokenize(s)
    return grammar(tokens).document()


def test_tokenize():
    assert tokenize(r"hello there |friend| derp \|herp ") == [
        "hello",
        "there",
        "|",
        "friend",
        "|",
        "derp",
        "\\",
        "|",
        "herp",
    ]
    assert tokenize("hello\nthere") == ["hello", "\n", "there"]


def test_parse():
    assert parse(r"hello there |friend| derp \|herp ") == [
        [["hello", "there"], ["friend"], ["derp", "\\"], ["herp"]]
    ]


def test_line_split_parse():
    assert parse("hello \n | there") == [[["hello"], ["there"]]]
    assert parse("hello | \n there") == [[["hello"], ["there"]]]
    assert parse("hello \n there") == [[["hello"]], [["there"]]]
    with pytest.raises(parsley.ParseError):
        parse("hello \n \n | there")


def test_no_missing_command():
    with pytest.raises(parsley.ParseError):
        parse("| there")
    with pytest.raises(parsley.ParseError):
        parse("|")
    with pytest.raises(parsley.ParseError):
        parse("|")


def test_empty_parse():
    assert parse("") == []


# command definition:
# - name
# - parameter types
# - input type
# - output type
# - for each language, command compiler

# parameters can be
# - concrete value
#     - language block
# - variable


# def test_language_grouping():
#    parsed = parse("")
#    group_languages()


def milkeval(pipeline):
    arg = sys.stdin
    for cmd in pipeline:
        cmd_name = cmd[0]
        rest = cmd[1:]
        launch_func = commands[cmd_name]
        arg = launch_func(arg, *rest)
    for message in arg:
        print(message)


# @pytest.mark.asyncio
# def test_run_simple():
#    result = milkeval(parse("echo hello"))


def run_repl():
    running = True
    while running:
        pass


if __name__ == "__main__":
    test_parse()
    test_tokenize()
