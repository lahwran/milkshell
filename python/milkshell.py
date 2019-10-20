#!/usr/bin/env python3
import functools
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


python_grammar = parsley.makeGrammar(
    r"""
    python = '{' python* '}' | ~'}' :x  
    block = <'{' python* '}'>:res (:x)* -> res
    """,
    {},
)


def tokenize_python(string):
    return re.findall(r"""
            (
                [^{}"'#]*\#[^\n]*\n
                | r?\"""(?:\\"|(?!\""").)*\"""
                | r?'''(?:\\'|(?!''').)*'''
                | r"[^"]*"
                | r'[^']*'
                | "(?:\\"|[^"])*"
                | '(?:\\'|[^'])*'
                | [^{}"'#]+
                | [{}]
                
            )
            """, string, re.VERBOSE)

@functools.lru_cache(50)
def recache(r):
    return re.compile(r, re.VERBOSE)

def tokenize_python(string):
    tokens = []
    offset = 0
    depth = 0
    while offset < len(string):
        match = recache(r"""
            (
                [^{}"'#]*\#[^\n]*\n
                | r?\"""(?:\\"|(?!\""").)*\"""
                | r?'''(?:\\'|(?!''').)*'''
                | r"[^"]*"
                | r'[^']*'
                | "(?:\\"|[^"])*"
                | '(?:\\'|[^'])*'
                | [{}]
                
            )
            """).search(string, offset)
        if not match: break
        token = match.group(1)
        if token == '{': depth += 1
        if token == '}': depth -= 1
        if token in ['{', '}'] and depth == 0: break

        assert match.end() > offset
        offset = match.end()
        print(offset)
    return tokens, string

def parse_python_block(string):
    tokens = tokenize_python(string)
    return python_grammar(tokens).block()

def check_tokenize_python(code):
    res = tokenize_python(code)
    assert "".join(res) == code
    res1 = tokenize_python("{" + code + "}")
    assert res1[1:-1] == res
    assert parse_python_block("{" + code + "}" + code) == res1
    return res


def test_tokenize_python():
    assert check_tokenize_python("") is not None
    assert check_tokenize_python("1") is not None
    assert check_tokenize_python("""   
     {
     eggbert \""" "hello" there! \""" def derp(hello:) completely! invalid! syntax!
      r''' ' ' ' ' r' ' ' ''' # this is a comment, }}}}} and should not  be interpreted " as anything else \"""
      {} {{} } {{{}{}}}
     } """) == ['   \n     ',
     '{',
     '\n     eggbert ',
     '""" "hello" there! """',
     ' def derp(hello:) completely! invalid! syntax!\n      r',
     "''' ' ' ' ' r' ' ' '''",
     ' # this is a comment, } and should not { be interpreted " as anything else '
     '"""\n',
     '      ',
     '{',
     '}',
     ' ',
     '{',
     '{',
     '}',
     ' ',
     '}',
     ' ',
     '{',
     '{',
     '{',
     '}',
     '{',
     '}',
     '}',
     '}',
     '\n     ',
     '}',
     ' '
     ]
    assert check_tokenize_python(open(__file__).read()+"\n") is not None

def extract_python_block(string):
    return tokenize_python(string)

languages = {
    "py": extract_python_block
}

def tokenize(string):
    tokens = []
    while len(string):
        match = re.search(r"""
            (
                [|\n]
                | "(?:\\"|[^"])*"
                | '[^']*'
                | @[a-zA-Z]+
                | [^ |\n]+
            )
            """, string, re.VERBOSE)
        if not match: return tokens
        token = match.group(1)
        if token.startswith("@"):
            language = languages[token[1:]]
            continue

        tokens.append(token)
        string = string[match.end():]
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
