# import argparse

# parser = argparse.ArgumentParser()
# parser.add_argument("command", help="which builtin command to launch")
# parser.add_argument("input", help="input address in milkshell format")
# parser.add_argument("output", help="output address in milkshell format")
import os
import sys

# json:delimited:fd:6,7
# json:delimited:fd:0
# fd:1
from typing import List

try:
    assert False
except AssertionError: pass
else:
    raise Exception("Assertions appear to be disabled")

FD_CHUNK_SIZE = 1

# TODO: handle read/write naming - allow multiple functions for read, write, etc
# TODO: handle different open modes on interators -
addr_handlers = {}

class _Milkaddr:
    def __init__(self,addr: (str, List[List[str]])):
        """
        Parse and nest flat milkaddr
        :param addr:
        :return:
        """
        name, remaining_args = addr
        addr_handler, parsers = addr_handlers[name]

        these_args = remaining_args[0]
        remaining_args = remaining_args[1:]
        assert len(these_args) <= len(parsers)
        parsed_args = []
        for arg, type in zip(these_args,parsers):
            if type == _Milkaddr:
                arg = (arg, remaining_args)
            parsed_args.append(type(arg))

        self.addr_handler = addr_handler
        self.name = name
        self.args = parsed_args

    # TODO: should this be async? if so, how do we split it by executor?
    # TODO: if not, what happens for endpoints that have async connect?
    def open(self):
        # TODO: recurse into any .open()s for our milkaddr args
        # TODO: check that their return types match required type args of our handler
        # TODO: run our handler with those args
        args = [x.open() if isinstance(x, _Milkaddr) else x for x in self.args]
        return self.addr_handler(*args)

READ = 'r'
WRITE = 'w'
PAIR = 'rw'
def parse_milkaddr(addr: str, mode: str):
    parts = addr.split(":")
    args = [a.split(",") for a in parts]
    assert len(args) > 1
    assert len(args[0]) == 1

    return _Milkaddr((args[0], args[1:]))


def milkaddr_handler(*types):
    def inner(func):
        if _Milkaddr in types:
            assert types.index(_Milkaddr) == len(types) - 1
        addr_handlers[func.__name__] = (func, types)
    return inner


@milkaddr_handler(int)
def fd_read(fd):
    import trio.hazmat
    return trio.hazmat.FdStream(int(fd))


@milkaddr_handler(int)
def fd_write(fd):
    return fd_read(fd)


@milkaddr_handler(int, int)
def fd(fd1, fd2=None):
    if fd2 is None:
        return fd_read(fd)

@milkaddr_handler(_Milkaddr)
def newlines(bytestream):
    # TODO: iterate through blocks of the bytestream, accumulating into a buffer, and return newline separated chunks
    pass





class Module:
    def __init__(self, *, default_command=None):
        self.commands = {
            "builtin:default": self.run_default_command,

        }

    def language_block(self, func):
        self.commands[f"lb:{func.__name__}"] = func

    def main(self, argv):
        command, input, output, debug = argv
        # args = parser.parse_args(argv)
        if ":" not in command:
            command = f"builtin:{command}"
        command_fn = self.commands[command]
        stdin = open_input(input)
        stdout = open_output(output)
        command_fn(stdin, stdout)  # , debug

    def run_default_command(self, stdin, stdout):
        raise NotImplementedError("No default command")
