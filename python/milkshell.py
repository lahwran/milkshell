# import argparse

# parser = argparse.ArgumentParser()
# parser.add_argument("command", help="which builtin command to launch")
# parser.add_argument("input", help="input address in milkshell format")
# parser.add_argument("output", help="output address in milkshell format")
import dataclasses

# json:delimited:fd:6,7
# json:delimited:fd:0
# fd:1
from enum import Enum
from typing import List, Tuple, Union, Any, Hashable, Callable, Dict

try:
    assert False
except AssertionError:
    pass
else:
    raise Exception("Assertions appear to be disabled")

FD_CHUNK_SIZE = 1

# TODO: handle read/write naming - allow multiple functions for read, write, etc
# TODO: handle different open modes on interators -
_addr_handlers_registry: Dict[str, Tuple[Dict[str, Callable], List[Callable[[str], Hashable]]]] = {}


# TODO: decide how to handle send wrappers like `newlines` in send mode.
#  should they be async iterators over the values to be sent, ie newlines(strings_iterator) -> strings_iterator?
#  async generators which take values from the yield and send them, ie newlines(outgoing_stream) -> generator?
#  should they keep the form of a callback to process encoding steps, ie newlines(outgoing_stream) -> _SenderHack? idk.
class _SenderHack:
    def __init__(self, target, encoder):
        self.encoder = encoder
        self.target = target

    async def send(self, value):
        # TODO: will encoders need async?
        encoded = self.encoder()
        await self.target.send_all(encoded)

    # TODO: implement async def wait_send_all_might_not_block?
    asend = send

class IterSendPair:
    def __init__(self, iterable, sendable):
        self.iterable = iterable
        self.sendable = sendable

    # these functions aren't async, but may return async values :Shh8lwYtLGyrwPutg
    def send(self, value):
        return self.sendable.send(value)

    def __aiter__(self):
        return self.iterable.__aiter__()

    def __anext__(self):
        return self.iterable.__anext__()


class Mode(Enum):
    READ = 'r'
    WRITE = 'w'
    PAIR = 'rw'


@dataclasses.dataclass(eq=True, frozen=True, unsafe_hash=True)
class _Milkaddr:
    addr_handlers: Tuple[Tuple[Mode, Callable], ...]
    args: Tuple[Hashable]


# TODO: should this be async?
# TODO: if not, what happens for endpoints that have async connect?
def open_addr(addr, mode: Mode, *, _shared_conn: (_Milkaddr, Any)=None):
    # TODO: check that return types match required type args of our handler
    if _shared_conn and addr == _shared_conn[0]:
        return _shared_conn[1]
    args = [open_addr(x, mode, _shared_conn=_shared_conn) if isinstance(x, _Milkaddr) else x for x in addr.args]
    addr_handler = dict(addr.addr_handlers)[mode]
    return addr_handler(*args)


# noinspection PyIncorrectDocstring
def open_addr_pair(milkaddr_read, milkaddr_write):
    """
    Open two parsed milkaddrs as a pair. If the addresses share eg a tcp connection endpoint, it
    will be connected as a single connection and split into a read half and a write half. The
    endpoints may share a low level connection and use different protocols for different directions,
    if desired.

    :param milkaddr_read:
    :param milkaddr_write:
    :return:
    """
    addr_cache = {}
    # TODO: handle when a stream takes two substreams
    read_addrs = [milkaddr_read]
    first_shared = None
    while type(read_addrs[-1].args[-1]) == _Milkaddr:
        read_addrs.append(read_addrs[-1].args[-1])

    write_addr = milkaddr_write
    while type(write_addr.args[-1]) == _Milkaddr:
        write_addr = write_addr.args[-1]

        # compares all their args recursively because python eq, as generated by @dataclass
        if write_addr in read_addrs:
            first_shared = write_addr
            break

    if first_shared is not None:
        conn = open_addr(first_shared, Mode.PAIR)
        shared_conn = (first_shared, conn)
    else:
        shared_conn = None
    read_conn = open_addr(milkaddr_read, Mode.READ, _shared_conn=shared_conn)
    write_conn = open_addr(milkaddr_read, Mode.READ, _shared_conn=shared_conn)
    return read_conn, write_conn


_MilkaddrPartial = Tuple[str, List[List[str]]]
def milkaddr(addr: Union[str, _MilkaddrPartial]) -> _Milkaddr:
    if isinstance(addr, str):
        parts = addr.split(":")
        args = [a.split(",") for a in parts]
        assert len(args) > 1, f"args was empty: {addr}"
        assert len(args[0]) == 1, f"args[0] had commas: {addr}"
        name = args[0][0]
        remaining_args_flat = args[1:]
    else:
        name, remaining_args_flat = addr

    addr_handlers, parsers = _addr_handlers_registry[name]

    these_args = remaining_args_flat[0]
    remaining_args = remaining_args_flat[1:]
    assert len(these_args) <= len(parsers)
    parsed_args: List[Hashable] = []
    done = False
    for arg, parser in zip(these_args, parsers):
        assert not done, "If a milkaddr is an argument to another milkaddr, it must be the last one"
        if parser == milkaddr:
            arg = (arg, remaining_args)
            # TODO: remove this check by allowing nesting. use something regexy to parse nesting?
            done = True
        val = parser(arg)
        assert hash(val) is not None
        parsed_args.append(val)

    return _Milkaddr(tuple((k, v) for (k, v) in addr_handlers.items()), tuple(parsed_args))


def milkaddr_handler(*types, modes: Union[str, Tuple[str, ...], List[str]]):
    def inner(func):
        if milkaddr in types:
            assert types.index(milkaddr) == len(types) - 1

        funcs, existing_types = _addr_handlers_registry.setdefault(func.__name__, ({}, types))
        assert existing_types == types, "Cannot change argument types based on open mode"

        if isinstance(modes, str):
            modes_converted = (modes,)
        else:
            modes_converted = modes

        for mode in modes_converted:
            funcs[mode] = func

    return inner


# noinspection PyUnusedLocal
@milkaddr_handler(int, modes=[Mode.READ, Mode.WRITE])
def fd(fd_):
    import trio.hazmat

    return trio.hazmat.FdStream(fd_)

@milkaddr_handler(int, int, modes=[Mode.PAIR])
def fd_pair(fd1, fd2):
    import trio.hazmat

    read_stream = trio.hazmat.FdStream(fd1)
    write_stream = trio.hazmat.FdStream(fd2)
    # noinspection PyArgumentList
    return trio.StapledStream(write_stream, read_stream)


@milkaddr_handler(milkaddr, modes=[Mode.READ])
async def newlines(bytestream):
    data = b""
    async for block in bytestream:
        # TODO: this code scans the same chunk multiple times unnecessarily
        data += block
        while b'\n' in data:
            # TODO: is this safe if data is utf-8 encoded?
            # TODO: should this include the newlines in the yielded values?
            before, newline, after = data.partition(b'\n')
            data = after
            # TODO: should move this up so we're searching for newlines in decoded values. need a way to handle
            #  partial buffers though
            yield before.decode('utf-8')
    # TODO: should this last yield be here at all? maybe it should instead be an assertion that the buffer was emptied?
    yield data.decode('utf-8')

@milkaddr_handler(milkaddr, modes=[Mode.WRITE])
def newlines(bytestream):
    return _SenderHack(bytestream, lambda value: value.encode("utf-8") + "\n")

@milkaddr_handler(milkaddr, modes=[Mode.READ])
async def json(str_iter):
    import json as json_module
    async for chunk in str_iter:
        if not chunk.strip(): continue
        yield json_module.loads(chunk)

@milkaddr_handler(milkaddr, modes=[Mode.WRITE])
def json(str_sender):
    import json as json_module
    return _SenderHack(str_sender, lambda value: json_module.dumps(value))

class Module:
    def __init__(self, *, default_command=None):
        self.commands = {
            "builtin:default": self.run_default_command,
        }

    def language_block(self, func):
        self.commands[f"lb:{func.__name__}"] = func

    def main(self, argv):
        _bin, command, input_addr_str, output_addr_str, debug_addr_str = argv
        # args = parser.parse_args(argv)
        if ":" not in command:
            command = f"builtin:{command}"
        command_fn = self.commands[command]
        addr_inp = milkaddr(input_addr_str)
        addr_out = milkaddr(output_addr_str)
        inp, out = open_addr_pair(addr_inp, addr_out)
        command_fn(inp, out)  # , debug

    def run_default_command(self, stdin, stdout):
        raise NotImplementedError("No default command")


def stdin():
    return []

def write_stdout(stream):
    if not stream: return
    for x in stream:
        print(str(x))
