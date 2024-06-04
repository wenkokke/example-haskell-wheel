import atexit
from contextlib import contextmanager
from typing import Iterator, List

from ._binding import (
    unsafe_hs_example_haskell_wheel_version,
    unsafe_hs_example_haskell_wheel_main,
    unsafe_hs_example_haskell_wheel_fib,
    unsafe_hs_example_haskell_wheel_init,
    unsafe_hs_example_haskell_wheel_exit,
)

VERSION: str = "1.3.0"


_hs_rts_init: bool = False


@contextmanager
def hs_rts_init(args: List[str] = []) -> Iterator[None]:
    global _hs_rts_init
    if not _hs_rts_init:
        _hs_rts_init = True
        unsafe_hs_example_haskell_wheel_init(args)
        atexit.register(unsafe_hs_example_haskell_wheel_exit)
    yield None


def version() -> str:
    with hs_rts_init():
        return unsafe_hs_example_haskell_wheel_version()


def main(args: List[str] = []) -> int:
    with hs_rts_init(args):
        return unsafe_hs_example_haskell_wheel_main()


def fib(n: int) -> int:
    with hs_rts_init():
        return unsafe_hs_example_haskell_wheel_fib(n)
