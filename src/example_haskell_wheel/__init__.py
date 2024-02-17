from contextlib import AbstractContextManager
from typing import List, Any, cast

from ._binding import (
    unsafe_hs_example_haskell_wheel_version,
    unsafe_hs_example_haskell_wheel_main,
    unsafe_hs_example_haskell_wheel_fib,
    unsafe_hs_example_haskell_wheel_init,
    unsafe_hs_example_haskell_wheel_exit,
)

VERSION: str = "1.2.0"


def version() -> str:
    try:
        unsafe_hs_example_haskell_wheel_init([])
        return str(unsafe_hs_example_haskell_wheel_version())
    finally:
        unsafe_hs_example_haskell_wheel_exit()


def main(args: List[str]) -> None:
    try:
        unsafe_hs_example_haskell_wheel_init(args)
        unsafe_hs_example_haskell_wheel_main()
    finally:
        unsafe_hs_example_haskell_wheel_exit()


class Session(AbstractContextManager["Session"]):
    def __enter__(self) -> "Session":
        unsafe_hs_example_haskell_wheel_init([])
        return self

    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> None:
        unsafe_hs_example_haskell_wheel_exit()

    def fib(self, n: int) -> int:
        return cast(int, unsafe_hs_example_haskell_wheel_fib(n))
