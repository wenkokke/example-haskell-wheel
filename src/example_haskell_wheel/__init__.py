from typing import List, cast

from ._binding import (
    unsafe_hs_example_haskell_wheel_version,
    unsafe_hs_example_haskell_wheel_main,
    unsafe_hs_example_haskell_wheel_fib,
    unsafe_hs_example_haskell_wheel_init,
    unsafe_hs_example_haskell_wheel_exit,
)

VERSION: str = "1.3.0"


class Session:
    def __init__(self, args: List[str] = []) -> None:
        self.args = args

    def __enter__(self) -> "Session":
        unsafe_hs_example_haskell_wheel_init(self.args)
        return self

    def __exit__(self, exc_type: None, exc_value: None, traceback: None) -> None:
        unsafe_hs_example_haskell_wheel_exit()

    def version(self) -> str:
        return unsafe_hs_example_haskell_wheel_version()

    def main(self) -> int:
        return unsafe_hs_example_haskell_wheel_main()

    def fib(self, n: int) -> int:
        return unsafe_hs_example_haskell_wheel_fib(n)
