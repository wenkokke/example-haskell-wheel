import sys

from typing import List

if sys.version_info < (3, 8):
    from typing_extensions import Protocol
else:
    from typing import Protocol


class Session(Protocol):
    def version(self) -> str: ...
    def main(self, args: List[str] = []) -> int: ...
    def fib(self, n: int) -> int: ...
