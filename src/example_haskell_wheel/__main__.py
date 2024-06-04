import sys
from typing import NoReturn
import example_haskell_wheel


def main() -> NoReturn:
    sys.exit(example_haskell_wheel.main(sys.argv))


if __name__ == "__main__":
    main()
