import pytest
from typing import Dict, List


def test_example_haskell_wheel_session() -> None:
    import example_haskell_wheel

    with example_haskell_wheel.Session() as session:
        # Test the version
        example_haskell_wheel.version() == example_haskell_wheel.VERSION

        # Test the fib function
        fib_fixture: Dict[int, int] = {
            11: 89,
            23: 28657,
            35: 9227465,
            46: 1836311903,
            # Cause an integer overflow:
            # 47: 2971215073,
            # 59: 956722026041,
        }

        # Test the fib function
        for input, golden_output in fib_fixture.items():
            assert session.fib(input) == golden_output


@pytest.mark.parametrize(
    "args, golden_output",
    [
        (["11"], "fib 11 -> 89"),
        (["23"], "fib 23 -> 28657"),
        (["35"], "fib 35 -> 9227465"),
        (["46"], "fib 46 -> 1836311903"),
        (["47"], "fib 47 -> 2971215073"),
        (["59"], "fib 59 -> 956722026041"),
    ],
)  # type: ignore
def test_example_haskell_wheel_main(args: List[str], golden_output: str) -> None:
    import subprocess

    actual_output = (
        subprocess.check_output(["example-haskell-wheel", *args])
        .decode("utf-8")
        .strip()
    )
    assert actual_output == golden_output
