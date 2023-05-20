from typing import List


def test_example_haskell_wheel_version() -> None:
    import example_haskell_wheel

    assert example_haskell_wheel.version() == example_haskell_wheel.VERSION


def assert_example_haskell_wheel_main(args: List[str], golden_output: str) -> None:
    import subprocess

    actual_output = (
        subprocess.check_output(["example-haskell-wheel", *args])
        .decode("utf-8")
        .strip()
    )
    assert actual_output == golden_output


def test_example_haskell_wheel_main() -> None:
    assert_example_haskell_wheel_main(["11"], "fib 11 -> 89")
    assert_example_haskell_wheel_main(["23"], "fib 23 -> 28657")
    assert_example_haskell_wheel_main(["35"], "fib 35 -> 9227465")
    assert_example_haskell_wheel_main(["47"], "fib 47 -> 2971215073")
    assert_example_haskell_wheel_main(["59"], "fib 59 -> 956722026041")
