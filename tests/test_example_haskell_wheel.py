from contextlib import redirect_stdout
from io import StringIO
import pytest
from typing import Iterator, List
import example_haskell_wheel
from example_haskell_wheel.typing import Session


@pytest.fixture(scope="session")  # type: ignore
def session() -> Iterator[Session]:
    with example_haskell_wheel.hs_rts_init(["example-haskell-wheel", "47"]):
        yield example_haskell_wheel


def test_example_haskell_wheel_version(session: Session) -> None:
    assert session.version() == example_haskell_wheel.VERSION


def test_example_haskell_wheel_main(session: Session) -> None:
    tmp_stdout = StringIO()
    with redirect_stdout(tmp_stdout):
        assert session.main() == 0
    assert tmp_stdout.getvalue().strip() == "fib 47 -> 2971215073"


@pytest.mark.parametrize(
    "input, golden_output",
    [
        (11, 89),
        (23, 28657),
        (35, 9227465),
        (46, 1836311903),
    ],
)  # type: ignore
def test_example_haskell_wheel_fib(
    session: Session, input: int, golden_output: int
) -> None:
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
def test_example_haskell_wheel_subprocess(args: List[str], golden_output: str) -> None:
    import subprocess

    actual_output = (
        subprocess.check_output(["example-haskell-wheel", *args])
        .decode("utf-8")
        .strip()
    )
    assert actual_output == golden_output
