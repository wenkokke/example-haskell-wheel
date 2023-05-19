def test_example_haskell_wheel_version() -> None:
    import example_haskell_wheel

    assert example_haskell_wheel.version() == example_haskell_wheel.VERSION

def test_example_haskell_wheel_main() -> None:
    import example_haskell_wheel
    import subprocess
    
    def test_case(input: int, result: int) -> None:
        output = subprocess.check_output(["example-haskell-wheel", f"{input}"]).decode("utf-8").strip()
        assert output == f"fib {input} -> {result}"

    test_case(11, 89)