def test_example_haskell_wheel_version() -> None:
    import example_haskell_wheel

    assert example_haskell_wheel.version() == example_haskell_wheel.VERSION

def test_example_haskell_wheel_main() -> None:
    import example_haskell_wheel
    import subprocess
    
    output = subprocess.check_output(["example-haskell-wheel", "11"]).strip()
    assert output == "fib 11 -> 89"