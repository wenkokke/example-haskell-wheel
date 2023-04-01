def test_fib():
    from example_haskell_wheel import fib

    assert fib(1) == 1
    assert fib(5) == 5
    assert fib(42) == 267914296
