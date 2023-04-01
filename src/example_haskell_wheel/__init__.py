from example_haskell_wheel._binding import (
    hs_fib,
    hs_defaultMain,
    hs_rts_init,
    hs_rts_exit,
)


def fib(n: int) -> int:
    hs_rts_init(["example-haskell-wheel"])
    r = hs_fib(n)
    hs_rts_exit()
    return r


def main():
    print("Let's ask Haskell to compute some Fibonacci numbers:")
    hs_rts_init(["example-haskell-wheel"])
    hs_defaultMain(["1", "5", "42", "book"])
    hs_rts_exit()
