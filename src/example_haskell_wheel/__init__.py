from example_haskell_wheel._binding import hs_rts_init, hs_rts_exit, hs_fib


def fib(n: int) -> int:
    hs_rts_init()
    r = hs_fib(n)
    hs_rts_exit()
    return r


def main():
    print(fib(42))
