import sys
import example_haskell_wheel


def main() -> None:
    with example_haskell_wheel.Session(sys.argv) as session:
        sys.exit(session.main())


if __name__ == "__main__":
    main()
