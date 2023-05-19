def test_example_haskell_wheel_version() -> None:
    import example_haskell_wheel

    assert example_haskell_wheel.version() == example_haskell_wheel.VERSION
