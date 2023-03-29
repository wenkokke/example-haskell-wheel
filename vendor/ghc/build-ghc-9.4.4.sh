#!/bin/sh

# Check if GHC 9.0.2 is installed:
INSTALLED=$(ghcup list --raw-format --tool=ghc --show-criteria=installed 2>/dev/null | grep 9.0.2)
if [ "x${INSTALLED}" = "x" ]; then
  ghcup install ghc 9.0.2
fi

# Check if GHC 9.0.2 is set:
SET=$(ghcup list --raw-format --tool=ghc --show-criteria=set 2>/dev/null | grep 9.0.2)
if [ "x${SET}" = "x" ]; then
  ghcup set ghc 9.0.2
fi

ghcup compile ghc \
  -j8 \
  -v 9.4.4 \
  -b 9.0.2 \
  -p ${PWD}/patches \
  -c ${PWD}/build.mk \
  -o 9.4.4-fPIC \
  -- \
  --with-system-libffi
