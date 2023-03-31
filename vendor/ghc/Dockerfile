FROM arm64v8/ubuntu:22.04

# Install GHCup & GHC 9.0.2
RUN apt-get update
RUN apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libnuma-dev libtinfo5
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.0.2 BOOTSTRAP_HASKELL_CABAL_VERSION=3.8.1.0 BOOTSTRAP_HASKELL_INSTALL_STACK=0 BOOTSTRAP_HASKELL_INSTALL_HLS=0 BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh

# Build GHC 9.4.4-fPIC
ADD patches /patches
ADD build.mk /build.mk
RUN ghcup compile ghc -v 9.4.4 -b 9.0.2 -p /patches -c /build.mk -o 9.4.4-fPIC -- --with-system-libffi
