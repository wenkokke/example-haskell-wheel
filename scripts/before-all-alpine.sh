#!/bin/sh

# GHC version and release URL
GHC_VERSION="9.4.4"
GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-x86_64-alpine3_12-linux-static-int_native.tar.xz"

# Cabal version and release URL
CABAL_VERSION="3.10.1.0"
CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v${CABAL_VERSION}.zip"

# Install GHC
wget -q "${GHC_RELEASE_URL}" -O "/tmp/ghc.tar.xz"
mkdir "/tmp/ghc"
tar xf "/tmp/ghc.tar.xz" -C "/tmp/ghc" --strip-components 1
cd "/tmp/ghc" && ./configure
cd "/tmp/ghc" && make install

# Install Cabal
wget "${CABAL_RELEASE_URL}" -O "/tmp/cabal.zip"
unzip -q "/tmp/cabal.zip" -d "/tmp" && mv "/tmp/cabal-cabal-install-v${CABAL_VERSION}" "/tmp/cabal"
cd "/tmp/cabal" && python3 "./bootstrap/bootstrap.py" -d "./bootstrap/linux-${GHC_VERSION}.json" -w "/usr/local/bin/ghc-${GHC_VERSION}"
cd "/tmp/cabal/cabal-install" && "/tmp/cabal/_build/bin/cabal" v2-update
cd "/tmp/cabal/cabal-install" && "/tmp/cabal/_build/bin/cabal" v2-install cabal-install --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"
