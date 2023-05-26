#!/bin/bash

# GHC version and source URL
GHC_VERSION="9.4.4"
GHC_SOURCE_URL="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-src.tar.xz"

## Check the bootstrap GHC version
GHC_BOOTSTRAP_VERSION="9.0.2"
GHC_BOOTSTRAP_VERSION_FOUND="$(ghc --numeric-version)"
if [ ! "${GHC_BOOTSTRAP_VERSION}" = "${GHC_BOOTSTRAP_VERSION_FOUND}" ]
then
  echo "Error: Building GHC ${GHC_VERSION} requires GHC ${GHC_BOOTSTRAP_VERSION}, found GHC ${GHC_BOOTSTRAP_VERSION_FOUND}."
  exit 1
fi

## Check the Cabal version
CABAL_VERSION="3.10.1.0"
CABAL_VERSION_FOUND="$(cabal --numeric-version)"
if [ ! "${CABAL_VERSION}" = "${CABAL_VERSION_FOUND}" ]
then
  echo "Error: Building GHC ${GHC_VERSION} requires Cabal ${CABAL_VERSION}, found Cabal ${CABAL_VERSION_FOUND}."
  exit 1
fi

## Install Alex and Happy
cabal v2-update
mkdir -p "/tmp/alex/bin"
cabal v2-install alex --overwrite-policy=always --install-method=copy --installdir="/tmp/alex/bin"
mkdir -p "/tmp/happy/bin"
cabal v2-install happy --overwrite-policy=always --install-method=copy --installdir="/tmp/happy/bin"

## Get GHC source
wget -q "${GHC_SOURCE_URL}" -O "/tmp/ghc.tar.xz"
mkdir "/tmp/ghc"
tar xf "/tmp/ghc.tar.xz" -C "/tmp/ghc" --strip-components 1

## Configure GHC
cp "/tmp/ghc/mk/build.mk.sample" "/tmp/ghc/mk/build.mk"
cat >> "/tmp/ghc/mk/build.mk" <<EOL

HADDOCK_DOCS=NO
EOL
cd "/tmp/ghc" && ./configure --target=aarch64-apple-darwin ALEX="/tmp/alex/bin/alex" GHC="ghc-${GHC_BOOTSTRAP_VERSION}" HAPPY="/tmp/happy/bin/happy"

## Build GHC
cd "/tmp/ghc" && make install
