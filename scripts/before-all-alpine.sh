#!/bin/bash

# Install prerequisites
apk add gmp-dev
apk add ncurses-dev

# GHC bootstrap version and release URL
GHC_BOOTSTRAP_VERSION="9.0.2"
GHC_BOOTSTRAP_RELEASE_URL="https://downloads.haskell.org/~ghc/${GHC_BOOTSTRAP_VERSION}/ghc-${GHC_BOOTSTRAP_VERSION}-x86_64-alpine3.12-linux-gmp.tar.xz"

# Install Bootstrap GHC
wget -q "${GHC_BOOTSTRAP_RELEASE_URL}" -O "/tmp/ghc-bootstrap.tar.xz"
mkdir "/tmp/ghc-bootstrap"
tar xf "/tmp/ghc-bootstrap.tar.xz" -C "/tmp/ghc-bootstrap" --strip-components 1
cd "/tmp/ghc-bootstrap" && ./configure
cd "/tmp/ghc-bootstrap" && make install


# Cabal version and release URL
CABAL_VERSION="3.10.1.0"
CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v${CABAL_VERSION}.zip"

# Install Cabal
wget "${CABAL_RELEASE_URL}" -O "/tmp/cabal.zip"
unzip -q "/tmp/cabal.zip" -d "/tmp" && mv "/tmp/cabal-cabal-install-v${CABAL_VERSION}" "/tmp/cabal"
cd "/tmp/cabal" && python3 "./bootstrap/bootstrap.py" -d "./bootstrap/linux-${GHC_BOOTSTRAP_VERSION}.json" -w "/usr/local/bin/ghc-${GHC_BOOTSTRAP_VERSION}"
cd "/tmp/cabal/cabal-install" && /tmp/cabal/_build/bin/cabal v2-update
cd "/tmp/cabal/cabal-install" && /tmp/cabal/_build/bin/cabal v2-install cabal-install --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"


# GHC version and source URL
GHC_VERSION="9.4.4"
GHC_SOURCE_URL="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-src.tar.xz"

# Build GHC from source

## Install Alex and Happy
/usr/local/bin/cabal v2-install alex --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"
/usr/local/bin/cabal v2-install happy --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"

## Get GHC source
wget -q "${GHC_SOURCE_URL}" -O "/tmp/ghc.tar.xz"
mkdir "/tmp/ghc"
tar xf "/tmp/ghc.tar.xz" -C "/tmp/ghc" --strip-components 1

## Create and apply the fpic-default patch
mkdir "/tmp/ghc-patches"
cat > "/tmp/ghc-patches/fpic-default.patch" <<EOL
Index: ghc/compiler/GHC/Driver/Session.hs
===================================================================
--- ghc.orig/compiler/GHC/Driver/Session.hs
+++ ghc/compiler/GHC/Driver/Session.hs
@@ -3871,6 +3871,7 @@ default_PIC platform =
     -- of that.  Subsequently we expect all code on aarch64/linux (and macOS) to
     -- be built with -fPIC.
     (OSDarwin,  ArchAArch64) -> [Opt_PIC]
+    (OSLinux,   ArchX86_64)  -> [Opt_PIC]
     (OSLinux,   ArchAArch64) -> [Opt_PIC, Opt_ExternalDynamicRefs]
     (OSLinux,   ArchARM {})  -> [Opt_PIC, Opt_ExternalDynamicRefs]
     (OSOpenBSD, ArchX86_64)  -> [Opt_PIC] -- Due to PIE support in
EOL
cd "/tmp" && patch -p0 < "/tmp/ghc-patches/fpic-default.patch"

## Create and update mk/build.mk
cp "/tmp/ghc/mk/build.mk.sample" "/tmp/ghc/mk/build.mk"
cat >> "/tmp/ghc/mk/build.mk" <<EOL

GhcLibHcOpts += -fPIC
GhcRtsHcOpts += -fPIC
GhcRtsCcOpts += -fPIC
BuildFlavour = quick
EOL

## Configure and build GHC
cd "/tmp/ghc" && ./configure ALEX="/usr/local/bin/alex" GHC="/usr/local/bin/ghc-${GHC_BOOTSTRAP_VERSION}" HAPPY="/usr/local/bin/happy"
cd "/tmp/ghc" && make install
