#!/bin/bash

# Install prerequisites
python3 -m pip install wget

# Install prerequisites
apk add gmp-dev

# GHC bootstrap version and release URL
GHC_BOOTSTRAP_RELEASE_URL="https://downloads.haskell.org/~ghc/9.0.2/ghc-9.0.2-x86_64-alpine3.12-linux-gmp.tar.xz"

# Install Bootstrap GHC
python3 -c "import wget; wget.download('${GHC_BOOTSTRAP_RELEASE_URL}', '/tmp/ghc-bootstrap.tar.xz')"
mkdir "/tmp/ghc-bootstrap"
tar xf "/tmp/ghc-bootstrap.tar.xz" -C "/tmp/ghc-bootstrap" --strip-components 1
cd "/tmp/ghc-bootstrap" && ./configure
cd "/tmp/ghc-bootstrap" && make install


# Cabal version and release URL
CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v3.10.1.0.zip"

# Install Cabal
python3 -c "import wget; wget.download('${CABAL_RELEASE_URL}', '/tmp/cabal.zip')"
unzip -q "/tmp/cabal.zip" -d "/tmp" && mv "/tmp/cabal-cabal-install-v3.10.1.0" "/tmp/cabal"
cd "/tmp/cabal" && python3 "./bootstrap/bootstrap.py" -d "./bootstrap/linux-9.0.2.json" -w "/usr/local/bin/ghc-9.0.2"
cd "/tmp/cabal/cabal-install" && /tmp/cabal/_build/bin/cabal v2-update
cd "/tmp/cabal/cabal-install" && /tmp/cabal/_build/bin/cabal v2-install cabal-install --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"


# GHC version and source URL
GHC_SOURCE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-src.tar.xz"

# Build GHC from source

## Install Alex and Happy
/usr/local/bin/cabal v2-install alex --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"
/usr/local/bin/cabal v2-install happy --overwrite-policy=always --install-method=copy --installdir="/usr/local/bin"

## Get GHC source
python3 -c "import wget; wget.download('${GHC_SOURCE_URL}', '/tmp/ghc.tar.xz')"
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
cd "/tmp/ghc" && ./configure ALEX="/usr/local/bin/alex" GHC="/usr/local/bin/ghc-9.0.2" HAPPY="/usr/local/bin/happy"
cd "/tmp/ghc" && make install
