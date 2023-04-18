#!/bin/sh

# NOTE: The prebuilt copy of GHC links against libgmp.so
ln -s /usr/lib64/libgmp.so.10 /usr/lib64/libgmp.so

# Install prerequisites
yum install -y wget

# Install GHC
wget -q https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-x86_64-centos7-linux.tar.xz -O /tmp/ghc.tar.xz
mkdir /tmp/ghc
tar xf /tmp/ghc.tar.xz -C /tmp/ghc --strip-components 1
cd /tmp/ghc && ./configure
cd /tmp/ghc && make install

# Install Cabal
wget https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v3.10.1.0.zip -O /tmp/cabal.zip
unzip -q /tmp/cabal.zip -d /tmp && mv /tmp/cabal-cabal-install-v3.10.1.0 /tmp/cabal
sed -ie 's/+ofd-locking/-ofd-locking/' /tmp/cabal/bootstrap/linux-9.4.4.json
cd /tmp/cabal && python ./bootstrap/bootstrap.py -d ./bootstrap/linux-9.4.4.json -w /usr/local/bin/ghc-9.4.4
cd /tmp/cabal/cabal-install && /tmp/cabal/_build/bin/cabal v2-update
cd /tmp/cabal/cabal-install && /tmp/cabal/_build/bin/cabal v2-install cabal-install --constraint='lukko -ofd-locking' --overwrite-policy=always --install-method=copy --installdir=/usr/local/bin
