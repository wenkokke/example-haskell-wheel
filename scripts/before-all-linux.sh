#!/bin/sh

# Check the current operating system
source /etc/os-release

# Install GHC and Cabal on CentOS 7
if [ "$ID" = "centos" ]
then
    # NOTE: The CentOS build of GHC links against "libgmp.so"
    cd /usr/lib64 && ln -s libgmp.so.10 libgmp.so
    
    # Install prerequisites
    yum install -y wget
    
    # GHC release URL
    GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-x86_64-centos7-linux.tar.xz"
    
    # Install GHC
    wget -q "${GHC_RELEASE_URL}" -O /tmp/ghc.tar.xz
    mkdir /tmp/ghc
    tar xf /tmp/ghc.tar.xz -C /tmp/ghc --strip-components 1
    cd /tmp/ghc && ./configure
    cd /tmp/ghc && make install
    
    # Cabal release URL
    CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v3.10.1.0.zip"
    
    # Install Cabal
    wget "${CABAL_RELEASE_URL}" -O /tmp/cabal.zip
    unzip -q /tmp/cabal.zip -d /tmp && mv /tmp/cabal-cabal-install-v3.10.1.0 /tmp/cabal
    sed -ie 's/+ofd-locking/-ofd-locking/' /tmp/cabal/bootstrap/linux-9.4.4.json
    cd /tmp/cabal && python3 ./bootstrap/bootstrap.py -d ./bootstrap/linux-9.4.4.json -w /usr/local/bin/ghc-9.4.4
    cd /tmp/cabal/cabal-install && /tmp/cabal/_build/bin/cabal v2-update
    cd /tmp/cabal/cabal-install && /tmp/cabal/_build/bin/cabal v2-install cabal-install --constraint='lukko -ofd-locking' --overwrite-policy=always --install-method=copy --installdir=/usr/local/bin
fi

# Install GHC and Cabal on Alpine Linux 3.12
if [ "$ID" = "alpine" ]
then
    # GHC release URL
    GHC_RELEASE_URL="https://downloads.haskell.org/~ghc/9.4.4/ghc-9.4.4-x86_64-alpine3_12-linux-static-int_native.tar.xz"
    
    # Install GHC
    wget -q "${GHC_RELEASE_URL}" -O /tmp/ghc.tar.xz
    mkdir /tmp/ghc
    tar xf /tmp/ghc.tar.xz -C /tmp/ghc --strip-components 1
    cd /tmp/ghc && ./configure
    cd /tmp/ghc && make install
    
    # Cabal release URL
    CABAL_RELEASE_URL="https://github.com/haskell/cabal/archive/refs/tags/cabal-install-v3.10.1.0.zip"
    
    # Install Cabal
    wget "${CABAL_RELEASE_URL}" -O /tmp/cabal.zip
    unzip -q /tmp/cabal.zip -d /tmp && mv /tmp/cabal-cabal-install-v3.10.1.0 /tmp/cabal
    cd /tmp/cabal && python3 ./bootstrap/bootstrap.py -d ./bootstrap/linux-9.4.4.json -w /usr/local/bin/ghc-9.4.4
    cd /tmp/cabal/cabal-install && /tmp/cabal/_build/bin/cabal v2-update
    cd /tmp/cabal/cabal-install && /tmp/cabal/_build/bin/cabal v2-install cabal-install --overwrite-policy=always --install-method=copy --installdir=/usr/local/bin
fi
