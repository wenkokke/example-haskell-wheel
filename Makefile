GHC_VERSION := $(shell ghc --numeric-version)
GHC_LIBDIR := $(shell ghc --print-libdir)
GHC_RTS_LIBDIR := $(GHC_LIBDIR)/aarch64-osx-ghc-$(GHC_VERSION)/rts-1.0.2
GHC_RTS_INCLUDEDIR := $(GHC_RTS_LIBDIR)/include

MyFib_stub.h MyFib.hi MyFib.o: MyFib.hs
	ghc -static -c -O MyFib.hs

build: MyFib_stub.h MyFib.hi MyFib.o
	gcc -I$(GHC_RTS_INCLUDEDIR) -L$(GHC_RTS_LIBDIR) my_fib.c

test:
	ls $(GHC_RTS_LIBDIR)
