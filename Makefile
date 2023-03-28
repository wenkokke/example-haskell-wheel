GHC_VERSION := $(shell ghc --numeric-version)
GHC_LIBDIR := $(shell ghc --print-libdir)
GHC_RTS_LIBDIR := $(GHC_LIBDIR)/aarch64-osx-ghc-$(GHC_VERSION)/rts-1.0.2
GHC_RTS_INCLUDEDIR := $(GHC_RTS_LIBDIR)/include

MyFib_stub.h MyFib.hi MyFib.o: MyFib.hs
	ghc -c -O MyFib.hs

main main.o: MyFib_stub.h MyFib.hi MyFib.o
	ghc --make -no-hs-main -optc-O main.c MyFib -o main
