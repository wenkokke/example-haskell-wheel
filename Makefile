PYTHON_EXEC_PREFIX := $(shell python -c 'import sys; print(sys.exec_prefix)')
PYTHON_VERSION_TAG := $(shell python -c 'import platform; x, y, _ = platform.python_version_tuple(); print(f"python{x}.{y}")')
PYTHON_INCLUDE_DIR := $(PYTHON_EXEC_PREFIX)/include/$(PYTHON_VERSION_TAG)

GHC_LIB_DIR := $(shell ghc --print-libdir)/aarch64-osx-ghc-9.4.4
GHC_INCLUDE_DIR := $(GHC_LIB_DIR)/rts-1.0.2/include

.PHONY: run
run: fib/binding.so
	python fib/__main__.py

fib/binding.so: fib/binding.o Fib.o
	ghc \
		-o fib/binding.so \
		-shared \
		-dynamic \
		-fPIC \
		fib/binding.o \
		Fib.o \
		-L$(GHC_LIB_DIR) \
		-lHSrts-1.0.2-ghc9.4.4

fib/binding.o: Fib_stub.h fib/binding.c
	gcc \
		-fpic \
		-c fib/binding.c \
		-I. \
		-I$(PYTHON_INCLUDE_DIR) \
		-I$(GHC_INCLUDE_DIR) \
		-o fib/binding.o

Fib_stub.h Fib.hi Fib.o: Fib.hs
	ghc -c -dynamic -fPIC $<
