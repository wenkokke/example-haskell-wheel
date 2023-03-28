PYTHON_EXEC_PREFIX := $(shell python -c 'import sys; print(sys.exec_prefix)')
PYTHON_VERSION_TAG := $(shell python -c 'import platform; x, y, _ = platform.python_version_tuple(); print(f"python{x}.{y}")')
PYTHON_INCLUDE_DIR := $(PYTHON_EXEC_PREFIX)/include/$(PYTHON_VERSION_TAG)

GHC_VERSION := $(shell ghc --numeric-version)
GHC_LIB_DIR := $(wildcard $(shell ghc --print-libdir)/*-ghc-$(GHC_VERSION))
GHC_INCLUDE_DIR := $(firstword $(wildcard $(GHC_LIB_DIR)/include) $(wildcard $(GHC_LIB_DIR)/rts-*/include))
GHC_LIB_RTS := $(subst lib,,$(basename $(notdir $(firstword $(wildcard $(GHC_LIB_DIR)/libHSrts-*)))))

.PHONY: run
run: fib/_binding.so
	python fib/__main__.py

fib/_binding.so: fib/binding_wrap.o Fib.o
	ghc \
		-o fib/_binding.so \
		-shared \
		-dynamic \
		-fPIC \
		fib/binding_wrap.o \
		Fib.o \
		-L$(GHC_LIB_DIR) \
		-l$(GHC_LIB_RTS)

fib/binding_wrap.o: Fib_stub.h fib/binding_wrap.c
	gcc \
		-fpic \
		-c fib/binding_wrap.c \
		-I. \
		-I$(PYTHON_INCLUDE_DIR) \
		-I$(GHC_INCLUDE_DIR) \
		-o fib/binding_wrap.o


fib/binding_wrap.c: Fib_stub.h fib/binding.i
	swig \
		-python \
		fib/binding.i

Fib_stub.h Fib.hi Fib.o: Fib.hs
	ghc -c -dynamic -fPIC $<

.PHONY: clean
clean:
	git clean -fdX
