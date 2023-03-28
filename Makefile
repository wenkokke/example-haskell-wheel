PYTHON_EXEC_PREFIX := $(shell python -c 'import sys; print(sys.exec_prefix)')
PYTHON_VERSION_TAG := $(shell python -c 'import platform; x, y, _ = platform.python_version_tuple(); print(f"python{x}.{y}")')
PYTHON_INCLUDE_DIR := $(PYTHON_EXEC_PREFIX)/include/$(PYTHON_VERSION_TAG)

GHC_VERSION := $(shell ghc --numeric-version)

HSRTS_VERSION := $(shell ghc-pkg field rts version --simple-output)
HSRTS_LIB_DIRS := $(addprefix -L,$(shell ghc-pkg field rts library-dirs --simple-output))
HSRTS_INCLUDE_DIRS := $(addprefix -I,$(shell ghc-pkg field rts include-dirs --simple-output))
LD_OPTIONS := $(addprefix -I,$(shell ghc-pkg field rts ld-options --simple-output))
HSRTS_LIB_FLAGS := $(addprefix -l,$(shell ghc-pkg field rts hs-libraries --simple-output))
HSRTS_EXTRA_LIB_FLAGS := $(addprefix -l,$(shell ghc-pkg field rts extra-libraries --simple-output))

.PHONY: info
info:
	@echo "PYTHON_EXEC_PREFIX = $(PYTHON_EXEC_PREFIX)"
	@echo "PYTHON_VERSION_TAG = $(PYTHON_VERSION_TAG)"
	@echo "PYTHON_INCLUDE_DIR = $(PYTHON_INCLUDE_DIR)"
	@echo "HSRTS_VERSION = $(HSRTS_VERSION)"
	@echo "HSRTS_LIB_FLAGS = $(HSRTS_LIB_FLAGS)"
	@echo "HSRTS_LIB_DIRS = $(HSRTS_LIB_DIRS)"
	@echo "HSRTS_INCLUDE_DIRS = $(HSRTS_INCLUDE_DIRS)"
	@echo "LD_OPTIONS = $(LD_OPTIONS)"
	@echo "HSRTS_EXTRA_LIB_FLAGS = $(HSRTS_EXTRA_LIB_FLAGS)"

.PHONY: run
run: cbits/_binding.so
	python cbits/__main__.py

# Workaround for:
# ld: warning: -undefined dynamic_lookup may not work with chained fixups
ifeq ($(OS),Windows_NT)
else
  UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		LD_OPTIONS += -optl=-Wl,-no_fixup_chains
	endif
endif

cbits/_binding.so: cbits/binding_wrap.o src/Fib.o
	ghc -o cbits/_binding.so -shared -dynamic -fPIC cbits/binding_wrap.o src/Fib.o $(HSRTS_DYNLIB_DIRS) $(HSRTS_LIB_DIRS) $(LD_OPTIONS) $(HSRTS_LIB_FLAGS) $(HSRTS_EXTRA_LIB_FLAGS)

cbits/binding_wrap.o: src/Fib_stub.h cbits/binding_wrap.c
	gcc -fpic -c cbits/binding_wrap.c -Isrc -I$(PYTHON_INCLUDE_DIR) $(HSRTS_INCLUDE_DIRS) -o cbits/binding_wrap.o

cbits/binding_wrap.c: src/Fib_stub.h cbits/binding.i
	swig -python cbits/binding.i

src/Fib_stub.h src/Fib.hi src/Fib.o: src/Fib.hs
	ghc -c -dynamic -fPIC $<

.PHONY: clean
clean:
	git clean -fdX
