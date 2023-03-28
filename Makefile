PYTHON_EXEC_PREFIX := $(shell python -c 'import sys; print(sys.exec_prefix)')
PYTHON_VERSION_TAG := $(shell python -c 'import platform; x, y, _ = platform.python_version_tuple(); print(f"python{x}.{y}")')
PYTHON_INCLUDE_DIR := $(PYTHON_EXEC_PREFIX)/include/$(PYTHON_VERSION_TAG)

HSRTS_VERSION := $(shell ghc-pkg field rts version --simple-output)
HSRTS_LIBNAME := $(shell ghc-pkg field rts hs-libraries --simple-output)
HSRTS_LIB_FLAGS := $(addprefix -L,$(shell ghc-pkg field rts library-dirs --simple-output))
HSRTS_INCLUDE_FLAGS := $(addprefix -I,$(shell ghc-pkg field rts include-dirs --simple-output))
HSRTS_LD_OPTIONS := $(addprefix -I,$(shell ghc-pkg field rts ld-options --simple-output))
HSRTS_EXTRA_LIB_FLAGS := $(addprefix -l,$(shell ghc-pkg field rts extra-libraries --simple-output))

.PHONY: run
run: fib/_binding.so
	python fib/__main__.py

fib/_binding.so: fib/binding_wrap.o Fib.o
	ghc -o fib/_binding.so -shared -dynamic -fPIC fib/binding_wrap.o Fib.o $(HSRTS_LIB_FLAGS) $(HSRTS_LD_OPTIONS) -l$(HSRTS_LIBNAME) $(HSRTS_EXTRA_LIB_FLAGS)

fib/binding_wrap.o: Fib_stub.h fib/binding_wrap.c
	gcc -fpic -c fib/binding_wrap.c -I. -I$(PYTHON_INCLUDE_DIR) $(HSRTS_INCLUDE_FLAGS) -o fib/binding_wrap.o

fib/binding_wrap.c: Fib_stub.h fib/binding.i
	swig -python fib/binding.i

Fib_stub.h Fib.hi Fib.o: Fib.hs
	ghc -c -dynamic -fPIC $<

.PHONY: clean
clean:
	git clean -fdX
