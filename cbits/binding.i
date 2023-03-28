%module binding
%{
#include "Fib_stub.h"
extern HsInt32 hs_fib(HsInt32 n);
extern void hs_exit();

void hs_init_wrap()
{
  return hs_init(NULL, NULL);
}
%}

%typemap(in) HsInt32 {
  $1 = PyInt_AsLong($input);
}

%typemap(typecheck, precedence=SWIG_TYPECHECK_INTEGER) HsInt32 {
  $1 = PyInt_Check($input) ? 1 : 0;
}

%typemap(out) HsInt32 {
  $result = PyInt_FromLong($1);
}

HsInt32 hs_fib(HsInt32 n);
void hs_init_wrap();
void hs_exit();
