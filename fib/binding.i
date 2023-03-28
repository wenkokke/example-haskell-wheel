%module example
%{
#include "Fib_stub.h"
extern void hs_init(int argc, char** argv);
extern void hs_exit();
%}

%typemap(in) (int argc, char **argv) {
  /* Check if is a list */
  if (PyList_Check($input)) {
    int i;
    $1 = PyList_Size($input);
    $2 = (char **) malloc(($1+1)*sizeof(char *));
    for (i = 0; i < $1; i++) {
      PyObject *o = PyList_GetItem($input,i);
      if (PyString_Check(o))
    $2[i] = PyString_AsString(PyList_GetItem($input,i));
      else {
    PyErr_SetString(PyExc_TypeError,"list must contain strings");
    free($2);
    return NULL;
      }
    }
    $2[i] = 0;
  } else {
    PyErr_SetString(PyExc_TypeError,"not a list");
    return NULL;
  }
}

%typemap(freearg) (int argc, char **argv) {
  free((char *) $2);
}

extern HsInt32 fib_hs(HsInt32 n);
extern void hs_init(int argc, char** argv);
extern void hs_exit();
