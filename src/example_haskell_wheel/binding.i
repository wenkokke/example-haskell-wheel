%module binding
%{
#include "ExampleHaskellWheelBinding_stub.h"

char * unsafe_hs_example_haskell_wheel_version() {
  return hs_example_haskell_wheel_version();
}

int unsafe_hs_example_haskell_wheel_main() {
  return hs_example_haskell_wheel_main();
}

int unsafe_hs_example_haskell_wheel_fib(int n) {
  return hs_example_haskell_wheel_fib(n);
}

void unsafe_hs_example_haskell_wheel_init(int argc, char **argv) {
  hs_init(&argc, &argv);
}

void unsafe_hs_example_haskell_wheel_exit() {
  hs_exit();
}

void unsafe_py_write_stdout(const char * str) {
  PySys_FormatStdout("%s", str);
}

void unsafe_py_write_stderr(const char * str) {
  PySys_FormatStderr("%s", str);
}
%}

%typemap(in) (int argc, char **argv) {
  /* Check if is a list */
  if (PyList_Check($input)) {
    int i;
    $1 = PyList_Size($input);
    $2 = (char **) malloc(($1+1)*sizeof(char *));
    for (i = 0; i < $1; i++) {
      PyObject *o = PyList_GetItem($input, i);
      if (PyUnicode_Check(o)) {
        $2[i] = (char *) PyUnicode_AsUTF8AndSize(o, 0);
      } else {
        PyErr_SetString(PyExc_TypeError, "list must contain strings");
        SWIG_fail;
      }
    }
    $2[i] = 0;
  } else {
    PyErr_SetString(PyExc_TypeError, "not a list");
    SWIG_fail;
  }
}

%typemap(freearg) (int argc, char **argv) {
  free((char *) $2);
}

char * unsafe_hs_example_haskell_wheel_version();
int unsafe_hs_example_haskell_wheel_main();
int unsafe_hs_example_haskell_wheel_fib(int n);
void unsafe_hs_example_haskell_wheel_init(int argc, char **argv);
void unsafe_hs_example_haskell_wheel_exit();
