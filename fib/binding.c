#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "Fib_stub.h"

static PyObject *fib_init(PyObject *self, PyObject *args)
{
    int argc = 1;
    char *argv[] = {"fib", NULL};
    char **pargv = argv;

    hs_init(&argc, &pargv);

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *fib_exit(PyObject *self, PyObject *args)
{
    hs_exit();

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *fib(PyObject *self, PyObject *args)
{
    const int *n;
    int ret;

    if (!PyArg_ParseTuple(args, "i", &n))
        return NULL;

    ret = fib_hs(*n);

    return PyLong_FromLong(ret);
}

static PyMethodDef module_methods[] = {
    {
        .ml_name = "_fib",
        .ml_meth = (PyCFunction)fib,
        .ml_flags = METH_VARARGS,
        .ml_doc = "_fib(n)\n--\n\n\
               Compute the nth number in the Fibonacci sequence.",
    },
    {
        .ml_name = "_fib_init",
        .ml_meth = (PyCFunction)fib_init,
        .ml_flags = METH_NOARGS,
        .ml_doc = "_fib_init()\n--\n\n\
               Init the Haskell RTS.",
    },
    {
        .ml_name = "_fib_exit",
        .ml_meth = (PyCFunction)fib_exit,
        .ml_flags = METH_NOARGS,
        .ml_doc = "_fib_exit()\n--\n\n\
               Exit the Haskell RTS.",
    },
    {NULL},
};

static void module_free(void *self)
{
}

static struct PyModuleDef module_definition = {
    .m_base = PyModuleDef_HEAD_INIT,
    .m_name = "binding",
    .m_doc = NULL,
    .m_size = 0,
    .m_free = module_free,
    .m_methods = module_methods,
};

PyMODINIT_FUNC PyInit_binding(void)
{
    PyObject *module = PyModule_Create(&module_definition);
    if (module == NULL)
        return NULL;
    return module;
}
