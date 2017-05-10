/* Extension code.
**
** Eli Bendersky [http://eli.thegreenplace.net]
** This code is in the public domain.
*/
#include "Python.h"


/* RevgenState - reverse generator instance.
 *
 * sequence: ref to the sequence that's being iterated
 * seq_index: index of the next element in the sequence to yield
 * enum_index: next enumeration index to yield
 *
 * In pseudo-notation, the yielded tuple at each step is:
 *  enum_index, sequence[seq_index]
*/
typedef struct {
    PyObject_HEAD
    Py_ssize_t seq_index, enum_index;
    PyObject *sequence;
} RevgenState;


static PyObject *
revgen_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *sequence;

    if (!PyArg_UnpackTuple(args, "revgen", 1, 1, &sequence))
        return NULL;

    /* We expect an argument that supports the sequence protocol */
    if (!PySequence_Check(sequence)) {
        PyErr_SetString(PyExc_TypeError, "revgen() expects a sequence");
        return NULL;
    }

    Py_ssize_t len = PySequence_Length(sequence);
    if (len == -1)
        return NULL;

    /* Create a new RevgenState and initialize its state - pointing to the last
     * index in the sequence.
    */
    RevgenState *rgstate = (RevgenState *)type->tp_alloc(type, 0);
    if (!rgstate)
        return NULL;

    Py_INCREF(sequence);
    rgstate->sequence = sequence;
    rgstate->seq_index = len - 1;
    rgstate->enum_index = 0;

    return (PyObject *)rgstate;
}


static void
revgen_dealloc(RevgenState *rgstate)
{
    /* We need XDECREF here because when the generator is exhausted,
     * rgstate->sequence is cleared with Py_CLEAR which sets it to NULL.
    */
    Py_XDECREF(rgstate->sequence);
    Py_TYPE(rgstate)->tp_free(rgstate);
}


static PyObject *
revgen_next(RevgenState *rgstate)
{
    /* seq_index < 0 means that the generator is exhausted.
     * Returning NULL in this case is enough. The next() builtin will raise the
     * StopIteration error for us.
    */
    if (rgstate->seq_index >= 0) {
        PyObject *elem = PySequence_GetItem(rgstate->sequence,
                                            rgstate->seq_index);
        /* Exceptions from PySequence_GetItem are propagated to the caller
         * (elem will be NULL so we also return NULL).
        */
        if (elem) {
            PyObject *result = Py_BuildValue("lO", rgstate->enum_index, elem);
            rgstate->seq_index--;
            rgstate->enum_index++;
            return result;
        }
    }

    /* The reference to the sequence is cleared in the first generator call
     * after its exhaustion (after the call that returned the last element).
     * Py_CLEAR will be harmless for subsequent calls since it's idempotent
     * on NULL.
    */
    rgstate->seq_index = -1;
    Py_CLEAR(rgstate->sequence);
    return NULL;
}


PyTypeObject PyRevgen_Type = {
    PyVarObject_HEAD_INIT(&PyType_Type, 0)
    "revgen",                       /* tp_name */
    sizeof(RevgenState),            /* tp_basicsize */
    0,                              /* tp_itemsize */
    (destructor)revgen_dealloc,     /* tp_dealloc */
    0,                              /* tp_print */
    0,                              /* tp_getattr */
    0,                              /* tp_setattr */
    0,                              /* tp_reserved */
    0,                              /* tp_repr */
    0,                              /* tp_as_number */
    0,                              /* tp_as_sequence */
    0,                              /* tp_as_mapping */
    0,                              /* tp_hash */
    0,                              /* tp_call */
    0,                              /* tp_str */
    0,                              /* tp_getattro */
    0,                              /* tp_setattro */
    0,                              /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,             /* tp_flags */
    0,                              /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    PyObject_SelfIter,              /* tp_iter */
    (iternextfunc)revgen_next,      /* tp_iternext */
    0,                              /* tp_methods */
    0,                              /* tp_members */
    0,                              /* tp_getset */
    0,                              /* tp_base */
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    0,                              /* tp_init */
    PyType_GenericAlloc,            /* tp_alloc */
    revgen_new,                     /* tp_new */
};


static struct PyModuleDef spammodule = {
   PyModuleDef_HEAD_INIT,
   "spam",                  /* m_name */
   "",                      /* m_doc */
   -1,                      /* m_size */
};


PyMODINIT_FUNC
PyInit_spam(void)
{
    PyObject *module = PyModule_Create(&spammodule);
    if (!module)
        return NULL;

    if (PyType_Ready(&PyRevgen_Type) < 0)
        return NULL;
    Py_INCREF((PyObject *)&PyRevgen_Type);
    PyModule_AddObject(module, "revgen", (PyObject *)&PyRevgen_Type);

    return module;
}
