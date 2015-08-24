Sample of properly calling operator delete on in derived class
==============================================================

This sample demonstrates properly calling the deleting destructor (and hence the
right ``operator delete``) for a derived class that wasn't known at compilation
time.

When ``main`` is created, all the compiler knows is that it has a virtual
destructor. It doesn't know if any of the subclasses would actually implement a
custom ``operator delete``.

``sheeplib`` provides the ``Sheep`` class that implements the ``Animal``
interface. As long as both the shared library and the main executable were
compiled with a ABI-compliant compiler, this should work.
