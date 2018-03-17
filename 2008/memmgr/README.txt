Memory manager: provides memory from a fixed pool that is
allocated statically at link-time.

Useful for embedded systems, where dynamic allocation is not 
always supported. Even when it is, it's sometimes safer to 
pre-allocate the memory pool and not use the heap.

Written by Eli Bendersky (eliben@gmail.com). Inspired by the 
storage allocator in "The C Programming Language, 2nd Edition"
and various allocators found online.

-- License -- 

This code is in the public domain.

-- Package contents -- 

memmgr.h
   The header file to be included in your application. It contains
   the API of the memory manager and can be customized by
   defining pre-processor flags. Read the comment at its top for
   explanation.
  
memmgr.c
   The implementation of the memory manager.
  
main.c
   A sample main application that uses the memory manager, with
   some tests; the tests were designed for 32-bit systems.
