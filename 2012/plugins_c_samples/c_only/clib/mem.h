//
// clib/mem.h: Memory allocation utilities
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#ifndef MEM_H
#define MEM_H

#include <stddef.h>

/* The mem interface leaves the choice of how to handle 
** out-of-memory (OOM) errors in allocations to the user.
**
** If you define the MEM_ALLOC_FAILED_EXCEPTION macro, the mem
** allocation functions will raise the mem_alloc_failed exception
** in case of OOM.
** If this macro isn't defined, the mem allocation functions will
** simply "die" with an error message when an OOM occurs. 
*/
#ifdef MEM_ALLOC_FAILED_EXCEPTION
#include "except.h"
extern const exception mem_alloc_failed;
#endif


/* Allocation functions with out-of-memory handling.
*/
void* mem_alloc(size_t size);
void* mem_realloc(void* ptr, size_t size);
void* mem_calloc(size_t n, size_t size);


/* Similar to the standard functions. In case of out-of-memory, 
** NULL is returned.
*/
void* mem_try_alloc(size_t size);
void* mem_try_realloc(void* ptr, size_t size);
void* mem_try_calloc(size_t n, size_t size);

void mem_free(void* ptr);



#endif /* MEM_H */
