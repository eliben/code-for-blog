//----------------------------------------------------------------
// Statically-allocated memory manager
//
// by Eli Bendersky (eliben@gmail.com)
//
// This code is in the public domain.
//----------------------------------------------------------------
#include <stdio.h>
#include <assert.h>
#include "memmgr.h"


// A rudimentary test of the memory manager.
// Runs assuming default flags in memmgr.h:
//
// #define POOL_SIZE 8 * 1024
// #define MIN_POOL_ALLOC_QUANTAS 16
//
// And a 32-bit machine (sizeof(unsigned long) == 4)
//
void test_memmgr()
{
    if (sizeof(void*) != 4) {
        printf("WARNING: this test was designed for systems with pointer size = 4\n");
    }

    byte *p[30] = {0};
    int i;

    // Each header uses 8 bytes, so this allocates
    // 3 * (2048 + 8) = 6168 bytes, leaving us
    // with 8192 - 6168 = 2024
    //
    for (i = 0; i < 3; ++i)
    {
        p[i] = memmgr_alloc(2048);
        assert(p[i]);
    }

    // Allocate all the remaining memory
    //
    p[4] = memmgr_alloc(2016);
    assert(p[4]);

    // Nothing left...
    //
    p[5] = memmgr_alloc(1);
    assert(p[5] == 0);

    // Release the second block. This frees 2048 + 8 bytes.
    //
    memmgr_free(p[1]);
    p[1] = 0;

    // Now we can allocate several smaller chunks from the
    // free list. There, they can be smaller than the
    // minimal allocation size.
    // Allocations of 100 require 14 quantas (13 for the
    // requested space, 1 for the header). So it allocates
    // 112 bytes. We have 18 allocations to make:
    //
    for (i = 10; i < 28; ++i)
    {
        p[i] = memmgr_alloc(100);
        assert(p[i]);
    }

    // Not enough for another one...
    //
    p[28] = memmgr_alloc(100);
    assert(p[28] == 0);

    // Now free everything
    //
    for (i = 0; i < 30; ++i)
    {
        if (p[i])
            memmgr_free(p[i]);
    }

    memmgr_print_stats();
}


int main()
{
    test_memmgr();

    getchar();
    return 0;
}
