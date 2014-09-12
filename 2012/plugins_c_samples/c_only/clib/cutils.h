//
// clib/cutils.h: Utility constants, types and macros
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#ifndef CUTILS_H
#define CUTILS_H


typedef int BOOL;

#define TRUE 1
#define FALSE 0


/* Do not pass arguments with side effects (like ++i) to these
** macros.
*/
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof(arr[0]))


#endif /* CUTILS_H */
