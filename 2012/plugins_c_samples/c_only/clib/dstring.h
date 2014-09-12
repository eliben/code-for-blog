//
// clib/dstring.h: Dynamic string type
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#ifndef DSTRING_H
#define DSTRING_H

#include <stddef.h>


struct DString_t;
typedef struct DString_t* dstring;


/*************** Public interface ***************/

/* Create a new dstring, initialized with the contents of the 
** NUL-terminated C-string `cstr`. New memory is allocated 
** (`cstr` can be freed or go out of scope after this call).
*/
dstring dstring_new(const char* cstr);

/* Like dstring_new, but only up to the first n chars from cstr are
** taken.
*/
dstring dstring_new_len(const char* cstr, size_t n);

/* Create a new, empty dstring
*/
dstring dstring_empty(void);

/* Create a dstring from a printf-like format.
*/
dstring dstring_format(const char* format, ...);

/* Duplicate a dstring
*/
dstring dstring_dup(const dstring dstr);

/* Returns the internal NUL-terminated char* of the dstring.
** This is useful for passing dstrings to fprintf and other
** stdio functions, or for examining its characters one at a time.
** Use with caution!! 
*/
char* dstring_cstr(dstring dstr);

/* Length of dstring
*/
size_t dstring_len(dstring dstr);

/* Copy at most 'n' characters from 'src' to 'dest'. 
*/
dstring dstring_copy_len(dstring dest, dstring src, size_t n);

/* Copy the whole string in 'src' to 'dest'.
*/
dstring dstring_copy(dstring dest, dstring src);

/* Concatenate at most 'n' characters from 'src' to the end of 
** 'dest'
*/
dstring dstring_concat_len(dstring dest, dstring src, size_t n);

/* Concatenate the whole string in 'src' to the end of 'dest'.
*/
dstring dstring_concat_cstr(dstring dest, const char* src);

/* Concatenate the whole string in 'src' to the end of 'dest'
*/
dstring dstring_concat(dstring dest, dstring src);

/* Return the character at position 'pos' of the string. If 'pos'
** is out of bounds, '\0' is returned.
*/
char dstring_char_at(dstring dstr, size_t pos);

/* Replace all occurrences of 'src' in 'str' to 'dest'.
*/
void dstring_replace_char(dstring dstr, char src, char dest);

/* Compares the two strings. Return value similar to strcmp:
** positive if d1 > d2, zero if they're equal, negative if d2 < d1
*/
int dstring_compare(dstring d1, dstring d2);

/* Free the dstring. 
*/
void dstring_free(dstring dstr);




#endif /* DSTRING_H */

