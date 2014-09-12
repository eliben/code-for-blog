//
// clib/dstring.c: Dynamic string type
// 
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "dstring.h"
#include "mem.h"
#include "cutils.h"


struct DString_t
{
    /* The character buffer holding the string's data
    */
    char* str;
    
    /* Amount of occupied bytes in `str`, including trailing NUL
    */
    size_t len;
};


/* strdup isn't ANSI C, so here's one...
*/
static char* my_strdup(const char* str)
{
    size_t len = strlen(str) + 1;
    char* dup = mem_alloc(len);
    return memcpy(dup, str, len);
}


dstring dstring_dup(const dstring dstr)
{
    return dstring_new(dstr->str);
}


dstring dstring_new(const char* cstr)
{
    dstring dstr = mem_alloc(sizeof(*dstr));
    
    dstr->str = my_strdup(cstr);
    dstr->len = strlen(dstr->str) + 1;
    
    return dstr;
}


dstring dstring_new_len(const char* cstr, size_t n)
{
    size_t len = MIN(strlen(cstr), n) + 1;
    
    dstring dstr = mem_alloc(sizeof(*dstr));
    dstr->str = mem_alloc(len);
    dstr->len = len;
    strncpy(dstr->str, cstr, len - 1);
    dstr->str[len - 1] = '\0';

    return dstr;
}


dstring dstring_empty(void)
{
    return dstring_new("");
}


char* dstring_cstr(dstring dstr)
{
    return dstr->str;
}


size_t dstring_len(dstring dstr)
{
    return dstr->len - 1;
}


dstring dstring_format(const char* format, ...)
{
    va_list ap;
    char* buf;
    int len = 8, vslen;
    dstring dstr;

    /* Since there's no way telling in advance how long the 
    ** formatted string will be, we try to allocate increasingly
    ** large buffers until it fits in.
    */
    while (1)
    {
        /* A sentinel '\0' is placed at the end of the buffer.
        ** If it's overwritten by vsnprintf, this means there 
        ** was not enough space so we allocate a larger buffer
        **
        ** Note: it would be nice to rely on the return value of 
        ** vsnprintf instead of the sentinel, but in cases of 
        ** a small buffer, the return value is inconsistent for
        ** different compilers (MSVC vs. GCC)
        */
        buf = mem_alloc(len);
        buf[len - 2] = '\0';
        
        va_start(ap, format);
        vslen = vsnprintf(buf, len, format, ap);
        va_end(ap);
        
        if (buf[len - 2] != '\0')
        {
            mem_free(buf);
            len *= 2;
        }
        else
            break;
    }
    
    dstr = mem_alloc(sizeof(*dstr));
    dstr->str = buf;
    dstr->len = vslen + 1;
    return dstr;
}


void dstring_free(dstring dstr)
{
    if (dstr)
    {
        mem_free(dstr->str);
        mem_free(dstr);
    }
}


dstring dstring_copy_len(dstring dest, dstring src, size_t n)
{
    /* len_required is the total allocated length (including place
    ** for NUL) required to make the copy.
    ** 
    ** If n is set to be larger than the size of src, the whole
    ** src will be copied.
    ** In any case, if the required length is larger than the 
    ** currently allocated lenght, we reallocate the storate for
    ** the string to fit the new string copied from src.
    */
    n = MIN(n, src->len - 1);
    
    if (n > dest->len - 1)
    {
        size_t len_required = n + 1;
        mem_free(dest->str);
        dest->str = mem_alloc(len_required);
    }
    
    strncpy(dest->str, src->str, n);
    dest->str[n] = '\0';
    dest->len = n + 1;
    return dest;
}


dstring dstring_copy(dstring dest, dstring src)
{
    return dstring_copy_len(dest, src, src->len - 1);
}


dstring dstring_concat_len(dstring dest, dstring src, size_t n)
{
    n = MIN(n, src->len - 1);
    size_t new_len = dest->len + n;
    dest->str = mem_realloc(dest->str, new_len);
    memcpy(dest->str + dest->len - 1, src->str, n);
    dest->str[new_len - 1] = '\0';
    dest->len = new_len;
    return dest;
}


dstring dstring_concat(dstring dest, dstring src)
{
    return dstring_concat_len(dest, src, src->len - 1);
}


dstring dstring_concat_cstr(dstring dest, const char* src) {
    size_t srclen = strlen(src);
    size_t newlen = dest->len + srclen;
    dest->str = mem_realloc(dest->str, newlen);
    memcpy(dest->str + dest->len - 1, src, srclen);
    dest->str[newlen - 1] = '\0';
    dest->len = newlen;
    return dest;
}


int dstring_compare(dstring d1, dstring d2)
{
    return strcmp(d1->str, d2->str);
}


char dstring_char_at(dstring dstr, size_t pos)
{
    if (pos >= dstr->len)
        return '\0';
    else
        return dstr->str[pos];
}


void dstring_replace_char(dstring dstr, char src, char dest)
{
    for (char* p = dstr->str; *p; ++p) {
        if (*p == src)
            *p = dest;
    }
}



