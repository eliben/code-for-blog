/* Code sample: compiling shared libraries with -fpic.
**
** Eli Bendersky (https://eli.thegreenplace.net)
** This code is in the public domain.
*/
#include <stdio.h>

extern int ml_func(int, int);
extern int myglob;

int main(int argc, const char* argv[])
{
    /*printf("addr myglob = %p\n", (void*)&myglob);*/
    int t = ml_func(argc, argc);
    return t;
}
