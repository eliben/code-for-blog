/* Code sample: compiling shared libraries with -fpic.
**
** Eli Bendersky (https://eli.thegreenplace.net)
** This code is in the public domain.
*/
int myglob = 42;

int ml_util_func(int a)
{
    return a + 1;
}

int ml_func(int a, int b)
{
    int c = b + ml_util_func(a);
    myglob += c;
    return b + myglob;
}
