/* Code sample: compiling shared libraries with -fpic.
**
** Eli Bendersky (https://eli.thegreenplace.net)
** This code is in the public domain.
*/
int myglob = 42;

int ml_func(int a, int b)
{
    return myglob + a + b;
}
