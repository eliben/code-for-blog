#include <stdio.h>


void do_stuff()
{
    printf("Hello, ");
}


int main()
{
    for (int i = 0; i < 4; ++i)
        do_stuff();
    printf("world!\n");
    return 0;
}
