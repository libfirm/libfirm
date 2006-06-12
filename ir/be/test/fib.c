#include <stdio.h>

unsigned fib(unsigned n)
{
    if(n == 0)
        return 0;
    if(n == 1)
        return 1;

    return fib(n-1) + fib(n-2);
}

int main(int argc, char** argv) {
    unsigned n = 8;
    if(argc > 1)
        n = (unsigned) atoi(argv[1]);

    printf("Fib %u: %u\n", n, fib(n));

    return 0;
}
