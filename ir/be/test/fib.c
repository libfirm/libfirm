#include <stdio.h>

int b1, b2, b3;

unsigned fib(unsigned n)
{
    if(n == 0) {
        b1++;
        return 0;
    }
    if(n == 1) {
        b2++;
        return 1;
    }

    b3++;
    return fib(n-1) + fib(n-2);
}

int main(int argc, char** argv) {
    unsigned n = 8;
    if(argc > 1)
        n = (unsigned) atoi(argv[1]);

    b1 = b2 = b3 = 0;
    printf("Fib %u: %u\n", n, fib(n));
    printf("Branches: 1:%d 2:%d 3:%d\n", b1, b2, b3);

    return 0;
}
