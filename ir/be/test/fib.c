#include <stdio.h>

//#define COUNT_BRANCHES

#ifdef COUNT_BRANCHES
int b1, b2, b3;
#endif

unsigned fib(unsigned n)
{
    if(n == 0) {
#ifdef COUNT_BRANCHES
        b1++;
#endif
        return 0;
    }
    if(n == 1) {
#ifdef COUNT_BRANCHES
        b2++;
#endif
        return 1;
    }

#ifdef COUNT_BRANCHES
    b3++;
#endif
    return fib(n-1) + fib(n-2);
}

int main(int argc, char** argv) {
    unsigned n = 8;
    if(argc > 1)
        n = (unsigned) atoi(argv[1]);

#ifdef COUNT_BRANCHES
    b1 = b2 = b3 = 0;
#endif
    printf("Fib %u: %u\n", n, fib(n));
#ifdef COUNT_BRANCHES
    printf("Branches: 1:%d 2:%d 3:%d\n", b1, b2, b3);
#endif

    return 0;
}
