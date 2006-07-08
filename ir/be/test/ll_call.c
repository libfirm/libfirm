#include <stdio.h>

#ifdef __GNUC__
long long fac(long long n)
{
    if(n < 1)
        return 1;

    return (n*fac(n-1));
}

int main(void) {
    printf("Result:%lld (should be 2432902008176640000)\n",fac(20));

    return fac(20) != 2432902008176640000ULL;
}

#else
int main()
{
    return 0;
}
#endif
