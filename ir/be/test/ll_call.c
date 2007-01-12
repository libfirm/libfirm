#include <stdio.h>

long long fac(long long n)
{
    if(n < 1)
        return 1;

    return (n*fac(n-1));
}

int main(void) {
    printf("Result:%lld (should be 3628800)\n",fac(10));
    printf("Result:%lld (should be 39916800)\n",fac(11));
    printf("Result:%lld (should be 479001600)\n",fac(12));
    printf("Result:%lld (should be 6227020800)\n",fac(13));
    printf("Result:%lld (should be 87178291200)\n",fac(14));
    printf("Result:%lld (should be 1307674368000)\n",fac(15));
    printf("Result:%lld (should be 20922789888000)\n",fac(16));
    printf("Result:%lld (should be 355687428096000)\n",fac(17));
    printf("Result:%lld (should be 6402373705728000)\n",fac(18));
    printf("Result:%lld (should be 121645100408832000)\n",fac(19));
    printf("Result:%lld (should be 2432902008176640000)\n",fac(20));

	return fac(20) != 2432902008176640000ULL;
}
