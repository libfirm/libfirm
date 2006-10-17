#include <stdio.h>
#include <stdlib.h>

typedef long long int ll_t;

#ifdef __GNUC__
ll_t mul_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t shl_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t shr_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t add_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t sub_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t div_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t mod_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t divmod_ll(ll_t a, ll_t b) __attribute__((noinline));
ll_t abs_ll(ll_t a) __attribute__((noinline));
ll_t neg_ll(ll_t a) __attribute__((noinline));
#endif

ll_t mul_ll(ll_t a, ll_t b) {
	return a * b;
}

ll_t shl_ll(ll_t a, ll_t b) {
	return a << b;
}

ll_t shr_ll(ll_t a, ll_t b) {
	return a >> b;
}

ll_t add_ll(ll_t a, ll_t b) {
	return a + b;
}

ll_t sub_ll(ll_t a, ll_t b) {
	return a - b;
}

ll_t div_ll(ll_t a, ll_t b) {
	return a / b;
}

ll_t mod_ll(ll_t a, ll_t b) {
	return a % b;
}

ll_t divmod_ll(ll_t a, ll_t b) {
	return (a / b) + (a % b);
}

ll_t neg_ll(ll_t a) {
	return -a;
}

ll_t abs_ll(ll_t a) {
	return llabs(a);
}

#if 0
double conv_ll_d(ll_t a) {
	return (double)a;
}

ll_t conv_d_ll(double a) {
	return (ll_t)a;
}
#endif

int main(void) {
	ll_t a = 0xff;
	ll_t b = 0x123456789;
	ll_t c = 0x8001023000002460;
	double d = (double)c;

	printf("%lld * %lld  = %lld\n", a, b, mul_ll(a, b));
	printf("%lld + %lld  = %lld\n", a, b, add_ll(a, b));
	printf("%lld - %lld  = %lld\n", a, b, sub_ll(a, b));
	printf("%lld / %lld  = %lld\n", b, a, div_ll(b, a));
	printf("%lld %% %lld  = %lld\n", b, a, mod_ll(b, a));
	printf("%lld / + %% %lld  = %lld\n", b, a, divmod_ll(b, a));
	printf("%lld << %d = %lld\n", a, 2, shl_ll(a, 2));
	printf("%lld >> %d = %lld\n", a, 2, shr_ll(a, 2));
	printf("abs(%lld)    = %lld\n", c, abs_ll(c));
	printf("neg(%lld)    = %lld\n", b, neg_ll(b));
#if 0
	printf("conv(%lld)   = %lf\n",  c, conv_ll_d(c));
	printf("conv(%lf)    = %lld\n", d, conv_d_ll(d));
#endif
	return 0;
}
