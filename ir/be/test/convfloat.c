#include <stdio.h>

#ifdef __GNUC__
long long int conv_dbl_to_ll(double d) __attribute__((noinline));

long long int add_dbl_to_ll(double d1, double d2) __attribute__((noinline));
#endif

long long int conv_dbl_to_ll(double d) {
	return d;
}

long long int add_dbl_to_ll(double d1, double d2) {
	long long int a = d1;
	long long int b = d2;

	return d1 + d2;
}

int main(int argc) {
	double d = 5.45, d1 = 1.00, d2 = 2.5;

	printf("double (%lf) -> int = %lld\n", d, conv_dbl_to_ll(d));
	printf("double (%lf + %lf) -> int = %lld\n", d1, d2, add_dbl_to_ll(d1, d2));

	return 0;
}
