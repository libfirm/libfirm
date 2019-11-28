#include <stdint.h>
#include <stdbool.h>

#include "lfutil.h"

uint64_t gcd(uint64_t a, uint64_t b) {
	while (true) {
		if (a == 0) return b;
		b %= a;
		if (b == 0) return a;
		a %= b;
	}
}

uint64_t lcm(uint64_t a, uint64_t b) {
	int tmp = gcd(a, b);
	return tmp ? (a / tmp * b) : 0;
}
