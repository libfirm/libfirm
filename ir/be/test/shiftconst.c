int printf(const char *str, ...);

#define T        unsigned
#define tname(x) u_##x
#include "shiftconst.h"
#undef tname
#undef T

#define T        int
#define tname(x) i_##x
#include "shiftconst.h"
#undef tname
#undef T

#define T        long long
#define tname(x) ll_##x
#include "shiftconst.h"
#undef tname
#undef T

#define T        signed char
#define tname(x) sc_##x
#include "shiftconst.h"
#undef tname
#undef T

int main(void)
{
#define TEST(x)    printf(#x "(0xAABBCCDD) = 0x%X\n", x(0xAABBCCDD))
#define ALLTESTS(pf)  \
	TEST(pf##k1); \
	TEST(pf##k2); \
	TEST(pf##k3); \
	TEST(pf##k4); \
	TEST(pf##k5); \
	TEST(pf##k6); \
	TEST(pf##k7); \
	TEST(pf##k8);

	ALLTESTS(u_);
	ALLTESTS(i_);
	ALLTESTS(ll_);
	ALLTESTS(sc_);
	return 0;
}
