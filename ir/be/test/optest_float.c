#include <stdio.h>

#define TEST_FLOAT

#define tname(x) x##float
#define T float
#include "optest.h"

#undef tname
#undef T
#define tname(x) x##double
#define T double
#include "optest.h"

#if 0
#undef tname
#undef T
#define tname(x) x##long_double
#define T long_double
#include "optest.h"
#endif

int main(int argc, char *argv[]) {
	test_float();
	test_double();
#if 0
	test_long_double();
#endif
	return 0;
}
