#include <stdio.h>

#define tname(x) x##long
#define T long
#include "optest.h"

#undef tname
#undef T
#define tname(x) x##int
#define T int
#include "optest.h"

#undef tname
#undef T
#define tname(x) x##short
#define T short
#include "optest.h"

#undef tname
#undef T
#define tname(x) x##char
#define T char
#include "optest.h"

#define TEST_UNSIGNED

#undef tname
#undef T
#define tname(x) x##unsigned_long
#define T unsigned long
#include "optest.h"

#undef tname
#undef T
#define tname(x) x##unsigned_int
#define T unsigned int
#include "optest.h"

#undef tname
#undef T
#define tname(x) x##unsigned_short
#define T unsigned short
#include "optest.h"

#undef tname
#undef T
#define tname(x) x##unsigned_char
#define T unsigned char
#include "optest.h"

#if 0
#undef tname
#undef T
#define tname(x) x##unsigned_long_long
#define T unsigned long long
#include "optest.h"

#undef TEST_UNSIGNED

#undef tname
#undef T
#define tname(x) x##long_long
#define T long long
#include "optest.h"
#endif

int main(int argc, char *argv[]) {
	test_long();
	test_int();
	test_short();
	test_char();
	test_unsigned_long();
	test_unsigned_int();
	test_unsigned_short();
	test_unsigned_char();
#if 0
	test_unsigned_long_long();
	test_long_long();
#endif
	return 0;
}
