#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

union foo {
	float blop;
	int bla;
	struct {
		int a, b, c;
	} jup;
	const char *str;
};

typedef union foo *tree;

enum bla {
	BLA_1,
	BLA_2,
	BLA_3,
	BLA_4
};

const char* foo(enum bla type, tree dummy, ...) {
     va_list     ap;
	 const char *s1;

     va_start(ap, dummy);
	 s1 = va_arg(ap, const char*);
	 va_end(ap);

     return s1;
}

union foo bla = { .str = "bla" };

int main()
{
	const char *res = foo(BLA_2, &bla, "everything ok");
	puts(res);
	return 0;
}
