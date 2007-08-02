#include <stdlib.h>
#include <string.h>
#include "dumpmem.h"

struct bf {
	int a;
	unsigned x:13;
	unsigned y:17;
	unsigned z:3;
	unsigned char c;
	double d;
	unsigned w:9;
};

#ifdef offsetof
#undef offsetof
#endif
#define offsetof(TYPE, MEMB) ((char*) (&((TYPE *)0)->MEMB) - (char*) 0)

int main(int argc, char **argv) {
	struct bf mybf = { 0xffffffff, 4097, 65537, 5, 0xff, 4.5, 257 };

	if(argc > 1)
		dumpMem(&mybf, sizeof mybf);
	printf("sizeof mybf %d\n", sizeof mybf);
	printf("offset a = %d\n", offsetof(struct bf, a));
	printf("offset c = %d\n", offsetof(struct bf, c));
	printf("offset d = %d\n", offsetof(struct bf, d));

	printf("int a (expected -1): %d\n", mybf.a);
	printf("unsigned x:13 (expected 4097): %u\n", mybf.x);
	printf("unsigned y:17 (expected 65537): %u\n", mybf.y);
	printf("unsigned y:3 (expected 5): %u\n", mybf.z);
	printf("unsigned char c (expected ff): %x\n", mybf.c);
	printf("double d (expected 4.5): %.1f\n", mybf.d);
	printf("unsigned w:9 (expected 257): %u\n", mybf.w);

	return 0;
}
