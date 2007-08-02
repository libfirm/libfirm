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

struct bf mybf = { 0xffffffff, 4097, 65537, 5, 0xff, 4.5, 257 };

int main() {

	dumpMem(&mybf, sizeof mybf);
	printf("sizeof mybf %d\n", sizeof mybf);

	printf("int a (expected -1): %d\n", mybf.a);
	printf("unsigned x:13 (expected 4097): %u\n", mybf.x);
	printf("unsigned y:17 (expected 65537): %u\n", mybf.y);
	printf("unsigned y:3 (expected 5): %u\n", mybf.z);
	printf("unsigned char c (expected ff): %x\n", (unsigned)mybf.c);
	printf("double d (expected 4.5): %.1f\n", mybf.d);
	printf("unsigned w:9 (expected 257): %u\n", mybf.w);

	return 0;
}
