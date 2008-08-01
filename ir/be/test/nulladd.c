#include <stdlib.h>
#include <stdio.h>

static int * const a = (int *)0x10;
static int * const b = (int *)0x20;

int main() {
	printf("%p\n", b - a);
	return 0;
}
