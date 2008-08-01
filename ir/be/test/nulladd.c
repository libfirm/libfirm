#include <stdlib.h>
#include <stdio.h>

static const int *a = (int *)0x10;
static const int *b = (int *)0x20;

int main() {
	printf("%p\n", b - a);
	return 0;
}
