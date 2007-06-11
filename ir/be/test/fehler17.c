#include <stdio.h>

char *p = "\xFF";

int main() {
	printf("Result: %d (should be 255)\n", (unsigned char) (*p++));
	return 0;
}
