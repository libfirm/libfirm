#include <stdio.h>

char* blup()
{
	char* p = "all ok";

	if(0) {
		int i = 0;
		do {
			*--p = '0' - i % 10;
		} while((i /= 10) != 0);

		*--p = '-';
	}

	return p;
}

int main() {
	printf("result: %s\n", blup());
	return 0;
}
