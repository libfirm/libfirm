#include <stdio.h>

int k = 123;

int main()
{
	if(k & 16) {
		puts("correct");
		return 0;
	}
	return 1;
}
