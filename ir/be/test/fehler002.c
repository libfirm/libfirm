/*$ -fno-inline $*/
/* codeselector produces invalid AM for cmov */

#include <stdio.h>

int k = 20;

int func(void)
{
	k = 42;
	return 1;
}

int main(int argc, char **argv)
{
	int val = k;
	int res;
	if(func()) {
		res = val;
	} else {
		res = 20;
	}
	printf("Res: %d (should be 20)\n", res);
	return 0;
}
