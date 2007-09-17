/*$ -fno-inline $*/
#include <stdio.h>

int k;

int f(int a)
{
	if(k < 20) {
		rand();
		return a < 5 ? 10 : 20;
	} else {
		return a < 5 ? 20 : 0;
	}
}

int main(void)
{
	k = 21;
	printf("Res: %d should be 0\n", f(20));
	return 0;
}
