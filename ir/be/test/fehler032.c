/* testjmp optimizer invalid */

#include <stdio.h>

void *p = (void*) 0x12345;
void *p2 = 0;

int main()
{
	void *mp = p;
	void *mp2 = p2;
	if(mp && mp2) {
		printf("1\n");
	} else if (mp) {
		printf("2\n");
	} else {
		printf("3\n");
	}
	return 0;
}
