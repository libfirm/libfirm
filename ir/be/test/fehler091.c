/*$ -fdeconv $*/
/* fronent sometimes produces 16bit operations which the backend can't handle
 * correctly (yet)
 */
#include <stdio.h>

unsigned int k = 1;

int main(void)
{
	unsigned short x = k;

	x += 0xffff;
	x >>= 15;
	if(x == 0) {
		printf("ok\n");
		return 0;
	} else {
		printf("bad\n");
		return 1;
	}
}
