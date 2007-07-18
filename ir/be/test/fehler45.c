#include <stdio.h>

struct bitfield {
	unsigned int code : 8;
	unsigned int bit1 : 1;
	unsigned int bit2 : 1;
	unsigned int bit3 : 1;
} bf = {7, 1, 0, 1 };

int main()
{
	printf("Res: %d (should be 7)\n", bf.code);
	return 0;
}
