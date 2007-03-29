#include <stdio.h>

#define MASK	0x00000020

int ctrl_space_write ()
{
	unsigned int  reg_offset = 160;

	if ((reg_offset & MASK) == MASK)
	{
		return 1;
	}

	return (0);
}

int main (void)
{
	int res = ctrl_space_write();
	printf("Result: %d (should be 1)\n", res);
	return res == 1 ? 0 : 1;
}
