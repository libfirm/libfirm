/*$ -fno-inline $*/
#include <stdio.h>

#define CONST 42

int mul0(int x)
{
	return -x * CONST;
}

int mul1(int x, int y)
{
	return -x * -y;
}

int mul2(int x, int y, int z)
{
	return -x * (y - z);
}

int mul3(int x, int y, int z)
{
	return (x - y) * z;
}

int main(void)
{
#define TU(func,x,expect) \
	printf("%s(%d) = %d (should be %d)\n", #func, x, func(x), expect);
#define TB(func,x,y,expect) \
	printf("%s(%d,%d) = %d (should be %d)\n", #func, x, y, func(x,y), expect);
#define TT(func,x,y,z,expect) \
	printf("%s(%d,%d,%d) = %d (should be %d)\n", #func, x, y, z, func(x,y,z), expect);

	TU(mul0, 3, -126);
	TB(mul1, 20, 3, 60);
	TT(mul2, 9, 2, 5, 27);
	TT(mul3, 5, 2, 9, 27);
}
