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

int sub0(int x, int y, int z)
{
	return x - (y - z);
}

int sub1(int x, int y)
{
	return x - (y * CONST);
}

int sub2(int x, int y)
{
	return x - -y;
}

int sub3(int x, int y)
{
	return -x - y;
}

int cmp1(int x, int y) {
	return -x == -y;
}

int cmp2(int x, int y) {
	return -x != -y;
}

int cmp3(int x, int y) {
	return ~x == ~y;
}

int cmp4(int x, int y) {
	return ~x != ~y;
}

int cmp5(int x, int y, int z) {
	return x + z == z + y;
}

int cmp6(int x, int y, int z) {
	return x + z != y + z;
}

int cmp7(int x, int y, int z) {
	return x - z == y - z;
}

int cmp8(int x, int y, int z) {
	return z -x != z - y;
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
	TT(sub0, 42, 17, 59, 84);
	TB(sub1, 23, 17, -691);
	TB(sub2, 42, 17, 59);
	TB(sub3, 42, 17, -59);
	TB(cmp1, 42, 17, 0);
	TB(cmp2, 42, 17, 1);
	TB(cmp3, 42, 17, 0);
	TB(cmp4, 42, 17, 1);
	TT(cmp5, 42, 17, -4, 0);
	TT(cmp6, 42, 17, -4, 1);
	TT(cmp7, 42, 17, -4, 0);
	TT(cmp8, 42, 17, -4, 1);
}
