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

int sub4(int x) {
	return 6 - ~x;
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

int cmp9(int x) {
	return -x == 3;
}

int cmp10(int x) {
	return -x != 3;
}

int and1(int a, int b) {
	return (a|b)&a;
}

int and2(int a, int b) {
	return (a|b) & ~(a&b);
}

int and3(int a) {
	return (a & 2) == 2;
}

int and4(int a) {
	return (a & 2) == 4;
}

int and5(int a) {
	return (a & 2) != 4;
}

int or1(int a) {
	return (a | 2) != 0;
}

int or2(int a) {
	return (a | 7) == 0;
}

int add1(int x) {
	return x + ~x;
}

int shr1(int x) {
	return -(x >> 31);
}

int shrs1(unsigned x) {
	return -(x >> 31);
}

int demorgan1(int a, int b) {
	return (~a) & (~b);
}

int demorgan2(int a, int b) {
	return (~a) | (~b);
}

int eor1(int a, int b) {
	return a & (a ^ b);
}

int shl1(int a) {
	return (a << 3) == (5<<3);
}

int shl2(int a) {
	return (a << 3) == 41;
}

int shr2(unsigned int a) {
	return (a >> 3) == 5;
}

int shr3(unsigned int a) {
	return (a >> 3) == (1 << 29);
}

int shrs2(int a) {
	return (a >> 3) == 5;
}

int shrs3(int a) {
	return (a >> 3) == -5;
}

int shrs4(int a) {
	return (a >> 3) == (1 << 29);
}

int conv1(signed char a) {
	return (int)a < 0;
}

int conv2(unsigned char a) {
	return (int)a > 0;
}

int conv3(signed char a) {
	return (unsigned)a != 0;
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
	TU(sub4, 42, 49);
	TB(cmp1, 42, 17, 0);
	TB(cmp2, 42, 17, 1);
	TB(cmp3, 42, 17, 0);
	TB(cmp4, 42, 17, 1);
	TT(cmp5, 42, 17, -4, 0);
	TT(cmp6, 42, 17, -4, 1);
	TT(cmp7, 42, 17, -4, 0);
	TT(cmp8, 42, 17, -4, 1);
	TU(cmp9, -3, 1);
	TU(cmp10, -3, 0);
	TB(and1, 42, 17, 42);
	TB(and2, 42, 17, 42^17);
	TU(and3, 34, 1);
	TU(add1, -3, -1);
	TU(shr1, -3, 1);
	TU(shrs1, -3, -1);
	TB(demorgan1, 42, 17, ~(42|17));
	TB(demorgan2, 42, 17, ~(42&17));
	TB(eor1, 42, 44, 42&~44);
	TU(shl1, 5, 1);
	TU(shl1, 6, 0);
	TU(shl2, 5, 0);
	TU(shr2, 5<<3, 1);
	TU(shr2, 6<<3, 0);
	TU(shr3, 5, 0);
	TU(shrs2, 5<<3, 1);
	TU(shrs2, 6<<3, 0);
	TU(shrs3, -5<<3, 1);
	TU(shrs3, -6<<3, 0);
	TU(shrs4, 5, 0);
	TU(conv1, 3, 0);
	TU(conv2, 3, 1);
	TU(conv3, 3, 1);
	TU(and4, 7, 0);
	TU(and5, 7, 1);
	TU(or1, 7, 1);
	TU(or2, 7, 0);
}
