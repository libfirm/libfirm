/*$ -fno-inline $*/
#include <limits.h>
#include <stdio.h>

int f(int a, int b)
{
	return a < 0 & b < 0;
}

int f2(short a, short b)
{
	return a < b && b < a;
}

int f3(short a, short b)
{
	return a < b && b > a;
}

int f4(short a, short b, short c)
{
	return (a <= c) & (b <= c);
}

int g(unsigned a, unsigned b)
{
	return ((a >> 12) | 5) & ((b >> 12) | 5);
}

int g2(unsigned a, unsigned b)
{
	return (a & 5) | (b & 5);
}

int g3(int a, int b, int z)
{
	return (a | z) & (b | z);
}

int af(int a)
{
	return (a ? 1 : 0) && !a;
}

int at(int a)
{
	return (a ? 1 : 0) || !a;
}

int main()
{
#define UOP(func,val,should_be)	{ printf("%s(%d) -> %d (should be %d)\n", #func, val, func(val), should_be); }
#define BOP(func,val1,val2,should_be) { printf("%s(%d,%d) -> %d (should be %d)\n", #func, val1, val2, func(val1,val2), should_be); }
#define TOP(func,val1,val2,val3,should_be) { printf("%s(%d,%d,%d) -> %d (should be %d)\n", #func, val1, val2, val3, func(val1,val2,val3), should_be); }
	BOP(f, 0, 0, 0);
	BOP(f, -1, 0, 0);
	BOP(f, 0, -42, 0);
	BOP(f, -1, 1, 0);
	BOP(f, -42, -23, 1);
	BOP(f, 13, -1, 0);
	BOP(f, -1, -1, 1);
	BOP(f, INT_MIN, INT_MIN, 1);
	BOP(f, INT_MIN, -1, 1);
	BOP(f, -1, INT_MIN, 1);

	BOP(f2, 0, 0, 0);
	BOP(f2, -1, 0, 0);
	BOP(f2, 0, -42, 0);
	BOP(f2, -1, 1, 0);
	BOP(f2, -42, -23, 0);
	BOP(f2, 13, -1, 0);
	BOP(f2, -1, -1, 0);
	BOP(f2, SHRT_MIN, SHRT_MIN, 0);
	BOP(f2, SHRT_MIN, -1, 0);
	BOP(f2, -1, SHRT_MIN, 0);

	BOP(f3, 0, 0, 0);
	BOP(f3, -1, 0, 1);
	BOP(f3, 0, -42, 0);
	BOP(f3, -1, 1, 1);
	BOP(f3, -42, -23, 1);
	BOP(f3, 13, -1, 0);
	BOP(f3, -1, -1, 0);
	BOP(f3, SHRT_MIN, SHRT_MIN, 0);
	BOP(f3, SHRT_MIN, -1, 1);
	BOP(f3, -1, SHRT_MIN, 0);

	TOP(f4, 1, 2, 3, 1);
	TOP(f4, -1, -2, -3, 0);
	TOP(f4, SHRT_MIN, SHRT_MIN, -1, 1);
	TOP(f4, SHRT_MIN, SHRT_MIN, SHRT_MIN, 1);
	TOP(f4, SHRT_MAX, SHRT_MIN, SHRT_MAX, 1);
	TOP(f4, SHRT_MIN, SHRT_MIN, SHRT_MAX, 1);
	TOP(f4, 13, 42, SHRT_MAX, 1);
	TOP(f4, 0, 0, 0, 1);
	TOP(f4, 1, 1, 1, 1);

	BOP(g, UINT_MAX, UINT_MAX, 1048575);
	BOP(g, 0, 0, 5);
	BOP(g, 12345, 54321, 5);

	BOP(g2, UINT_MAX, UINT_MAX, 5);
	BOP(g2, 0, 0, 0);
	BOP(g2, 12345, 54321, 1);

	TOP(g3, 1, 2, 3, 3);
	TOP(g3, -1, -2, -3, -1);
	TOP(g3, INT_MIN, INT_MIN, -1, -1);
	TOP(g3, INT_MIN, INT_MIN, INT_MIN, INT_MIN);
	TOP(g3, INT_MAX, INT_MIN, INT_MAX, INT_MAX);
	TOP(g3, INT_MIN, INT_MIN, INT_MAX, -1);
	TOP(g3, 13, 42, INT_MAX, INT_MAX);
	TOP(g3, 0, 0, 0, 0);
	TOP(g3, 1, 1, 1, 1);

	UOP(af, 0, 0);
	UOP(af, 1, 0);
	UOP(af, 42, 0);
	UOP(af, -1, 0);

	UOP(at, 0, 1);
	UOP(at, 1, 1);
	UOP(at, 42, 1);
	UOP(at, -1, 1);
	return 0;
}
