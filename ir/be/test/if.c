#include <stdio.h>
#include <assert.h>

int test1(int a)
{
	return a == 0;
}

int test2(int a, int b)
{
	return a == b;
}

int test2a(int a, int b)
{
	return a != b;
}

int test3(int a, int b)
{
	return a == 0 && b + 1 < 10;
}

int test4(int a, int b)
{
	return a == 0 || b + 1 < 10;
}

int test5(int a, int b)
{
	return a > b ? a : b - 1;
}

int test6(int a, int b)
{
	return a < 0 ? -1 : (a > 0 ? 1 : 0);
}

int test6a(int a, int b)
{
	if(a < b)
		;
	else
		;

	return 0;
}

int test7(int a, int b)
{
	int i, res = 0;

	for(i = 0; i < a; ++i)
		res += i * b;

	return res;
}

int test8(int a, int b, int c)
{
	return a < b ? (a < c ? a : c) : (b < c ? b : c);
}

int test9(int a, int b)
{
	return a ? b : b;
}

int testam(int a, int b, int c)
{
	if(a < 42)
		return b;
	else
		return c;
}

int main()
{
#define TU(t,a,s)	printf("%s(%d) = %d (should be %d)\n", \
                           #t, a, t(a), s); \
                    assert(t(a) == s);

#define TB(t,a,b,s)	printf("%s(%d,%d) = %d (should be %d)\n", \
                            #t, a, b, t(a,b), s); \
                    assert(t(a,b) == s);

#define TT(t,a,b,c,s)	printf("%s(%d,%d,%d) = %d (should be %d)\n", \
                            #t, a, b, c, t(a,b,c), s); \
                    assert(t(a,b,c) == s);


	TU(test1, 0, 1);
	TU(test1, 42, 0);

	TB(test2, 0, 0, 1);
	TB(test2, -23, -23, 1);
	TB(test2, 0, 42, 0);
	TB(test2, -1, 0, 0);

	TB(test2a, 0, 0, 0);
	TB(test2a, -23, -23, 0);
	TB(test2a, 0, 42, 1);
	TB(test2a, -1, 0, 1);

	TB(test3, 0, 8, 1);
	TB(test3, 0, -200, 1);
	TB(test3, 0, 10, 0);
	TB(test3, 42, 8, 0);
	TB(test3, -1, -200, 0);
	TB(test3, -1, 10, 0);

	TB(test4, 0, 8, 1);
	TB(test4, 0, -200, 1);
	TB(test4, 0, 10, 1);
	TB(test4, 42, 8, 1);
	TB(test4, -1, -200, 1);
	TB(test4, -1, 10, 0);

	TB(test5, 42, -1, 42);
	TB(test5, 1, 0, 1);
	TB(test5, 0, 1, 0);
	TB(test5, -1, 42, 41);

	TB(test6, -5, 42, -1);
	TB(test6, -20, -1, -1);
	TB(test6, 20, -1, 1);
	TB(test6a, 42, -1, 0);

	TB(test7, 3, 2, 6);

	TT(test8, 0, 2, 3, 0);
	TT(test8, 0, 2, 1, 0);
	TT(test8, -1, 42, 5, -1);
	TT(test8, -7, 4, -42, -42);
	TT(test8, 0, 2, -1, -1);
	TT(test8, 24, 123, 7, 7);

	TT(testam, -24, 13, 7, 13);
	TT(testam, 102, 13, 7, 7);

	TB(test9, 3, 2, 2);
	TB(test9, -42, -42, -42);

	return 0;
}
