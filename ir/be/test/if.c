#if 1
int test1(int a)
{
	return a == 0;
}

int test2(int a, int b)
{
	return a == b;
}
#endif

int test3(int a, int b)
{
	return a == 0 && b + 1 < 10;
}

#if 1
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
	// return a > 0 ? 1 : (a < 0 ? -1 : 0);
	return a < 0 ? -1 : (a > 0 ? 1 : 0);
	//return a == 0 ? 0 : (a > 0 ? 1 : -1);
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
#endif

int main()
{
	return 0;
}
