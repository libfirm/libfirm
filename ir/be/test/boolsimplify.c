int range0(int a)
{
	return 0 <= a && a < 10;
}

int range1(int a)
{
	return 1 <= a && a < 10;
}

int test_lt_and_lt(int a)
{
	return a < 5 && a < 10;
}

int test_lt_and_eq(int a)
{
	return a < 5 && a == 10;
}

int test_lt_and_gt(int a)
{
	return a < 5 && a > 10;
}

int test_eq_and_lt(int a)
{
	return a == 5 && a < 10;
}

int test_eq_and_eq(int a)
{
	return a == 5 && a == 10;
}

int test_eq_and_gt(int a)
{
	return a == 5 && a > 10;
}

int test_ge_and_lt(int a)
{
	return a >= 5 && a < 6;
}

int test_gt_and_lt(int a)
{
	return a > 5 && a < 6;
}

int test_lt_or_lt(int a)
{
	return a < 5 || a < 10;
}

int test_lt_or_eq(int a)
{
	return a < 5 || a == 5;
}

int test_ne_or_ne(int a)
{
	return a != 5 || a != 10;
}

int main(void)
{
	return 0;
}
