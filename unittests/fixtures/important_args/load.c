// // assert_local_important_args("f", 3);

int f(int *p, int x);

int f(int *p, int x)
{
	if (*p + x) return 1;
	return 0;
}
