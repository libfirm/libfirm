// FIRM_IMPORTANT_ARGS: 1

int f(int *p, int x);

int f(int *p, int x)
{
	if (*p + x) return 1;
	return 0;
}
