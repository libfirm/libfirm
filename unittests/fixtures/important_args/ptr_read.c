// FIRM_IMPORTANT_ARGS: 3

int f(int *p, int x);

int f(int *p, int x)
{
	return *(p + x);
}
