// FIRM_IMPORTANT_ARGS: 1

int f(int x);

int f(int x)
{
	if (x / 2 < 0) {
		return 0;
	}
	return 1;
}
