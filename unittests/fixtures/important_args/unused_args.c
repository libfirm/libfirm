// FIRM_IMPORTANT_ARGS: 2

int f(int x, int y);

int f(int x, int y)
{
	(void) x;
	if (y < 0) {
		return 0;
	}
	return 1;
}
