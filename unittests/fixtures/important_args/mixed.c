// FIRM_IMPORTANT_ARGS: 1

int f(int x, int y);

int f(int x, int y)
{
	if (x < 0) {
		return y;
	}
	return y + 1;
}
