// assert_local_important_args("f", 2);

int f(int x, int y);

int f(int x, int y)
{
	(void) x;
	if (y < 0) {
		return 0;
	}
	return 1;
}
