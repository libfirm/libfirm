// assert_local_important_args("f", 1);

int f(int x, int y);

int f(int x, int y)
{
	if (x < 0) {
		return y;
	}
	return y + 1;
}
