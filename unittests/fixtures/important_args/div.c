// assert_local_important_args("f", 1);

int f(int x);

int f(int x)
{
	if (x / 2 < 0) {
		return 0;
	}
	return 1;
}
