// assert_important_args("g", 1);
// assert_important_args("f", 1);

int f(int x);

static int g(int x)
{
	if (x < 0) {
		return 0;
	}
	return 1;
}

int f(int x)
{
	return g(x);
}
