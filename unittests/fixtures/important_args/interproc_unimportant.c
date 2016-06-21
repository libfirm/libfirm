// assert_important_args("g", 0);
// assert_important_args("f", 0);

int f(int x);

static int g(int x)
{
	return x + 1;
}

int f(int x)
{
	return g(x);
}
