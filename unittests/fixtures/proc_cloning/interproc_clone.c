
static int g(int x)
{
	if (x < 0) {
		return 0;
	}
	return 1;
}

static int f(int x)
{
	return g(x);
}

int main(void)
{
	return f(42);
}
