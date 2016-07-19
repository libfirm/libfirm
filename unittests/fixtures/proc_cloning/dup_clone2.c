int f(int x);
int f(int x)
{
	if (x < 0) {
		return 0;
	}
	return 1;
}

int g(void);
int g(void)
{
	return f(42);
}

int main(void)
{
	return f(42) + g();
}
