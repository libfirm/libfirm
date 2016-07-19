int f(int x);
int f(int x)
{
	if (x < 0) {
		return 0;
	}
	return 1;
}

int main(void)
{
	return f(42) + f(42);
}
