int w[] = { 1, 2, 3 };
int *x = w;

int f(long long a)
{
	return x[a];
}

int main(void)
{
	printf("%d (should be 2)\n", f(1));
	return 0;
}
