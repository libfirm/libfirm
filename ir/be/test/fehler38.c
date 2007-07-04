#ifdef __GNUC__
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE __declspec(noinline)
#endif


int NO_INLINE f(int y, int z)
{
	int x = 0;
	if (y) x++;
	if (z) x++;
	return x;
}


int main(void)
{
	printf("%d (0)\n", f(0, 0));
	printf("%d (1)\n", f(0, 1));
	printf("%d (1)\n", f(1, 0));
	printf("%d (2)\n", f(1, 1));
	return 0;
}
