/* Very subtle if conversion bug: print is correct, return value is incorrect */

#ifdef __GNUC__
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE __declspec(noinline)
#endif

static inline int f(unsigned int x)
{
	if (x == 0xFFFFFFFF)
		return 0;
	else
		return x;
}


unsigned int q = 89497;


int NO_INLINE main2(void)
{
	printf("%d = 1\n", f(q) != 0);
	return f(q) != 0;
}


int main(void)
{
	return !main2();
}
