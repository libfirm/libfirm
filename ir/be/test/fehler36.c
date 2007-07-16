#include <stdio.h>
#include <stdlib.h>

#ifdef __GNUC__
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE __declspec(noinline)
#endif

float NO_INLINE t2()
{
	float a;
	return a + 12.54f;
}

float NO_INLINE t()
{
	exit(0);
}

int main()
{
	t();
	t2();
	return 0;
}
