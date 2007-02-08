#include <stdio.h>
#include <stdlib.h>

unsigned int
f1 (int diff)
{
	return ((unsigned int) (diff < 0 ? -diff : diff));
}

unsigned int
f2 (unsigned int diff)
{
	return ((unsigned int) ((signed int) diff < 0 ? -diff : diff));
}

unsigned long long
f3 (long long diff)
{
	return ((unsigned long long) (diff < 0 ? -diff : diff));
}

unsigned long long
f4 (unsigned long long diff)
{
	return ((unsigned long long) ((signed long long) diff < 0 ? -diff : diff));
}

int main ()
{
	int i;
	for (i = 0; i <= 10; i++)
	{
#if 0
		if (f1 (i) != i) {
			printf("f1(%d)\n", i);
			abort ();
		}
		if (f1 (-i) != i) {
			printf("f1(%d)\n", -i);
			abort ();
		}
		if (f2 (i) != i) {
			printf("f2(%d)\n", i);
			abort ();
		}
#endif
		if ((int) f2 (-i) != i) {
			printf("f2(%d) -> %d\n", -i, f2(-i));
			abort ();
		}
#if 0
		if (f3 ((long long) i) != i) {
			printf("f3(%lld)\n", i);
			abort ();
		}
		if (f3 ((long long) -i) != i) {
			printf("f3(%lld)\n", -i);
			abort ();
		}
		if (f4 ((long long) i) != i) {
			printf("f4(%lld)\n", i);
			abort ();
		}
		if (f4 ((long long) -i) != i) {
			printf("f4(%d)\n", -i);
			abort ();
		}
#endif
	}
	exit (0);
}
