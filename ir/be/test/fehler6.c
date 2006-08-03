#include <stdio.h>

int main()
{
	int i, n=3 , v, dig, set;

	//printf ("Enter n: ");
	//scanf ("%d", &n);

	v = 1 << n;

	for (i=0; i < v; i++) {
		set = i ^ (i>>1);

		printf(" i: %d  set: %d \n",i,set);
		for (dig=1 << (n-1); dig; dig >>= 1)
		{
			printf("\ni: %d v: %d dig: %d set:%d\n",i,v,dig, set);
			printf (" %d", ((set & dig) ? 1 : 0));
			printf("\ni: %d v: %d dig: %d set:%d\n",i,v,dig, set);
		}
		printf ("\n");
	}

	return 0;
}
