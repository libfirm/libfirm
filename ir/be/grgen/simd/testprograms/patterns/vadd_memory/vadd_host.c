#include <stdio.h>
#include <malloc.h>

//int a[4], b[4], c[4];
//int *a, *b, *c;

void vadd_host(int *a, int *b, int *c)
{
	//int *a = (int *) malloc(8), *b = (int *) malloc(8), *c = (int *) malloc(8);
	//int a[2], b[2], c[2];


	c[0] = a[0] + b[0];
	c[1] = a[0] + b[1];

	printf("%d %d", c[0], c[1]);
}

/*void test_cond(int n, int *a, int *b)
{
	if(n > 10)
	{
		a[0] = b[1];
		a[1] = b[0];
	}
	else
	{
		a[1] = b[1];
		a[0] = b[0];
	}
} */
