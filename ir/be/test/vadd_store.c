/************************************************************************
 * Program:  vadd_store.c
 * Function: Add 2 vectors (lying in memory) and store the result in
 *           a another vector in memory.
 *           Used as a test for the simd optimization.
 * Author:   Andreas Schoesser
 * Date:     2007-02-13
 ************************************************************************/

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

#if 0
void vadd_store(void)
{
	/*Also possible: Local pointers instead of function parameters: */
	//int *a = (int *) alloca(8), *b = (int *) alloca(8), *c = (int *) alloca(8);
	int a[2], b[2], c[2];
	int i;

	printf("1. vload -> vadd -> vstore\n===================\n\n");

	for(i = 0; i < 2; i++)
	{
		a[i] = rand() % 10;
		b[i] = rand() % 10;
	}


/*  Also possible: Local arrays, but may be optimized so that no pattern is found
	int a[2], b[2], c[2];	   */


/*  Version 1: Directly adding.  */

	c[0] = a[0] + b[0];
	c[1] = a[1] + b[1];



/*	Version 2: Adding of local variables

	int a0, a1, b0, b1, c0, c1;

	a0 = a[0];
	a1 = a[1];
	b0 = b[0];
	b1 = b[1];

	c0 = a0 + b0;
	c1 = a1 + b1;

	c[0] = c0;
	c[1] = c1; */



/*  Version 3: Mixed adding of locals and pointers

	int a0, b1, c0;

	a0 = a[0];
	b1 = b[1];

	c0 = a0 + b[0];
	c[1] = a[1] + b1;

	c[0] = c0; */

	for(i = 0; i < 2; i++)
		printf("%d + %d = %d\n", a[i], b[i], c[i]);
	printf("\n");

}
#endif

#if 1
void vadd_loop(void)
{
	/* Version 4: Manually unrolled loop  */

	int i, j;
	int d[5][5], e[5][5], f[5][5];

	//printf("2. vload -> vadd -> vstore, multi dimensional array, in loop\n==========================================\n\n");

	for(j = 0; j < 4; j++)
		for(i = 0; i < 2; i++)
		{
			e[j][i] = rand() % 10;
			f[j][i] = rand() % 10;
		}

	for(j = 0; j < 4; j++)
		//for(i = 0; i < 2; i ++ )
	{
		d[j][0] = e[j][0] + f[j][0];
		d[j][1] = e[j][1] + f[j][1];
	}

	for(j = 0; j < 4; j++)
	{
		for(i = 0; i < 2; i++)
			printf("%d + %d = %d\n", e[j][i], f[j][i], d[j][i]);
		printf("\n");
	}
}
#endif

int main()
{
	srand(12345);

	//vadd_store();
	vadd_loop();

	return 0;
}
