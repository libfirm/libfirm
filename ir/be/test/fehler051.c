/************************************************************************
* Program:  maxps.c
* Function: Add 2 vectors (lying in memory) and store the result in
*           a another vector in memory.
*           Used as a test for the simd optimization.
* Author:   Andreas Schoesser
* Date:     2007-02-13
************************************************************************/

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

float maxps();
float ueberlappung();
//void vadd_loop();
//void array_test(int *a[]);

int main()
{
	int a[5][5];

	a[1][1] = 20;

	srand(12345);

	printf("1. vload -> vadd -> vstore\n===================\n\n");
	ueberlappung();

	printf("2. vload -> vadd -> vstore, multi dimensional array, in loop\n==========================================\n\n");
	//	vadd_loop();

	//	array_test(a);

	return 0;
}

float ueberlappung()
{
	float a[4], b[4], c[4], d[4];
	float a0, a1, a2, a3;
	float b0, b1, b2, b3;
	float c0, c1, c2, c3;
	float sp1, sp2;
	int i;

	for(i = 0; i < 4; i++)
	{
		a[i] = rand() % 10;
		b[i] = rand() % 10;
		c[i] = rand() % 10;
		d[i] = rand() % 10;
	}



	// find vload 2x
	sp1 = a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];

	// find vmul
	sp2 = b[0] * d[0] + b[1] * d[1] + b[2] * d[2] + b[3] * d[3];

	// Usage to prevent optimizations other than SIMD opt
	for(i = 0; i < 4; i++)
		printf("%f %f %f %f\n", a[i], b[i], c[i], d[i]);
	printf("\n");
	return(sp1 + sp2);
}
