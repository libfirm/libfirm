/************************************************************************
 * Program:  scalar_product.c
 * Function: Calculates the scalar product of vector lying in memory
 *           Used as a test for the simd optimization.
 * Author:   Andreas Schoesser
 * Date:     2007-06-13
 ************************************************************************/

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <time.h>

float scalar_product(float *a, float *b, unsigned int max_elements);

main()
{
	float res;
	int i, max_elements = 100;
	double  d_zeitdauer;

	// Allocate memory and make sure pointers are aligned to 16 byte addresses
	char *a = malloc(16 + max_elements * sizeof(float));
	char *b = malloc(16 + max_elements * sizeof(float));
	float c;
	char *ca = &a[0] + 16 - (unsigned int) ((unsigned int) &a[0] % 16);
	char *cb = &b[0] + 16 - (unsigned int) ((unsigned int) &b[0] % 16);

	float *aa = (float *) ca;
	float *ab = (float *) cb;

	printf("Scalar product\n==============\n\n");

	//printf("Array Position: %u, %u, %u, %u\n", a, b, aa, ba/*(unsigned int) &aa[0] % 16, (unsigned int) &ba[0] % 16*/);

	// Fill both arrays with random values
	for(i = 0; i < max_elements; i++)
	{
		aa[i] = (float) (rand() % 10);
		ab[i] = (float) (rand() % 10);

		//printf("(%g * %g)  +  ", a[i], b[i]);
	}

	//for(i = 0; i < max_elements - 4; i += 4)
	res = scalar_product(aa, ab, max_elements);

	printf("\nResult: %g\n", res);
}


float scalar_product(float * a, float * b, unsigned int max_elements)
{
	float res;
	int   i;

	/*for(i = 0; i < 4; i++)
	{
		a[i] = (float) (rand() % 10);
		b[i] = (float) (rand() % 10);

		printf("(%g * %g)  +  ", a[i], b[i]);
	}*/

	for(i = 0; i < max_elements; i += 4)
		res += a[i] * b[i] + a[i + 1] * b[i + 1] + a[i + 2] * b[i + 2] + a[i + 3] * b[i + 3];

	return(res);
}
