/**
 * taken from: http://coastal.er.usgs.gov/rvm/toolkit/rvmlibv2.c
 * unknown license
 */

/*------------------------------------------------------*/
/* gauss.c                                              */
/*                                                      */
/* 20 jan 89                                            */
/*                                                      */
/* Now does full pivoting--row and column swapping.     */
/* Requires keeping track of which variable corresponds */
/* to each vector position.                             */
/*                                                      */
/* 18 jan 89                                            */
/* paul o'neill                                         */
/*                                                      */
/* from rob holman's pascal program--geo.pas            */
/*                                                      */
/* Gauss-Jordan procedure to solve even-determined      */
/* square matrix equation Ax = vec,where A is N x N     */
/* and vec is N x 1.  This is taken roughly from        */
/* Menke's book (Geophysical Data Analysis: Discrete    */
/* Inverse Theory--1984 p210 ), but performs actual     */
/* row switching to simplify the programming.           */
/* Partial pivoting is used.                            */
/*------------------------------------------------------*/
#include "gaussjordan.h"

#include "xmalloc.h"
#include <math.h>
#include <stdlib.h>

#define SMALL 0.00001

int firm_gaussjordansolve(double *A, double *vec, unsigned nsize)
{
	double   *scramvec  = XMALLOCN(double,   nsize);
	unsigned *x         = XMALLOCN(unsigned, nsize);
	int       res       = 0;

#define _A(row,col) A[(row)*nsize + (col)]
	/* init x[] */
	for (unsigned i = 0; i < nsize; ++i) {
		x[i] = i;
	}

	/* triangularize A */
	/* ie A has zeros below its diagonal */
	unsigned biggest_r = 0;
	unsigned biggest_c = 0;
	for (unsigned col = 0; col < nsize - 1; ++col) {
		double big = 0;
		/* find the largest left in LRH box */
		for (unsigned row = col; row < nsize; ++row) {
			for (unsigned col2 = col; col2 < nsize; ++col2) {
				double temp = fabs(_A(row,col2));
				if (temp > big) {
					biggest_r = row;
					biggest_c = col2;
					big       = temp; /* largest element left */
				}
			}
		}
		if (big < SMALL) {
			res = -1;
			goto end;
		}

		/* swap rows */
		for (unsigned i = 0; i < nsize; ++i) {
			double temp = _A(col,i);
			_A(col,i) = _A(biggest_r,i);
			_A(biggest_r,i) = temp;
		}
		/* swap vec elements */
		double temp = vec[col];
		vec[col] = vec[biggest_r];
		vec[biggest_r] = temp;

		/* swap columns */
		for (unsigned i = 0; i < nsize; ++i) {
			double temp = _A(i,col);
			_A(i,col) = _A(i,biggest_c);
			_A(i,biggest_c) = temp;
		}
		/* swap unknowns */
		unsigned t = x[col];
		x[col] = x[biggest_c];
		x[biggest_c] = t;

		/* partially annihilate this col */
		/* zero columns below diag */
		for (unsigned row = col+1; row < nsize; ++row) {
			/* changes during calc */
			double temp = _A(row,col) / _A(col,col);

			/* annihilates A[][] */
			for (unsigned i = col; i < nsize; ++i)
				_A(row,i) = _A(row,i) - temp * _A(col,i);

			/* same op on vec */
			vec[row] = vec[row] - temp * vec[col];
		}
	}

	/* back-solve, store solution in scramvec */
	scramvec[nsize - 1] = vec[nsize - 1] / _A(nsize - 1,nsize - 1);

	/* answer needs sorting */
	for (unsigned i = nsize - 1; i-- > 0;) {
		int sum = 0;
		for (unsigned j = i+1; j < nsize; ++j)
			sum += _A(i,j) * scramvec[j];
		scramvec[i] = (vec[i] - sum) / _A(i,i);
	}
	/* reorder unknowns--return in vec */
	for (unsigned i = 0; i < nsize; ++i) {
		unsigned j = x[i];
		vec[j] = scramvec[i];
	}

end:
	free(x);
	free(scramvec);

	return res;
}

#undef _A
