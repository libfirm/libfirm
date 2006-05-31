#ifndef FIRM_GAUSSJORDAN_H_
#define FIRM_GAUSSJORDAN_H_

/**
 * solves a system of linear equations and returns 0 if successful
 *
 * @param A    the linear equations as matrix
 * @param b    the result vector, will contain the result if successful
 */
int firm_gaussjordansolve(double *A, double *b, int nsize);

#endif
