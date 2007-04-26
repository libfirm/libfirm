/**
 * @file
 * @brief solves a system of linear equations
 */
#ifndef FIRM_ADT_GAUSSJORDAN_H
#define FIRM_ADT_GAUSSJORDAN_H

/**
 * solves a system of linear equations and returns 0 if successful
 *
 * @param A    the linear equations as matrix
 * @param b    the result vector, will contain the result if successful
 */
int firm_gaussjordansolve(double *A, double *b, int nsize);

#endif
