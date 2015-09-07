/**
 * @file
 * @brief solves a system of linear equations
 */
#ifndef FIRM_ADT_GAUSSJORDAN_H
#define FIRM_ADT_GAUSSJORDAN_H

#include "../begin.h"

/**
 * @ingroup algorithms
 * @defgroup gassjordan  Gauss Jordan Elimination
 * Solves a system of linear equations
 * @{
 */

/**
 * Solves a system of linear equations.
 *
 * @param matrix  the linear equations as matrix (square matrix, \p n x \p n)
 * @param result  the result vector, will contain the result if successful
 * @param n       the size of the equation system
 * @returns  0 if successful, -1 if ill-conditioned matrix
 */
FIRM_API int firm_gaussjordansolve(double *matrix, double *result, unsigned n);

/** @} */

#include "../end.h"

#endif
