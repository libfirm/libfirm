#ifndef MATRIX_H_
#define MATRIX_H_

#include <stdio.h>

typedef struct _gs_matrix_t gs_matrix_t;

/**
 * Allocate a new matrix and init internal data for a matrix of size
 * row_init X col_init. Matrix cannot grow beyond these init values.
 * All elements are initially (implicitly) set to 0.
 */
gs_matrix_t *gs_new_matrix(int n_init_rows, int n_init_cols);

/**
 * Free space used by matrix m
 */
void gs_delete_matrix(gs_matrix_t *m);

void gs_matrix_assure_row_capacity(gs_matrix_t *m, int row, int min_capacity);

void gs_matrix_trim_row_capacities(gs_matrix_t *m);

void gs_matrix_delete_zero_entries(gs_matrix_t *m);

/**
 * Sets m[row, col] to val
 * @p increase If non-zero @p val is added to the existing
 */
void gs_matrix_set(gs_matrix_t *m, int row, int col, double val);

/**
 * Returns the value stored in m[row, col].
 */
double gs_matrix_get(const gs_matrix_t *m, int row, int col);

/**
 * Performs one step of the Gauss-Seidel algorithm
 * @p m         The iteration matrix
 * @p x         The iteration vector
 * @p a         The dimension of the matrix (axa matrix)
 */
double gs_matrix_gauss_seidel(const gs_matrix_t *m, double *x, int n);

unsigned gs_matrix_get_n_entries(const gs_matrix_t *m);

/**
 * Dumps the matrix factor*m to the stream @p out.
 */
void gs_matrix_dump(const gs_matrix_t *m, int a, int b, FILE *out);

int gs_matrix_get_sizeof_allocated_memory(const gs_matrix_t *m);

void gs_matrix_export(const gs_matrix_t *m, double *nw, int size);

#endif /*MATRIX_H_*/
