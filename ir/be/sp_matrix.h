/**
 * Author:      Daniel Grund
 * Date:		07.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.

 * Sparse matrix storage with linked lists for rows and cols.
 * I did not need floats, so this is all integer.
 */

#ifndef _SP_MATRIX_H
#define _SP_MATRIX_H

#include <stdio.h>

typedef struct _matrix_elem_t {
	int row, col, val;
} matrix_elem_t;

typedef struct _sp_matrix_t sp_matrix_t;

/**
 * Allocate a new matrix and init internal data for a matrix of size
 * row_init X col_init. Matrix cannot grow beyond these init values.
 * All elements are initially (implicit) set to 0.
 */
sp_matrix_t *new_matrix(int rows, int cols);

/**
 * Free space used by matrix m
 */
void del_matrix(sp_matrix_t *m);

/**
 * Sets m[row, col] to val
 */
void matrix_set(sp_matrix_t *m, int row, int col, int val);

/**
 * Returns the value stored in m[row, col].
 */
int matrix_get(const sp_matrix_t *m, int row, int col);

/**
 * Returns the number of (not-0-)entries.
 */
int matrix_get_entries(const sp_matrix_t *m);

/**
 * Returns the number of rows in this matrix; the height; the first dimension
 */
int matrix_get_rowcount(const sp_matrix_t *m);

/**
 * Returns the number of cols in this matrix; the width; the second dimension
 */
int matrix_get_rowcount(const sp_matrix_t *m);

/**
 * Start iteration over all matrix elements. Row by row, from top to bottom.
 * @return NULL if the matrix is empty, else the first element.
 */
const matrix_elem_t *matrix_first(sp_matrix_t *m);

/**
 * Start iteratation over a row. Elements are returned from left to right.
 * @return NULL if row is empty, else the first element.
 */
const matrix_elem_t *matrix_row_first(sp_matrix_t *m, int row);

/**
 * Start iteratation over a column. Elements are returned from top to bottom.
 * @return NULL if column is empty, else the first element.
 */
const matrix_elem_t *matrix_col_first(sp_matrix_t *m, int row);

/**
 * @return the next element in iteration order or NULL if iteration is done.
 */
const matrix_elem_t *matrix_next(sp_matrix_t *m);

/**
 * m    The matrix
 * curr The variable to assign all elements to during iteration
 * Save against removal of curr
 */
#define matrix_foreach(m,curr) \
		for (curr = matrix_first(m); curr; curr = matrix_next(m))

/**
 * m    The matrix
 * r    The row
 * curr The variable to assign all elements to during iteration
 * Save against removal of curr
 */
#define matrix_foreach_in_row(m,r,curr) \
		for (curr = matrix_row_first(m, r); curr; curr = matrix_next(m))

/**
 * m    The matrix
 * c    The col
 * curr The variable to assign all elements to during iteration
 * Save against removal of curr
 */
#define matrix_foreach_in_col(m,c,curr) \
		for (curr = matrix_col_first(m, c); curr; curr = matrix_next(m))

/**
 * Changes the matrix into an equivalent one with maximal number zero-rows.
 * The only equivalence transformation is:
 * 	Adding a constant to Qij and substracting it from Qji
 */
void matrix_optimize(sp_matrix_t *m);

/**
 * Dumps the matrix factor*m to the stream @p out.
 * Remark: I dont need spaces between the elements. So feel free to add
 *         char *seperator to the arguments.
 */
void matrix_dump(sp_matrix_t *m, FILE *out, int factor);

/**
 * Perform a self test with a sqare matrix of dimensions d.
 */
void matrix_self_test(int d);

#endif
