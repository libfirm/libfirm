/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Sparse matrix storage with linked lists for rows and cols.
 * @author  Daniel Grund
 */
#ifndef LPP_SP_MATRIX_H
#define LPP_SP_MATRIX_H

#include <stdio.h>

/**
 * A matrix element.
 */
typedef struct matrix_elem_t {
	int   row;  /* row index */
	int   col;  /* column index */
	float val;  /* the actual value of the entry */
} matrix_elem_t;

typedef struct sp_matrix_t sp_matrix_t;

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
void matrix_set(sp_matrix_t *m, int row, int col, double val);

/**
 * Sets m[row, cols[0,...,i]] to val for i in (0, ..., num_cols - 1)
 * Following assumptions are done here:
 * - the current row inserted is the last inserted row so far
 * - cols[] is sorted ascending by col number
 */
void matrix_set_row_bulk(sp_matrix_t *m, int row, int *cols, int num_cols, double val);

/**
 * Returns the value stored in m[row, col].
 */
double matrix_get(const sp_matrix_t *m, int row, int col);

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
int matrix_get_colcount(const sp_matrix_t *m);

/**
 * Start iteration over all matrix elements. Row by row, from top to bottom.
 * @return NULL if the matrix is empty, else the first element.
 */
const matrix_elem_t *matrix_first(sp_matrix_t *m);

/**
 * Start iteration over a row. Elements are returned from left to right.
 * @return NULL if row is empty, else the first element.
 */
const matrix_elem_t *matrix_row_first(sp_matrix_t *m, int row);

/**
 * Start iteration over a column. Elements are returned from top to bottom.
 * @return NULL if column is empty, else the first element.
 */
const matrix_elem_t *matrix_col_first(sp_matrix_t *m, int col);

/**
 * @return the next element in iteration order or NULL if iteration is done.
 */
const matrix_elem_t *matrix_next(sp_matrix_t *m);

/**
 * @return the size for a single matrix element
 */
unsigned matrix_get_elem_size(void);

/**
 * m    The matrix
 * curr The variable to assign all elements to during iteration
 * Save against removal of curr
 */
#define matrix_foreach(m,curr) \
		for (matrix_elem_t const *curr = matrix_first(m); curr; curr = matrix_next(m))

/**
 * m    The matrix
 * r    The row
 * curr The variable to assign all elements to during iteration
 * Save against removal of curr
 */
#define matrix_foreach_in_row(m,r,curr) \
		for (matrix_elem_t const *curr = matrix_row_first(m, r); curr; curr = matrix_next(m))

/**
 * m    The matrix
 * c    The col
 * curr The variable to assign all elements to during iteration
 * Save against removal of curr
 */
#define matrix_foreach_in_col(m,c,curr) \
		for (matrix_elem_t const *curr = matrix_col_first(m, c); curr; curr = matrix_next(m))

/**
 * Changes the matrix into an equivalent one with maximal number zero-rows.
 * The only equivalence transformation is:
 * Adding a constant to Qij and subtracting it from Qji
 */
void matrix_optimize(sp_matrix_t *m);

/**
 * Dumps the matrix factor*m to the stream @p out.
 * Remark: I dont need spaces between the elements. So feel free to add
 *         char *seperator to the arguments.
 */
void matrix_dump(sp_matrix_t *m, FILE *out, int factor);

/**
 * Perform a self test with a square matrix of dimensions d.
 */
void matrix_self_test(int d);

#endif
