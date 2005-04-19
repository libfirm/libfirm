/**
 * Sparse matrix storage with linked lists for rows and cols.
 * I did not need floats, so this is all integer.
 * @author Daniel Grund
 * @date 07.04.2005
 */

#ifndef _SP_MATRIX_H
#define _SP_MATRIX_H

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
 * Returns the number of rows in this matrix; the height; the first dimension
 */
int matrix_get_rowcount(const sp_matrix_t *m);

/**
 * Returns the number of cols in this matrix; the width; the second dimension
 */
int matrix_get_rowcount(const sp_matrix_t *m);

/**
 * Start iteratation over a row. Elements are returned from left to right.
 * @return NULL if row is empty, else the first element.
 */
matrix_elem_t *matrix_row_first(sp_matrix_t *m, int row);

/**
 * Start iteratation over a column. Elements are returned from top to bottom.
 * @return NULL if column is empty, else the first element.
 */
matrix_elem_t *matrix_col_first(sp_matrix_t *m, int row);

/**
 * @return the next element in iteration order or NULL if iteration is done.
 */
matrix_elem_t *matrix_next(sp_matrix_t *m);

/**
 * m    The matrix
 * r    The row
 * curr The variable to assign all elements to during iteration
 */
#define matrix_foreach_in_row(m,r,curr) \
		for (curr = matrix_row_first(m, r); curr; curr = matrix_next(m))

/**
 * m    The matrix
 * c    The col
 * curr The variable to assign all elements to during iteration
 */
#define matrix_foreach_in_col(m,c,curr) \
		for (curr = matrix_col_first(m, c); curr; curr = matrix_next(m))

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
