/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Sparse matrix storage with linked lists for rows and cols.
 * @author  Daniel Grund
 *
 * Sparse matrix storage with linked lists for rows and cols.
 * Matrix is optimized for left-to-right and top-to-bottom access.
 * Complexity is O(1) then.
 * Random access or right-to-left and bottom-to-top is O(m*n).
 */
#include "sp_matrix.h"

#include "bitset.h"
#include "panic.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum iter_direction_t {
	down, right, all
} iter_direction_t;

/**
 * Embedded list pointer.
 */
typedef struct sp_matrix_list_head_t {
	struct sp_matrix_list_head_t *next;
} sp_matrix_list_head_t;

/**
 * A matrix entry.
 */
typedef struct entry_t {
	sp_matrix_list_head_t col_chain; /**< points to next element in same column */
	sp_matrix_list_head_t row_chain; /**< points to next element in same row */
	matrix_elem_t         e;         /**< The actual element */
} entry_t;

struct sp_matrix_t {
	/* These specify the dimensions of the matrix.
	 * They equal the largest values ever used in matrix_set */
	int maxrow, maxcol;
	/* These are the dimensions of allocated arrays below.
	 * rowc >= maxrow and colc >= maxcol hold. */
	int rowc, colc;
	/* number of entries */
	int entries;
	/* arrays of sp_matrix_list_head* as entry-points to rows and cols */
	sp_matrix_list_head_t **rows, **cols;
	/* for iteration: first is to remember start-point;
	 *                last was returned just before
	 *                next is used in case last was removed from list */
	iter_direction_t dir;
	sp_matrix_list_head_t *first, *last, *next;
	int iter_row; /* used for iteration over all elements */
	/* for each column the last inserted element in col list */
	sp_matrix_list_head_t **last_col_el;
	/* for each row the last inserted element in row list */
	sp_matrix_list_head_t **last_row_el;
};

#define SP_MATRIX_INIT_LIST_HEAD(ptr) do { (ptr)->next = NULL; } while (0)

#define _offsetof(type,member) ((char *) &(((type *) 0)->member) - (char *) 0)
#define _container_of(ptr,type,member) ((type *) ((char *) (ptr) - _offsetof(type, member)))

#define is_empty_row(row) (row > m->maxrow || (m->rows[row])->next == NULL)
#define is_empty_col(col) (col > m->maxcol || (m->cols[col])->next == NULL)

#define list_entry_by_col(h) (&_container_of(h, entry_t, col_chain)->e)
#define list_entry_by_row(h) (&_container_of(h, entry_t, row_chain)->e)

/**
 * Returns the size of a single matrix element.
 */
unsigned matrix_get_elem_size(void)
{
	return sizeof(entry_t);
}

/**
 * Returns the new size for an array of size old_size,
 *  which must at least store an entry at position min.
 */
static inline int m_new_size(int old_size, int min)
{
	unsigned bits = 0;
	(void)old_size;
	assert(min >= old_size);
	while (min > 0) {
		min >>= 1;
		bits++;
	}
	assert(bits < sizeof(min) * 8 - 1);
	return 1 << bits;
}

/**
 * Allocates space for @p count entries in the rows array and
 * initializes all entries from @p start to the end.
 */
static inline void m_alloc_row(sp_matrix_t *m, int start, int count)
{
	int p;

	m->rowc        = count;
	m->rows        = XREALLOC(m->rows, sp_matrix_list_head_t *, m->rowc);
	m->last_row_el = XREALLOC(m->last_row_el, sp_matrix_list_head_t *, m->rowc);

	for (p = start; p < m->rowc; ++p) {
		m->rows[p] = XMALLOC(sp_matrix_list_head_t);
		SP_MATRIX_INIT_LIST_HEAD(m->rows[p]);
		m->last_row_el[p] = m->rows[p];
	}
}

/**
 * Allocates space for @p count entries in the cols array and
 * initializes all entries from @p start to the end.
 */
static inline void m_alloc_col(sp_matrix_t *m, int start, int count)
{
	int p;

	m->colc        = count;
	m->cols        = XREALLOC(m->cols, sp_matrix_list_head_t*, m->colc);
	m->last_col_el = XREALLOC(m->last_col_el, sp_matrix_list_head_t*, m->colc);

	for (p = start; p < m->colc; ++p) {
		m->cols[p] = XMALLOC(sp_matrix_list_head_t);
		SP_MATRIX_INIT_LIST_HEAD(m->cols[p]);
		m->last_col_el[p] = m->cols[p];
	}
}

/**
 * Searches in row @p row for the matrix element m[row, col], starting at element @p start.
 * @return If the element exists:
 *            Element m[row, col] and @p prev points to the sp_matrix_list_head in the entry_t holding the element.
 *         Else: NULL and @p prev points to the sp_matrix_list_head after which the element would be inserted.
 *         @p prev_prev always points to the previous element of @p prev
 */
static inline matrix_elem_t *m_search_in_row_from(const sp_matrix_t *m,
                                                  int row, int col,
                                                  sp_matrix_list_head_t *start,
                                                  sp_matrix_list_head_t **prev,
                                                  sp_matrix_list_head_t **prev_prev)
{
	sp_matrix_list_head_t *row_start;
	matrix_elem_t         *res = NULL;

	row_start = m->rows[row];
	*prev     = start;

	while ((*prev)->next != NULL && list_entry_by_row((*prev)->next)->col <= col) {
		(*prev_prev) = (*prev);
		*prev        = (*prev)->next;
	}

	if (*prev != row_start) {
		matrix_elem_t *me = list_entry_by_row(*prev);

		if (me->row == row && me->col == col)
			res = me;
	}

	if (res) {
		m->last_row_el[row] = *prev;
	}

	return res;
}

/**
 * Searches in row @p row for the matrix element m[row, col].
 * @return If the element exists:
 *            Element m[row, col] and @p prev points to the sp_matrix_list_head in the entry_t holding the element.
 *         Else: NULL and @p prev points to the sp_matrix_list_head after which the element would be inserted.
 *         @p prev_prev always points to the previous element of @p prev
 */
static inline matrix_elem_t *m_search_in_row(const sp_matrix_t *m,
                                             int row, int col,
                                             sp_matrix_list_head_t **prev,
                                             sp_matrix_list_head_t **prev_prev)
{
	sp_matrix_list_head_t *start = m->rows[row];

	*prev_prev = NULL;

	if (m->last_row_el[row] != start) {
		matrix_elem_t *el = list_entry_by_row(m->last_row_el[row]);
		if (el->col < col) {
			*prev_prev = start = m->last_row_el[row];
		}
	}

	return m_search_in_row_from(m, row, col, start, prev, prev_prev);
}

/**
 * Searches in col @p col for the matrix element m[row, col], starting at @p start.
 * @return If the element exists:
 *            Element m[row, col] and @p prev points to the sp_matrix_list_head in the entry_t holding the element.
 *         Else: NULL and @p prev points to the sp_matrix_list_head after which the element would be inserted.
 *         @p prev_prev always points to the previous element of @p prev
 */
static inline matrix_elem_t *m_search_in_col_from(const sp_matrix_t *m,
                                                  int row, int col,
                                                  sp_matrix_list_head_t *start,
                                                  sp_matrix_list_head_t **prev,
                                                  sp_matrix_list_head_t **prev_prev)
{
	sp_matrix_list_head_t *col_start;
	matrix_elem_t         *res = NULL;

	col_start = m->cols[col];
	*prev     = start;

	while ((*prev)->next != NULL && list_entry_by_col((*prev)->next)->row <= row) {
		*prev_prev = (*prev);
		*prev      = (*prev)->next;
	}

	if (*prev != col_start) {
		matrix_elem_t *me = list_entry_by_col(*prev);

		if (me->row == row && me->col == col)
			res = me;
	}

	if (res) {
		m->last_col_el[col] = *prev;
	}

	return res;
}

/**
 * Searches in col @p col for the matrix element m[row, col].
 * @return If the element exists:
 *            Element m[row, col] and @p prev points to the sp_matrix_list_head in the entry_t holding the element.
 *         Else: NULL and @p prev points to the sp_matrix_list_head after which the element would be inserted.
 *         @p prev_prev always points to the previous element of @p prev
 */
static inline matrix_elem_t *m_search_in_col(const sp_matrix_t *m,
                                             int row, int col,
                                             sp_matrix_list_head_t **prev,
                                             sp_matrix_list_head_t **prev_prev)
{
	sp_matrix_list_head_t *start = m->cols[col];

	*prev_prev = NULL;

	if (m->last_col_el[col] != start) {
		matrix_elem_t *el = list_entry_by_col(m->last_col_el[col]);
		if (el->row < row) {
			*prev_prev = start = m->last_col_el[col];
		}
	}

	return m_search_in_col_from(m, row, col, start, prev, prev_prev);
}

sp_matrix_t *new_matrix(int row_init, int col_init)
{
	sp_matrix_t *res = XMALLOCZ(sp_matrix_t);
	res->maxrow = -1;
	res->maxcol = -1;
	m_alloc_row(res, 0, MAX(0, row_init));
	m_alloc_col(res, 0, MAX(0, col_init));
	return res;
}

void del_matrix(sp_matrix_t *m)
{
	int i;

	for (i = 0; i < m->rowc; ++i) {
		if (! is_empty_row(i)) {
			entry_t *e;
			sp_matrix_list_head_t *n;

			n = m->rows[i]->next;
			do {
				/* get current matrix element */
				e = _container_of(n, entry_t, row_chain);
				n = n->next;
				free(e);
			} while (n != NULL);

		}
		free(m->rows[i]);
	}
	for (i = 0; i < m->colc; ++i)
		free(m->cols[i]);
	free(m->last_col_el);
	free(m->last_row_el);
	free(m->rows);
	free(m->cols);
	free(m);
}

void matrix_set(sp_matrix_t *m, int row, int col, double val)
{
	matrix_elem_t *me = NULL;
	entry_t       *entr;
	sp_matrix_list_head_t *leftof      = NULL;
	sp_matrix_list_head_t *above       = NULL;
	sp_matrix_list_head_t *prev_leftof = NULL;
	sp_matrix_list_head_t *prev_above  = NULL;

	/* if necessary enlarge the matrix */
	if (row > m->maxrow) {
		m->maxrow = row;
		if (row >= m->rowc)
			m_alloc_row(m, m->rowc, m_new_size(m->rowc, row));
	}
	if (col > m->maxcol) {
		m->maxcol = col;
		if (col >= m->colc)
			m_alloc_col(m, m->colc, m_new_size(m->colc, col));
	}

	/* search for existing entry */
	if (m->maxrow < m->maxcol)
		me = m_search_in_col(m, row, col, &above, &prev_above);
	else
		me = m_search_in_row(m, row, col, &leftof, &prev_leftof);

	/* if it exists, set the value and return */
	if (me) {
		if (val != 0) {
			me->val = (float)val;
		} else {
			entr = _container_of(me, entry_t, e);

			/* remove row_chain entry */
			if (prev_leftof)
				prev_leftof->next = entr->row_chain.next;
			else
				m->rows[row]->next = entr->row_chain.next;

			/* remove col_chain entry */
			if (prev_above)
				prev_above->next = entr->col_chain.next;
			else
				m->cols[col]->next = entr->col_chain.next;

			entr->row_chain.next = NULL;
			entr->col_chain.next = NULL;

			/* set the last pointer to the "previous" element */
			if (m->last_col_el[col] == &entr->col_chain ||
				m->last_row_el[row] == &entr->row_chain)
			{
				m->last_col_el[col] = prev_above  ? prev_above  : m->cols[col];
				m->last_row_el[row] = prev_leftof ? prev_leftof : m->rows[row];
			}

			free(entr);
			m->entries--;
		}
		return;
	}

	/* if it does not exist and 0 should be set just quit */
	if (val == 0)
		return;

	/* if it does not exist and val != 0 search the other direction */
	if (m->maxrow >= m->maxcol)
		m_search_in_col(m, row, col, &above, &prev_above);
	else
		m_search_in_row(m, row, col, &leftof, &prev_leftof);
	/* now leftof and above are the entry_t's prior the new one in each direction */

	/* insert new entry */
	entr        = XMALLOC(entry_t);
	entr->e.row = row;
	entr->e.col = col;
	entr->e.val = (float)val;

	/* add row_chain entry */
	entr->row_chain.next = leftof->next;
	leftof->next = &entr->row_chain;

	/* add col_chain entry */
	entr->col_chain.next = above->next;
	above->next = &entr->col_chain;

	m->last_col_el[col] = &entr->col_chain;
	m->last_row_el[row] = &entr->row_chain;

	m->entries++;
}

void matrix_set_row_bulk(sp_matrix_t *m, int row, int *cols, int num_cols, double val)
{
	matrix_elem_t *me = NULL;
	entry_t       *entr;
	int           i;
	sp_matrix_list_head_t *leftof, *above;
	sp_matrix_list_head_t *prev_leftof, *prev_above;

	/* if necessary enlarge the matrix */
	if (row > m->maxrow) {
		m->maxrow = row;
		if (row >= m->rowc)
			m_alloc_row(m, m->rowc, m_new_size(m->rowc, row));
	}
	if (cols[num_cols - 1] > m->maxcol) {
		m->maxcol = cols[num_cols - 1];
		if (cols[num_cols - 1] >= m->colc)
			m_alloc_col(m, m->colc, m_new_size(m->colc, cols[num_cols - 1]));
	}

    /* set start values */
	prev_above  = NULL;
	prev_leftof = NULL;

	for (i = 0; i < num_cols; ++i) {
		/* search for existing entry */
		me = m_search_in_row(m, row, cols[i], &leftof, &prev_leftof);

		/* if it exists, set the value and return */
		if (me) {
			if (val != 0) {
				me->val = (float)val;
			} else {
				entr = _container_of(me, entry_t, e);

				/* remove row_chain entry */
				if (prev_leftof)
					prev_leftof->next = entr->row_chain.next;
				else
					m->rows[row]->next = entr->row_chain.next;

				/* remove col_chain entry */
				if (prev_above)
					prev_above->next = entr->col_chain.next;
				else
					m->cols[cols[i]]->next = entr->col_chain.next;

				entr->row_chain.next = NULL;
				entr->col_chain.next = NULL;

				/* set the last pointer to the "previous" element */
				if (m->last_col_el[cols[i]] == &entr->col_chain ||
					m->last_row_el[row]     == &entr->row_chain)
				{
					m->last_col_el[cols[i]] = prev_above  ? prev_above  : m->cols[cols[i]];
					m->last_row_el[row]     = prev_leftof ? prev_leftof : m->rows[row];
				}

				free(entr);
				m->entries--;
			}

			continue;
		}

		/* if it does not exist and 0 should be set just quit */
		if (val == 0)
			continue;

		/* we have to search the col list as well, to get the above pointer */
		m_search_in_col(m, row, cols[i], &above, &prev_above);

		/* now leftof and above are the entry_t's prior the new one in each direction */

		/* insert new entry */
		entr        = XMALLOC(entry_t);
		entr->e.row = row;
		entr->e.col = cols[i];
		entr->e.val = (float)val;

		m->last_col_el[cols[i]] = &entr->col_chain;
		m->last_row_el[row]     = &entr->row_chain;

		/* add row_chain entry */
		entr->row_chain.next = leftof->next;
		leftof->next = &entr->row_chain;

		/* add col_chain entry */
		entr->col_chain.next = above->next;
		above->next = &entr->col_chain;

		m->entries++;
	}
}

double matrix_get(const sp_matrix_t *m, int row, int col)
{
	sp_matrix_list_head_t *dummy, *dummy2;
	matrix_elem_t *me;

	if (is_empty_row(row) || is_empty_col(col))
		return 0.0;

	if (m->maxrow < m->maxcol)
		me = m_search_in_col(m, row, col, &dummy, &dummy2);
	else
		me = m_search_in_row(m, row, col, &dummy, &dummy2);

	if (me)
		assert(me->col == col && me->row == row);

	return me ? me->val : 0.0;
}

int matrix_get_entries(const sp_matrix_t *m)
{
	return m->entries;
}

int matrix_get_rowcount(const sp_matrix_t *m)
{
	return m->maxrow + 1;
}

int matrix_get_colcount(const sp_matrix_t *m)
{
	return m->maxcol + 1;
}

const matrix_elem_t *matrix_row_first(sp_matrix_t *m, int row)
{
	if (is_empty_row(row))
		return NULL;

	m->dir   = right;
	m->first = m->rows[row];
	m->last  = m->first->next;
	m->next  = m->last ? m->last->next : NULL;

	assert (list_entry_by_row(m->last)->row == row);

	return list_entry_by_row(m->last);
}

const matrix_elem_t *matrix_col_first(sp_matrix_t *m, int col)
{
	if (is_empty_col(col))
		return NULL;

	m->dir   = down;
	m->first = m->cols[col];
	m->last  = m->first->next;
	m->next  = m->last ? m->last->next : NULL;

	assert (list_entry_by_col(m->last)->col == col);

	return list_entry_by_col(m->last);
}

static inline const matrix_elem_t *matrix_first_from(sp_matrix_t *m, int startrow)
{
	const matrix_elem_t *res;
	int i;

	for (i = startrow; i <= m->maxrow; ++i) {
		res = matrix_row_first(m, i);
		if (res) {
			m->iter_row = i;
			m->dir      = all;
			return res;
		}
	}

	return NULL;
}

const matrix_elem_t *matrix_first(sp_matrix_t *m)
{
	return matrix_first_from(m, 0);
}

const matrix_elem_t *matrix_next(sp_matrix_t *m)
{
	assert(m->first && "Start iteration with matrix_???_first, before calling me!");

	if (m->next == NULL) {
		if (m->dir == all)
			return matrix_first_from(m, ++m->iter_row);
		else
			return NULL;
	}

	m->last = m->next;
	m->next = m->next->next;

	if (m->dir == down)
		return list_entry_by_col(m->last);
	else /* right or all */
		return list_entry_by_row(m->last);
}

static int cmp_count(const void *e1, const void *e2)
{
	return (int *)e2 - (int *)e1;
}

static inline void matrix_fill_row(sp_matrix_t *m, int row, bitset_t *fullrow)
{
	bitset_set(fullrow, row);
	matrix_foreach_in_col(m, row, e) {
		if (! bitset_is_set(fullrow, e->row)) {
			assert(0.0 == matrix_get(m, e->col, e->row));
			matrix_set(m, e->col, e->row, e->val);
			matrix_set(m, e->row, e->col, 0.0);
		}
	}
}

void matrix_optimize(sp_matrix_t *m)
{
	int i, size, redo;
	int *c;
	bitset_t *fullrow;

	size = MAX(m->maxcol, m->maxrow)+1;

	/* kill all double-entries (Mij and Mji are set) */
	matrix_foreach(m, e) {
		double t_val;

		assert(e->row != e->col && "Root has itself as arg. Ok. But the arg (=root) will always have the same color as root");
		t_val = matrix_get(m, e->col, e->row);
		if (fabs(t_val) > 1e-10) {
			matrix_set(m, e->col, e->row, 0);
			matrix_set(m, e->row, e->col, e->val + t_val);
		}
	}

	c       = ALLOCAN(int, size);
	redo    = 1;
	fullrow = bitset_alloca(size);

	/* kill 'all' rows containing only 1 entry */
	while (redo) {
		redo = 0;
		/* count elements in rows */
		memset(c, 0, size * sizeof(*c));

		matrix_foreach(m, e)
			c[e->row]++;

		for (i = 0; i<size; ++i)
			if (c[i] == 1 && ! bitset_is_set(fullrow, i)) {
				redo = 1;
				/* if the other row isn't empty move the e in there, else fill e's row */
				matrix_elem_t const *const e = matrix_row_first(m, i);
				if (e) {
					if (c[e->col] > 0)
						matrix_fill_row(m, e->col, fullrow);
					else
						matrix_fill_row(m, e->row, fullrow);
				}
			}
	}


	memset(c, 0, size * sizeof(*c));
	matrix_foreach(m, e)
		c[e->row]++;

	QSORT(c, size, cmp_count);

	for (i = 0; i < size; ++i) {
		if (! bitset_is_set(fullrow, i))
			matrix_fill_row(m, i, fullrow);
	}
}

void matrix_dump(sp_matrix_t *m, FILE *out, int factor)
{
	int i, o, last_idx;

	for (i = 0; i <= m->maxrow; ++i) {
		last_idx = -1;
		matrix_foreach_in_row(m, i, e) {
			for (o = last_idx + 1; o < e->col; ++o)
				fprintf(out, " %4.1f" , 0.0);

			fprintf(out, " %4.1f", factor * e->val);
			last_idx = e->col;
		}

		for (o = last_idx + 1; o <= m->maxcol; ++o)
			fprintf(out, " %4.1f" , 0.0);

		fprintf(out, "\n");
	}
}

void matrix_self_test(int d)
{
	sp_matrix_t *m = new_matrix(10, 10);

	for (int i = 0; i < d; ++i) {
		for (int o = 0; o < d; ++o) {
			matrix_set(m, i, o, i*o);
		}
	}

	for (int i = 0; i < d; ++i) {
		for (int o = 0; o<d; ++o) {
			if (matrix_get(m, i, o) != i*o)
				panic("matrix_get/set failed");
		}
	}

	int i = 1;
	matrix_foreach_in_row(m,1,e) {
		if (e->val != i)
			panic("matrix row iter failed");
		++i;
	}
	if (matrix_next(m))
		panic("matrix_foreach ended early");

	i = d-1;
	matrix_foreach_in_col(m,d-1,e) {
		if (e->val != i)
			panic("matrix foreach col failed");
		i += d-1;
	}
	if (matrix_next(m))
		panic("matrix_foreach_col ended early");
	del_matrix(m);

	m = new_matrix(16,16);
	matrix_set(m, 1,1,9);
	matrix_set(m, 1,2,8);
	matrix_set(m, 1,3,7);
	matrix_set(m, 1,3,6);
	matrix_set(m, 1,2,5);
	matrix_set(m, 1,1,4);
	i = 1;
	matrix_foreach_in_row(m, 1, e) {
		if (e->row != 1 || e->col != i || e->val != i+3)
			panic("matrix get/set 2 failed");
		i++;
	}
	if (i != 4)
		panic("matrix foreach 2 failed");
	del_matrix(m);

	m = new_matrix(5,5);
	matrix_set(m, 1,1,1);
	matrix_set(m, 2,2,2);
	matrix_set(m, 3,3,3);
	matrix_set(m, 3,5,4);
	matrix_set(m, 4,4,5);
	matrix_set(m, 5,5,6);
	i = 0;
	matrix_foreach(m, e) {
		if (e->val != ++i)
			panic("matrix foreach failed");
	}
	if (i != 6)
		panic("matrix_foreach failed");
	matrix_set(m, 1,1,0);
	if (matrix_get_entries(m) != 5)
		panic("matrix get/set 3 failed");
	del_matrix(m);
}
