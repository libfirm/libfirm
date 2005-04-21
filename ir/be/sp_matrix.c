/**
 * Sparse matrix storage with linked lists for rows and cols.
 * I did not need floats, so this is all integer.
 * @author Daniel Grund
 * @date 07.04.2005
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "sp_matrix.h"
#include "list.h"

typedef enum _iter_direction_t {
	down, right
} iter_direction_t;

typedef struct _entry_t {
	struct list_head col_chain; /**< points to next element in same column */
	struct list_head row_chain; /**< points to next element in same row */
	matrix_elem_t e;
} entry_t;

struct _sp_matrix_t {
	/* These specify the dimensions of the matrix.
	 * They equal the largest values ever used in matrix_set */
	int maxrow, maxcol;
	/* These are the dimensions of allocated arrays below.
	 * rowc >= maxrow and colc >= maxcol hold. */
	int rowc, colc;
	/* arrays of list_head* as entry-points to rows and cols */
	struct list_head **rows, **cols;
	/* for iteration */
	iter_direction_t dir;
	struct list_head *first, *last; /* first is to remember start-point; last was returned just before */
};

#define _offsetof(type,member) ((char *) &(((type *) 0)->member) - (char *) 0)
#define _container_of(ptr,type,member) ((type *) ((char *) (ptr) - _offsetof(type, member)))
#define is_empty_row(row) (row>m->maxrow || list_empty(m->rows[row]))
#define is_empty_col(col) (col>m->maxcol || list_empty(m->cols[col]))
#define list_entry_by_col(h) (&list_entry(h, entry_t, col_chain)->e)
#define list_entry_by_row(h) (&list_entry(h, entry_t, row_chain)->e)

/**
 * Returns the new size for an array of size old_size,
 *  which must at least store an entry at position min.
 */
static INLINE int _m_new_size(int old_size, int min) {
	int bits = 0;
	assert(min>=old_size);
	while (min>0) {
		min >>= 1;
		bits++;
	}
	assert(bits < sizeof(min)*8-1);
	return 1 << bits;
}

/**
 * Allocates space for @p count entries in the rows array and
 * intitializes all entries from @p start to the end.
 */
static INLINE void _m_alloc_row(sp_matrix_t *m, int start, int count) {
	int p;
	m->rowc = count;
	m->rows = realloc(m->rows, m->rowc * sizeof(*m->rows));
	assert(m->rows);
	for (p=start; p<m->rowc; ++p) {
		m->rows[p] = malloc(sizeof(*m->rows[p]));
		assert(m->rows[p]);
		INIT_LIST_HEAD(m->rows[p]);
	}
}

/**
 * Allocates space for @p count entries in the cols array and
 * intitializes all entries from @p start to the end.
 */
static INLINE void _m_alloc_col(sp_matrix_t *m, int start, int count) {
	int p;
	m->colc = count;
	m->cols = realloc(m->cols, m->colc * sizeof(*m->cols));
	assert(m->cols);
	for (p=start; p<m->colc; ++p) {
		m->cols[p] = malloc(sizeof(*m->cols[p]));
		assert(m->cols[p]);
		INIT_LIST_HEAD(m->cols[p]);
	}
}

/**
 * Searches in row @p row for the matrix element m[row, col].
 * @return If the element exists: Element m[row, col] and @p prev points to the list_head in the entry_t holding the element.
 *         Else: NULL and @p prev points to the list_head after which the element would be inserted.
 */
static INLINE matrix_elem_t *_m_search_in_row(const sp_matrix_t *m, int row, int col, struct list_head **prev) {
	struct list_head *start;
	start = *prev = m->rows[row];
	while ((*prev)->next != start && list_entry_by_row((*prev)->next)->col <= col)
		*prev = (*prev)->next;
	if (*prev != start) {
		matrix_elem_t *me = list_entry_by_row(*prev);
		if (me->row == row && me->col == col)
			return me;
	}
	return NULL;
}

/**
 * Searches in col @p col for the matrix element m[row, col].
 * @return If the element exists: Element m[row, col] and @p prev points to the list_head in the entry_t holding the element.
 *         Else: NULL and @p prev points to the list_head after which the element would be inserted.
 */
static INLINE matrix_elem_t *_m_search_in_col(const sp_matrix_t *m, int row, int col, struct list_head **prev) {
	struct list_head *start;
	start = *prev = m->cols[col];
	while ((*prev)->next != start && list_entry_by_col((*prev)->next)->row <= row)
		*prev = (*prev)->next;
	if (*prev != start) {
		matrix_elem_t *me = list_entry_by_col(*prev);
		if (me->row == row && me->col == col)
			return me;
	}
	return NULL;
}

sp_matrix_t *new_matrix(int row_init, int col_init) {
	sp_matrix_t *res = calloc(1, sizeof(*res));
	_m_alloc_row(res, 0, row_init);
	_m_alloc_col(res, 0, col_init);
	return res;
}

void del_matrix(sp_matrix_t *m) {
	int i;
	entry_t *e, *tmp;

	for (i=0; i<m->rowc; ++i) {
		list_for_each_entry_safe(entry_t, e, tmp, m->rows[i], row_chain)
			free(e);
		free(m->rows[i]);
	}
	for (i=0; i<m->colc; ++i)
		free(m->cols[i]);
	free(m->rows);
	free(m->cols);
	free(m);
}

void matrix_set(sp_matrix_t *m, int row, int col, int val) {
	matrix_elem_t *me = NULL;
	entry_t *entr;
	struct list_head *leftof, *above;

	/* if necessary enlarge the matrix */
	if (row>m->maxrow) {
		m->maxrow = row;
		if (row>=m->rowc)
			_m_alloc_row(m, m->rowc, _m_new_size(m->rowc, row));
	}
	if (col>m->maxcol) {
		m->maxcol = col;
		if (col>=m->colc)
			_m_alloc_col(m, m->colc, _m_new_size(m->colc, col));
	}

	/* search for existing entry */
	if (m->maxrow < m->maxcol)
		me = _m_search_in_col(m, row, col, &above);
	else
		me = _m_search_in_row(m, row, col, &leftof);

	/* if it exists, set the value and return */
	if (me) {
		if (val != 0) {
			me->val = val;
		} else {
			entr = _container_of(me, entry_t, e);
			list_del(&entr->row_chain);
			list_del(&entr->col_chain);
			free(entr);
		}
		return;
	}

	/* if it does not exist search the other direction */
	if (m->maxrow >= m->maxcol)
		_m_search_in_col(m, row, col, &above);
	else
		_m_search_in_row(m, row, col, &leftof);
	/* now leftof and above are the entry_t's prior the new one in each direction */

	/* insert new entry */
	entr = malloc(sizeof(*entr));
	entr->e.row = row;
	entr->e.col = col;
	entr->e.val = val;
	list_add(&entr->row_chain, leftof);
	list_add(&entr->col_chain, above);
}

int matrix_get(const sp_matrix_t *m, int row, int col) {
	struct list_head *dummy;
	matrix_elem_t *me;

	if (is_empty_row(row) || is_empty_col(col))
		return 0;

	if (m->maxrow < m->maxcol)
		me = _m_search_in_col(m, row, col, &dummy);
	else
		me = _m_search_in_row(m, row, col, &dummy);

	if (me)
		assert(me->col == col && me->row == row);

	return me?me->val:0;
}

int matrix_get_rowcount(const sp_matrix_t *m) {
	return m->maxrow+1;
}

int matrix_get_colcount(const sp_matrix_t *m) {
	return m->maxcol+1;
}

matrix_elem_t *matrix_row_first(sp_matrix_t *m, int row) {
	if (is_empty_row(row))
		return NULL;
	m->dir = right;
	m->first = m->rows[row];
	m->last = m->rows[row]->next;
	assert ( (&list_entry(m->last, entry_t, row_chain)->e)->row == row);
	return &list_entry(m->last, entry_t, row_chain)->e;
}

matrix_elem_t *matrix_col_first(sp_matrix_t *m, int col) {
	if (is_empty_col(col))
		return NULL;
	m->dir = down;
	m->first = m->cols[col];
	m->last = m->cols[col]->next;
	assert ( (&list_entry(m->last, entry_t, col_chain)->e)->col == col);
	return &list_entry(m->last, entry_t, col_chain)->e;
}

matrix_elem_t *matrix_next(sp_matrix_t *m) {
	assert(m->last && "Start iteration with matrix_???_first, before calling me!");
	if (m->last->next == m->first)
		return NULL;
	m->last = m->last->next;
	if (m->dir == right)
		return &list_entry(m->last, entry_t, row_chain)->e;
	else
		return &list_entry(m->last, entry_t, col_chain)->e;
}

void matrix_dump(sp_matrix_t *m, FILE *out, int factor) {
	int i, o, last_idx;
	matrix_elem_t *e;

	for (i=0; i <= m->maxrow; ++i) {
		last_idx = -1;
		matrix_foreach_in_row(m, i, e) {
			for (o=last_idx+1; o<e->col; ++o)
				fprintf(out, "0");
			fprintf(out, "%d", factor*e->val);
			last_idx = e->col;
		}
		for (o=last_idx+1; o<=m->maxcol; ++o)
			fprintf(out, "0");
		fprintf(out, "\n");
	}
}

void matrix_self_test(int d) {
	int i, o;
	matrix_elem_t *e;
	sp_matrix_t *m = new_matrix(10, 10);

	for (i=0; i<d; ++i)
		for (o=0; o<d; ++o)
			matrix_set(m, i, o, i*o);

	for (i=0; i<d; ++i)
		for (o=0; o<d; ++o)
			assert(matrix_get(m, i, o) == i*o);

	i = 0;
	matrix_foreach_in_row(m,1,e) {
		assert(e->val == i);
		i++;
	}
	assert(!matrix_next(m)); /*iter must finish */

	i = 0;
	matrix_foreach_in_col(m,d-1,e) {
		assert(e->val == i);
		i += d-1;
	}
	assert(!matrix_next(m));
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
		assert(e->row == 1 && e->col == i && e->val == i+3);
		i++;
	}
	assert(i == 4);
	del_matrix(m);
}
