/********************************************************************
 ********************************************************************
 **
 ** libhungarian by Cyrill Stachniss, 2004
 **
 ** Added and adapted to libFirm by Christian Wuerdig, 2006
 **
 ** Solving the Minimum Assignment Problem using the
 ** Hungarian Method.
 **
 ** ** This file may be freely copied and distributed! **
 **
 ** Parts of the used code was originally provided by the
 ** "Stanford GraphGase", but I made changes to this code.
 ** As asked by  the copyright node of the "Stanford GraphGase",
 ** I hereby proclaim that this file are *NOT* part of the
 ** "Stanford GraphGase" distrubition!
 **
 ** This file is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied
 ** warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 ** PURPOSE.
 **
 ********************************************************************
 ********************************************************************/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "irtools.h"
#include "xmalloc.h"
#include "debug.h"
#include "obst.h"
#include "bitset.h"

#include "hungarian.h"

#define INF (0x7FFFFFFF)

struct _hungarian_problem_t {
	int      num_rows;          /**< number of rows */
	int      num_cols;          /**< number of columns */
	int      **cost;            /**< the cost matrix */
	int      width;             /**< the width for cost matrix dumper */
	int      max_cost;          /**< the maximal costs in the matrix */
	int      match_type;        /**< PERFECT or NORMAL matching */
	bitset_t *missing_left;     /**< left side nodes having no edge to the right side */
	bitset_t *missing_right;    /**< right side nodes having no edge to the left side */
	struct obstack obst;
	DEBUG_ONLY(firm_dbg_module_t *dbg);
};

static INLINE void *get_init_mem(struct obstack *obst, long sz) {
	void *p = obstack_alloc(obst, sz);
	memset(p, 0, sz);
	return p;
}

static void hungarian_dump_f(FILE *f, int **C, int rows, int cols, int width) {
	int i, j;

	fprintf(f , "\n");
	for (i = 0; i < rows; i++) {
		fprintf(f, " [");
		for (j = 0; j < cols; j++) {
			fprintf(f, "%*d", width, C[i][j]);
		}
		fprintf(f, "]\n");
	}
	fprintf(f, "\n");
}

void hungarian_print_costmatrix(hungarian_problem_t *p) {
	hungarian_dump_f(stderr, p->cost, p->num_rows, p->num_cols, p->width);
}

/**
 * Create the object and allocate memory for the data structures.
 */
hungarian_problem_t *hungarian_new(int rows, int cols, int width, int match_type) {
	int i;
	hungarian_problem_t *p = xmalloc(sizeof(*p));

	memset(p, 0, sizeof(p));

	FIRM_DBG_REGISTER(p->dbg, "firm.hungarian");

	/*
		Is the number of cols  not equal to number of rows ?
		If yes, expand with 0 - cols / 0 - cols
	*/
	rows = MAX(cols, rows);
	cols = rows;

	obstack_init(&p->obst);

	p->num_rows   = rows;
	p->num_cols   = cols;
	p->width      = width;
	p->match_type = match_type;

	/*
		In case of normal matching, we have to keep
		track of nodes without edges to kill them in
		the assignment later.
	*/
	if (match_type == HUNGARIAN_MATCH_NORMAL) {
		p->missing_left  = bitset_obstack_alloc(&p->obst, rows);
		p->missing_right = bitset_obstack_alloc(&p->obst, cols);
		bitset_set_all(p->missing_left);
		bitset_set_all(p->missing_right);
	}

	/* allocate space for cost matrix */
	p->cost = (int **)get_init_mem(&p->obst, rows * sizeof(p->cost[0]));
	for (i = 0; i < p->num_rows; i++)
		p->cost[i] = (int *)get_init_mem(&p->obst, cols * sizeof(p->cost[0][0]));

	return p;
}

/**
 * Prepare the cost matrix.
 */
void hungarian_prepare_cost_matrix(hungarian_problem_t *p, int mode) {
	int i, j;

	if (mode == HUNGARIAN_MODE_MAXIMIZE_UTIL) {
		for (i = 0; i < p->num_rows; i++) {
			for (j = 0; j < p->num_cols; j++) {
				p->cost[i][j] = p->max_cost - p->cost[i][j];
			}
		}
	}
	else if (mode == HUNGARIAN_MODE_MINIMIZE_COST) {
		/* nothing to do */
	}
	else
		fprintf(stderr, "Unknown mode. Mode was set to HUNGARIAN_MODE_MINIMIZE_COST.\n");
}

/**
 * Set cost[left][right] to cost.
 */
void hungarian_add(hungarian_problem_t *p, int left, int right, int cost) {
	assert(p->num_rows > left  && "Invalid row selected.");
	assert(p->num_cols > right && "Invalid column selected.");

	p->cost[left][right] = cost;
	p->max_cost          = MAX(p->max_cost, cost);

	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		bitset_clear(p->missing_left, left);
		bitset_clear(p->missing_right, right);
	}
}

/**
 * Set cost[left][right] to 0.
 */
void hungarian_remv(hungarian_problem_t *p, int left, int right) {
	assert(p->num_rows > left  && "Invalid row selected.");
	assert(p->num_cols > right && "Invalid column selected.");

	p->cost[left][right] = 0;

	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		bitset_set(p->missing_left, left);
		bitset_set(p->missing_right, right);
	}
}

/**
 * Frees all allocated memory.
 */
void hungarian_free(hungarian_problem_t* p) {
	obstack_free(&p->obst, NULL);
	xfree(p);
}

/**
 * Do the assignment.
 */
int hungarian_solve(hungarian_problem_t* p, int *assignment, int *final_cost, int cost_threshold) {
	int i, j, m, n, k, l, s, t, q, unmatched, cost;
	int *col_mate;
	int *row_mate;
	int *parent_row;
	int *unchosen_row;
	int *row_dec;
	int *col_inc;
	int *slack;
	int *slack_row;

	cost = 0;
	m    = p->num_rows;
	n    = p->num_cols;

	col_mate     = xcalloc(p->num_rows, sizeof(col_mate[0]));
	unchosen_row = xcalloc(p->num_rows, sizeof(unchosen_row[0]));
	row_dec      = xcalloc(p->num_rows, sizeof(row_dec[0]));
	slack_row    = xcalloc(p->num_rows, sizeof(slack_row[0]));

	row_mate     = xcalloc(p->num_cols, sizeof(row_mate[0]));
	parent_row   = xcalloc(p->num_cols, sizeof(parent_row[0]));
	col_inc      = xcalloc(p->num_cols, sizeof(col_inc[0]));
	slack        = xcalloc(p->num_cols, sizeof(slack[0]));

	memset(assignment, -1, m * sizeof(assignment[0]));

	/* Begin subtract column minima in order to start with lots of zeros 12 */
	DBG((p->dbg, LEVEL_1, "Using heuristic\n"));

	for (l = 0; l < n; ++l) {
		s = p->cost[0][l];

		for (k = 1; k < m; ++k) {
			if (p->cost[k][l] < s)
				s = p->cost[k][l];
		}

		cost += s;

		if (s != 0) {
			for (k = 0; k < m; ++k)
				p->cost[k][l] -= s;
		}
	}
	/* End subtract column minima in order to start with lots of zeros 12 */

	/* Begin initial state 16 */
	t = 0;
	for (l = 0; l < n; ++l) {
		row_mate[l]   = -1;
		parent_row[l] = -1;
		col_inc[l]    = 0;
		slack[l]      = INF;
	}

	for (k = 0; k < m; ++k) {
		s = p->cost[k][0];

		for (l = 1; l < n; ++l) {
			if (p->cost[k][l] < s)
				s = p->cost[k][l];
		}

		row_dec[k] = s;

		for (l = 0; l < n; ++l) {
			if (s == p->cost[k][l] && row_mate[l] < 0) {
				col_mate[k] = l;
				row_mate[l] = k;
				DBG((p->dbg, LEVEL_1, "matching col %d == row %d\n", l, k));
				goto row_done;
			}
		}

		col_mate[k] = -1;
		DBG((p->dbg, LEVEL_1, "node %d: unmatched row %d\n", t, k));
		unchosen_row[t++] = k;
row_done: ;
	}
	/* End initial state 16 */

	/* Begin Hungarian algorithm 18 */
	if (t == 0)
		goto done;

	unmatched = t;
	while (1) {
		DBG((p->dbg, LEVEL_1, "Matched %d rows.\n", m - t));
		q = 0;

		while (1) {
			while (q < t) {
				/* Begin explore node q of the forest 19 */
				k = unchosen_row[q];
				s = row_dec[k];

				for (l = 0; l < n; ++l) {
					if (slack[l]) {
						int del = p->cost[k][l] - s + col_inc[l];

						if (del < slack[l]) {
							if (del == 0) {
								if (row_mate[l] < 0)
									goto breakthru;

								slack[l]      = 0;
								parent_row[l] = k;
								DBG((p->dbg, LEVEL_1, "node %d: row %d == col %d -- row %d\n", t, row_mate[l], l, k));
								unchosen_row[t++] = row_mate[l];
							}
							else {
								slack[l]     = del;
								slack_row[l] = k;
							}
						}
					}
				}
				/* End explore node q of the forest 19 */
				q++;
			}

			/* Begin introduce a new zero into the matrix 21 */
			s = INF;
			for (l = 0; l < n; ++l) {
				if (slack[l] && slack[l] < s)
					s = slack[l];
			}

			for (q = 0; q < t; ++q)
				row_dec[unchosen_row[q]] += s;

			for (l = 0; l < n; ++l) {
				if (slack[l]) {
					slack[l] -= s;
					if (slack[l] == 0) {
						/* Begin look at a new zero 22 */
						k = slack_row[l];
						DBG((p->dbg, LEVEL_1, "Decreasing uncovered elements by %d produces zero at [%d, %d]\n", s, k, l));
						if (row_mate[l] < 0) {
							for (j = l + 1; j < n; ++j) {
								if (slack[j] == 0)
									col_inc[j] += s;
							}
							goto breakthru;
						}
						else {
							parent_row[l] = k;
							DBG((p->dbg, LEVEL_1, "node %d: row %d == col %d -- row %d\n", t, row_mate[l], l, k));
							unchosen_row[t++] = row_mate[l];
						}
						/* End look at a new zero 22 */
					}
				}
				else {
					col_inc[l] += s;
				}
			}
			/* End introduce a new zero into the matrix 21 */
		}
breakthru:
		/* Begin update the matching 20 */
		DBG((p->dbg, LEVEL_1, "Breakthrough at node %d of %d.\n", q, t));
		while (1) {
			j           = col_mate[k];
			col_mate[k] = l;
			row_mate[l] = k;

			DBG((p->dbg, LEVEL_1, "rematching col %d == row %d\n", l, k));
			if (j < 0)
				break;

			k = parent_row[j];
			l = j;
		}
		/* End update the matching 20 */

		if (--unmatched == 0)
			goto done;

		/* Begin get ready for another stage 17 */
		t = 0;
		for (l = 0; l < n; ++l) {
			parent_row[l] = -1;
			slack[l]      = INF;
		}

		for (k = 0; k < m; ++k) {
			if (col_mate[k] < 0) {
				DBG((p->dbg, LEVEL_1, "node %d: unmatched row %d\n", t, k));
				unchosen_row[t++] = k;
			}
		}
		/* End get ready for another stage 17 */
	}
done:

	/* Begin double check the solution 23 */
	for (k = 0; k < m; ++k) {
		for (l = 0; l < n; ++l) {
			if (p->cost[k][l] < row_dec[k] - col_inc[l])
				return -1;
		}
	}

	for (k = 0; k < m; ++k) {
		l = col_mate[k];
		if (l < 0 || p->cost[k][l] != row_dec[k] - col_inc[l])
			return -2;
	}

	for (k = l = 0; l < n; ++l) {
		if (col_inc[l])
			k++;
	}

	if (k > m)
		return -3;
	/* End double check the solution 23 */

	/* End Hungarian algorithm 18 */

	/* collect the assigned values */
	for (i = 0; i < m; ++i) {
		if (cost_threshold > 0 && p->cost[i][col_mate[i]] >= cost_threshold)
			assignment[i] = -1; /* remove matching having cost > threshold */
		else
			assignment[i] = col_mate[i];
	}

	/* In case of normal matching: remove impossible ones */
	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		for (i = 0; i < m; ++i) {
			if (bitset_is_set(p->missing_left, i) || bitset_is_set(p->missing_right, col_mate[i]))
				assignment[i] = -1;
		}
	}

	for (k = 0; k < m; ++k) {
		for (l = 0; l < n; ++l) {
			p->cost[k][l] = p->cost[k][l] - row_dec[k] + col_inc[l];
		}
	}

	for (i = 0; i < m; ++i)
		cost += row_dec[i];

	for (i = 0; i < n; ++i)
		cost -= col_inc[i];

	DBG((p->dbg, LEVEL_1, "Cost is %d\n", cost));

	xfree(slack);
	xfree(col_inc);
	xfree(parent_row);
	xfree(row_mate);
	xfree(slack_row);
	xfree(row_dec);
	xfree(unchosen_row);
	xfree(col_mate);

	*final_cost = cost;

	return 0;
}
