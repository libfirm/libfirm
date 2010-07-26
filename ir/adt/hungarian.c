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

/**
 * @file
 * @brief   Solving the Minimum Assignment Problem using the Hungarian Method.
 * @version $Id$
 */
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "irtools.h"
#include "xmalloc.h"
#include "debug.h"
#include "obst.h"
#include "bitset.h"
#include "error.h"

#include "hungarian.h"

struct hungarian_problem_t {
	unsigned num_rows;          /**< number of rows */
	unsigned num_cols;          /**< number of columns */
	int      **cost;            /**< the cost matrix */
	int      max_cost;          /**< the maximal costs in the matrix */
	int      match_type;        /**< PERFECT or NORMAL matching */
	bitset_t *missing_left;     /**< left side nodes having no edge to the right side */
	bitset_t *missing_right;    /**< right side nodes having no edge to the left side */
	struct obstack obst;
	DEBUG_ONLY(firm_dbg_module_t *dbg);
};

static void hungarian_dump_f(FILE *f, int **C, unsigned n_rows, unsigned n_cols,
                             int width)
{
	unsigned r;

	fprintf(f , "\n");
	for (r = 0; r < n_rows; r++) {
		unsigned c;
		fprintf(f, " [");
		for (c = 0; c < n_cols; c++) {
			fprintf(f, "%*d", width, C[r][c]);
		}
		fprintf(f, "]\n");
	}
	fprintf(f, "\n");
}

void hungarian_print_cost_matrix(hungarian_problem_t *p, int width)
{
	hungarian_dump_f(stderr, p->cost, p->num_rows, p->num_cols, width);
}

hungarian_problem_t *hungarian_new(unsigned n_rows, unsigned n_cols,
                                   match_type_t match_type)
{
	unsigned r;
	hungarian_problem_t *p = XMALLOCZ(hungarian_problem_t);

	FIRM_DBG_REGISTER(p->dbg, "firm.hungarian");

	/*
		Is the number of cols  not equal to number of rows ?
		If yes, expand with 0 - cols / 0 - cols
	*/
	n_rows = MAX(n_cols, n_rows);
	n_cols = n_rows;

	obstack_init(&p->obst);

	p->num_rows   = n_rows;
	p->num_cols   = n_cols;
	p->match_type = match_type;

	/*
		In case of normal matching, we have to keep
		track of nodes without edges to kill them in
		the assignment later.
	*/
	if (match_type == HUNGARIAN_MATCH_NORMAL) {
		p->missing_left  = bitset_obstack_alloc(&p->obst, n_rows);
		p->missing_right = bitset_obstack_alloc(&p->obst, n_cols);
		bitset_set_all(p->missing_left);
		bitset_set_all(p->missing_right);
	}

	/* allocate space for cost matrix */
	p->cost = OALLOCNZ(&p->obst, int*, n_rows);
	for (r = 0; r < p->num_rows; r++)
		p->cost[r] = OALLOCNZ(&p->obst, int, n_cols);

	return p;
}

void hungarian_prepare_cost_matrix(hungarian_problem_t *p,
                                   hungarian_mode_t mode)
{
	unsigned r, c;

	if (mode == HUNGARIAN_MODE_MAXIMIZE_UTIL) {
		for (r = 0; r < p->num_rows; r++) {
			for (c = 0; c < p->num_cols; c++) {
				p->cost[r][c] = p->max_cost - p->cost[r][c];
			}
		}
	} else if (mode == HUNGARIAN_MODE_MINIMIZE_COST) {
		/* nothing to do */
	} else {
		panic("Unknown hungarian problem mode\n");
	}
}

void hungarian_add(hungarian_problem_t *p, unsigned left, unsigned right,
                   int cost)
{
	assert(p->num_rows > left  && "Invalid row selected.");
	assert(p->num_cols > right && "Invalid column selected.");
	assert(cost >= 0);

	p->cost[left][right] = cost;
	p->max_cost          = MAX(p->max_cost, cost);

	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		bitset_clear(p->missing_left, left);
		bitset_clear(p->missing_right, right);
	}
}

void hungarian_remove(hungarian_problem_t *p, unsigned left, unsigned right)
{
	assert(p->num_rows > left  && "Invalid row selected.");
	assert(p->num_cols > right && "Invalid column selected.");

	/* Set cost[left][right] to 0. */
	p->cost[left][right] = 0;

	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		bitset_set(p->missing_left, left);
		bitset_set(p->missing_right, right);
	}
}

void hungarian_free(hungarian_problem_t* p)
{
	obstack_free(&p->obst, NULL);
	xfree(p);
}

int hungarian_solve(hungarian_problem_t* p, unsigned *assignment,
                    int *final_cost, int cost_threshold)
{
	int       cost          = 0;
	unsigned  num_rows      = p->num_rows;
	unsigned  num_cols      = p->num_cols;
	unsigned *col_mate      = XMALLOCNZ(unsigned, num_rows);
	unsigned *row_mate      = XMALLOCNZ(unsigned, num_cols);
	unsigned *parent_row    = XMALLOCNZ(unsigned, num_cols);
	unsigned *unchosen_row  = XMALLOCNZ(unsigned, num_rows);
	int      *row_dec       = XMALLOCNZ(int, num_rows);
	int      *col_inc       = XMALLOCNZ(int, num_cols);
	int      *slack         = XMALLOCNZ(int, num_cols);
	unsigned *slack_row     = XMALLOCNZ(unsigned, num_rows);
	unsigned  r;
	unsigned  c;
	unsigned  t;
	unsigned  unmatched;

	memset(assignment, -1, num_rows * sizeof(assignment[0]));

	/* Begin subtract column minima in order to start with lots of zeros 12 */
	DBG((p->dbg, LEVEL_1, "Using heuristic\n"));

	for (c = 0; c < num_cols; ++c) {
		int s = p->cost[0][c];

		for (r = 1; r < num_rows; ++r) {
			if (p->cost[r][c] < s)
				s = p->cost[r][c];
		}

		cost += s;
		if (s == 0)
			continue;

		for (r = 0; r < num_rows; ++r)
			p->cost[r][c] -= s;
	}
	/* End subtract column minima in order to start with lots of zeros 12 */

	/* Begin initial state 16 */
	t = 0;
	for (c = 0; c < num_cols; ++c) {
		row_mate[c]   = (unsigned) -1;
		parent_row[c] = (unsigned) -1;
		col_inc[c]    = 0;
		slack[c]      = INT_MAX;
	}

	for (r = 0; r < num_rows; ++r) {
		int s = p->cost[r][0];

		for (c = 1; c < num_cols; ++c) {
			if (p->cost[r][c] < s)
				s = p->cost[r][c];
		}

		row_dec[r] = s;

		for (c = 0; c < num_cols; ++c) {
			if (s == p->cost[r][c] && row_mate[c] == (unsigned)-1) {
				col_mate[r] = c;
				row_mate[c] = r;
				DBG((p->dbg, LEVEL_1, "matching col %d == row %d\n", c, r));
				goto row_done;
			}
		}

		col_mate[r] = (unsigned)-1;
		DBG((p->dbg, LEVEL_1, "node %d: unmatched row %d\n", t, r));
		unchosen_row[t++] = r;
row_done: ;
	}
	/* End initial state 16 */

	/* Begin Hungarian algorithm 18 */
	if (t == 0)
		goto done;

	unmatched = t;
	for (;;) {
		unsigned q = 0;
		unsigned j;
		DBG((p->dbg, LEVEL_1, "Matched %d rows.\n", num_rows - t));

		for (;;) {
			int s;
			while (q < t) {
				/* Begin explore node q of the forest 19 */
				r = unchosen_row[q];
				s = row_dec[r];

				for (c = 0; c < num_cols; ++c) {
					if (slack[c]) {
						int del = p->cost[r][c] - s + col_inc[c];

						if (del < slack[c]) {
							if (del == 0) {
								if (row_mate[c] == (unsigned)-1)
									goto breakthru;

								slack[c]      = 0;
								parent_row[c] = r;
								DBG((p->dbg, LEVEL_1, "node %d: row %d == col %d -- row %d\n", t, row_mate[c], c, r));
								unchosen_row[t++] = row_mate[c];
							} else {
								slack[c]     = del;
								slack_row[c] = r;
							}
						}
					}
				}
				/* End explore node q of the forest 19 */
				q++;
			}

			/* Begin introduce a new zero into the matrix 21 */
			s = INT_MAX;
			for (c = 0; c < num_cols; ++c) {
				if (slack[c] && slack[c] < s)
					s = slack[c];
			}

			for (q = 0; q < t; ++q)
				row_dec[unchosen_row[q]] += s;

			for (c = 0; c < num_cols; ++c) {
				if (slack[c]) {
					slack[c] -= s;
					if (slack[c] == 0) {
						/* Begin look at a new zero 22 */
						r = slack_row[c];
						DBG((p->dbg, LEVEL_1, "Decreasing uncovered elements by %d produces zero at [%d, %d]\n", s, r, c));
						if (row_mate[c] == (unsigned)-1) {
							for (j = c + 1; j < num_cols; ++j) {
								if (slack[j] == 0)
									col_inc[j] += s;
							}
							goto breakthru;
						} else {
							parent_row[c] = r;
							DBG((p->dbg, LEVEL_1, "node %d: row %d == col %d -- row %d\n", t, row_mate[c], c, r));
							unchosen_row[t++] = row_mate[c];
						}
						/* End look at a new zero 22 */
					}
				} else {
					col_inc[c] += s;
				}
			}
			/* End introduce a new zero into the matrix 21 */
		}
breakthru:
		/* Begin update the matching 20 */
		DBG((p->dbg, LEVEL_1, "Breakthrough at node %d of %d.\n", q, t));
		for (;;) {
			j           = col_mate[r];
			col_mate[r] = c;
			row_mate[c] = r;

			DBG((p->dbg, LEVEL_1, "rematching col %d == row %d\n", c, r));
			if (j == (unsigned)-1)
				break;

			r = parent_row[j];
			c = j;
		}
		/* End update the matching 20 */

		if (--unmatched == 0)
			goto done;

		/* Begin get ready for another stage 17 */
		t = 0;
		for (c = 0; c < num_cols; ++c) {
			parent_row[c] = -1;
			slack[c]      = INT_MAX;
		}

		for (r = 0; r < num_rows; ++r) {
			if (col_mate[r] == (unsigned)-1) {
				DBG((p->dbg, LEVEL_1, "node %d: unmatched row %d\n", t, r));
				unchosen_row[t++] = r;
			}
		}
		/* End get ready for another stage 17 */
	}
done:

	/* Begin double check the solution 23 */
	for (r = 0; r < num_rows; ++r) {
		for (c = 0; c < num_cols; ++c) {
			if (p->cost[r][c] < row_dec[r] - col_inc[c])
				return -1;
		}
	}

	for (r = 0; r < num_rows; ++r) {
		c = col_mate[r];
		if (c == (unsigned)-1 || p->cost[r][c] != row_dec[r] - col_inc[c])
			return -2;
	}

	for (r = c = 0; c < num_cols; ++c) {
		if (col_inc[c])
			r++;
	}

	if (r > num_rows)
		return -3;
	/* End double check the solution 23 */

	/* End Hungarian algorithm 18 */

	/* collect the assigned values */
	for (r = 0; r < num_rows; ++r) {
		if (cost_threshold > 0 && p->cost[r][col_mate[r]] >= cost_threshold)
			assignment[r] = -1; /* remove matching having cost > threshold */
		else
			assignment[r] = col_mate[r];
	}

	/* In case of normal matching: remove impossible ones */
	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		for (r = 0; r < num_rows; ++r) {
			if (bitset_is_set(p->missing_left, r)
			        || bitset_is_set(p->missing_right, col_mate[r]))
				assignment[r] = -1;
		}
	}

	for (r = 0; r < num_rows; ++r) {
		for (c = 0; c < num_cols; ++c) {
			p->cost[r][c] = p->cost[r][c] - row_dec[r] + col_inc[c];
		}
	}

	for (r = 0; r < num_rows; ++r)
		cost += row_dec[r];

	for (c = 0; c < num_cols; ++c)
		cost -= col_inc[c];

	DBG((p->dbg, LEVEL_1, "Cost is %d\n", cost));

	xfree(slack);
	xfree(col_inc);
	xfree(parent_row);
	xfree(row_mate);
	xfree(slack_row);
	xfree(row_dec);
	xfree(unchosen_row);
	xfree(col_mate);

	if (final_cost != NULL)
		*final_cost = cost;

	return 0;
}
