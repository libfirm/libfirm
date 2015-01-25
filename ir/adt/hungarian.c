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
 */
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "util.h"
#include "xmalloc.h"
#include "debug.h"
#include "panic.h"
#include "hungarian.h"
#include "raw_bitset.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

struct hungarian_problem_t {
	unsigned      num_rows;      /**< number of rows */
	unsigned      num_cols;      /**< number of columns */
	unsigned     *cost;          /**< the cost matrix */
	unsigned      max_cost;      /**< the maximal costs in the matrix */
	match_type_t  match_type;    /**< PERFECT or NORMAL matching */
	unsigned     *missing_left;  /**< bitset: left side nodes having no edge to
	                                  the right side */
	unsigned     *missing_right; /**< bitset: right side nodes having no edge to
	                              the left side */
};

static void hungarian_dump_f(FILE *f, const unsigned *cost,
                             unsigned num_rows, unsigned num_cols, int width)
{
	unsigned r, c;

	fprintf(f , "\n");
	for (r = 0; r < num_rows; r++) {
		fprintf(f, " [");
		for (c = 0; c < num_cols; c++) {
			fprintf(f, "%*u", width, cost[r*num_cols + c]);
		}
		fprintf(f, "]\n");
	}
	fprintf(f, "\n");
}

void hungarian_print_cost_matrix(hungarian_problem_t *p, int width)
{
	hungarian_dump_f(stderr, p->cost, p->num_rows, p->num_cols, width);
}

hungarian_problem_t *hungarian_new(unsigned num_rows, unsigned num_cols,
                                   match_type_t match_type)
{
	hungarian_problem_t *p = XMALLOCZ(hungarian_problem_t);

	FIRM_DBG_REGISTER(dbg, "firm.hungarian");

	/*
		Is the number of cols  not equal to number of rows ?
		If yes, expand with 0 - cols / 0 - cols
	*/
	num_rows = MAX(num_cols, num_rows);
	num_cols = num_rows;

	p->num_rows   = num_rows;
	p->num_cols   = num_cols;
	p->match_type = match_type;

	/*
		In case of normal matching, we have to keep
		track of nodes without edges to kill them in
		the assignment later.
	*/
	if (match_type == HUNGARIAN_MATCH_NORMAL) {
		p->missing_left  = rbitset_malloc(num_rows);
		p->missing_right = rbitset_malloc(num_cols);
		rbitset_set_all(p->missing_left,  num_rows);
		rbitset_set_all(p->missing_right, num_cols);
	}

	/* allocate space for cost matrix */
	p->cost = XMALLOCNZ(unsigned, num_rows * num_cols);
	return p;
}

void hungarian_prepare_cost_matrix(hungarian_problem_t *p,
                                   hungarian_mode_t mode)
{
	if (mode == HUNGARIAN_MODE_MAXIMIZE_UTIL) {
		unsigned  r, c;
		unsigned  num_cols = p->num_cols;
		unsigned *cost     = p->cost;
		unsigned  max_cost = p->max_cost;
		for (r = 0; r < p->num_rows; r++) {
			for (c = 0; c < p->num_cols; c++) {
				cost[r*num_cols + c] = max_cost - cost[r*num_cols + c];
			}
		}
	} else if (mode == HUNGARIAN_MODE_MINIMIZE_COST) {
		/* nothing to do */
	} else {
		panic("unknown hungarian problem mode");
	}
}

void hungarian_add(hungarian_problem_t *p, unsigned left, unsigned right,
                   unsigned cost)
{
	assert(p->num_rows > left  && "Invalid row selected.");
	assert(p->num_cols > right && "Invalid column selected.");

	p->cost[left*p->num_cols + right] = cost;
	p->max_cost                       = MAX(p->max_cost, cost);

	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		rbitset_clear(p->missing_left, left);
		rbitset_clear(p->missing_right, right);
	}
}

void hungarian_remove(hungarian_problem_t *p, unsigned left, unsigned right)
{
	assert(p->num_rows > left  && "Invalid row selected.");
	assert(p->num_cols > right && "Invalid column selected.");

	p->cost[left*p->num_cols + right] = 0;

	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		rbitset_set(p->missing_left, left);
		rbitset_set(p->missing_right, right);
	}
}

void hungarian_free(hungarian_problem_t* p)
{
	free(p->missing_left);
	free(p->missing_right);
	free(p->cost);
	free(p);
}

int hungarian_solve(hungarian_problem_t* p, unsigned *assignment,
                    unsigned *final_cost, unsigned cost_threshold)
{
	int       result       = 0;
	unsigned  res_cost     = 0;
	unsigned  num_rows     = p->num_rows;
	unsigned  num_cols     = p->num_cols;
	unsigned *cost         = p->cost;
	unsigned *col_mate     = XMALLOCNZ(unsigned, num_rows);
	unsigned *row_mate     = XMALLOCNZ(unsigned, num_cols);
	unsigned *parent_row   = XMALLOCNZ(unsigned, num_cols);
	unsigned *unchosen_row = XMALLOCNZ(unsigned, num_rows);
	int      *row_dec      = XMALLOCNZ(int, num_rows);
	int      *col_inc      = XMALLOCNZ(int, num_cols);
	int      *slack        = XMALLOCNZ(int, num_cols);
	unsigned *slack_row    = XMALLOCNZ(unsigned, num_rows);
	unsigned  r;
	unsigned  c;
	unsigned  t;
	unsigned  unmatched;

	memset(assignment, -1, num_rows * sizeof(assignment[0]));

	/* Begin subtract column minima in order to start with lots of zeros 12 */
	DBG((dbg, LEVEL_1, "Using heuristic\n"));

	for (c = 0; c < num_cols; ++c) {
		unsigned col_mininum = cost[0*num_cols + c];

		for (r = 1; r < num_rows; ++r) {
			if (cost[r*num_cols + c] < col_mininum)
				col_mininum = cost[r*num_cols + c];
		}

		if (col_mininum == 0)
			continue;

		res_cost += col_mininum;
		for (r = 0; r < num_rows; ++r)
			cost[r*num_cols + c] -= col_mininum;
	}
	/* End subtract column minima in order to start with lots of zeros 12 */

	/* Begin initial state 16 */
	unmatched = 0;
	for (c = 0; c < num_cols; ++c) {
		row_mate[c]   = (unsigned) -1;
		parent_row[c] = (unsigned) -1;
		col_inc[c]    = 0;
		slack[c]      = INT_MAX;
	}

	for (r = 0; r < num_rows; ++r) {
		unsigned row_minimum = cost[r*num_cols + 0];

		for (c = 1; c < num_cols; ++c) {
			if (cost[r*num_cols + c] < row_minimum)
				row_minimum = cost[r*num_cols + c];
		}

		row_dec[r] = row_minimum;

		for (c = 0; c < num_cols; ++c) {
			if (cost[r*num_cols + c] != row_minimum)
				continue;
			if (row_mate[c] != (unsigned)-1)
				continue;

			col_mate[r] = c;
			row_mate[c] = r;
			DBG((dbg, LEVEL_1, "matching col %u == row %u\n", c, r));
			goto row_done;
		}

		col_mate[r] = (unsigned)-1;
		DBG((dbg, LEVEL_1, "node %u: unmatched row %u\n", unmatched, r));
		unchosen_row[unmatched++] = r;
row_done: ;
	}
	/* End initial state 16 */

	/* Begin Hungarian algorithm 18 */
	if (unmatched == 0)
		goto done;

	t = unmatched;
	for (;;) {
		unsigned q = 0;
		unsigned j;
		DBG((dbg, LEVEL_1, "Matched %u rows.\n", num_rows - t));

		for (;;) {
			int s;
			while (q < t) {
				/* Begin explore node q of the forest 19 */
				r = unchosen_row[q];
				s = row_dec[r];

				for (c = 0; c < num_cols; ++c) {
					if (slack[c]) {
						int del = cost[r*num_cols + c] - s + col_inc[c];

						if (del < slack[c]) {
							if (del == 0) {
								if (row_mate[c] == (unsigned)-1)
									goto breakthru;

								slack[c]      = 0;
								parent_row[c] = r;
								DBG((dbg, LEVEL_1, "node %u: row %u == col %u -- row %u\n", t, row_mate[c], c, r));
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
						DBG((dbg, LEVEL_1, "Decreasing uncovered elements by %d produces zero at [%u, %u]\n", s, r, c));
						if (row_mate[c] == (unsigned)-1) {
							for (j = c + 1; j < num_cols; ++j) {
								if (slack[j] == 0)
									col_inc[j] += s;
							}
							goto breakthru;
						} else {
							parent_row[c] = r;
							DBG((dbg, LEVEL_1, "node %u: row %u == col %u -- row %u\n", t, row_mate[c], c, r));
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
		DBG((dbg, LEVEL_1, "Breakthrough at node %u of %u.\n", q, t));
		for (;;) {
			j           = col_mate[r];
			col_mate[r] = c;
			row_mate[c] = r;

			DBG((dbg, LEVEL_1, "rematching col %u == row %u\n", c, r));
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
			parent_row[c] = (unsigned) -1;
			slack[c]      = INT_MAX;
		}

		for (r = 0; r < num_rows; ++r) {
			if (col_mate[r] == (unsigned)-1) {
				DBG((dbg, LEVEL_1, "node %u: unmatched row %u\n", t, r));
				unchosen_row[t++] = r;
			}
		}
		/* End get ready for another stage 17 */
	}
done:

	/* Begin double check the solution 23 */
	for (r = 0; r < num_rows; ++r) {
		for (c = 0; c < num_cols; ++c) {
			if ((int) cost[r*num_cols + c] < row_dec[r] - col_inc[c]) {
				result = -1;
				goto ret;
			}
		}
	}

	for (r = 0; r < num_rows; ++r) {
		c = col_mate[r];
		if (c == (unsigned)-1
		    || cost[r*num_cols + c] != (unsigned) (row_dec[r] - col_inc[c])) {
		    result = -2;
		    goto ret;
		}
	}

	for (r = c = 0; c < num_cols; ++c) {
		if (col_inc[c])
			r++;
	}

	if (r > num_rows) {
		result = -3;
		goto ret;
	}
	/* End double check the solution 23 */

	/* End Hungarian algorithm 18 */

	/* collect the assigned values */
	for (r = 0; r < num_rows; ++r) {
		if (cost_threshold > 0
		    && cost[r*num_cols + col_mate[r]] >= cost_threshold)
			assignment[r] = -1; /* remove matching having cost > threshold */
		else
			assignment[r] = col_mate[r];
	}

	/* In case of normal matching: remove impossible ones */
	if (p->match_type == HUNGARIAN_MATCH_NORMAL) {
		for (r = 0; r < num_rows; ++r) {
			if (rbitset_is_set(p->missing_left, r)
			        || rbitset_is_set(p->missing_right, col_mate[r]))
				assignment[r] = -1;
		}
	}

	for (r = 0; r < num_rows; ++r) {
		for (c = 0; c < num_cols; ++c) {
			cost[r*num_cols + c] = cost[r*num_cols + c] - row_dec[r] + col_inc[c];
		}
	}

	for (r = 0; r < num_rows; ++r)
		res_cost += row_dec[r];

	for (c = 0; c < num_cols; ++c)
		res_cost -= col_inc[c];

	DBG((dbg, LEVEL_1, "Cost is %d\n", res_cost));

ret:
	if (final_cost != NULL)
		*final_cost = res_cost;

	free(col_mate);
	free(row_mate);
	free(parent_row);
	free(unchosen_row);
	free(row_dec);
	free(col_inc);
	free(slack);
	free(slack_row);

	return result;
}
