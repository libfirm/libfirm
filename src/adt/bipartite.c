/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Specialized implementation for perfect bipartite matching.
 * @author  Sebastian Hack
 */
#include "bipartite.h"

#include "bitset.h"
#include "xmalloc.h"
#include <assert.h>
#include <stdio.h>

struct bipartite_t {
	unsigned  n_left;
	unsigned  n_right;
	bitset_t *adj[];
};

bipartite_t *bipartite_new(unsigned const n_left, unsigned const n_right)
{
	bipartite_t *gr = XMALLOCFZ(bipartite_t, adj, n_left);
	gr->n_left  = n_left;
	gr->n_right = n_right;

	for (unsigned i = 0; i < n_left; ++i)
		gr->adj[i] = bitset_malloc(n_right);

	return gr;
}

void bipartite_free(bipartite_t *const gr)
{
	for (unsigned i = 0; i < gr->n_left; ++i)
		free(gr->adj[i]);
	free(gr);
}

void bipartite_add(bipartite_t *const gr, unsigned const i, unsigned const j)
{
	assert(i < gr->n_left && j < gr->n_right);
	bitset_set(gr->adj[i], j);
}

void bipartite_remv(bipartite_t *const gr, unsigned const i, unsigned const j)
{
	assert(i < gr->n_left && j < gr->n_right);
	bitset_clear(gr->adj[i], j);
}

int bipartite_adj(bipartite_t const *const gr, unsigned const i,
                  unsigned const j)
{
	assert(i < gr->n_left && j < gr->n_right);
	return bitset_is_set(gr->adj[i], j);
}

static int apply_alternating_path(bipartite_t const *const gr,
                                  int *const matching,
                                  bitset_t *const matched_left,
                                  bitset_t *const matched_right)
{
	bool done_something = false;
	bitset_t *const tmp = bitset_alloca(gr->n_right);

	for (unsigned left = 0; left < gr->n_left; ++left) {
		bitset_t *left_adj = gr->adj[left];
		bitset_copy(tmp, left_adj);

		if (matching[left] >= 0) {
			int old_right = matching[left];

			/* Check of all neighbors of the left node are already matched.
			 * We cannot improve this edge then. */
			if (bitset_contains(left_adj, matched_right))
				continue;

			bitset_andnot(tmp, matched_right);
			unsigned right = bitset_next_set(tmp, 0);
			assert(right != ~0u);

			/* We have to find another left node which has the old right one as
			 * a neighbor. This node must not be part of a matching */
			unsigned i;
			for (i = 0; i < gr->n_left; ++i)
				if (i != left && bitset_is_set(gr->adj[i], old_right) && !bitset_is_set(matched_left, i))
					break;

			/* If no such node can be found, exit. */
			if (i >= gr->n_left)
				continue;

			/* Else, we can improve this edge. */
			matching[left] = right;
			matching[i] = old_right;
			bitset_set(matched_left, i);
			bitset_set(matched_right, right);
			done_something = true;
		} else {
			/* We have to create a new single edge */
			assert(!bitset_is_set(matched_left, left));

			bitset_andnot(tmp, matched_right);
			if (bitset_is_empty(tmp))
				continue;

			unsigned right = bitset_next_set(tmp, 0);
			assert(!bitset_is_set(matched_right, right));
			matching[left] = right;
			bitset_set(matched_left, left);
			bitset_set(matched_right, right);
			done_something = true;
		}
	}

	return done_something;
}

void bipartite_matching(bipartite_t const *const gr, int *const matching)
{
	bitset_t *const matched_left = bitset_alloca(gr->n_left);
	bitset_t *const matched_right = bitset_alloca(gr->n_right);

	memset(matching, -1, gr->n_left * sizeof(int));
	while (apply_alternating_path(gr, matching, matched_left, matched_right)) {
	}
}

void bipartite_dump_f(FILE *const f, bipartite_t const *gr)
{
	for (unsigned i = 0; i < gr->n_left; ++i) {
		fprintf(f, "%u: ", i);
		bitset_fprint(f, gr->adj[i]);
		fprintf(f, "\n");
	}
}

void bipartite_dump(const char *const name, bipartite_t const *gr)
{
	FILE *const f = fopen(name, "w");
	if (f != NULL) {
		bipartite_dump_f(f, gr);
		fclose(f);
	}
}
