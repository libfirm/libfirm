/*
 * Specialized implementation for perfect bipartite matching.
 * @author Sebastian Hack
 * @cvs-id $Id$
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdio.h>
#include <assert.h>

#include "bitset.h"
#include "bipartite.h"
#include "xmalloc.h"

struct _bipartite_t {
	int n_left, n_right;
	bitset_t *adj[1];
};

bipartite_t *bipartite_new(int n_left, int n_right)
{
	int i, size;
	bipartite_t *gr;

	size = n_left > 0 ? n_left - 1 : 0;
	gr = xmalloc(sizeof(*gr) + size * sizeof(void *));
	memset(gr, 0, sizeof(*gr));

	gr->n_left = n_left;
	gr->n_right = n_right;

	for(i = 0; i < n_left; ++i)
		gr->adj[i] = bitset_malloc(n_right);

	return gr;
}

void bipartite_free(bipartite_t *gr)
{
	int i;
	for(i = 0; i < gr->n_left; ++i)
		bitset_free(gr->adj[i]);
	free(gr);
}

void bipartite_add(bipartite_t *gr, int i, int j)
{
	assert(i < gr->n_left && j < gr->n_right);
	bitset_set(gr->adj[i], j);
}

void bipartite_remv(bipartite_t *gr, int i, int j)
{
	assert(i < gr->n_left && j < gr->n_right);
	bitset_clear(gr->adj[i], j);
}

int bipartite_adj(const bipartite_t *gr, int i, int j)
{
	assert(i < gr->n_left && j < gr->n_right);
	return bitset_is_set(gr->adj[i], j);
}

static int apply_alternating_path(const bipartite_t *gr, int *matching,
		bitset_t *matched_left, bitset_t *matched_right)
{
	int left, right;
	int done_something = 0;
	bitset_t *tmp = bitset_alloca(gr->n_right);

	for(left = 0; left < gr->n_left; ++left) {
		bitset_t *left_adj = gr->adj[left];
		int i;

		bitset_copy(tmp, left_adj);

		if(matching[left] >= 0) {
			int old_right = matching[left];

			/* Check of all neighbors of the left node are already matched.
			 * We cannot improve this edge then. */
			if(bitset_contains(left_adj, matched_right))
				continue;

			bitset_andnot(tmp, matched_right);
			right = bitset_next_set(tmp, 0);

			assert(right != -1);

			/*
				We have to find another left node which has the old right one as a neighbor.
				This node must not be part of a matching
			*/
			for(i = 0; i < gr->n_left; ++i)
				if(i != left && bitset_is_set(gr->adj[i], old_right) && !bitset_is_set(matched_left, i))
					break;

			/* If no such node can be found, exit. */
			if(i >= gr->n_left)
				continue;

			/* Else, we can improve this edge. */
			matching[left] = right;
			matching[i] = old_right;
			bitset_set(matched_left, i);
			bitset_set(matched_right, right);
			done_something = 1;
		}


		/* We have to create a new single edge */
		else {
			assert(!bitset_is_set(matched_left, left));

			bitset_andnot(tmp, matched_right);
			if(bitset_popcnt(tmp) == 0)
				continue;

			right = bitset_min(tmp);
			assert(!bitset_is_set(matched_right, right));
			matching[left] = right;
			bitset_set(matched_left, left);
			bitset_set(matched_right, right);
			done_something = 1;
		}
	}

	return done_something;
}

void bipartite_matching(const bipartite_t *gr, int *matching)
{
	bitset_t *matched_left = bitset_alloca(gr->n_left);
	bitset_t *matched_right = bitset_alloca(gr->n_right);

	memset(matching, -1, gr->n_left * sizeof(int));
	while(apply_alternating_path(gr, matching, matched_left, matched_right));
}

void bipartite_dump_f(FILE *f, const bipartite_t *gr)
{
	int i;

	for(i = 0; i < gr->n_left; ++i) {
		fprintf(f, "%d: ", i);
		bitset_fprint(f, gr->adj[i]);
		fprintf(f, "\n");
	}
}

void bipartite_dump(const char *name, const bipartite_t *gr) {
	FILE *f = fopen(name, "w");

	if (f) {
		bipartite_dump_f(f, gr);
		fclose(f);
	}
}
