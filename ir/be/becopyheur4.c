/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Simple copy minimization heuristics.
 * @author      Christian Wuerdig
 * @date        27.04.2007
 * @version     $Id$
 *
 * This is the C implementation of the mst algorithm
 * originally written in Java by Sebastian Hack.
 * (also known as "heur3" :)
 * Performs simple copy minimization.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <float.h>

#include "array.h"
#include "irnode_t.h"
#include "bitset.h"
#include "raw_bitset.h"
#include "irphase_t.h"
#include "pqueue.h"
#include "pset_new.h"
#include "xmalloc.h"
#include "pdeq.h"
#include "irprintf.h"
#include "irbitset.h"

#include "bearch.h"
#include "beifg.h"
#include "be_t.h"
#include "becopyopt_t.h"
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define COL_COST_INFEASIBLE       DBL_MAX
#define AFF_NEIGHBOUR_FIX_BENEFIT 128.0
#define NEIGHBOUR_CONSTR_COSTS    64.0

#define DBG_AFF_CHUNK(env, level, chunk) DEBUG_ONLY(do { if (firm_dbg_get_mask(dbg) & (level)) dbg_aff_chunk((env), (chunk)); } while(0))
#define DBG_COL_COST(env, level, cost)   DEBUG_ONLY(do { if (firm_dbg_get_mask(dbg) & (level)) dbg_col_cost((env), (cost)); } while(0))

static int last_chunk_id = 0;

typedef struct _col_cost_t {
	int    col;
	double cost;
} col_cost_t;

/**
 * An affinity chunk.
 */
typedef struct _aff_chunk_t {
	bitset_t *nodes;                /**< A bitset containing all nodes inside this chunk. */
	int      weight;                /**< Weight of this chunk */
	unsigned weight_consistent : 1; /**< Set if the weight is consistent. */
	int      id;                    /**< For debugging: An id of this chunk. */
} aff_chunk_t;

/**
 * An affinity edge.
 */
typedef struct _aff_edge_t {
	ir_node *src;                   /**< Source node. */
	ir_node *tgt;                   /**< Target node. */
	double  weight;                 /**< The weight of this edge. */
} aff_edge_t;

/* main coalescing environment */
typedef struct _co_mst_env_t {
	int              n_regs;         /**< number of regs in class */
	int              k;              /**< number of non-ignore registers in class */
	bitset_t         *ignore_regs;   /**< set containing all global ignore registers */
	int              *map_regs;      /**< map the available colors to the available registers */
	ir_phase         ph;             /**< phase object holding data for nodes */
	pqueue           *chunks;        /**< priority queue for chunks */
	pset_new_t       chunkset;       /**< set holding all chunks */
	be_ifg_t         *ifg;           /**< the interference graph */
	const arch_env_t *aenv;          /**< the arch environment */
	copy_opt_t       *co;            /**< the copy opt object */
} co_mst_env_t;

/* stores coalescing related information for a node */
typedef struct _co_mst_irn_t {
	ir_node     *irn;              /**< the irn this information belongs to */
	aff_chunk_t *chunk;            /**< the chunk this irn belongs to */
	bitset_t    *adm_colors;       /**< set of admissible colors for this irn */
	ir_node     **int_neighs;      /**< array of all interfering neighbours (cached for speed reasons) */
	int         n_neighs;          /**< length of the interfering neighbours array. */
	int         int_aff_neigh;     /**< number of interfering affinity neighbours */
	int         col;               /**< color currently assigned */
	int         init_col;          /**< the initial color */
	int         tmp_col;           /**< a temporary assigned color */
	unsigned    fixed     : 1;     /**< the color is fixed */
	unsigned    tmp_fixed : 1;     /**< the color is temporary fixed */
} co_mst_irn_t;

#define get_co_mst_irn(mst_env, irn) (phase_get_or_set_irn_data(&(mst_env)->ph, (irn)))

typedef int decide_func_t(const co_mst_irn_t *node, int col);

#ifdef DEBUG_libfirm

/**
 * Write a chunk to stderr for debugging.
 */
static void dbg_aff_chunk(const co_mst_env_t *env, const aff_chunk_t *c) {
	int idx;
	if (c->weight_consistent)
		ir_fprintf(stderr, " $%d ", c->weight);
	ir_fprintf(stderr, "{");
	bitset_foreach(c->nodes, idx) {
		ir_node *n = get_idx_irn(env->co->irg, idx);
		ir_fprintf(stderr, " %+F,", n);
	}
	ir_fprintf(stderr, "}");
}

/**
 * Dump all admissible colors to stderr.
 */
static void dbg_admissible_colors(const co_mst_env_t *env, const co_mst_irn_t *node) {
	int idx;
	if (bitset_popcnt(node->adm_colors) < 1)
		fprintf(stderr, "no admissible colors?!?");
	else {
		bitset_foreach(node->adm_colors, idx)
			fprintf(stderr, " %d", idx);
	}
}

/**
 * Dump color-cost pairs to stderr.
 */
static void dbg_col_cost(const co_mst_env_t *env, const col_cost_t *cost) {
	int i;
	for (i = 0; i < env->n_regs; ++i) {
		if (cost[i].cost == COL_COST_INFEASIBLE)
			fprintf(stderr, " (%d, INF)", cost[i].col);
		else
			fprintf(stderr, " (%d, %.1f)", cost[i].col, cost[i].cost);
	}
}

#endif /* DEBUG_libfirm */

static INLINE int get_mst_irn_col(const co_mst_irn_t *node) {
	return node->tmp_fixed ? node->tmp_col : node->col;
}

/**
 * @return 1 if node @p node has color @p col, 0 otherwise.
 */
static int decider_has_color(const co_mst_irn_t *node, int col) {
	return get_mst_irn_col(node) == col;
}

/**
 * @return 1 if node @p node has not color @p col, 0 otherwise.
 */
static int decider_hasnot_color(const co_mst_irn_t *node, int col) {
	return get_mst_irn_col(node) != col;
}

/**
 * Always returns true.
 */
static int decider_always_yes(const co_mst_irn_t *node, int col) {
	return 1;
}

/** compares two affinity edges by its weight */
static int cmp_aff_edge(const void *a, const void *b) {
	const aff_edge_t *e1 = a;
	const aff_edge_t *e2 = b;

	if (e2->weight == e1->weight) {
		if (e2->src->node_idx == e1->src->node_idx)
			return QSORT_CMP(e2->tgt->node_idx, e1->tgt->node_idx);
		else
			return QSORT_CMP(e2->src->node_idx, e1->src->node_idx);
	}
	/* sort in descending order */
	return QSORT_CMP(e2->weight, e1->weight);
}

/** compares to color-cost pairs */
static int cmp_col_cost(const void *a, const void *b) {
	const col_cost_t *c1 = a;
	const col_cost_t *c2 = b;

	return c1->cost < c2->cost ? -1 : 1;
}

/**
 * Creates a new affinity chunk
 */
static INLINE aff_chunk_t *new_aff_chunk(co_mst_env_t *env) {
	aff_chunk_t *c = xmalloc(sizeof(*c));
	c->weight            = -1;
	c->weight_consistent = 0;
	c->nodes             = bitset_irg_malloc(env->co->irg);
	c->id                = last_chunk_id++;
	pset_new_insert(&env->chunkset, c);
	return c;
}

/**
 * Frees all memory allocated by an affinity chunk.
 */
static INLINE void delete_aff_chunk(co_mst_env_t *env, aff_chunk_t *c) {
	pset_new_remove(&env->chunkset, c);
	bitset_free(c->nodes);
	free(c);
}

/**
 * Adds a node to an affinity chunk
 */
static INLINE void aff_chunk_add_node(aff_chunk_t *c, co_mst_irn_t *node) {
	c->weight_consistent = 0;
	node->chunk          = c;
	bitset_set(c->nodes, get_irn_idx(node->irn));
}

/**
 * In case there is no phase information for irn, initialize it.
 */
static void *co_mst_irn_init(ir_phase *ph, ir_node *irn, void *old) {
	co_mst_irn_t *res = old ? old : phase_alloc(ph, sizeof(res[0]));
	co_mst_env_t *env = ph->priv;

	if (res != old) {
		const arch_register_req_t *req;
		void     *nodes_it = be_ifg_nodes_iter_alloca(env->ifg);
		ir_node  *neigh;
		unsigned len;

		res->irn           = irn;
		res->chunk         = NULL;
		res->fixed         = 0;
		res->tmp_fixed     = 0;
		res->tmp_col       = -1;
		res->int_neighs    = NULL;
		res->int_aff_neigh = 0;
		res->col           = arch_register_get_index(arch_get_irn_register(env->aenv, irn));
		res->init_col      = res->col;

		DB((dbg, LEVEL_4, "Creating phase info for %+F\n", irn));

		/* set admissible registers */
		res->adm_colors = bitset_obstack_alloc(phase_obst(ph), env->n_regs);

		/* Exclude colors not assignable to the irn */
		req = arch_get_register_req(env->aenv, irn, -1);
		if (arch_register_req_is(req, limited))
			rbitset_copy_to_bitset(req->limited, res->adm_colors);
		else
			bitset_set_all(res->adm_colors);

		/* exclude global ignore registers as well */
		bitset_andnot(res->adm_colors, env->ignore_regs);

		/* set the number of interfering affinity neighbours to -1, they are calculated later */
		res->int_aff_neigh = -1;

		/* build list of interfering neighbours */
		len = 0;
		be_ifg_foreach_neighbour(env->ifg, nodes_it, irn, neigh) {
			obstack_ptr_grow(phase_obst(ph), neigh);
			++len;
		}
		res->int_neighs = obstack_finish(phase_obst(ph));
		res->n_neighs   = len;
	}
	return res;
}

/**
 * Check if affinity chunk @p chunk interferes with node @p irn.
 */
static INLINE int aff_chunk_interferes(co_mst_env_t *env, const aff_chunk_t *chunk, ir_node *irn) {
	const co_mst_irn_t *node = get_co_mst_irn(env, irn);
	const ir_node      *neigh;
	int                i;

	for (i = 0; i < node->n_neighs; ++i) {
		neigh = node->int_neighs[i];
		if (! arch_irn_is(env->aenv, neigh, ignore) && bitset_is_set(chunk->nodes, get_irn_idx(neigh)))
			return 1;
	}

	return 0;
}

/**
 * Check if there are interference edges from c1 to c2.
 * @param env   The global co_mst environment
 * @param c1    A chunk
 * @param c2    Another chunk
 * @return 1 if there are interferences between nodes of c1 and c2, 0 otherwise.
 */
static INLINE int aff_chunks_interfere(co_mst_env_t *env, const aff_chunk_t *c1, const aff_chunk_t *c2) {
	int idx;

	if (c1 == c2)
		return 0;

	/* check if there is a node in c2 having an interfering neighbor in c1 */
	bitset_foreach(c2->nodes, idx) {
		ir_node *n = get_idx_irn(env->co->irg, idx);

		if (aff_chunk_interferes(env, c1, n))
			return 1;
	}

	return 0;
}

/**
 * Returns the affinity chunk of @p irn or creates a new
 * one with @p irn as element if there is none assigned.
 */
static INLINE aff_chunk_t *get_aff_chunk(co_mst_env_t *env, ir_node *irn) {
	co_mst_irn_t *node = get_co_mst_irn(env, irn);
	return node->chunk;
}

/**
 * Let chunk(src) absorb the nodes of chunk(tgt) (only possible when there
 * are no interference edges from chunk(src) to chunk(tgt)).
 * @return 1 if successful, 0 if not possible
 */
static int aff_chunk_absorb(co_mst_env_t *env, ir_node *src, ir_node *tgt) {
	aff_chunk_t *c1 = get_aff_chunk(env, src);
	aff_chunk_t *c2 = get_aff_chunk(env, tgt);

	DEBUG_ONLY(
		DB((dbg, LEVEL_4, "Attempt to let c1 (id %d): ", c1 ? c1->id : -1));
		if (c1) {
			DBG_AFF_CHUNK(env, LEVEL_4, c1);
		} else {
			DB((dbg, LEVEL_4, "{%+F}", src));
		}
		DB((dbg, LEVEL_4, "\n\tabsorb c2 (id %d): ", c2 ? c2->id : -1));
		if (c2) {
			DBG_AFF_CHUNK(env, LEVEL_4, c2);
		} else {
			DB((dbg, LEVEL_4, "{%+F}", tgt));
		}
		DB((dbg, LEVEL_4, "\n"));
	)

	if (c1 == NULL) {
		if (c2 == NULL) {
			/* no chunk exists */
			co_mst_irn_t *mirn = get_co_mst_irn(env, src);
			int i;

			for (i = mirn->n_neighs - 1; i >= 0; --i) {
				if (mirn->int_neighs[i] == tgt)
					break;
			}
			if (i < 0) {
				/* create one containing both nodes */
				c1 = new_aff_chunk(env);
				aff_chunk_add_node(c1, get_co_mst_irn(env, src));
				aff_chunk_add_node(c1, get_co_mst_irn(env, tgt));
				goto absorbed;
			}
		} else {
			/* c2 already exists */
			if (! aff_chunk_interferes(env, c2, src)) {
				aff_chunk_add_node(c2, get_co_mst_irn(env, src));
				goto absorbed;
			}
		}
	} else if (c2 == NULL) {
		/* c1 already exists */
		if (! aff_chunk_interferes(env, c1, tgt)) {
			aff_chunk_add_node(c1, get_co_mst_irn(env, tgt));
			goto absorbed;
		}
	} else if (c1 != c2 && ! aff_chunks_interfere(env, c1, c2)) {
		int idx;

		bitset_or(c1->nodes, c2->nodes);
		c1->weight_consistent = 0;

		bitset_foreach(c2->nodes, idx) {
			ir_node      *n  = get_idx_irn(env->co->irg, idx);
			co_mst_irn_t *mn = get_co_mst_irn(env, n);
			mn->chunk = c1;
		}

		delete_aff_chunk(env, c2);
		goto absorbed;
	}
	DB((dbg, LEVEL_4, " ... c1 interferes with c2, skipped\n"));
	return 0;

absorbed:
	DB((dbg, LEVEL_4, " ... absorbed\n"));
	return 1;
}

/**
 * Assures that the weight of the given chunk is consistent.
 */
static void aff_chunk_assure_weight(const co_mst_env_t *env, aff_chunk_t *c) {
	if (! c->weight_consistent) {
		int w = 0;
		int idx;

		bitset_foreach(c->nodes, idx) {
			ir_node               *n  = get_idx_irn(env->co->irg, idx);
			const affinity_node_t *an = get_affinity_info(env->co, n);

			if (an != NULL) {
				neighb_t *neigh;
				co_gs_foreach_neighb(an, neigh) {
					const ir_node      *m    = neigh->irn;
					const int          m_idx = get_irn_idx(m);

					/* skip ignore nodes */
					if (arch_irn_is(env->aenv, m, ignore))
						continue;

					w += bitset_is_set(c->nodes, m_idx) ? neigh->costs : 0;
				}
			}
		}

		c->weight            = w;
		c->weight_consistent = 1;
	}
}

/**
 * Count the number of interfering affinity neighbours
 */
static int count_interfering_aff_neighs(co_mst_env_t *env, const affinity_node_t *an) {
	const neighb_t     *neigh;
	ir_node            *irn  = an->irn;
	const co_mst_irn_t *node = get_co_mst_irn(env, irn);
	int                res   = 0;

	co_gs_foreach_neighb(an, neigh) {
		const ir_node *n = neigh->irn;
		int           i;

		/* skip ignore nodes */
		if (arch_irn_is(env->aenv, n, ignore))
			continue;

		/* check if the affinity neighbour interfere */
		for (i = 0; i < node->n_neighs; ++i) {
			if (node->int_neighs[i] == n) {
				++res;
				break;
			}
		}
	}
	return res;
}


/**
 * Build chunks of nodes connected by affinity edges.
 * We start at the heaviest affinity edge.
 * The chunks of the two edge-defining nodes will be
 * merged if there are no interference edges from one
 * chunk to the other.
 */
static void build_affinity_chunks(co_mst_env_t *env) {
	void        *nodes_it = be_ifg_nodes_iter_alloca(env->ifg);
	aff_edge_t  *edges    = NEW_ARR_F(aff_edge_t, 0);
	ir_node     *n;
	int         i, len;
	aff_chunk_t *curr_chunk;
	pset_new_iterator_t iter;

	/* at first we create the affinity edge objects */
	be_ifg_foreach_node(env->ifg, nodes_it, n) {
		int             n_idx = get_irn_idx(n);
		co_mst_irn_t    *n1;
		affinity_node_t *an;

		/* skip ignore nodes */
		if (arch_irn_is(env->aenv, n, ignore))
			continue;

		n1 = get_co_mst_irn(env, n);
		an = get_affinity_info(env->co, n);

		if (an != NULL) {
			neighb_t *neigh;

			if (n1->int_aff_neigh < 0)
				n1->int_aff_neigh = count_interfering_aff_neighs(env, an);
			co_gs_foreach_neighb(an, neigh) {
				ir_node *m    = neigh->irn;
				int     m_idx = get_irn_idx(m);

				/* record the edge in only one direction */
				if (n_idx < m_idx) {
					co_mst_irn_t *n2;
					aff_edge_t   edge;

					/* skip ignore nodes */
					if (arch_irn_is(env->aenv, m, ignore))
						continue;

					edge.src = n;
					edge.tgt = m;

					n2 = get_co_mst_irn(env, m);
					if (n2->int_aff_neigh < 0) {
						affinity_node_t *am = get_affinity_info(env->co, m);
						n2->int_aff_neigh = count_interfering_aff_neighs(env, am);
					}
					edge.weight = (double)neigh->costs / (double)(1 + n1->int_aff_neigh + n2->int_aff_neigh);
					ARR_APP1(aff_edge_t, edges, edge);
				}
			}
		}
	}

	/* now: sort edges and build the affinity chunks */
	len = ARR_LEN(edges);
	qsort(edges, len, sizeof(edges[0]), cmp_aff_edge);
	for (i = 0; i < len; ++i) {
		DBG((dbg, LEVEL_1, "edge (%u,%u) %f\n", edges[i].src->node_idx, edges[i].tgt->node_idx, edges[i].weight));

		(void)aff_chunk_absorb(env, edges[i].src, edges[i].tgt);
	}

	/* now insert all chunks into a priority queue */
	foreach_pset_new(&env->chunkset, curr_chunk, iter) {
		aff_chunk_assure_weight(env, curr_chunk);

		DBG((dbg, LEVEL_1, "entry #%d", curr_chunk->id));
		DBG_AFF_CHUNK(env, LEVEL_1, curr_chunk);
		DBG((dbg, LEVEL_1, "\n"));

		pqueue_put(env->chunks, curr_chunk, curr_chunk->weight);
	}
	foreach_phase_irn(&env->ph, n) {
		co_mst_irn_t *mirn = get_co_mst_irn(env, n);

		if (mirn->chunk == NULL) {
			/* no chunk is allocated so far, do it now */
			aff_chunk_t *curr_chunk = new_aff_chunk(env);
			aff_chunk_add_node(curr_chunk, mirn);

			aff_chunk_assure_weight(env, curr_chunk);

			DBG((dbg, LEVEL_1, "entry #%d", curr_chunk->id));
			DBG_AFF_CHUNK(env, LEVEL_1, curr_chunk);
			DBG((dbg, LEVEL_1, "\n"));

			pqueue_put(env->chunks, curr_chunk, curr_chunk->weight);
		}
	}

	DEL_ARR_F(edges);
}

/**
 * Greedy collect affinity neighbours into thew new chunk @p chunk starting at node @p node.
 */
static void expand_chunk_from(co_mst_env_t *env, co_mst_irn_t *node, bitset_t *visited,
	aff_chunk_t *chunk, aff_chunk_t *orig_chunk, decide_func_t *decider, int col)
{
	waitq *nodes = new_waitq();

	DBG((dbg, LEVEL_1, "\nExpanding new chunk (id %d) from %+F:", chunk->id, node->irn));

	/* init queue and chunk */
	waitq_put(nodes, node);
	bitset_set(visited, get_irn_idx(node->irn));
	aff_chunk_add_node(chunk, node);
	DB((dbg, LEVEL_1, " %+F", node->irn));

	/* as long as there are nodes in the queue */
	while (! waitq_empty(nodes)) {
		co_mst_irn_t    *n  = waitq_get(nodes);
		affinity_node_t *an = get_affinity_info(env->co, n->irn);

		/* check all affinity neighbors */
		if (an != NULL) {
			neighb_t *neigh;
			co_gs_foreach_neighb(an, neigh) {
				ir_node      *m    = neigh->irn;
				int          m_idx = get_irn_idx(m);
				co_mst_irn_t *n2;

				/* skip ignore nodes */
				if (arch_irn_is(env->aenv, m, ignore))
					continue;

				n2 = get_co_mst_irn(env, m);

				if (! bitset_is_set(visited, m_idx)       &&
					decider(n2, col)                      &&
					! n2->fixed                           &&
					! aff_chunk_interferes(env, chunk, m) &&
					bitset_is_set(orig_chunk->nodes, m_idx))
				{
					/*
						following conditions are met:
						- neighbour is not visited
						- neighbour likes the color
						- neighbour has not yet a fixed color
						- the new chunk doesn't interfere with the neighbour
						- neighbour belongs or belonged once to the original chunk
					*/
					bitset_set(visited, m_idx);
					aff_chunk_add_node(chunk, n2);
					DB((dbg, LEVEL_1, " %+F", n2->irn));
					/* enqueue for further search */
					waitq_put(nodes, n2);
				}
			}
		}
	}

	DB((dbg, LEVEL_1, "\n"));

	del_waitq(nodes);
}

/**
 * Fragment the given chunk into chunks having given color and not having given color.
 */
static aff_chunk_t *fragment_chunk(co_mst_env_t *env, int col, aff_chunk_t *c, waitq *tmp) {
	bitset_t    *visited = bitset_irg_malloc(env->co->irg);
	int         idx;
	aff_chunk_t *best = NULL;

	bitset_foreach(c->nodes, idx) {
		ir_node       *irn;
		co_mst_irn_t  *node;
		aff_chunk_t   *tmp_chunk;
		decide_func_t *decider;
		int           check_for_best;

		if (bitset_is_set(visited, idx))
			continue;

		irn  = get_idx_irn(env->co->irg, idx);
		node = get_co_mst_irn(env, irn);

		if (get_mst_irn_col(node) == col) {
			decider        = decider_has_color;
			check_for_best = 1;
		}
		else {
			decider        = decider_hasnot_color;
			check_for_best = 0;
		}

		/* create a new chunk starting at current node */
		tmp_chunk = new_aff_chunk(env);
		waitq_put(tmp, tmp_chunk);
		expand_chunk_from(env, node, visited, tmp_chunk, c, decider, col);
		assert(bitset_popcnt(tmp_chunk->nodes) > 0 && "No nodes added to chunk");

		/* remember the local best */
		aff_chunk_assure_weight(env, tmp_chunk);
		if (check_for_best && (! best || best->weight < tmp_chunk->weight))
			best = tmp_chunk;
	}

	assert(best && "No chunk found?");
	bitset_free(visited);
	return best;
}

/**
 * Initializes an array of color-cost pairs.
 * Sets forbidden colors to costs COL_COST_INFEASIBLE and all others to @p c.
 */
static INLINE void col_cost_init(co_mst_env_t *env, col_cost_t *cost, double c) {
	int i;

	for (i = 0; i < env->n_regs; ++i) {
		cost[i].col = i;
		if (bitset_is_set(env->ignore_regs, i))
			cost[i].cost = COL_COST_INFEASIBLE;
		else
			cost[i].cost = c;
	}
}

/**
 * Initializes an array of color-cost pairs.
 * Sets all colors except color @p col to COL_COST_INFEASIBLE and @p col to 0.0
 */
static INLINE void col_cost_init_single(co_mst_env_t *env, col_cost_t *cost, int col) {
	assert(! bitset_is_set(env->ignore_regs, col) && "Attempt to use forbidden color.");
	col_cost_init(env, cost, COL_COST_INFEASIBLE);
	cost[col].col = 0;
	cost[0].col   = col;
	cost[0].cost  = 0.0;
}

/**
 * Resets the temporary fixed color of all nodes within wait queue @p nodes.
 * ATTENTION: the queue is empty after calling this function!
 */
static INLINE void reject_coloring(waitq *nodes) {
	while (! waitq_empty(nodes)) {
		co_mst_irn_t *n = waitq_get(nodes);
		n->tmp_fixed = 0;
	}
}

/**
 * Determines the costs for each color if it would be assigned to node @p node.
 */
static void determine_color_costs(co_mst_env_t *env, co_mst_irn_t *node, col_cost_t *costs) {
	affinity_node_t *an = get_affinity_info(env->co, node->irn);
	neighb_t        *aff_neigh;
	int             idx, i;

	col_cost_init(env, costs, 0.0);

	/* calculate (negative) costs for affinity neighbours */
	if (an != NULL) {
		co_gs_foreach_neighb(an, aff_neigh) {
			ir_node      *m = aff_neigh->irn;
			co_mst_irn_t *neigh;
			double       c;

			/* skip ignore nodes */
			if (arch_irn_is(env->aenv, m, ignore))
				continue;

			neigh = get_co_mst_irn(env, m);
			c     = (double)aff_neigh->costs;

			/* calculate costs for fixed affinity neighbours */
			if (neigh->tmp_fixed || neigh->fixed) {
				int col = get_mst_irn_col(neigh);
				costs[col].cost -= c * AFF_NEIGHBOUR_FIX_BENEFIT;
			}
		}
	}

	/* calculate (positive) costs for interfering neighbours */
	for (i = 0; i < node->n_neighs; ++i) {
		co_mst_irn_t *neigh;
		int          col, col_cnt;
		ir_node      *int_neigh;

		int_neigh = node->int_neighs[i];

		/* skip ignore nodes */
		if (arch_irn_is(env->aenv, int_neigh, ignore))
			continue;

		neigh   = get_co_mst_irn(env, int_neigh);
		col     = get_mst_irn_col(neigh);
		col_cnt = bitset_popcnt(neigh->adm_colors);

		if (neigh->tmp_fixed || neigh->fixed) {
			/* colors of fixed interfering neighbours are infeasible */
			costs[col].cost = COL_COST_INFEASIBLE;
		}
		else if (col_cnt < env->k) {
			/* calculate costs for constrained interfering neighbours */
			double ratio = 1.0 - ((double)col_cnt / (double)env->k);

			bitset_foreach_clear(neigh->adm_colors, idx) {
				/* check only explicitly forbidden colors (skip global forbidden ones) */
				if (! bitset_is_set(env->ignore_regs, idx)) {
					costs[col].cost += ratio * NEIGHBOUR_CONSTR_COSTS;
				}
			}
		}
	}

	/* set all not admissible colors to COL_COST_INFEASIBLE */
	bitset_foreach_clear(node->adm_colors, idx)
		costs[idx].cost = COL_COST_INFEASIBLE;
}

/* need forward declaration due to recursive call */
static int recolor_nodes(co_mst_env_t *env, co_mst_irn_t *node, col_cost_t *costs, waitq *changed_ones);

/**
 * Tries to change node to a color but @p explude_col.
 * @return 1 if succeeded, 0 otherwise.
 */
static int change_node_color_excluded(co_mst_env_t *env, co_mst_irn_t *node, int exclude_col, waitq *changed_ones) {
	int col = get_mst_irn_col(node);
	int res = 0;

	/* neighbours has already a different color -> good, temporary fix it */
	if (col != exclude_col) {
		node->tmp_fixed = 1;
		node->tmp_col   = col;
		waitq_put(changed_ones, node);
		return 1;
	}

	/* The node has the color it should not have _and_ has not been visited yet. */
	if (! (node->tmp_fixed || node->fixed)) {
		col_cost_t *costs = alloca(env->n_regs * sizeof(costs[0]));

		/* Get the costs for giving the node a specific color. */
		determine_color_costs(env, node, costs);

		/* Since the node must not have the not_col, set the costs for that color to "infinity" */
		costs[exclude_col].cost = COL_COST_INFEASIBLE;

		/* sort the colors according costs, cheapest first. */
		qsort(costs, env->n_regs, sizeof(costs[0]), cmp_col_cost);

		/* Try recoloring the node using the color list. */
		res = recolor_nodes(env, node, costs, changed_ones);
	}

	return res;
}

/**
 * Tries to bring node @p node to cheapest color and color all interfering neighbours with other colors.
 * ATTENTION: Expect @p costs already sorted by increasing costs.
 * @return 1 if coloring could be applied, 0 otherwise.
 */
static int recolor_nodes(co_mst_env_t *env, co_mst_irn_t *node, col_cost_t *costs, waitq *changed_ones) {
	int   i;
	waitq *local_changed = new_waitq();
	waitq *tmp           = new_waitq();

	DBG((dbg, LEVEL_1, "\tRecoloring %+F with color-costs", node->irn));
	DBG_COL_COST(env, LEVEL_1, costs);
	DB((dbg, LEVEL_1, "\n"));

	for (i = 0; i < env->n_regs; ++i) {
		int tgt_col  = costs[i].col;
		int neigh_ok = 1;
		int j;

		/* If the costs for that color (and all successive) are infinite, bail out we won't make it anyway. */
		if (costs[i].cost == COL_COST_INFEASIBLE) {
			node->tmp_fixed = 0;
			del_waitq(local_changed);
			del_waitq(tmp);
			return 0;
		}

		/* Set the new color of the node and mark the node as temporarily fixed. */
		assert(! node->tmp_fixed && "Node must not have been temporary fixed.");
		node->tmp_fixed = 1;
		node->tmp_col   = tgt_col;

		assert(waitq_empty(local_changed) && "Node queue should be empty here.");
		waitq_put(local_changed, node);

		/* try to color all interfering neighbours with current color forbidden */
		for (j = 0; j < node->n_neighs; ++j) {
			co_mst_irn_t *nn;
			ir_node      *neigh;

			neigh = node->int_neighs[j];

			/* skip ignore nodes */
			if (arch_irn_is(env->aenv, neigh, ignore))
				continue;

			nn = get_co_mst_irn(env, neigh);

			/*
				Try to change the color of the neighbor and record all nodes which
				get changed in the tmp list. Add this list to the "changed" list for
				that color. If we did not succeed to change the color of the neighbor,
				we bail out and try the next color.
			*/
			if (get_mst_irn_col(nn) == tgt_col) {
				/* try to color neighbour with tgt_col forbidden */
				neigh_ok = change_node_color_excluded(env, nn, tgt_col, tmp);

				/* join lists of changed nodes */
				while (! waitq_empty(tmp))
					waitq_put(local_changed, waitq_get(tmp));

				if (! neigh_ok)
					break;
			}
		}

		/*
			We managed to assign the target color to all neighbors, so from the perspective
			of the current node, every thing was ok and we can return safely.
		*/
		if (neigh_ok) {
			/* append the local_changed ones to global ones */
			while (! waitq_empty(local_changed))
				waitq_put(changed_ones, waitq_get(local_changed));
			del_waitq(local_changed);
			del_waitq(tmp);
			return 1;
		}
		else {
			/* coloring of neighbours failed, so we try next color */
			reject_coloring(local_changed);
		}
	}

	del_waitq(local_changed);
	del_waitq(tmp);
	return 0;
}

/**
 * Tries to bring node @p node and all it's neighbours to color @p tgt_col.
 * @return 1 if color @p col could be applied, 0 otherwise
 */
static int change_node_color(co_mst_env_t *env, co_mst_irn_t *node, int tgt_col, waitq *changed_ones) {
	int col = get_mst_irn_col(node);

	/* if node already has the target color -> good, temporary fix it */
	if (col == tgt_col) {
		DBG((dbg, LEVEL_4, "\t\tCNC: %+F has already color %d, fix temporary\n", node->irn, tgt_col));
		if (! node->tmp_fixed) {
			node->tmp_fixed = 1;
			node->tmp_col   = tgt_col;
			waitq_put(changed_ones, node);
		}
		return 1;
	}

	/*
		Node has not yet a fixed color and target color is admissible
		-> try to recolor node and it's affinity neighbours
	*/
	if (! (node->fixed || node->tmp_fixed) && bitset_is_set(node->adm_colors, tgt_col)) {
		col_cost_t *costs = alloca(env->n_regs * sizeof(costs[0]));
		int        res;

		col_cost_init_single(env, costs, tgt_col);

		DBG((dbg, LEVEL_4, "\t\tCNC: Attempt to recolor %+F ===>>\n", node->irn));
		res = recolor_nodes(env, node, costs, changed_ones);
		DBG((dbg, LEVEL_4, "\t\tCNC: <<=== Recoloring of %+F %s\n", node->irn, res ? "succeeded" : "failed"));

		return res;
	}

	DEBUG_ONLY(
		if (firm_dbg_get_mask(dbg) & LEVEL_4) {
			if (node->fixed || node->tmp_fixed)
				DB((dbg, LEVEL_4, "\t\tCNC: %+F has already fixed color %d\n", node->irn, col));
			else {
				DB((dbg, LEVEL_4, "\t\tCNC: color %d not admissible for %+F (", tgt_col, node->irn));
				dbg_admissible_colors(env, node);
				DB((dbg, LEVEL_4, ")\n"));
			}
		}
	)

	return 0;
}

/**
 * Tries to color an affinity chunk (or at least a part of it).
 * Inserts uncolored parts of the chunk as a new chunk into the priority queue.
 */
static void color_aff_chunk(co_mst_env_t *env, aff_chunk_t *c) {
	aff_chunk_t *best_chunk   = NULL;
	int         best_color    = -1;
	waitq       *changed_ones = new_waitq();
	waitq       *tmp_chunks   = new_waitq();
	bitset_t    *visited;
	int         col, idx;

	DB((dbg, LEVEL_2, "fragmentizing chunk #%d", c->id));
	DBG_AFF_CHUNK(env, LEVEL_2, c);
	DB((dbg, LEVEL_2, "\n"));


	/* check which color is the "best" for the given chunk */
	for (col = 0; col < env->k; ++col) {
		int         reg_col = env->map_regs[col];
		int         one_good = 0;
		aff_chunk_t *local_best;

		DB((dbg, LEVEL_3, "\ttrying color %d\n", reg_col));

		/* try to bring all nodes of given chunk to the current color. */
		bitset_foreach(c->nodes, idx) {
			ir_node      *irn  = get_idx_irn(env->co->irg, idx);
			co_mst_irn_t *node = get_co_mst_irn(env, irn);

			assert(! node->fixed && "Node must not have a fixed color.");

			DB((dbg, LEVEL_4, "\t\tBringing %+F from color %d to color %d ...\n", irn, node->col, reg_col));
			one_good |= change_node_color(env, node, reg_col, changed_ones);
			DB((dbg, LEVEL_4, "\t\t... %+F attempt from %d to %d %s\n", irn, node->col, reg_col, one_good ? "succeeded" : "failed"));
		}

		/* try next color when failed */
		if (! one_good)
			continue;

		/* fragment the chunk according to the coloring */
		local_best = fragment_chunk(env, reg_col, c, tmp_chunks);

		/* search the best of the good list
		   and make it the new best if it is better than the current */
		if (local_best) {
			aff_chunk_assure_weight(env, local_best);

			DB((dbg, LEVEL_4, "\t\tlocal best chunk (id %d) for color %d: ", local_best->id, reg_col));
			DBG_AFF_CHUNK(env, LEVEL_4, local_best);

			if (! best_chunk || best_chunk->weight < local_best->weight) {
				best_chunk = local_best;
				best_color = reg_col;
				DB((dbg, LEVEL_4, "\n\t\t... setting global best chunk (id %d), color %d\n", best_chunk->id, best_color));
			} else {
				DB((dbg, LEVEL_4, "\n\t\t... omitting, global best is better\n"));
			}
		}

		/* reject the coloring and bring the coloring to the initial state */
		reject_coloring(changed_ones);
	}

	/* free all intermediate created chunks except best one */
	while (! waitq_empty(tmp_chunks)) {
		aff_chunk_t *tmp = waitq_get(tmp_chunks);
		if (tmp != best_chunk)
			delete_aff_chunk(env, tmp);
	}
	del_waitq(tmp_chunks);

	/* return if coloring failed */
	if (! best_chunk) {
		del_waitq(changed_ones);
		return;
	}

	DB((dbg, LEVEL_2, "\tbest chunk #%d ", best_chunk->id));
	DBG_AFF_CHUNK(env, LEVEL_2, best_chunk);
	DB((dbg, LEVEL_2, "using color %d\n", best_color));

	/* get the best fragment from the best list and color it */
	bitset_foreach(best_chunk->nodes, idx) {
		ir_node      *irn  = get_idx_irn(env->co->irg, idx);
		co_mst_irn_t *node = get_co_mst_irn(env, irn);
		int          res;

		res = change_node_color(env, node, best_color, changed_ones);
		assert(res && "color manifesting failed");
		node->fixed = 1;
		node->chunk = best_chunk;
	}

	/* materialize colors on changed nodes */
	while (! waitq_empty(changed_ones)) {
		co_mst_irn_t *n = waitq_get(changed_ones);
		n->tmp_fixed = 0;
		n->col       = n->tmp_col;
	}

	/* remove the nodes in best chunk from original chunk */
	bitset_andnot(c->nodes, best_chunk->nodes);

	/* we have to get the nodes back into the original chunk because they are scattered over temporary chunks */
	bitset_foreach(c->nodes, idx) {
		ir_node      *n  = get_idx_irn(env->co->irg, idx);
		co_mst_irn_t *nn = get_co_mst_irn(env, n);
		nn->chunk = c;
	}

	/* fragment the remaining chunk */
	visited = bitset_irg_malloc(env->co->irg);
	bitset_or(visited, best_chunk->nodes);
	bitset_foreach(c->nodes, idx) {
		if (! bitset_is_set(visited, idx)) {
			aff_chunk_t  *new_chunk = new_aff_chunk(env);
			ir_node      *irn       = get_idx_irn(env->co->irg, idx);
			co_mst_irn_t *node      = get_co_mst_irn(env, irn);

			expand_chunk_from(env, node, visited, new_chunk, c, decider_always_yes, 0);
			aff_chunk_assure_weight(env, new_chunk);
			pqueue_put(env->chunks, new_chunk, new_chunk->weight);
		}
	}

	/* clear obsolete chunks and free some memory */
	delete_aff_chunk(env, best_chunk);
	bitset_free(visited);
	del_waitq(changed_ones);
}

/**
 * Main driver for mst safe coalescing algorithm.
 */
int co_solve_heuristic_mst(copy_opt_t *co)
{
	unsigned     n_regs       = co->cls->n_regs;
	bitset_t     *ignore_regs = bitset_alloca(n_regs);
	unsigned     k, idx, num;
	ir_node      *irn;
	co_mst_env_t mst_env;

	/* init phase */
	phase_init(&mst_env.ph, "co_mst", co->irg, PHASE_DEFAULT_GROWTH, co_mst_irn_init, &mst_env);

	k = be_put_ignore_regs(co->cenv->birg, co->cls, ignore_regs);
	k = n_regs - k;

	/* Create a color to register number map. In some architectures registers are ignore "in the middle"
	   of the register set. */
	mst_env.map_regs = NEW_ARR_D(int, phase_obst(&mst_env.ph), k);
	for (idx = num = 0; idx < n_regs; ++idx) {
		if (bitset_is_set(ignore_regs, idx))
			continue;
		mst_env.map_regs[num++] = idx;
	}
	assert(num == k);

	mst_env.n_regs      = n_regs;
	mst_env.k           = k;
	mst_env.chunks      = new_pqueue();
	mst_env.co          = co;
	mst_env.ignore_regs = ignore_regs;
	mst_env.ifg         = co->cenv->ifg;
	mst_env.aenv        = co->aenv;
	pset_new_init(&mst_env.chunkset);

	DBG((dbg, LEVEL_1, "==== Coloring %+F, class %s ====\n", co->irg, co->cls->name));

	/* build affinity chunks */
	build_affinity_chunks(&mst_env);

	/* color chunks as long as there are some */
	while (! pqueue_empty(mst_env.chunks)) {
		aff_chunk_t *chunk = pqueue_get(mst_env.chunks);

		color_aff_chunk(&mst_env, chunk);
		DB((dbg, LEVEL_4, "<<<====== Coloring chunk (%d) done\n", chunk->id));
		delete_aff_chunk(&mst_env, chunk);
	}

	/* apply coloring */
	foreach_phase_irn(&mst_env.ph, irn) {
		co_mst_irn_t *mirn = get_co_mst_irn(&mst_env, irn);
		const arch_register_t *reg;

		if (arch_irn_is(mst_env.aenv, irn, ignore))
			continue;

		assert(mirn->fixed && "Node should have fixed color");

		/* skip nodes where color hasn't changed */
		if (mirn->init_col == mirn->col)
			continue;

		reg = arch_register_for_index(co->cls, mirn->col);
		arch_set_irn_register(co->aenv, irn, reg);
		DB((dbg, LEVEL_1, "%+F set color from %d to %d\n", irn, mirn->init_col, mirn->col));
	}

	/* free allocated memory */
	del_pqueue(mst_env.chunks);
	phase_free(&mst_env.ph);
	pset_new_destroy(&mst_env.chunkset);

	return 0;
}

void be_init_copyheur4(void) {
	FIRM_DBG_REGISTER(dbg, "firm.be.co.heur4");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copyheur4);
