/**
 * This is the C implementation of the trivial mst algo
 * originally written in Java by Sebastian Hack.
 * Performs simple copy minimzation.
 *
 * @author Christian Wuerdig
 * @date   27.04.2007
 * @id     $Id$
 */
#if 0

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "array.h"
#include "irnode.h"
#include "bitset.h"
#include "raw_bitset.h"
#include "irphase_t.h"

#include "bearch.h"
#include "beifg.h"
#include "be_t.h"

typedef struct _aff_chunk_t {
	ir_node **nodes;
	int     weight;
} aff_chunk_t;

typedef struct _aff_edge_t {
	ir_node *src;
	ir_node *tgt;
	double  weight;
} aff_edge_t;

/* main coalescing environment*/
typedef struct _co_mst_safe_env_t {
	int              n_regs;         /**< number of regs in class */
	int              k;              /**< number of non-ignore registers in class */
	bitset_t         *ignore_regs;   /**< set containing all global ignore registers */
	ir_phase         ph;             /**< phase object holding data for nodes */
	pqueue           *chunks;        /**< priority queue for chunks */
	be_ifg_t         *ifg;           /**< the interference graph */
	const arch_env_t *aenv;          /**< the arch environment */
} co_mst_safe_env_t;

/* stores coalescing related information for a node */
typedef struct _co_mst_safe_irn_t {
	ir_node     *irn;
	aff_chunk_t *chunk;
	bitset_t    *adm_colors;
	int         init_col;
	int         tmp_col;
	int         int_neigh;
	unsigned    fixed : 1;
} co_mst_safe_irn_t;

#define get_co_mst_safe_irn(mst_env, irn) (phase_get_or_set_irn_data(&(mst_env)->ph, (irn)))

/* compares two affinity edges */
static int cmp_aff_edge(const void *a, const void *b) {
	const aff_edge_t * const *e1 = d1;
	const aff_edge_t * const *e2 = d2;

	/* sort in descending order */
	return (*e1)->weight < (*e2)->weight ? 1 : -1;
}

/**
 * In case there is no phase information for irn, initialize it.
 */
static void *co_mst_safe_irn_init(ir_phase *ph, ir_node *irn, void *old) {
	co_mst_safe_irn_t *res = old ? old : phase_alloc(ph, sizeof(res[0]));
	co_mst_safe_env_t *env = ph->priv;

	if (res != old) {
		void                      *neigh_it = be_ifg_neighbours_iter_alloca(env->ifg);
		const arch_register_req_t *req;
		ir_node                   *m;

		res->irn       = irn;
		res->chunk     = NULL;
		res->fixed     = 0;
		res->tmp_col   = -1;
		res->int_neigh = 0;
		res->init_col  = arch_register_get_index(arch_get_irn_register(env->aenv, irn));

		/* set admissible registers */
		res->adm_colors = bitset_obstack_alloc(phase_obst(ph), env->n_regs);

		/* Exclude colors not assignable to the irn */
		req = arch_get_register_req(env->aenv, irn, -1);
		if (arch_register_req_is(req, limited))
			rbitset_copy_to_bitset(req->limited, res->adm_colors);

		/* exclude global ignore registers as well */
		bitset_andnot(res->adm_colors, env->ignore_regs);

		/* calculate the number of interfering neigbours */
		be_ifg_foreach_neighbour(env->ifg, neigh_it, irn, m) {
			if (! arch_irn_is(env->aenv, m, ignore))
				res->int_neigh++;
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
static void build_affinity_chunks(co_mst_safe_env_t *env) {
	void       *nodes_it = be_ifg_nodes_iter_alloca(env->ifg);
	aff_edge_t **edges   = NEW_ARR_F(aff_edge_t *, 0);
	int        i;

	/* at first we create the affinity edge objects */
	be_ifg_foreach_node(env->ifg, nodes_it, n) {
		int               n_idx = get_irn_idx(n);
		co_mst_safe_irn_t *n1;
		affinity_node_t   *an;

		if (arch_irn_is(env->aenv, n, ignore))
			continue;

		n1 = get_co_mst_safe_irn(env, n);
		an = get_affinity_info(co, n);

		if (an != NULL) {
			neighb_t *neigh;
			co_gs_foreach_neighb(an, neigh) {
				ir_node           *m    = neigh->irn;
				int               m_idx = get_irn_idx(m);
				co_mst_safe_irn_t *n2;

				if (arch_irn_is(env->aenv, m, ignore))
					continue;

				n2 = get_co_mst_safe_irn(env, m);

				/* record the edge in only one direction and only to non-ignore nodes */
				if (n_idx < m_idx) {
					aff_edge_t *edge = phase_alloc(&env->ph, sizeof(*edge));

					edge->src    = n;
					edge->tgt    = m;
					edge->weight = (double)neigh->cost / (double)(1 + n1->int_neigh + n2->int_neigh);
					ARR_APP1(aff_edge_t *, edges, edge);
				}
			}
		}
	}

	/* now: sort edges and build the chunks */
	qsort(edges, ARR_LEN(edges), sizeof(edges[0]), cmp_aff_edge);
	for (i = 0; i < ARR_LEN(edges); ++i) {

	}

	DEL_ARR_F(edges);
}

/**
 * Main driver for mst safe coalescing algorithm.
 */
int co_solve_heuristic_mst_safe(copy_opt_t *co)
{
	be_ifg_t *ifg         = co->cenv->ifg;
	unsigned n_regs       = co->cenv->cls->n_regs;
	bitset_t *ignore_regs = bitset_alloca(n_regs);
	unsigned k;
	co_mst_safe_env_t mst_env;

	/* init phase */
	phase_init(&mst_env.ph, "co_mst_safe", co->irg, PHASE_DEFAULT_GROWTH, co_mst_safe_irn_init, &mst_env);

	k = be_put_ignore_regs(co->cenv->birg, co->cenv->cls, ignore_regs);
	k = n_regs - k;

	mst_env.n_regs = n_regs;
	mst_env.k      = k;
	mst_env.chunks = new_pqueue();
	mst_env.co     = co;

	/* build affinity chunks */
	build_affinity_chunks(&mst_env);

	/* color chunks as long as there are some */
	while (! pqueue_empty(mst_env.chunks)) {
		aff_chunk_t *chunk = pqueue_get(mst_env.chunks);
		color_aff_chunk(&mst_env, chunk);
	}

	/* free allocated memory */
	del_pqueue(mst_env.chunks);
	phase_free(&mst_env.ph);
}

#endif
