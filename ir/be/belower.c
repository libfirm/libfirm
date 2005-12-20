/**
 * Author:      Christian Wuerdig
 * Date:        2005/12/14
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:      $Id$
 *
 * Performs lowering of perm nodes and spill/reload optimization.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bearch.h"
#include "belower.h"
#include "benode_t.h"
#include "bechordal_t.h"
#include "besched.h"

#include "irgmod.h"
#include "iredges.h"
#include "irgwalk.h"

#undef is_Perm
#define is_Perm(arch_env, irn) (arch_irn_classify(arch_env, irn) == arch_irn_class_perm)

/* lowering walker environment */
typedef struct _lower_env_t {
	be_chordal_env_t *chord_env;
	int               do_copy;
} lower_env_t;

/* holds a perm register pair */
typedef struct _reg_pair_t {
	const arch_register_t *in_reg;    /**< a perm IN register */
	ir_node               *in_node;   /**< the in node to which the register belongs */

	const arch_register_t *out_reg;   /**< a perm OUT register */
	ir_node               *out_node;  /**< the out node to which the register belongs */

	int                    checked;   /**< indicates whether the pair was check for cycle or not */
} reg_pair_t;

typedef enum _perm_type_t {
	PERM_CYCLE,
	PERM_CHAIN,
	PERM_SWAP,
	PERM_COPY
} perm_type_t;

typedef struct _perm_cycle_t {
	const arch_register_t **elems;       /**< the registers in the cycle */
	int                     n_elems;     /**< number of elements in the cycle */
	perm_type_t             type;        /**< type (CHAIN or CYCLE) */
} perm_cycle_t;

/* Compare the in registers of two register pairs */
static int compare_reg_pair(const void *a, const void *b) {
	const reg_pair_t *pair_a = a;
	const reg_pair_t *pair_b = b;

	if (pair_a->in_reg->index > pair_b->in_reg->index)
		return 1;
	else
		return -1;
}

static int get_n_checked_pairs(reg_pair_t *pairs, int n) {
	int i, n_checked = 0;

	for (i = 0; i < n; i++) {
		if (pairs[i].checked)
			n_checked++;
	}

	return n_checked;
}

/**
 * Gets the node corresponding to a register from an array of register pairs.
 * NOTE: The given registers pairs and the register to look for must belong
 *       to the same register class.
 *
 * @param pairs  The array of register pairs
 * @param n      The number of pairs
 * @param reg    The register to look for
 * @param in_out 0 == look for IN register, 1 == look for OUT register
 * @return The corresponding node or NULL if not found
 */
static ir_node *get_node_for_register(reg_pair_t *pairs, int n, const arch_register_t *reg, int in_out) {
	int i;

	for (i = 0; i < n; i++) {
		if (in_out) {
			/* out register matches */
			if (pairs[i].out_reg->index == reg->index)
				return pairs[i].out_node;
		}
		else {
			/* in register matches */
			if (pairs[i].in_reg->index == reg->index)
				return pairs[i].in_node;
		}
	}

	return NULL;
}

/**
 * Gets an array of register pairs and tries to identify a cycle or chain starting
 * at position start.
 *
 * @param cycle Variable to hold the cycle
 * @param pairs Array of register pairs
 * @param start Index to start
 * @return The cycle or chain
 */
static perm_cycle_t *get_perm_cycle(perm_cycle_t *cycle, reg_pair_t *pairs, int n, int start) {
	int head         = pairs[start].in_reg->index;
	int cur_idx      = pairs[start].out_reg->index;
	int n_pairs_done = get_n_checked_pairs(pairs, n) + 1;
	int idx;

	/* assume worst case: all remaining pairs build a cycle or chain */
	cycle->elems    = calloc(n - n_pairs_done, sizeof(cycle->elems[0]));
	cycle->n_elems  = 2;  /* initial number of elements is 2 */
	cycle->elems[0] = pairs[start].in_reg;
	cycle->elems[1] = pairs[start].out_reg;
	cycle->type     = PERM_CHAIN; /* default is CHAIN, only changed when we found a cycle */

	/* mark the first pair as checked */
	pairs[start].checked = 1;

	idx = 2;
	/* check for cycle or end of a chain */
	while (cur_idx != head && n_pairs_done < n) {
		/* goto next register in cycle or chain */
		cur_idx = pairs[cur_idx].out_reg->index;

		/* it's not the first element: insert it */
		if (cur_idx != head) {
			cycle->elems[idx++] = pairs[cur_idx].out_reg;
			cycle->n_elems++;

			pairs[cur_idx].checked = 1;
			n_pairs_done++;
		}
		else {
			/* we are there where we started -> CYCLE */
			cycle->type = PERM_CYCLE;
		}
	}

	return cycle;
}

/**
 * Lowers a perm node.  Resolves cycles and creates a bunch of
 * copy and swap operations to permute registers.
 *
 * @param irn      The perm node
 * @param walk_env The environment
 */
static void lower_perms_walker(ir_node *irn, void *walk_env) {
	const be_node_factory_t     *fact;
	const arch_register_class_t *reg_class;
	const arch_env_t            *arch_env;
	lower_env_t     *env = walk_env;
	reg_pair_t      *pairs;
	const ir_edge_t *edge;
	perm_cycle_t    *cycle;
	int              n, i, pn, do_copy;
	ir_node         *block, *arg, *res, *sched_point, *in[2];
	ir_node         *cpyxchg = NULL;

	fact     = env->chord_env->main_env->node_factory;
	arch_env = env->chord_env->main_env->arch_env;
	do_copy  = env->do_copy;

	/* check if perm */
	if (! is_Perm(arch_env, irn))
		return;

	block = get_nodes_block(irn);

	/*
		Get the schedule predecessor node to the perm
		NOTE: This works with auto-magic. If we insert the
			new copy/exchange nodes after this node, everything
			should be ok.
	*/
	sched_point = sched_prev(irn);
	assert(sched_point && "Perm is not scheduled or has no predecessor");

	n = get_irn_arity(irn);
	assert(n == get_irn_n_edges(irn) && "perm's in and out numbers different");

	reg_class = arch_get_irn_register(arch_env, get_irn_n(irn, 0))->reg_class;
	pairs     = calloc(n, sizeof(pairs[0]));

	/* build the list of register pairs (in, out) */
	i = 0;
	foreach_out_edge(irn, edge) {
		pairs[i].out_node = get_edge_src_irn(edge);
		pn                = get_Proj_proj(pairs[i].out_node);
		pairs[i].in_node  = get_irn_n(irn, pn);

		pairs[i].in_reg  = arch_get_irn_register(arch_env, pairs[i].in_node);
		pairs[i].out_reg = arch_get_irn_register(arch_env, pairs[i].out_node);

		pairs[i].checked = 0;
		i++;
	}

	/* sort the register pairs by the indices of the in registers */
	qsort(pairs, n, sizeof(pairs[0]), compare_reg_pair);

	/* Mark all equal pairs as checked, and exchange the OUT proj with
		the IN node. */
	i = 0;
	while (pairs[i].in_reg->index == pairs[i].out_reg->index) {
		/* remove the proj from the schedule */
		sched_remove(pairs[i].out_node);

		/* remove the argument from schedule */
		sched_remove(pairs[i].in_node);

		/* exchange the proj with the argument */
		exchange(pairs[i].out_node, pairs[i].in_node);

		/* add the argument after the magic scheduling point */
		sched_add_after(sched_point, pairs[i].in_node);

		pairs[i++].checked = 1;
	}

	/* Set do_copy to 0 if it's on but we have no free register */
	if (do_copy) {
		do_copy = 0;
	}

	/* check for cycles and chains */
	while (get_n_checked_pairs(pairs, n) < n) {
		i = 0;

		/* go to the first not-checked pair */
		while (pairs[i].checked) i++;
		cycle = calloc(1, sizeof(*cycle));
		cycle = get_perm_cycle(cycle, pairs, n, i);

//todo: - iff PERM_CYCLE && do_copy -> determine free temp reg and insert copy to/from it before/after
//        the copy cascade (this reduces the cycle into a chain)

		/* build copy/swap nodes from back to front */
		for (i = cycle->n_elems - 2; i >= 0; i++) {
			arg = get_node_for_register(pairs, n, cycle->elems[i], 0);
			res = get_node_for_register(pairs, n, cycle->elems[i + 1], 1);

			/*
				If we have a cycle and don't copy: we need to create exchange nodes
				NOTE: An exchange node is a perm node with 2 INs and 2 OUTs
				IN_1  = in node with register i
				IN_2  = in node with register i + 1
				OUT_1 = out node with register i + 1
				OUT_2 = out node with register i
			*/
			if (cycle->type == PERM_CYCLE && !do_copy) {
				in[0] = arg;
				in[1] = get_node_for_register(pairs, n, cycle->elems[i + 1], 0);

				cpyxchg = new_Perm(fact, reg_class, env->chord_env->irg, block, 2, in);
				set_Proj_pred(res, cpyxchg);
				set_Proj_proj(res, 0);
				set_Proj_pred(get_node_for_register(pairs, n, cycle->elems[i], 1), cpyxchg);
				set_Proj_proj(get_node_for_register(pairs, n, cycle->elems[i], 1), 1);
			}
			else {
				cpyxchg = new_Copy(fact, reg_class, env->chord_env->irg, block, arg);
				arch_set_irn_register(arch_env, cpyxchg, cycle->elems[i + 1]);

				/* remove the proj from the schedule */
				sched_remove(res);

				/* exchange copy node and proj */
				exchange(res, cpyxchg);
			}

			/* insert the copy/exchange node in schedule after the magic schedule node (see above) */
			sched_add_after(sched_point, cpyxchg);
		}

		free(cycle->elems);
		free(cycle);
	}

	/* remove the perm from schedule */
	sched_remove(irn);
}

/**
 * Walks over all nodes in an irg and performs perm lowering.
 *
 * @param chord_env The chordal environment containing the irg
 * @param do_copy   1 == resolve cycles with a free reg if available
 */
void lower_perms(be_chordal_env_t *chord_env, int do_copy) {
	lower_env_t env;

	env.chord_env = chord_env;
	env.do_copy   = do_copy;

	irg_block_walk_graph(chord_env->irg,  NULL, lower_perms_walker, &env);
}
