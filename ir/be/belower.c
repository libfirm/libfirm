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

#include <stdlib.h>

#include "ircons.h"
#include "debug.h"

#include "bearch.h"
#include "belower.h"
#include "benode_t.h"
#include "bechordal_t.h"
#include "besched_t.h"

#include "irgmod.h"
#include "iredges_t.h"
#include "irgwalk.h"

#undef is_Perm
#define is_Perm(arch_env, irn) (arch_irn_classify(arch_env, irn) == arch_irn_class_perm)

#undef is_Call
#define is_Call(arch_env, irn) (arch_irn_classify(arch_env, irn) == arch_irn_class_call)

/* lowering walker environment */
typedef struct _lower_env_t {
	be_chordal_env_t  *chord_env;
	int                do_copy;
	firm_dbg_module_t *dbg_module;
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

/* structure to represent cycles or chains in a perm */
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

/* returns the number register pairs marked as checked */
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

	if (in_out) {
		for (i = 0; i < n; i++) {
			/* out register matches */
			if (pairs[i].out_reg->index == reg->index)
				return pairs[i].out_node;
		}
	}
	else {
		for (i = 0; i < n; i++) {
			/* in register matches */
			if (pairs[i].in_reg->index == reg->index)
				return pairs[i].in_node;
		}
	}

	return NULL;
}

/**
 * Gets the index in the register pair array where the in/out register
 * corresponds to reg_idx.
 *
 * @param pairs  The array of register pairs
 * @param n      The number of pairs
 * @param reg    The register index to look for
 * @param in_out 0 == look for IN register, 1 == look for OUT register
 * @return The corresponding index in pairs or -1 if not found
 */
static int get_pairidx_for_regidx(reg_pair_t *pairs, int n, int reg_idx, int in_out) {
	int i;

	if (in_out) {
		for (i = 0; i < n; i++) {
			/* out register matches */
			if (pairs[i].out_reg->index == reg_idx)
				return i;
		}
	}
	else {
		for (i = 0; i < n; i++) {
			/* in register matches */
			if (pairs[i].in_reg->index == reg_idx)
				return i;
		}
	}

	return -1;
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
	int cur_pair_idx = start;
	int n_pairs_done = get_n_checked_pairs(pairs, n);
	int idx, done = 0;
	perm_type_t cycle_tp = PERM_CYCLE;

	/* We could be right in the middle of a chain, so we need to find the start */
	while (head != cur_idx && !done) {
		/* goto previous register in cycle or chain */
		cur_pair_idx = get_pairidx_for_regidx(pairs, n, head, 1);

		if (cur_pair_idx < 0) {
			cycle_tp = PERM_CHAIN;
			done = 1;
		}
		else {
			head  = pairs[cur_pair_idx].in_reg->index;
			start = cur_pair_idx;
		}
	}

	/* assume worst case: all remaining pairs build a cycle or chain */
	cycle->elems    = calloc((n - n_pairs_done) * 2, sizeof(cycle->elems[0]));
	cycle->n_elems  = 2;  /* initial number of elements is 2 */
	cycle->elems[0] = pairs[start].in_reg;
	cycle->elems[1] = pairs[start].out_reg;
	cycle->type     = cycle_tp;
	n_pairs_done++;

	idx = 2;
	/* check for cycle or end of a chain */
	while (cur_idx != head && n_pairs_done < n) {
		/* goto next register in cycle or chain */
		cur_pair_idx = get_pairidx_for_regidx(pairs, n, cur_idx, 0);

		if (cur_pair_idx < 0)
			break;

		cur_idx = pairs[cur_pair_idx].out_reg->index;

		/* it's not the first element: insert it */
		if (cur_idx != head) {
			cycle->elems[idx++] = pairs[cur_pair_idx].out_reg;
			cycle->n_elems++;

			n_pairs_done++;
		}
		else {
			/* we are there where we started -> CYCLE */
			cycle->type = PERM_CYCLE;
		}
	}

	/* mark all pairs having one in/out register with cycle in common as checked */
	for (idx = 0; idx < cycle->n_elems; idx++) {
		cur_pair_idx = get_pairidx_for_regidx(pairs, n, cycle->elems[idx]->index, 0);

		if (cur_pair_idx >= 0)
			pairs[cur_pair_idx].checked = 1;

		cur_pair_idx = get_pairidx_for_regidx(pairs, n, cycle->elems[idx]->index, 1);

		if (cur_pair_idx >= 0)
			pairs[cur_pair_idx].checked = 1;
	}

	return cycle;
}

/**
 * Lowers a perm node.  Resolves cycles and creates a bunch of
 * copy and swap operations to permute registers.
 * Note: The caller of this function has to make sure, that irn
 *       is a Perm node.
 *
 * @param irn      The perm node
 * @param block    The block the perm node belongs to
 * @param walk_env The environment
 */
static void lower_perm_node(ir_node *irn, void *walk_env) {
	const arch_register_class_t *reg_class;
	const arch_env_t            *arch_env;
	firm_dbg_module_t           *mod;
	lower_env_t     *env = walk_env;
	reg_pair_t      *pairs;
	const ir_edge_t *edge;
	perm_cycle_t    *cycle;
	int              n, i, pn, do_copy, j;
	ir_node         *sched_point, *block, *in[2];
	ir_node         *arg1, *arg2, *res1, *res2;
	ir_node         *cpyxchg = NULL;

	arch_env = env->chord_env->main_env->arch_env;
	do_copy  = env->do_copy;
	mod      = env->dbg_module;
	block    = get_nodes_block(irn);

	/*
		Get the schedule predecessor node to the perm
		NOTE: This works with auto-magic. If we insert the
			new copy/exchange nodes after this node, everything
			should be ok.
	*/
	sched_point = sched_prev(irn);
	DBG((mod, LEVEL_1, "sched point is %+F\n", sched_point));
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
	for (i = 0; i < n; i++) {
		if (pairs[i].in_reg->index == pairs[i].out_reg->index) {
			DBG((mod, LEVEL_1, "%+F removing equal perm register pair (%+F, %+F, %s)\n",
				irn, pairs[i].in_node, pairs[i].out_node, pairs[i].out_reg->name));

			/* We have to check for a special case:
				The in-node could be a Proj from a Perm. In this case,
				we need to correct the projnum */
			if (is_Perm(arch_env, pairs[i].in_node) && is_Proj(pairs[i].in_node)) {
				set_Proj_proj(pairs[i].out_node, get_Proj_proj(pairs[i].in_node));
			}

			/* remove the proj from the schedule */
			sched_remove(pairs[i].out_node);

			/* reroute the edges from the proj to the argument */
			edges_reroute(pairs[i].out_node, pairs[i].in_node, env->chord_env->irg);

			pairs[i].checked = 1;
		}
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

		DB((mod, LEVEL_1, "%+F: following %s created:\n  ", irn, cycle->type == PERM_CHAIN ? "chain" : "cycle"));
		for (j = 0; j < cycle->n_elems; j++) {
			DB((mod, LEVEL_1, " %s", cycle->elems[j]->name));
		}
		DB((mod, LEVEL_1, "\n"));

		/* We don't need to do anything if we have a Perm with two
			elements which represents a cycle, because those nodes
			already represent exchange nodes */
		if (n == 2 && cycle->type == PERM_CYCLE) {
			free(cycle);
			continue;
		}

//TODO: - iff PERM_CYCLE && do_copy -> determine free temp reg and insert copy to/from it before/after
//        the copy cascade (this reduces the cycle into a chain)

		/* build copy/swap nodes */
		for (i = 0; i < cycle->n_elems - 1; i++) {
			arg1 = get_node_for_register(pairs, n, cycle->elems[i], 0);
			arg2 = get_node_for_register(pairs, n, cycle->elems[i + 1], 0);

			res1 = get_node_for_register(pairs, n, cycle->elems[i], 1);
			res2 = get_node_for_register(pairs, n, cycle->elems[i + 1], 1);

			/*
				If we have a cycle and don't copy: we need to create exchange nodes
				NOTE: An exchange node is a perm node with 2 INs and 2 OUTs
				IN_1  = in node with register i
				IN_2  = in node with register i + 1
				OUT_1 = out node with register i + 1
				OUT_2 = out node with register i
			*/
			if (cycle->type == PERM_CYCLE && !do_copy) {
				in[0] = arg1;
				in[1] = arg2;

				DBG((mod, LEVEL_1, "%+F creating exchange node (%+F, %s) and (%+F, %s) with\n",
					irn, arg1, cycle->elems[i]->name, arg2, cycle->elems[i + 1]->name));
				DBG((mod, LEVEL_1, "%+F                        (%+F, %s) and (%+F, %s)\n",
					irn, res1, cycle->elems[i]->name, res2, cycle->elems[i + 1]->name));

				cpyxchg = be_new_Perm(reg_class, env->chord_env->irg, block, 2, in);

				sched_remove(res1);
				sched_remove(res2);

				set_Proj_pred(res2, cpyxchg);
				set_Proj_proj(res2, 0);
				set_Proj_pred(res1, cpyxchg);
				set_Proj_proj(res1, 1);

				sched_add_after(sched_point, res1);
				sched_add_after(sched_point, res2);

				arch_set_irn_register(arch_env, res2, cycle->elems[i + 1]);
				arch_set_irn_register(arch_env, res1, cycle->elems[i]);
			}
			else {
				DBG((mod, LEVEL_1, "%+F creating copy node (%+F, %s) -> (%+F, %s)\n",
					irn, arg1, cycle->elems[i]->name, res2, cycle->elems[i + 1]->name));

				cpyxchg = be_new_Copy(reg_class, env->chord_env->irg, block, arg1);
				arch_set_irn_register(arch_env, cpyxchg, cycle->elems[i + 1]);

				/* remove the proj from the schedule */
				sched_remove(res2);

				/* exchange copy node and proj */
				exchange(res2, cpyxchg);
			}

			/* insert the copy/exchange node in schedule after the magic schedule node (see above) */
			sched_add_after(sched_point, cpyxchg);

			DBG((mod, LEVEL_1, "replacing %+F with %+F, placed new node after %+F\n", irn, cpyxchg, sched_point));
		}

		free(cycle->elems);
		free(cycle);
	}

	/* remove the perm from schedule */
	sched_remove(irn);
}



/**
 * Adds Projs to keep nodes for each register class, which eats the
 * caller saved registers.
 * Note: The caller has to make sure, that call is a Call
 *
 * @param call     The Call node
 * @param walk_env The walker environment
 */
static void lower_call_node(ir_node *call, const void *walk_env) {
	const arch_env_t            *arch_env = walk_env;
	const arch_register_class_t *reg_class;
	int                          i, j, set_size = 0, pn, keep_arity;
	arch_isa_t                  *isa      = arch_env_get_isa(arch_env);
	const ir_node               *proj_T   = NULL;
	ir_node                     **in_keep, *block = get_nodes_block(call);
	bitset_t                    *proj_set;
	const ir_edge_t             *edge;
	const arch_register_t       *reg;

	/* Prepare the bitset where we store the projnums which are already in use*/
	for (i = 0; i < arch_isa_get_n_reg_class(isa); i++) {
		reg_class  = arch_isa_get_reg_class(isa, i);
		set_size  += arch_register_class_n_regs(reg_class);
	}

	in_keep = malloc(set_size * sizeof(ir_node *));

	proj_set = bitset_malloc(set_size);
	bitset_clear_all(proj_set);

	/* check if there is a ProjT node and which arguments are used */
	foreach_out_edge(call, edge) {
		if (get_irn_mode(get_edge_src_irn(edge)) == mode_T)
			proj_T = get_edge_src_irn(edge);
	}

	/* set all used arguments */
	if (proj_T) {
		foreach_out_edge(proj_T, edge) {
			ir_node *proj = get_edge_src_irn(edge);

			assert(is_Proj(proj));
			bitset_set(proj_set, get_Proj_proj(proj));
		}
	}
	else {
		proj_T = new_r_Proj(current_ir_graph, block, call, mode_T, pn_Call_T_result);
		last_proj = call;
	}

	/* Create for each caller save register a proj (keep node argument) */
	/* if this proj is not already present */
	for (i = 0; i < arch_isa_get_n_reg_class(isa); i++) {

		/* reset the keep input, as we need one keep for each register class */
		memset(in_keep, 0, set_size * sizeof(ir_node *));
		keep_arity = 0;
		reg_class  = arch_isa_get_reg_class(isa, i);

		for (j = 0; j < arch_register_class_n_regs(reg_class); j++) {
			reg = arch_register_for_index(reg_class, j);

			/* only check caller save registers */
			if (arch_register_type_is(reg, caller_saved)) {
				pn = isa->impl->get_projnum_for_register(isa, reg);
				if (!bitset_is_set(proj_set, pn)) {
					ir_node *proj = new_r_Proj(current_ir_graph, block, (ir_node *)proj_T, mode_Is, pn);

					in_keep[keep_arity++] = proj;
				}
			}
		}

		/* ok, we found some caller save register which are not in use but must be saved */
		if (keep_arity) {
			be_new_Keep(reg_class, current_ir_graph, block, keep_arity, in_keep);
		}
	}

	bitset_free(proj_set);
	return;
}



/**
 * Calls the backend code generator functions to lower Spill and
 * Reload nodes into Store and Load. The backend is fully responsible
 * for creating the new nodes and setting their input correct.
 * Note: The caller of this has to make sure that irn is a Spill
 *       or Reload!
 *
 * @param irn      The Spill/Reload node
 * @param walk_env The walker environment
 */
static void lower_spill_reload(ir_node *irn, void *walk_env) {
	lower_env_t           *env  = walk_env;
	arch_code_generator_t *cg   = env->chord_env->main_env->cg;
	const arch_env_t      *aenv = env->chord_env->main_env->arch_env;
	ir_node               *res  = NULL;
	ir_node               *sched_point;

	if (be_is_Spill(irn) && cg->impl->lower_spill) {
		res = cg->impl->lower_spill(cg, irn);
	}
	else if (be_is_Reload(irn) && cg->impl->lower_reload) {
		res = cg->impl->lower_reload(cg, irn);
		if (res && res != irn) {
			/* copy the result register from the reload to the load */
			arch_set_irn_register(aenv, res, arch_get_irn_register(aenv, irn));
		}
	}

	if (res && res != irn) {
		sched_point = sched_prev(irn);
		sched_remove(irn);
		exchange(irn, res);
		sched_add_after(sched_point, res);
	}
	else {
		DBG((env->dbg_module, LEVEL_1, "node %+F not lowered\n", irn));
	}

	return;
}


/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_before_sched_walker(ir_node *irn, const void *walk_env) {
	const arch_env_t *arch_env = walk_env;

	if (!is_Block(irn) && !is_Proj(irn)) {
		if (is_Call(arch_env, irn)) {
			lower_call_node(irn, walk_env);
		}
	}

	return;
}


/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_after_ra_walker(ir_node *irn, void *walk_env) {
	lower_env_t      *env      = walk_env;
	const arch_env_t *arch_env = env->chord_env->main_env->arch_env;

	if (!is_Block(irn) && !is_Proj(irn)) {
		if (is_Perm(arch_env, irn)) {
			lower_perm_node(irn, walk_env);
		}
		else if (be_is_Spill(irn) || be_is_Reload(irn)) {
			lower_spill_reload(irn, walk_env);
		}
	}

	return;
}



/**
 * Walks over all blocks in an irg and performs lowering need
 * to be done before scheduling (e.g. call lowering).
 *
 * @param chord_env The chordal environment containing the irg
 * @param do_copy   1 == resolve cycles with a free reg if available
 */
void lower_nodes_before_sched(ir_graph *irg, const void *env) {
	irg_walk_blkwise_graph(irg, NULL, lower_nodes_before_sched_walker, env);
}



/**
 * Walks over all blocks in an irg and performs lowering need to be
 * done after register allocation (e.g. perm and spill/reload lowering).
 *
 * @param chord_env The chordal environment containing the irg
 * @param do_copy   1 == resolve cycles with a free reg if available
 */
void lower_nodes_after_ra(be_chordal_env_t *chord_env, int do_copy) {
	lower_env_t env;

	env.chord_env  = chord_env;
	env.do_copy    = do_copy;
	env.dbg_module = firm_dbg_register("ir.be.lower");

	irg_walk_blkwise_graph(chord_env->irg, NULL, lower_nodes_after_ra_walker, &env);
}

#undef is_Perm
#undef is_Call
