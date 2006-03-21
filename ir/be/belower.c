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

#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif

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
	int idx;
	perm_type_t cycle_tp = PERM_CYCLE;

	/* We could be right in the middle of a chain, so we need to find the start */
	while (head != cur_idx) {
		/* goto previous register in cycle or chain */
		cur_pair_idx = get_pairidx_for_regidx(pairs, n, head, 1);

		if (cur_pair_idx < 0) {
			cycle_tp = PERM_CHAIN;
			break;
		}
		else {
			head  = pairs[cur_pair_idx].in_reg->index;
			start = cur_pair_idx;
		}
	}

	/* assume worst case: all remaining pairs build a cycle or chain */
	cycle->elems    = xcalloc((n - n_pairs_done) * 2, sizeof(cycle->elems[0]));
	cycle->n_elems  = 2;  /* initial number of elements is 2 */
	cycle->elems[0] = pairs[start].in_reg;
	cycle->elems[1] = pairs[start].out_reg;
	cycle->type     = cycle_tp;
	cur_idx         = pairs[start].out_reg->index;

	idx = 2;
	/* check for cycle or end of a chain */
	while (cur_idx != head) {
		/* goto next register in cycle or chain */
		cur_pair_idx = get_pairidx_for_regidx(pairs, n, cur_idx, 0);

		if (cur_pair_idx < 0)
			break;

		cur_idx = pairs[cur_pair_idx].out_reg->index;

		/* it's not the first element: insert it */
		if (cur_idx != head) {
			cycle->elems[idx++] = pairs[cur_pair_idx].out_reg;
			cycle->n_elems++;
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

	arch_env = env->chord_env->birg->main_env->arch_env;
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
	pairs     = alloca(n * sizeof(pairs[0]));

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
		cycle = xcalloc(1, sizeof(*cycle));
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

		/* build copy/swap nodes from back to front */
		for (i = cycle->n_elems - 2; i >= 0; i--) {
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

				/* At this point we have to handle the following problem:     */
				/*                                                            */
				/* If we have a cycle with more than two elements, then       */
				/* this could correspond to the following Perm node:          */
				/*                                                            */
				/*   +----+   +----+   +----+                                 */
				/*   | r1 |   | r2 |   | r3 |                                 */
				/*   +-+--+   +-+--+   +--+-+                                 */
				/*     |        |         |                                   */
				/*     |        |         |                                   */
				/*   +-+--------+---------+-+                                 */
				/*   |         Perm         |                                 */
				/*   +-+--------+---------+-+                                 */
				/*     |        |         |                                   */
				/*     |        |         |                                   */
				/*   +-+--+   +-+--+   +--+-+                                 */
				/*   |Proj|   |Proj|   |Proj|                                 */
				/*   | r2 |   | r3 |   | r1 |                                 */
				/*   +----+   +----+   +----+                                 */
				/*                                                            */
				/* This node is about to be split up into two 2x Perm's       */
				/* for which we need 4 Proj's and the one additional Proj     */
				/* of the first Perm has to be one IN of the second. So in    */
				/* general we need to create one additional Proj for each     */
				/* "middle" Perm and set this to one in node of the successor */
				/* Perm.                                                      */

				DBG((mod, LEVEL_1, "%+F creating exchange node (%+F, %s) and (%+F, %s) with\n",
					irn, arg1, cycle->elems[i]->name, arg2, cycle->elems[i + 1]->name));
				DBG((mod, LEVEL_1, "%+F                        (%+F, %s) and (%+F, %s)\n",
					irn, res1, cycle->elems[i]->name, res2, cycle->elems[i + 1]->name));

				cpyxchg = be_new_Perm(reg_class, env->chord_env->irg, block, 2, in);

				if (i > 0) {
					/* cycle is not done yet */
					int pidx = get_pairidx_for_regidx(pairs, n, cycle->elems[i]->index, 0);

					/* create intermediate proj */
					res1 = new_r_Proj(get_irn_irg(irn), block, cpyxchg, get_irn_mode(res1), 0);

					/* set as in for next Perm */
					pairs[pidx].in_node = res1;
				}
				else {
					sched_remove(res1);
				}

				sched_remove(res2);

				set_Proj_pred(res2, cpyxchg);
				set_Proj_proj(res2, 0);
				set_Proj_pred(res1, cpyxchg);
				set_Proj_proj(res1, 1);

				sched_add_after(sched_point, res1);
				sched_add_after(sched_point, res2);

				arch_set_irn_register(arch_env, res2, cycle->elems[i + 1]);
				arch_set_irn_register(arch_env, res1, cycle->elems[i]);

				/* insert the copy/exchange node in schedule after the magic schedule node (see above) */
				sched_add_after(sched_point, cpyxchg);

				DBG((mod, LEVEL_1, "replacing %+F with %+F, placed new node after %+F\n", irn, cpyxchg, sched_point));

				/* set the new scheduling point */
				sched_point = res1;
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

				/* insert the copy/exchange node in schedule after the magic schedule node (see above) */
				sched_add_after(sched_point, cpyxchg);

				/* set the new scheduling point */
				sched_point = cpyxchg;
			}
		}

		free((void *) cycle->elems);
		free(cycle);
	}

	/* remove the perm from schedule */
	sched_remove(irn);
}



static int get_n_out_edges(const ir_node *irn) {
	const ir_edge_t *edge;
	int cnt = 0;

	foreach_out_edge(irn, edge) {
		cnt++;
	}

	return cnt;
}

static ir_node *belower_skip_proj(ir_node *irn) {
	while(is_Proj(irn))
		irn = get_Proj_pred(irn);
	return irn;
}

static void fix_in(ir_node *irn, ir_node *old, ir_node *nw) {
	int i, n;

	irn = belower_skip_proj(irn);
	n   = get_irn_arity(irn);

	for (i = 0; i < n; i++) {
		if (get_irn_n(irn, i) == old) {
			set_irn_n(irn, i, nw);
			break;
		}
	}
}

static void gen_assure_different_pattern(ir_node *irn, be_irg_t *birg, ir_node *other_different) {
	const arch_env_t          *arch_env = birg->main_env->arch_env;
	ir_node                   *in[2], *keep, *cpy, *temp;
	ir_node                   *block = get_nodes_block(irn);
	firm_dbg_module_t         *mod   = firm_dbg_register("firm.be.lower");
	const arch_register_class_t *cls = arch_get_irn_reg_class(arch_env, other_different, -1);

	if (arch_irn_is(arch_env, other_different, ignore) || ! mode_is_datab(get_irn_mode(other_different))) {
		DBG((mod, LEVEL_1, "ignore constraint for %+F because other_irn is ignore or not a datab node\n", irn));
		return;
	}

	/* Make a not spillable copy of the different node   */
	/* this is needed because the different irn could be */
	/* in block far far away                             */
	/* The copy is optimized later if not needed         */

	temp = new_rd_Unknown(birg->irg, get_irn_mode(other_different));
	cpy = be_new_Copy(cls, birg->irg, block, temp);
	be_node_set_flags(cpy, BE_OUT_POS(0), arch_irn_flags_dont_spill);

	in[0] = irn;
	in[1] = cpy;

	/* Let the irn use the copy instead of the old other_different */
	fix_in(irn, other_different, cpy);

	/* Add the Keep resp. CopyKeep and reroute the users */
	/* of the other_different irn in case of CopyKeep.   */
	if (get_n_out_edges(other_different) == 0) {
		keep = be_new_Keep(cls, birg->irg, block, 2, in);
	}
	else {
		keep = be_new_CopyKeep_single(cls, birg->irg, block, cpy, irn, get_irn_mode(other_different));
		edges_reroute(other_different, keep, birg->irg);
	}

	/* after rerouting: let the copy point to the other_different irn */
	set_irn_n(cpy, 0, other_different);

	DBG((mod, LEVEL_1, "created %+F for %+F to assure should_be_different\n", keep, irn));
}

/**
 * Checks if node has a should_be_different constraint in output
 * and adds a Keep then to assure the constraint.
 */
static void assure_different_constraints(ir_node *irn, be_irg_t *birg) {
	const arch_env_t          *arch_env = birg->main_env->arch_env;
	const arch_register_req_t *req;
	arch_register_req_t        req_temp;
	int i, n;

	req = arch_get_register_req(arch_env, &req_temp, irn, -1);

	if (req) {
		if (arch_register_req_is(req, should_be_different)) {
			gen_assure_different_pattern(irn, birg, req->other_different);
		}
		else if (arch_register_req_is(req, should_be_different_from_all)) {
			n = get_irn_arity(belower_skip_proj(irn));
			for (i = 0; i < n; i++) {
				gen_assure_different_pattern(irn, birg, get_irn_n(belower_skip_proj(irn), i));
			}
		}
	}
}



/**
 * Calls the functions to assure register constraints.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void assure_constraints_walker(ir_node *irn, void *walk_env) {
	if (is_Block(irn))
		return;

	if (mode_is_datab(get_irn_mode(irn)))
		assure_different_constraints(irn, walk_env);

	return;
}



/**
 * Walks over all nodes to assure register constraints.
 *
 * @param birg  The birg structure containing the irg
 */
void assure_constraints(be_irg_t *birg) {
	irg_walk_blkwise_graph(birg->irg, NULL, assure_constraints_walker, birg);
}



/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_after_ra_walker(ir_node *irn, void *walk_env) {
	lower_env_t      *env      = walk_env;
	const arch_env_t *arch_env = env->chord_env->birg->main_env->arch_env;

	if (!is_Block(irn) && !is_Proj(irn)) {
		if (is_Perm(arch_env, irn)) {
			lower_perm_node(irn, walk_env);
		}
	}

	return;
}



/**
 * Walks over all blocks in an irg and performs lowering need to be
 * done after register allocation (e.g. perm lowering).
 *
 * @param chord_env The chordal environment containing the irg
 * @param do_copy   1 == resolve cycles with a free reg if available
 */
void lower_nodes_after_ra(be_chordal_env_t *chord_env, int do_copy) {
	lower_env_t env;

	env.chord_env  = chord_env;
	env.do_copy    = do_copy;
	env.dbg_module = firm_dbg_register("firm.be.lower");

	irg_walk_blkwise_graph(chord_env->irg, NULL, lower_nodes_after_ra_walker, &env);
}

#undef is_Perm
#undef is_Call
