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
 * @brief       Performs lowering of perm nodes. Inserts copies to assure register constraints.
 * @author      Christian Wuerdig
 * @date        14.12.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "ircons.h"
#include "debug.h"
#include "irhooks.h"
#include "xmalloc.h"
#include "irnodeset.h"
#include "irgmod.h"
#include "iredges_t.h"
#include "irgwalk.h"

#include "bearch_t.h"
#include "belower.h"
#include "benode_t.h"
#include "besched_t.h"
#include "bestat.h"
#include "bessaconstr.h"
#include "benodesets.h"
#include "beintlive_t.h"

#undef KEEP_ALIVE_COPYKEEP_HACK

/* associates op with it's copy and CopyKeep */
typedef struct {
	ir_node *op;         /* an irn which must be different */
	ir_nodeset_t copies; /* all non-spillable copies of this irn */
	const arch_register_class_t *cls;
} op_copy_assoc_t;

/* environment for constraints */
typedef struct {
	be_irg_t       *birg;
	pset           *op_set;
	struct obstack obst;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
} constraint_env_t;

/* lowering walker environment */
typedef struct _lower_env_t {
	be_irg_t         *birg;
	const arch_env_t *arch_env;
	unsigned          do_copy : 1;
	DEBUG_ONLY(firm_dbg_module_t *dbg_module;)
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

//
/* Compare the two operands */
static int cmp_op_copy_assoc(const void *a, const void *b) {
	const op_copy_assoc_t *op1 = a;
	const op_copy_assoc_t *op2 = b;

	return op1->op != op2->op;
}

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
	ir_graph        *irg = get_irn_irg(irn);
	const arch_register_class_t *reg_class;
	const arch_env_t            *arch_env;
	lower_env_t     *env         = walk_env;
	int             real_size    = 0;
	int             keep_perm    = 0;
	int             n, i, pn, do_copy, j, n_ops;
	reg_pair_t      *pairs;
	const ir_edge_t *edge;
	perm_cycle_t    *cycle;
	ir_node         *sched_point, *block, *in[2];
	ir_node         *arg1, *arg2, *res1, *res2;
	ir_node         *cpyxchg = NULL;
	DEBUG_ONLY(firm_dbg_module_t *mod;)

	arch_env = env->arch_env;
	do_copy  = env->do_copy;
	DEBUG_ONLY(mod = env->dbg_module;)
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
			if (be_is_Perm(pairs[i].in_node) && is_Proj(pairs[i].in_node)) {
				set_Proj_proj(pairs[i].out_node, get_Proj_proj(pairs[i].in_node));
			}

#ifdef SCHEDULE_PROJS
			/* remove the proj from the schedule */
			sched_remove(pairs[i].out_node);
#endif

			/* reroute the edges from the proj to the argument */
			exchange(pairs[i].out_node, pairs[i].in_node);
			//edges_reroute(pairs[i].out_node, pairs[i].in_node, env->birg->irg);
			//set_irn_n(pairs[i].out_node, 0, new_Bad());

			pairs[i].checked = 1;
		}
	}

	/* Set do_copy to 0 if it's on but we have no free register */
	if (do_copy) {
		do_copy = 0;
	}

	real_size = n - get_n_checked_pairs(pairs, n);

	be_do_stat_perm(reg_class->name, reg_class->n_regs, irn, block, n, real_size);

	/* check for cycles and chains */
	while (get_n_checked_pairs(pairs, n) < n) {
		i = n_ops = 0;

		/* go to the first not-checked pair */
		while (pairs[i].checked) i++;
		cycle = xcalloc(1, sizeof(*cycle));
		cycle = get_perm_cycle(cycle, pairs, n, i);

		DB((mod, LEVEL_1, "%+F: following %s created:\n  ", irn, cycle->type == PERM_CHAIN ? "chain" : "cycle"));
		for (j = 0; j < cycle->n_elems; j++) {
			DB((mod, LEVEL_1, " %s", cycle->elems[j]->name));
		}
		DB((mod, LEVEL_1, "\n"));

		/*
			We don't need to do anything if we have a Perm with two
			elements which represents a cycle, because those nodes
			already represent exchange nodes
		*/
		if (n == 2 && cycle->type == PERM_CYCLE) {
			free(cycle);
			keep_perm = 1;
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

				cpyxchg = be_new_Perm(reg_class, irg, block, 2, in);
				n_ops++;

				if (i > 0) {
					/* cycle is not done yet */
					int pidx = get_pairidx_for_regidx(pairs, n, cycle->elems[i]->index, 0);

					/* create intermediate proj */
					res1 = new_r_Proj(irg, block, cpyxchg, get_irn_mode(res1), 0);

					/* set as in for next Perm */
					pairs[pidx].in_node = res1;
				}
				else {
#ifdef SCHEDULE_PROJS
					sched_remove(res1);
#endif
				}

#ifdef SCHEDULE_PROJS
				sched_remove(res2);
#endif

				set_Proj_pred(res2, cpyxchg);
				set_Proj_proj(res2, 0);
				set_Proj_pred(res1, cpyxchg);
				set_Proj_proj(res1, 1);

#ifdef SCHEDULE_PROJS
				sched_add_after(sched_point, res1);
				sched_add_after(sched_point, res2);
#endif
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

				cpyxchg = be_new_Copy(reg_class, irg, block, arg1);
				arch_set_irn_register(arch_env, cpyxchg, cycle->elems[i + 1]);
				n_ops++;

#ifdef SCHEDULE_PROJS
				/* remove the proj from the schedule */
				sched_remove(res2);
#endif
				/* exchange copy node and proj */
				exchange(res2, cpyxchg);

				/* insert the copy/exchange node in schedule after the magic schedule node (see above) */
				sched_add_after(sched_point, cpyxchg);

				/* set the new scheduling point */
				sched_point = cpyxchg;
			}
		}

		be_do_stat_permcycle(reg_class->name, irn, block, cycle->type == PERM_CHAIN, cycle->n_elems, n_ops);

		free((void *) cycle->elems);
		free(cycle);
	}

	/* remove the perm from schedule */
	if (! keep_perm) {
		sched_remove(irn);
		be_kill_node(irn);
	}
}



static int get_n_out_edges(const ir_node *irn) {
	const ir_edge_t *edge;
	int cnt = 0;

	foreach_out_edge(irn, edge) {
		cnt++;
	}

	return cnt;
}

static INLINE ir_node *belower_skip_proj(ir_node *irn) {
	while(is_Proj(irn))
		irn = get_Proj_pred(irn);
	return irn;
}

static ir_node *find_copy(constraint_env_t *env, ir_node *irn, ir_node *op) {
	const arch_env_t *arch_env = be_get_birg_arch_env(env->birg);
	ir_node          *block    = get_nodes_block(irn);
	ir_node          *cur_node;

	for (cur_node = sched_prev(irn);
		! is_Block(cur_node) && be_is_Copy(cur_node) && get_nodes_block(cur_node) == block;
		cur_node = sched_prev(cur_node))
	{
		if (be_get_Copy_op(cur_node) == op && arch_irn_is(arch_env, cur_node, dont_spill))
			return cur_node;
	}

	return NULL;
}

static void gen_assure_different_pattern(ir_node *irn, ir_node *other_different, constraint_env_t *env) {
	be_irg_t                    *birg     = env->birg;
	ir_graph                    *irg      = be_get_birg_irg(birg);
	pset                        *op_set   = env->op_set;
	const arch_env_t            *arch_env = be_get_birg_arch_env(birg);
	ir_node                     *block    = get_nodes_block(irn);
	const arch_register_class_t *cls      = arch_get_irn_reg_class(arch_env, other_different, -1);
	ir_node                     *in[2], *keep, *cpy;
	op_copy_assoc_t             key, *entry;
	DEBUG_ONLY(firm_dbg_module_t *mod     = env->dbg;)

	if (arch_irn_is(arch_env, other_different, ignore) || ! mode_is_datab(get_irn_mode(other_different))) {
		DBG((mod, LEVEL_1, "ignore constraint for %+F because other_irn is ignore or not a datab node\n", irn));
		return;
	}

	/* Make a not spillable copy of the different node   */
	/* this is needed because the different irn could be */
	/* in block far far away                             */
	/* The copy is optimized later if not needed         */

	/* check if already exists such a copy in the schedule immediatly before */
	cpy = find_copy(env, belower_skip_proj(irn), other_different);
	if (! cpy) {
		cpy = be_new_Copy(cls, irg, block, other_different);
		be_node_set_flags(cpy, BE_OUT_POS(0), arch_irn_flags_dont_spill);
		DBG((mod, LEVEL_1, "created non-spillable %+F for value %+F\n", cpy, other_different));
	}
	else {
		DBG((mod, LEVEL_1, "using already existing %+F for value %+F\n", cpy, other_different));
	}

	in[0] = irn;
	in[1] = cpy;

	/* Add the Keep resp. CopyKeep and reroute the users */
	/* of the other_different irn in case of CopyKeep.   */
	if (get_n_out_edges(other_different) == 0) {
		keep = be_new_Keep(cls, irg, block, 2, in);
	}
	else {
		keep = be_new_CopyKeep_single(cls, irg, block, cpy, irn, get_irn_mode(other_different));
		be_node_set_reg_class(keep, 1, cls);
	}

	DBG((mod, LEVEL_1, "created %+F(%+F, %+F)\n\n", keep, irn, cpy));

	/* insert copy and keep into schedule */
	assert(sched_is_scheduled(irn) && "need schedule to assure constraints");
	if (! sched_is_scheduled(cpy))
		sched_add_before(belower_skip_proj(irn), cpy);
	sched_add_after(irn, keep);

	/* insert the other different and it's copies into the set */
	key.op         = other_different;
	entry          = pset_find(op_set, &key, nodeset_hash(other_different));

	if (! entry) {
		entry         = obstack_alloc(&env->obst, sizeof(*entry));
		ir_nodeset_init(&entry->copies);
		entry->op     = other_different;
		entry->cls    = cls;
	}

	/* insert copy */
	ir_nodeset_insert(&entry->copies, cpy);

	/* insert keep in case of CopyKeep */
	if (be_is_CopyKeep(keep)) {
		ir_nodeset_insert(&entry->copies, keep);
	}

	pset_insert(op_set, entry, nodeset_hash(other_different));
}

/**
 * Checks if node has a should_be_different constraint in output
 * and adds a Keep then to assure the constraint.
 */
static void assure_different_constraints(ir_node *irn, constraint_env_t *env) {
	const arch_register_req_t *req;
	const arch_env_t          *arch_env = be_get_birg_arch_env(env->birg);

	req = arch_get_register_req(arch_env, irn, -1);

	if (arch_register_req_is(req, should_be_different)) {
		ir_node *different_from = get_irn_n(belower_skip_proj(irn), req->other_different);
		gen_assure_different_pattern(irn, different_from, env);
	} else if (arch_register_req_is(req, should_be_different_from_all)) {
		int i, n = get_irn_arity(belower_skip_proj(irn));
		for (i = 0; i < n; i++) {
			gen_assure_different_pattern(irn, get_irn_n(belower_skip_proj(irn), i), env);
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

	if (sched_is_scheduled(irn) && mode_is_datab(get_irn_mode(irn)))
		assure_different_constraints(irn, walk_env);

	return;
}

/**
 * Melt all copykeeps pointing to the same node
 * (or Projs of the same node), copying the same operand.
 */
static void melt_copykeeps(constraint_env_t *cenv) {
	be_irg_t *birg = cenv->birg;
	ir_graph *irg  = be_get_birg_irg(birg);
	op_copy_assoc_t *entry;

	/* for all */
	foreach_pset(cenv->op_set, entry) {
		int     idx, num_ck;
		ir_node *cp;
		struct obstack obst;
		ir_nodeset_iterator_t iter;
		ir_node **ck_arr, **melt_arr;

		obstack_init(&obst);

		/* collect all copykeeps */
		num_ck = idx = 0;
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			if (be_is_CopyKeep(cp)) {
				obstack_grow(&obst, &cp, sizeof(cp));
				++num_ck;
			}
#ifdef KEEP_ALIVE_COPYKEEP_HACK
			else {
				set_irn_mode(cp, mode_ANY);
				keep_alive(cp);
			}
#endif /* KEEP_ALIVE_COPYKEEP_HACK */
		}

		/* compare each copykeep with all other copykeeps */
		ck_arr = (ir_node **)obstack_finish(&obst);
		for (idx = 0; idx < num_ck; ++idx) {
			ir_node *ref, *ref_mode_T;

			if (ck_arr[idx]) {
				int j, n_melt;
				ir_node **new_ck_in;
				ir_node *new_ck;
				ir_node *sched_pt = NULL;

				n_melt     = 1;
				ref        = ck_arr[idx];
				ref_mode_T = skip_Proj(get_irn_n(ref, 1));
				obstack_grow(&obst, &ref, sizeof(ref));

				DBG((cenv->dbg, LEVEL_1, "Trying to melt %+F:\n", ref));

				/* check for copykeeps pointing to the same mode_T node as the reference copykeep */
				for (j = 0; j < num_ck; ++j) {
					ir_node *cur_ck = ck_arr[j];

					if (j != idx && cur_ck && skip_Proj(get_irn_n(cur_ck, 1)) == ref_mode_T) {
						obstack_grow(&obst, &cur_ck, sizeof(cur_ck));
						ir_nodeset_remove(&entry->copies, cur_ck);
						DBG((cenv->dbg, LEVEL_1, "\t%+F\n", cur_ck));
						ck_arr[j] = NULL;
						++n_melt;
						sched_remove(cur_ck);
					}
				}
				ck_arr[idx] = NULL;

				/* check, if we found some candidates for melting */
				if (n_melt == 1) {
					DBG((cenv->dbg, LEVEL_1, "\tno candidate found\n"));
					continue;
				}

				ir_nodeset_remove(&entry->copies, ref);
				sched_remove(ref);

				melt_arr = (ir_node **)obstack_finish(&obst);
				/* melt all found copykeeps */
				NEW_ARR_A(ir_node *, new_ck_in, n_melt);
				for (j = 0; j < n_melt; ++j) {
					new_ck_in[j] = get_irn_n(melt_arr[j], 1);

					/* now, we can kill the melted keep, except the */
					/* ref one, we still need some information      */
					if (melt_arr[j] != ref)
						be_kill_node(melt_arr[j]);
				}

#ifdef KEEP_ALIVE_COPYKEEP_HACK
				new_ck = be_new_CopyKeep(entry->cls, irg, get_nodes_block(ref), be_get_CopyKeep_op(ref), n_melt, new_ck_in, mode_ANY);
				keep_alive(new_ck);
#else
				new_ck = be_new_CopyKeep(entry->cls, irg, get_nodes_block(ref), be_get_CopyKeep_op(ref), n_melt, new_ck_in, get_irn_mode(ref));
#endif /* KEEP_ALIVE_COPYKEEP_HACK */

				/* set register class for all keeped inputs */
				for (j = 1; j <= n_melt; ++j)
					be_node_set_reg_class(new_ck, j, entry->cls);

				ir_nodeset_insert(&entry->copies, new_ck);

				/* find scheduling point */
				if (get_irn_mode(ref_mode_T) == mode_T) {
					/* walk along the Projs */
					for (sched_pt = sched_next(ref_mode_T); is_Proj(sched_pt) || be_is_Keep(sched_pt) || be_is_CopyKeep(sched_pt); sched_pt = sched_next(sched_pt))
						/* just walk along the schedule until a non-Proj/Keep/CopyKeep node is found*/ ;
				}
				else {
					sched_pt = ref_mode_T;
				}

				sched_add_before(sched_pt, new_ck);
				DBG((cenv->dbg, LEVEL_1, "created %+F, scheduled before %+F\n", new_ck, sched_pt));

				/* finally: kill the reference copykeep */
				be_kill_node(ref);
			}
		}

		obstack_free(&obst, NULL);
	}
}

/**
 * Walks over all nodes to assure register constraints.
 *
 * @param birg  The birg structure containing the irg
 */
void assure_constraints(be_irg_t *birg) {
	ir_graph         *irg      = be_get_birg_irg(birg);
	const arch_env_t *arch_env = be_get_birg_arch_env(birg);
	constraint_env_t cenv;
	op_copy_assoc_t  *entry;
	ir_node          **nodes;
	FIRM_DBG_REGISTER(firm_dbg_module_t *mod, "firm.be.lower.constr");

	be_assure_dom_front(birg);

	DEBUG_ONLY(cenv.dbg = mod;)
	cenv.birg   = birg;
	cenv.op_set = new_pset(cmp_op_copy_assoc, 16);
	obstack_init(&cenv.obst);

	irg_walk_blkwise_graph(irg, NULL, assure_constraints_walker, &cenv);

	/* melt copykeeps, pointing to projs of */
	/* the same mode_T node and keeping the */
	/* same operand                         */
	melt_copykeeps(&cenv);

	/* for all */
	foreach_pset(cenv.op_set, entry) {
		int     n;
		ir_node *cp;
		ir_nodeset_iterator_t iter;
		be_ssa_construction_env_t senv;

		n     = ir_nodeset_size(&entry->copies);
		nodes = alloca(n * sizeof(nodes[0]));

		/* put the node in an array */
		DBG((mod, LEVEL_1, "introduce copies for %+F ", entry->op));

		/* collect all copies */
		n = 0;
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			nodes[n++] = cp;
			DB((mod, LEVEL_1, ", %+F ", cp));
		}

		DB((mod, LEVEL_1, "\n"));

		/* introduce the copies for the operand and it's copies */
		be_ssa_construction_init(&senv, birg);
		be_ssa_construction_add_copy(&senv, entry->op);
		be_ssa_construction_add_copies(&senv, nodes, n);
		be_ssa_construction_fix_users(&senv, entry->op);
		be_ssa_construction_destroy(&senv);

		/* Could be that not all CopyKeeps are really needed, */
		/* so we transform unnecessary ones into Keeps.       */
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			if (be_is_CopyKeep(cp) && get_irn_n_edges(cp) < 1) {
				ir_node *keep;
				int     n = get_irn_arity(cp);

				keep = be_new_Keep(arch_get_irn_reg_class(arch_env, cp, -1),
					irg, get_nodes_block(cp), n, (ir_node **)&get_irn_in(cp)[1]);
				sched_add_before(cp, keep);

				/* Set all ins (including the block) of the CopyKeep BAD to keep the verifier happy. */
				sched_remove(cp);
				be_kill_node(cp);
			}
		}

		ir_nodeset_destroy(&entry->copies);
	}

	del_pset(cenv.op_set);
	obstack_free(&cenv.obst, NULL);
	be_liveness_invalidate(be_get_birg_liveness(birg));
}


/**
 * Push nodes that do not need to be permed through the Perm.
 * This is commonly a reload cascade at block ends.
 * @note This routine needs interference.
 * @note Probably, we can implement it a little more efficient.
 *       Especially searching the frontier lazily might be better.
 * @param perm The perm.
 * @param data The walker data (lower_env_t).
 * @return     1, if there is something left to perm over.
 *             0, if removed the complete perm.
 */
static int push_through_perm(ir_node *perm, void *data)
{
	lower_env_t *env       = data;
	const arch_env_t *aenv = env->arch_env;

	ir_graph *irg     = get_irn_irg(perm);
	ir_node *bl       = get_nodes_block(perm);
	int n             = get_irn_arity(perm);
	int *map          = alloca(n * sizeof(map[0]));
	ir_node **projs   = alloca(n * sizeof(projs[0]));
	bitset_t *keep    = bitset_alloca(n);
	ir_node *frontier = sched_first(bl);
	FIRM_DBG_REGISTER(firm_dbg_module_t *mod, "firm.be.lower.permmove");

	int i, new_size, n_keep;
	const ir_edge_t *edge;
	ir_node *last_proj, *irn;
	const arch_register_class_t *cls;

	DBG((mod, LEVEL_1, "perm move %+F irg %+F\n", perm, irg));

	/* get some proj and find out the register class of the proj. */
	foreach_out_edge (perm, edge) {
		last_proj  = get_edge_src_irn(edge);
		cls = arch_get_irn_reg_class(aenv, last_proj, -1);
		assert(is_Proj(last_proj));
		break;
	}

	/* find the point in the schedule after which the
	 * potentially movable nodes must be defined.
	 * A perm will only be pushed up to first instruction
	 * which lets an operand of itself die.  */

	sched_foreach_reverse_from (sched_prev(perm), irn) {
		for(i = get_irn_arity(irn) - 1; i >= 0; --i) {
			ir_node *op = get_irn_n(irn, i);
			if(arch_irn_consider_in_reg_alloc(aenv, cls, op)
				&& !values_interfere(env->birg, op, last_proj)) {
				frontier = sched_next(irn);
				goto found_front;
			}
		}
	}
found_front:

	DBG((mod, LEVEL_2, "\tfrontier: %+F\n", frontier));

	foreach_out_edge (perm, edge) {
		ir_node *proj  = get_edge_src_irn(edge);
		int nr         = get_Proj_proj(proj);
		ir_node *op    = get_irn_n(perm, nr);

		assert(nr < n);

		/* we will need the last Proj as an insertion point
		 * for the instruction(s) pushed through the Perm */
		if (sched_comes_after(last_proj, proj))
			last_proj = proj;

		projs[nr] = proj;

		bitset_set(keep, nr);
		if (!is_Proj(op) && get_nodes_block(op) == bl
				&& (op == frontier || sched_comes_after(frontier, op))) {
			for (i = get_irn_arity(op) - 1; i >= 0; --i) {
				ir_node *opop = get_irn_n(op, i);
				if (!arch_irn_consider_in_reg_alloc(aenv, cls, opop)) {
					bitset_clear(keep, nr);
					break;
				}
			}
		}
	}

	n_keep = bitset_popcnt(keep);

	/* well, we could not push enything through the perm */
	if (n_keep == n)
		return 1;

	assert(is_Proj(last_proj));

	DBG((mod, LEVEL_2, "\tkeep: %d, total: %d, mask: %b\n", n_keep, n, keep));
	last_proj = sched_next(last_proj);
	for (new_size = 0, i = 0; i < n; ++i) {
		ir_node *proj  = projs[i];

		if (bitset_is_set(keep, i)) {
			map[i] = new_size++;
			set_Proj_proj(proj, map[i]);
			DBG((mod, LEVEL_1, "\targ %d remap to %d\n", i, map[i]));
		}

		else {
			ir_node *move = get_irn_n(perm, i);

			DBG((mod, LEVEL_2, "\tmoving %+F before %+F, killing %+F\n", move, last_proj, proj));

			/* move the movable node in front of the Perm */
			sched_remove(move);
			sched_add_before(last_proj, move);

			/* give it the proj's register */
			arch_set_irn_register(aenv, move, arch_get_irn_register(aenv, proj));

			/* reroute all users of the proj to the moved node. */
			edges_reroute(proj, move, irg);

#ifdef SCHEDULE_PROJS
			/* remove the proj from the schedule. */
			sched_remove(proj);
#endif

			/* and like it to bad so it is no more in the use array of the perm */
			set_Proj_pred(proj, get_irg_bad(irg));

			map[i] = -1;
		}

	}

	if (n_keep > 0)
		be_Perm_reduce(perm, new_size, map);

	return n_keep > 0;
}

/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_after_ra_walker(ir_node *irn, void *walk_env) {
	if (! is_Block(irn) && ! is_Proj(irn)) {
		if (be_is_Perm(irn)) {
			int perm_stayed = push_through_perm(irn, walk_env);
			if (perm_stayed)
				lower_perm_node(irn, walk_env);
		}
	}

	return;
}

/**
 * Walks over all blocks in an irg and performs lowering need to be
 * done after register allocation (e.g. perm lowering).
 *
 * @param birg      The birg object
 * @param do_copy   1 == resolve cycles with a free reg if available
 */
void lower_nodes_after_ra(be_irg_t *birg, int do_copy) {
	lower_env_t env;
	ir_graph    *irg = be_get_birg_irg(birg);

	env.birg     = birg;
	env.arch_env = be_get_birg_arch_env(birg);
	env.do_copy  = do_copy;
	FIRM_DBG_REGISTER(env.dbg_module, "firm.be.lower");

	/* we will need interference */
	be_liveness_assure_chk(be_get_birg_liveness(birg));

	irg_walk_blkwise_graph(irg, NULL, lower_nodes_after_ra_walker, &env);
}
