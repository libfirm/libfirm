/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
#include "config.h"

#include <stdlib.h>

#include "ircons.h"
#include "debug.h"
#include "irhooks.h"
#include "xmalloc.h"
#include "irnodeset.h"
#include "irnodemap.h"
#include "irgmod.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "array_t.h"

#include "bearch_t.h"
#include "belower.h"
#include "benode_t.h"
#include "besched_t.h"
#include "bestat.h"
#include "bessaconstr.h"
#include "beintlive_t.h"

#undef KEEP_ALIVE_COPYKEEP_HACK

DEBUG_ONLY(static firm_dbg_module_t *dbg;)
DEBUG_ONLY(static firm_dbg_module_t *dbg_constr;)

/** Associates an ir_node with it's copy and CopyKeep. */
typedef struct {
	ir_nodeset_t copies; /**< all non-spillable copies of this irn */
	const arch_register_class_t *cls;
} op_copy_assoc_t;

/** Environment for constraints. */
typedef struct {
	be_irg_t       *birg;
	ir_nodemap_t   op_set;
	struct obstack obst;
} constraint_env_t;

/** Lowering walker environment. */
typedef struct _lower_env_t {
	be_irg_t         *birg;
	unsigned          do_copy : 1;
} lower_env_t;

/** Holds a Perm register pair. */
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

/** Structure to represent cycles or chains in a Perm. */
typedef struct _perm_cycle_t {
	const arch_register_t **elems;       /**< the registers in the cycle */
	int                     n_elems;     /**< number of elements in the cycle */
	perm_type_t             type;        /**< type (CHAIN or CYCLE) */
} perm_cycle_t;

/** Compare the in registers of two register pairs. */
static int compare_reg_pair(const void *a, const void *b) {
	const reg_pair_t *pair_a = a;
	const reg_pair_t *pair_b = b;

	if (pair_a->in_reg->index > pair_b->in_reg->index)
		return 1;
	else
		return -1;
}

/** returns the number register pairs marked as checked. */
static int get_n_checked_pairs(reg_pair_t *pairs, int n) {
	int i, n_checked = 0;

	for (i = 0; i < n; i++) {
		if (pairs[i].checked)
			n_checked++;
	}

	return n_checked;
}

/**
 * Gets the node corresponding to an IN register from an array of register pairs.
 * NOTE: The given registers pairs and the register to look for must belong
 *       to the same register class.
 *
 * @param pairs  The array of register pairs
 * @param n      The number of pairs
 * @param reg    The register to look for
 * @return The corresponding node or NULL if not found
 */
static ir_node *get_node_for_in_register(reg_pair_t *pairs, int n, const arch_register_t *reg) {
	int i;

	for (i = 0; i < n; i++) {
		/* in register matches */
		if (pairs[i].in_reg->index == reg->index)
			return pairs[i].in_node;
	}

	return NULL;
}

/**
 * Gets the node corresponding to an OUT register from an array of register pairs.
 * NOTE: The given registers pairs and the register to look for must belong
 *       to the same register class.
 *
 * @param pairs  The array of register pairs
 * @param n      The number of pairs
 * @param reg    The register to look for
 * @return The corresponding node or NULL if not found
 */
static ir_node *get_node_for_out_register(reg_pair_t *pairs, int n, const arch_register_t *reg) {
	int i;

	for (i = 0; i < n; i++) {
		/* out register matches */
		if (pairs[i].out_reg->index == reg->index)
			return pairs[i].out_node;
	}

	return NULL;
}

/**
 * Gets the index in the register pair array where the in register
 * corresponds to reg_idx.
 *
 * @param pairs  The array of register pairs
 * @param n      The number of pairs
 * @param reg    The register index to look for
 *
 * @return The corresponding index in pairs or -1 if not found
 */
static int get_pairidx_for_in_regidx(reg_pair_t *pairs, int n, unsigned reg_idx) {
	int i;

	for (i = 0; i < n; i++) {
		/* in register matches */
		if (pairs[i].in_reg->index == reg_idx)
			return i;
	}
	return -1;
}

/**
 * Gets the index in the register pair array where the out register
 * corresponds to reg_idx.
 *
 * @param pairs  The array of register pairs
 * @param n      The number of pairs
 * @param reg    The register index to look for
 *
 * @return The corresponding index in pairs or -1 if not found
 */
static int get_pairidx_for_out_regidx(reg_pair_t *pairs, int n, unsigned reg_idx) {
	int i;

	for (i = 0; i < n; i++) {
		/* out register matches */
		if (pairs[i].out_reg->index == reg_idx)
			return i;
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
	unsigned head    = pairs[start].in_reg->index;
	unsigned cur_idx = pairs[start].out_reg->index;
	int cur_pair_idx = start;
	int n_pairs_done = get_n_checked_pairs(pairs, n);
	int idx;
	perm_type_t cycle_tp = PERM_CYCLE;

	/* We could be right in the middle of a chain, so we need to find the start */
	while (head != cur_idx) {
		/* goto previous register in cycle or chain */
		cur_pair_idx = get_pairidx_for_out_regidx(pairs, n, head);

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
	cycle->elems    = XMALLOCNZ(const arch_register_t*, (n - n_pairs_done) * 2);
	cycle->n_elems  = 2;  /* initial number of elements is 2 */
	cycle->elems[0] = pairs[start].in_reg;
	cycle->elems[1] = pairs[start].out_reg;
	cycle->type     = cycle_tp;
	cur_idx         = pairs[start].out_reg->index;

	idx = 2;
	/* check for cycle or end of a chain */
	while (cur_idx != head) {
		/* goto next register in cycle or chain */
		cur_pair_idx = get_pairidx_for_in_regidx(pairs, n, cur_idx);

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
		cur_pair_idx = get_pairidx_for_in_regidx(pairs, n, cycle->elems[idx]->index);

		if (cur_pair_idx >= 0)
			pairs[cur_pair_idx].checked = 1;

		cur_pair_idx = get_pairidx_for_out_regidx(pairs, n, cycle->elems[idx]->index);

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
	lower_env_t     *env         = walk_env;
	int             keep_perm    = 0;
	int             n, i, pn, do_copy, j, n_ops;
	reg_pair_t      *pairs;
	const ir_edge_t *edge;
	ir_node         *sched_point, *block, *in[2];
	ir_node         *arg1, *arg2, *res1, *res2;
	ir_node         *cpyxchg = NULL;

	do_copy  = env->do_copy;
	block    = get_nodes_block(irn);

	/*
		Get the schedule predecessor node to the perm
		NOTE: This works with auto-magic. If we insert the
			new copy/exchange nodes after this node, everything
			should be ok.
	*/
	sched_point = sched_prev(irn);
	DBG((dbg, LEVEL_1, "perm: %+F\n", irn));
	DBG((dbg, LEVEL_1, "sched point is %+F\n", sched_point));
	assert(sched_point && "Perm is not scheduled or has no predecessor");

	n = get_irn_arity(irn);
	assert(n == get_irn_n_edges(irn) && "perm's in and out numbers different");

	reg_class = arch_get_irn_register(get_irn_n(irn, 0))->reg_class;
	pairs     = alloca(n * sizeof(pairs[0]));

	/* build the list of register pairs (in, out) */
	i = 0;
	foreach_out_edge(irn, edge) {
		pairs[i].out_node = get_edge_src_irn(edge);
		pn                = get_Proj_proj(pairs[i].out_node);
		pairs[i].in_node  = get_irn_n(irn, pn);

		pairs[i].in_reg  = arch_get_irn_register(pairs[i].in_node);
		pairs[i].out_reg = arch_get_irn_register(pairs[i].out_node);

		pairs[i].checked = 0;
		i++;
	}

	/* sort the register pairs by the indices of the in registers */
	qsort(pairs, n, sizeof(pairs[0]), compare_reg_pair);

	/* Mark all equal pairs as checked, and exchange the OUT proj with
		the IN node. */
	for (i = 0; i < n; i++) {
		if (pairs[i].in_reg->index == pairs[i].out_reg->index) {
			DBG((dbg, LEVEL_1, "%+F removing equal perm register pair (%+F, %+F, %s)\n",
				irn, pairs[i].in_node, pairs[i].out_node, pairs[i].out_reg->name));

			/* reroute the edges from the proj to the argument */
			exchange(pairs[i].out_node, pairs[i].in_node);

			pairs[i].checked = 1;
		}
	}

	/* Set do_copy to 0 if it's on but we have no free register */
	if (do_copy) {
		do_copy = 0;
	}

	/* check for cycles and chains */
	while (get_n_checked_pairs(pairs, n) < n) {
		perm_cycle_t *cycle;

		i = n_ops = 0;

		/* go to the first not-checked pair */
		while (pairs[i].checked) i++;
		cycle = XMALLOCZ(perm_cycle_t);
		cycle = get_perm_cycle(cycle, pairs, n, i);

		DB((dbg, LEVEL_1, "%+F: following %s created:\n  ", irn, cycle->type == PERM_CHAIN ? "chain" : "cycle"));
		for (j = 0; j < cycle->n_elems; j++) {
			DB((dbg, LEVEL_1, " %s", cycle->elems[j]->name));
		}
		DB((dbg, LEVEL_1, "\n"));

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
			arg1 = get_node_for_in_register(pairs, n, cycle->elems[i]);
			arg2 = get_node_for_in_register(pairs, n, cycle->elems[i + 1]);

			res1 = get_node_for_out_register(pairs, n, cycle->elems[i]);
			res2 = get_node_for_out_register(pairs, n, cycle->elems[i + 1]);
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

				DBG((dbg, LEVEL_1, "%+F creating exchange node (%+F, %s) and (%+F, %s) with\n",
					irn, arg1, cycle->elems[i]->name, arg2, cycle->elems[i + 1]->name));
				DBG((dbg, LEVEL_1, "%+F                        (%+F, %s) and (%+F, %s)\n",
					irn, res1, cycle->elems[i]->name, res2, cycle->elems[i + 1]->name));

				cpyxchg = be_new_Perm(reg_class, irg, block, 2, in);
				n_ops++;

				if (i > 0) {
					/* cycle is not done yet */
					int pidx = get_pairidx_for_in_regidx(pairs, n, cycle->elems[i]->index);

					/* create intermediate proj */
					res1 = new_r_Proj(irg, block, cpyxchg, get_irn_mode(res1), 0);

					/* set as in for next Perm */
					pairs[pidx].in_node = res1;
				}

				set_Proj_pred(res2, cpyxchg);
				set_Proj_proj(res2, 0);
				set_Proj_pred(res1, cpyxchg);
				set_Proj_proj(res1, 1);

				arch_set_irn_register(res2, cycle->elems[i + 1]);
				arch_set_irn_register(res1, cycle->elems[i]);

				/* insert the copy/exchange node in schedule after the magic schedule node (see above) */
				sched_add_after(sched_point, cpyxchg);

				DBG((dbg, LEVEL_1, "replacing %+F with %+F, placed new node after %+F\n", irn, cpyxchg, sched_point));

				/* set the new scheduling point */
				sched_point = res1;
			}
			else {
				DBG((dbg, LEVEL_1, "%+F creating copy node (%+F, %s) -> (%+F, %s)\n",
					irn, arg1, cycle->elems[i]->name, res2, cycle->elems[i + 1]->name));

				cpyxchg = be_new_Copy(reg_class, irg, block, arg1);
				arch_set_irn_register(cpyxchg, cycle->elems[i + 1]);
				n_ops++;

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
	if (! keep_perm) {
		sched_remove(irn);
		kill_node(irn);
	}
}



static int has_irn_users(const ir_node *irn) {
	return get_irn_out_edge_first_kind(irn, EDGE_KIND_NORMAL) != 0;
}

/**
 * Skip all Proj nodes.
 */
static INLINE ir_node *belower_skip_proj(ir_node *irn) {
	while(is_Proj(irn))
		irn = get_Proj_pred(irn);
	return irn;
}

static ir_node *find_copy(ir_node *irn, ir_node *op)
{
	ir_node *cur_node;

	for (cur_node = irn;;) {
		cur_node = sched_prev(cur_node);
		if (! be_is_Copy(cur_node))
			return NULL;
		if (be_get_Copy_op(cur_node) == op && arch_irn_is(cur_node, dont_spill))
			return cur_node;
	}
}

static void gen_assure_different_pattern(ir_node *irn, ir_node *other_different, constraint_env_t *env) {
	be_irg_t                    *birg     = env->birg;
	ir_graph                    *irg      = be_get_birg_irg(birg);
	ir_nodemap_t                *op_set   = &env->op_set;
	ir_node                     *block    = get_nodes_block(irn);
	const arch_register_class_t *cls      = arch_get_irn_reg_class(other_different, -1);
	ir_node                     *in[2], *keep, *cpy;
	op_copy_assoc_t             *entry;

	if (arch_irn_is(other_different, ignore) ||
			!mode_is_datab(get_irn_mode(other_different))) {
		DBG((dbg_constr, LEVEL_1, "ignore constraint for %+F because other_irn is ignore or not a datab node\n", irn));
		return;
	}

	/* Make a not spillable copy of the different node   */
	/* this is needed because the different irn could be */
	/* in block far far away                             */
	/* The copy is optimized later if not needed         */

	/* check if already exists such a copy in the schedule immediately before */
	cpy = find_copy(belower_skip_proj(irn), other_different);
	if (! cpy) {
		cpy = be_new_Copy(cls, irg, block, other_different);
		be_node_set_flags(cpy, BE_OUT_POS(0), arch_irn_flags_dont_spill);
		DBG((dbg_constr, LEVEL_1, "created non-spillable %+F for value %+F\n", cpy, other_different));
	}
	else {
		DBG((dbg_constr, LEVEL_1, "using already existing %+F for value %+F\n", cpy, other_different));
	}

	in[0] = irn;
	in[1] = cpy;

	/* Add the Keep resp. CopyKeep and reroute the users */
	/* of the other_different irn in case of CopyKeep.   */
	if (has_irn_users(other_different)) {
		keep = be_new_CopyKeep_single(cls, irg, block, cpy, irn, get_irn_mode(other_different));
		be_node_set_reg_class(keep, 1, cls);
	}
	else {
		keep = be_new_Keep(cls, irg, block, 2, in);
	}

	DBG((dbg_constr, LEVEL_1, "created %+F(%+F, %+F)\n\n", keep, irn, cpy));

	/* insert copy and keep into schedule */
	assert(sched_is_scheduled(irn) && "need schedule to assure constraints");
	if (! sched_is_scheduled(cpy))
		sched_add_before(belower_skip_proj(irn), cpy);
	sched_add_after(irn, keep);

	/* insert the other different and it's copies into the map */
	entry = ir_nodemap_get(op_set, other_different);
	if (! entry) {
		entry      = obstack_alloc(&env->obst, sizeof(*entry));
		entry->cls = cls;
		ir_nodeset_init(&entry->copies);

		ir_nodemap_insert(op_set, other_different, entry);
	}

	/* insert copy */
	ir_nodeset_insert(&entry->copies, cpy);

	/* insert keep in case of CopyKeep */
	if (be_is_CopyKeep(keep)) {
		ir_nodeset_insert(&entry->copies, keep);
	}
}

/**
 * Checks if node has a must_be_different constraint in output and adds a Keep
 * then to assure the constraint.
 *
 * @param irn          the node to check
 * @param skipped_irn  if irn is a Proj node, its predecessor, else irn
 * @param env          the constraint environment
 */
static void assure_different_constraints(ir_node *irn, ir_node *skipped_irn, constraint_env_t *env) {
	const arch_register_req_t *req = arch_get_register_req(irn, -1);

	if (arch_register_req_is(req, must_be_different)) {
		const unsigned other = req->other_different;
		int i;

		if (arch_register_req_is(req, should_be_same)) {
			const unsigned same = req->other_same;

			if (is_po2(other) && is_po2(same)) {
				int idx_other = ntz(other);
				int idx_same  = ntz(same);

				/*
				 * We can safely ignore a should_be_same x must_be_different y
				 * IFF both inputs are equal!
				 */
				if (get_irn_n(skipped_irn, idx_other) == get_irn_n(skipped_irn, idx_same)) {
					return;
				}
			}
		}
		for (i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_node *different_from = get_irn_n(skipped_irn, i);
				gen_assure_different_pattern(irn, different_from, env);
			}
		}
	}
}

/**
 * Calls the functions to assure register constraints.
 *
 * @param block    The block to be checked
 * @param walk_env The walker environment
 */
static void assure_constraints_walker(ir_node *block, void *walk_env) {
	ir_node *irn;

	sched_foreach_reverse(block, irn) {
		ir_mode *mode = get_irn_mode(irn);

		if (mode == mode_T) {
			const ir_edge_t *edge;

			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);

				mode = get_irn_mode(proj);
				if (mode_is_datab(mode))
					assure_different_constraints(proj, irn, walk_env);
			}
		} else if (mode_is_datab(mode)) {
			assure_different_constraints(irn, irn, walk_env);
		}
	}
}

/**
 * Melt all copykeeps pointing to the same node
 * (or Projs of the same node), copying the same operand.
 */
static void melt_copykeeps(constraint_env_t *cenv) {
	be_irg_t *birg = cenv->birg;
	ir_graph *irg  = be_get_birg_irg(birg);
	ir_nodemap_iterator_t map_iter;
	ir_nodemap_entry_t    map_entry;

	/* for all */
	foreach_ir_nodemap(&cenv->op_set, map_entry, map_iter) {
		op_copy_assoc_t *entry = map_entry.data;
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

				DBG((dbg_constr, LEVEL_1, "Trying to melt %+F:\n", ref));

				/* check for copykeeps pointing to the same mode_T node as the reference copykeep */
				for (j = 0; j < num_ck; ++j) {
					ir_node *cur_ck = ck_arr[j];

					if (j != idx && cur_ck && skip_Proj(get_irn_n(cur_ck, 1)) == ref_mode_T) {
						obstack_grow(&obst, &cur_ck, sizeof(cur_ck));
						ir_nodeset_remove(&entry->copies, cur_ck);
						DBG((dbg_constr, LEVEL_1, "\t%+F\n", cur_ck));
						ck_arr[j] = NULL;
						++n_melt;
						sched_remove(cur_ck);
					}
				}
				ck_arr[idx] = NULL;

				/* check, if we found some candidates for melting */
				if (n_melt == 1) {
					DBG((dbg_constr, LEVEL_1, "\tno candidate found\n"));
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
						kill_node(melt_arr[j]);
				}

#ifdef KEEP_ALIVE_COPYKEEP_HACK
				new_ck = be_new_CopyKeep(entry->cls, irg, get_nodes_block(ref), be_get_CopyKeep_op(ref), n_melt, new_ck_in, mode_ANY);
				keep_alive(new_ck);
#else
				new_ck = be_new_CopyKeep(entry->cls, irg, get_nodes_block(ref), be_get_CopyKeep_op(ref), n_melt, new_ck_in, get_irn_mode(ref));
#endif /* KEEP_ALIVE_COPYKEEP_HACK */

				/* set register class for all kept inputs */
				for (j = 1; j <= n_melt; ++j)
					be_node_set_reg_class(new_ck, j, entry->cls);

				ir_nodeset_insert(&entry->copies, new_ck);

				/* find scheduling point */
				sched_pt = ref_mode_T;
				do {
					/* just walk along the schedule until a non-Keep/CopyKeep node is found */
					sched_pt = sched_next(sched_pt);
				} while (be_is_Keep(sched_pt) || be_is_CopyKeep(sched_pt));

				sched_add_before(sched_pt, new_ck);
				DBG((dbg_constr, LEVEL_1, "created %+F, scheduled before %+F\n", new_ck, sched_pt));

				/* finally: kill the reference copykeep */
				kill_node(ref);
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
	ir_graph              *irg = be_get_birg_irg(birg);
	constraint_env_t      cenv;
	ir_node               **nodes;
	ir_nodemap_iterator_t map_iter;
	ir_nodemap_entry_t    map_entry;

	FIRM_DBG_REGISTER(dbg_constr, "firm.be.lower.constr");

	cenv.birg = birg;
	ir_nodemap_init(&cenv.op_set);
	obstack_init(&cenv.obst);

	irg_block_walk_graph(irg, NULL, assure_constraints_walker, &cenv);

	/* melt copykeeps, pointing to projs of */
	/* the same mode_T node and keeping the */
	/* same operand                         */
	melt_copykeeps(&cenv);

	/* for all */
	foreach_ir_nodemap(&cenv.op_set, map_entry, map_iter) {
		op_copy_assoc_t *entry = map_entry.data;
		int     n;
		ir_node *cp;
		ir_nodeset_iterator_t iter;
		be_ssa_construction_env_t senv;

		n     = ir_nodeset_size(&entry->copies);
		nodes = alloca(n * sizeof(nodes[0]));

		/* put the node in an array */
		DBG((dbg_constr, LEVEL_1, "introduce copies for %+F ", map_entry.node));

		/* collect all copies */
		n = 0;
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			nodes[n++] = cp;
			DB((dbg_constr, LEVEL_1, ", %+F ", cp));
		}

		DB((dbg_constr, LEVEL_1, "\n"));

		/* introduce the copies for the operand and it's copies */
		be_ssa_construction_init(&senv, birg);
		be_ssa_construction_add_copy(&senv, map_entry.node);
		be_ssa_construction_add_copies(&senv, nodes, n);
		be_ssa_construction_fix_users(&senv, map_entry.node);
		be_ssa_construction_destroy(&senv);

		/* Could be that not all CopyKeeps are really needed, */
		/* so we transform unnecessary ones into Keeps.       */
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			if (be_is_CopyKeep(cp) && get_irn_n_edges(cp) < 1) {
				ir_node *keep;
				int     n = get_irn_arity(cp);

				keep = be_new_Keep(arch_get_irn_reg_class(cp, -1),
					irg, get_nodes_block(cp), n, get_irn_in(cp) + 1);
				sched_add_before(cp, keep);

				/* Set all ins (including the block) of the CopyKeep BAD to keep the verifier happy. */
				sched_remove(cp);
				kill_node(cp);
			}
		}

		ir_nodeset_destroy(&entry->copies);
	}

	ir_nodemap_destroy(&cenv.op_set);
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
	lower_env_t *env = data;

	ir_graph *irg     = get_irn_irg(perm);
	ir_node *bl       = get_nodes_block(perm);
	ir_node *node;
	int  arity        = get_irn_arity(perm);
	int *map;
	int *proj_map;
	bitset_t *moved   = bitset_alloca(arity);
	int n_moved;
	int new_size;
	ir_node *frontier = bl;
	FIRM_DBG_REGISTER(firm_dbg_module_t *mod, "firm.be.lower.permmove");

	int i, n;
	const ir_edge_t *edge;
	ir_node *one_proj = NULL, *irn;
	const arch_register_class_t *cls = NULL;

	DBG((mod, LEVEL_1, "perm move %+F irg %+F\n", perm, irg));

	/* get some Proj and find out the register class of that Proj. */
	edge     = get_irn_out_edge_first_kind(perm, EDGE_KIND_NORMAL);
	one_proj = get_edge_src_irn(edge);
	assert(is_Proj(one_proj));
	cls      = arch_get_irn_reg_class(one_proj, -1);

	/* Find the point in the schedule after which the
	 * potentially movable nodes must be defined.
	 * A Perm will only be pushed up to first instruction
	 * which lets an operand of itself die.
	 * If we would allow to move the Perm above this instruction,
	 * the former dead operand would be live now at the point of
	 * the Perm, increasing the register pressure by one.
	 */
	sched_foreach_reverse_from (sched_prev(perm), irn) {
		for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
			ir_node *op = get_irn_n(irn, i);
			if (arch_irn_consider_in_reg_alloc(cls, op) &&
			    !values_interfere(env->birg, op, one_proj)) {
				frontier = irn;
				goto found_front;
			}
		}
	}
found_front:

	DBG((mod, LEVEL_2, "\tfrontier: %+F\n", frontier));

	node = sched_prev(perm);
	n_moved = 0;
	while(!sched_is_begin(node)) {
		const arch_register_req_t *req;
		int                        input = -1;
		ir_node                   *proj;

		/* search if node is a INPUT of Perm */
		foreach_out_edge(perm, edge) {
			ir_node *out = get_edge_src_irn(edge);
			int      pn  = get_Proj_proj(out);
			ir_node *in  = get_irn_n(perm, pn);
			if (node == in) {
				proj  = out;
				input = pn;
				break;
			}
		}
		/* it wasn't an input to the perm, we can't do anything more */
		if(input < 0)
			break;
		if(!sched_comes_after(frontier, node))
			break;
		if (arch_irn_is(node, modify_flags))
			break;
		if(is_Proj(node)) {
			req = arch_get_register_req(get_Proj_pred(node),
			                            -1 - get_Proj_proj(node));
		} else {
			req = arch_get_register_req(node, -1);
		}
		if(req->type != arch_register_req_type_normal)
			break;
		for(i = get_irn_arity(node) - 1; i >= 0; --i) {
			ir_node *opop = get_irn_n(node, i);
			if (arch_irn_consider_in_reg_alloc(cls, opop)) {
				break;
			}
		}
		if(i >= 0)
			break;

		DBG((mod, LEVEL_2, "\tmoving %+F after %+F, killing %+F\n", node, perm, proj));

		/* move the movable node in front of the Perm */
		sched_remove(node);
		sched_add_after(perm, node);

		/* give it the proj's register */
		arch_set_irn_register(node, arch_get_irn_register(proj));

		/* reroute all users of the proj to the moved node. */
		edges_reroute(proj, node, irg);

		/* and kill it */
		set_Proj_pred(proj, new_Bad());
		kill_node(proj);

		bitset_set(moved, input);
		n_moved++;

		node = sched_prev(node);
	}

	/* well, we could not push anything through the perm */
	if(n_moved == 0)
		return 1;

	new_size = arity - n_moved;
	if(new_size == 0) {
		return 0;
	}

	map      = alloca(new_size * sizeof(map[0]));
	proj_map = alloca(arity * sizeof(proj_map[0]));
	memset(proj_map, -1, sizeof(proj_map[0]));
	n   = 0;
	for(i = 0; i < arity; ++i) {
		if(bitset_is_set(moved, i))
			continue;
		map[n]      = i;
		proj_map[i] = n;
		n++;
	}
	assert(n == new_size);
	foreach_out_edge(perm, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		int      pn   = get_Proj_proj(proj);
		pn = proj_map[pn];
		assert(pn >= 0);
		set_Proj_proj(proj, pn);
	}

	be_Perm_reduce(perm, new_size, map);
	return 1;
}

/**
 * Calls the corresponding lowering function for the node.
 *
 * @param irn      The node to be checked for lowering
 * @param walk_env The walker environment
 */
static void lower_nodes_after_ra_walker(ir_node *irn, void *walk_env)
{
	int perm_stayed;

	if (!be_is_Perm(irn))
		return;

	perm_stayed = push_through_perm(irn, walk_env);
	if (!perm_stayed)
		return;

	lower_perm_node(irn, walk_env);
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
	ir_graph    *irg;

	FIRM_DBG_REGISTER(dbg, "firm.be.lower");

	env.birg    = birg;
	env.do_copy = do_copy;

	/* we will need interference */
	be_liveness_assure_chk(be_get_birg_liveness(birg));

	irg = be_get_birg_irg(birg);
	irg_walk_graph(irg, NULL, lower_nodes_after_ra_walker, &env);
}
