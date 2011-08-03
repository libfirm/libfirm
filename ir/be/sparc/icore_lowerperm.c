/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Special iCore Perm lowering
 * @author   Manuel Mohr
 */
#include "config.h"

#include <stdlib.h>

#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "bearch.h"
#include "belower.h"
#include "benode.h"
#include "besched.h"
#include "belive.h"

#include "execfreq.h"
#include "statev.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg_icore;)

/** TODO:  Once the perm instruction is implemented, this value can be
 * increased */
static const int MAX_PERM_SIZE = 2;

/** Holds a Perm register pair. */
typedef struct reg_pair_t {
	const arch_register_t *in_reg;    /**< a perm IN register */
	ir_node               *in_node;   /**< the in node to which the register belongs */

	const arch_register_t *out_reg;   /**< a perm OUT register */
	ir_node               *out_node;  /**< the out node to which the register belongs */

	int                    checked;   /**< indicates whether the pair was check for cycle or not */
} reg_pair_t;

typedef enum perm_type_t {
	PERM_CYCLE,
	PERM_CHAIN
} perm_type_t;

/** Structure to represent the register movements that a Perm describes.
 * The type field holds the type of register movement, i.e. cycle or chain. */
typedef struct perm_move_t {
	const arch_register_t **elems;       /**< the registers in the cycle */
	int                     n_elems;     /**< number of elements in the cycle */
	perm_type_t             type;        /**< type (CHAIN or CYCLE) */
} perm_move_t;

/** returns the number register pairs marked as checked. */
static int get_n_unchecked_pairs(reg_pair_t const *const pairs, int const n)
{
	int n_unchecked = 0;
	int i;

	for (i = 0; i < n; i++) {
		if (!pairs[i].checked)
			n_unchecked++;
	}

	return n_unchecked;
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
static ir_node *get_node_for_in_register(reg_pair_t *pairs, int n, const arch_register_t *reg)
{
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
static ir_node *get_node_for_out_register(reg_pair_t *pairs, int n, const arch_register_t *reg)
{
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
static int get_pairidx_for_in_regidx(reg_pair_t *pairs, int n, unsigned reg_idx)
{
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
static int get_pairidx_for_out_regidx(reg_pair_t *pairs, int n, unsigned reg_idx)
{
	int i;

	for (i = 0; i < n; i++) {
		/* out register matches */
		if (pairs[i].out_reg->index == reg_idx)
			return i;
	}
	return -1;
}

/**
 * Gets an array of register pairs and tries to identify a cycle or chain
 * starting at position start.
 *
 * @param move     Variable to hold the register movements
 * @param pairs    Array of register pairs
 * @param n_pairs  length of the pairs array
 * @param start    Index to start
 */
static void get_perm_move_info(perm_move_t *const move,
                               reg_pair_t  *const pairs,
                               int          const n_pairs,
                               int                start)
{
	int         head         = pairs[start].in_reg->index;
	int         cur_idx      = pairs[start].out_reg->index;
	int   const n_pairs_todo = get_n_unchecked_pairs(pairs, n_pairs);
	perm_type_t move_type    = PERM_CYCLE;
	int         idx;

	/* We could be right in the middle of a chain, so we need to find the start */
	while (head != cur_idx) {
		/* goto previous register in cycle or chain */
		int const cur_pair_idx = get_pairidx_for_out_regidx(pairs, n_pairs, head);

		if (cur_pair_idx < 0) {
			move_type = PERM_CHAIN;
			break;
		} else {
			head  = pairs[cur_pair_idx].in_reg->index;
			start = cur_pair_idx;
		}
	}

	/* assume worst case: all remaining pairs build a cycle or chain */
	move->elems    = XMALLOCNZ(const arch_register_t*, n_pairs_todo * 2);
	move->n_elems  = 2;  /* initial number of elements is 2 */
	move->elems[0] = pairs[start].in_reg;
	move->elems[1] = pairs[start].out_reg;
	move->type     = move_type;
	cur_idx        = pairs[start].out_reg->index;

	idx = 2;
	/* check for cycle or end of a chain */
	while (cur_idx != head) {
		/* goto next register in cycle or chain */
		int const cur_pair_idx = get_pairidx_for_in_regidx(pairs, n_pairs, cur_idx);

		if (cur_pair_idx < 0)
			break;

		cur_idx = pairs[cur_pair_idx].out_reg->index;

		/* it's not the first element: insert it */
		if (cur_idx != head) {
			move->elems[idx++] = pairs[cur_pair_idx].out_reg;
			move->n_elems++;
		} else {
			/* we are there where we started -> CYCLE */
			move->type = PERM_CYCLE;
		}
	}

	/* mark all pairs having one in/out register with cycle in common as checked */
	for (idx = 0; idx < move->n_elems; idx++) {
		int cur_pair_idx;

		cur_pair_idx = get_pairidx_for_in_regidx(pairs, n_pairs, move->elems[idx]->index);
		if (cur_pair_idx >= 0)
			pairs[cur_pair_idx].checked = 1;

		cur_pair_idx = get_pairidx_for_out_regidx(pairs, n_pairs, move->elems[idx]->index);
		if (cur_pair_idx >= 0)
			pairs[cur_pair_idx].checked = 1;
	}
}

static void build_register_pair_list(reg_pair_t *pairs, int *n, ir_node *irn)
{
	const ir_edge_t *edge;
	const ir_edge_t *next;
	*n = 0;

	foreach_out_edge_safe(irn, edge, next) {
		ir_node               *const out     = get_edge_src_irn(edge);
		long                   const pn      = get_Proj_proj(out);
		ir_node               *const in      = get_irn_n(irn, pn);
		arch_register_t const *const in_reg  = arch_get_irn_register(in);
		arch_register_t const *const out_reg = arch_get_irn_register(out);
		reg_pair_t            *      pair;

		/* If a register is left untouched by the Perm node, we do not
		 * have to generate copy/swap instructions later on. */
		if (in_reg == out_reg) {
			DBG((dbg_icore, LEVEL_1, "%+F removing equal perm register pair (%+F, %+F, %s)\n",
					irn, in, out, out_reg->name));
			exchange(out, in);
			continue;
		}

		pair           = &pairs[(*n)++];
		pair->in_node  = in;
		pair->in_reg   = in_reg;
		pair->out_node = out;
		pair->out_reg  = out_reg;
		pair->checked  = 0;
	}
}

static void emit_stat_info(const ir_node *irn, const perm_move_t *move)
{
	ir_node      *block     = get_nodes_block(irn);
	ir_graph     *irg       = get_irn_irg(block);
	ir_exec_freq *exec_freq = be_get_irg_exec_freq(irg);
	double        freq      = get_block_execfreq(exec_freq, block);

	if (move->type == PERM_CYCLE) {
		stat_ev_int("cycle_length", move->n_elems);
		stat_ev_dbl("cycle_weight", freq * move->n_elems);
	}
	else if (move->type == PERM_CHAIN) {
		stat_ev_int("chain_length", move->n_elems);
		stat_ev_dbl("chain_weight", freq * move->n_elems);
	}
}

static void reduce_perm_size(ir_node *irn, const perm_move_t* move, reg_pair_t *const pairs, int n_pairs)
{
	const arch_register_class_t *const reg_class   = arch_get_irn_register(get_irn_n(irn, 0))->reg_class;
	ir_node                     *const block       = get_nodes_block(irn);
	/* Get the schedule predecessor node to the perm.
	 * NOTE: This works with auto-magic. If we insert the new copy/exchange
	 * nodes after this node, everything should be ok. */
	ir_node                     *      sched_point = sched_prev(irn);
	int                                i;

	/* build copy/swap nodes from back to front */
	for (i = move->n_elems - MAX_PERM_SIZE; i >= 0; --i) {
		ir_node *arg1 = get_node_for_in_register(pairs, n_pairs, move->elems[i]);
		ir_node *arg2 = get_node_for_in_register(pairs, n_pairs, move->elems[i + 1]);

		ir_node *res1 = get_node_for_out_register(pairs, n_pairs, move->elems[i]);
		ir_node *res2 = get_node_for_out_register(pairs, n_pairs, move->elems[i + 1]);
		/* If we have a cycle and don't copy: we need to create exchange
		 * nodes
		 * NOTE: An exchange node is a perm node with 2 INs and 2 OUTs
		 * IN_1  = in  node with register i
		 * IN_2  = in  node with register i + 1
		 * OUT_1 = out node with register i + 1
		 * OUT_2 = out node with register i */
		if (move->type == PERM_CYCLE) {
			ir_node *in[2];
			ir_node *cpyxchg;

			in[0] = arg1;
			in[1] = arg2;

			/* At this point we have to handle the following problem:
			 *
			 * If we have a cycle with more than two elements, then this
			 * could correspond to the following Perm node:
			 *
			 *   +----+   +----+   +----+
			 *   | r1 |   | r2 |   | r3 |
			 *   +-+--+   +-+--+   +--+-+
			 *     |        |         |
			 *     |        |         |
			 *   +-+--------+---------+-+
			 *   |         Perm         |
			 *   +-+--------+---------+-+
			 *     |        |         |
			 *     |        |         |
			 *   +-+--+   +-+--+   +--+-+
			 *   |Proj|   |Proj|   |Proj|
			 *   | r2 |   | r3 |   | r1 |
			 *   +----+   +----+   +----+
			 *
			 * This node is about to be split up into two 2x Perm's for
			 * which we need 4 Proj's and the one additional Proj of the
			 * first Perm has to be one IN of the second. So in general
			 * we need to create one additional Proj for each "middle"
			 * Perm and set this to one in node of the successor Perm. */

			DBG((dbg_icore, LEVEL_1, "%+F creating exchange node (%+F, %s) and (%+F, %s) with\n",
						irn, arg1, move->elems[i]->name, arg2, move->elems[i + 1]->name));
			DBG((dbg_icore, LEVEL_1, "%+F                        (%+F, %s) and (%+F, %s)\n",
						irn, res1, move->elems[i]->name, res2, move->elems[i + 1]->name));

			cpyxchg = be_new_Perm(reg_class, block, 2, in);

			if (i > 0) {
				/* cycle is not done yet */
				int pidx = get_pairidx_for_in_regidx(pairs, n_pairs, move->elems[i]->index);

				/* create intermediate proj */
				res1 = new_r_Proj(cpyxchg, get_irn_mode(res1), 0);

				/* set as in for next Perm */
				pairs[pidx].in_node = res1;
			}

			set_Proj_pred(res2, cpyxchg);
			set_Proj_proj(res2, 0);
			set_Proj_pred(res1, cpyxchg);
			set_Proj_proj(res1, 1);

			arch_set_irn_register(res2, move->elems[i + 1]);
			arch_set_irn_register(res1, move->elems[i]);

			/* insert the copy/exchange node in schedule after the magic schedule node
			 * (see comment in lower_perm_node) */
			sched_add_after(skip_Proj(sched_point), cpyxchg);

			DB((dbg_icore, LEVEL_1, "replacing %+F with %+F, placed new node after %+F\n", irn, cpyxchg, sched_point));

			/* set the new scheduling point */
			sched_point = res1;
		} else {
			ir_node *cpyxchg;

			DB((dbg_icore, LEVEL_1, "%+F creating copy node (%+F, %s) -> (%+F, %s)\n",
						irn, arg1, move->elems[i]->name, res2, move->elems[i + 1]->name));

			cpyxchg = be_new_Copy(reg_class, block, arg1);
			arch_set_irn_register(cpyxchg, move->elems[i + 1]);

			/* exchange copy node and proj */
			exchange(res2, cpyxchg);

			/* insert the copy/exchange node in schedule after the magic schedule node (see above) */
			sched_add_after(skip_Proj(sched_point), cpyxchg);

			/* set the new scheduling point */
			sched_point = cpyxchg;
		}
	}
}

/**
 * Lowers a perm node.  Resolves cycles and creates a bunch of
 * copy and swap operations to permute registers.
 * Note: The caller of this function has to make sure that irn
 *       is a Perm node.
 *
 * @param irn      The perm node
 */
static void lower_perm_node(ir_node *irn)
{
	int         const arity        = get_irn_arity(irn);
	reg_pair_t *const pairs        = ALLOCAN(reg_pair_t, arity);
	int               n_pairs      = 0;
	int               keep_perm    = 0;
	ir_node    *      sched_point  = sched_prev(irn);

	assert(be_is_Perm(irn) && "Non-Perm node passed to lower_perm_node");
	DBG((dbg_icore, LEVEL_1, "perm: %+F, sched point is %+F\n", irn, sched_point));
	assert(sched_point && "Perm is not scheduled or has no predecessor");

	assert(arity == get_irn_n_edges(irn) && "perm's in and out numbers different");

	/* Build the list of register pairs (in, out) */
	build_register_pair_list(pairs, &n_pairs, irn);

	DBG((dbg_icore, LEVEL_1, "%+F has %d unresolved constraints\n", irn, n_pairs));

	/* Check for cycles and chains */
	while (get_n_unchecked_pairs(pairs, n_pairs) > 0) {
		perm_move_t move;
		int         i;

		/* Go to the first not-checked pair */
		for (i = 0; pairs[i].checked; ++i) {
		}

		/* Identifies cycles or chains in the given list of register pairs.
		 * Found cycles/chains are written to move.n_elems, the type (cycle
		 * or chain) is saved in move.type. */
		get_perm_move_info(&move, pairs, n_pairs, i);

		emit_stat_info(irn, &move);

		DB((dbg_icore, LEVEL_1, "%+F: following %s created:\n  ", irn, move.type == PERM_CHAIN ? "chain" : "cycle"));
		for (i = 0; i < move.n_elems; ++i) {
			DB((dbg_icore, LEVEL_1, " %s", move.elems[i]->name));
		}
		DB((dbg_icore, LEVEL_1, "\n"));

		if (move.type == PERM_CYCLE && arity <= MAX_PERM_SIZE) {
			/* We don't need to do anything if we have a Perm with fewer than
			 * MAX_PERM_SIZE elements which represents a cycle, because those
			 * nodes can be directly handled by the iCore backend using a
			 * permutation instruction. */
			keep_perm = 1;
		} else {
			/* Otherwise, we want to replace the big Perm node with a series
			 * of smaller ones. */

			reduce_perm_size(irn, &move, pairs, n_pairs);
		}

		free(move.elems);
	}

	/* Remove the perm from schedule */
	if (!keep_perm) {
		sched_remove(irn);
		kill_node(irn);
	}
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
	(void) walk_env;

	if (!be_is_Perm(irn))
		return;

	perm_stayed = push_through_perm(irn);
	if (perm_stayed)
		lower_perm_node(irn);
}

void icore_lower_nodes_after_ra(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg_icore, "firm.be.lower.icore");

	/* we will need interference */
	be_liveness_assure_chk(be_get_irg_liveness(irg));

	irg_walk_graph(irg, NULL, lower_nodes_after_ra_walker, NULL);
}
