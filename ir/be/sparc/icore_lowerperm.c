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

#include "icore_lowerperm.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irdump.h"

#include "bearch.h"
#include "belower.h"
#include "benode.h"
#include "besched.h"
#include "belive.h"
#include "beirg.h"

#include "gen_sparc_new_nodes.h"

#include "execfreq.h"
#include "statev.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg_icore;)

/** TODO:  Once the perm instruction is implemented, this value can be
 * increased */
static const int MAX_PERMI_SIZE = 5;
static ir_node *sched_point;

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

static void set_Permi_reg_reqs(ir_node *irn, const arch_register_t *reg)
{
	const arch_register_class_t *class;
	const arch_register_req_t *req;
	const arch_register_req_t **in_reqs;
	struct obstack *obs;
	int i;
	const int arity = get_irn_arity(irn);

	assert(is_sparc_Permi(irn) || is_sparc_Permi23(irn));
	/* Get register requirement.  Assumes all input/registers belong to the
	 * same register class and have the same requirements. */
	class = arch_register_get_class(reg);
	req = class->class_req;

	/* Set in register requirements. */
	obs = be_get_be_obst(get_irn_irg(irn));
	in_reqs = OALLOCNZ(obs, const arch_register_req_t*, arity);
	for (i = 0; i < arity; ++i) {
		in_reqs[i] = req;
	}
	arch_set_in_register_reqs(irn, in_reqs);

	/* Set out register requirements. */
	for (i = 0; i < arity; ++i) {
		arch_set_out_register_req(irn, i, req);
	}
}

static void split_big_cycle(ir_node *irn, const perm_move_t *move, reg_pair_t *const pairs, int n_pairs)
{
	ir_node *const block      = get_nodes_block(irn);
	int            i;
	const int      cycle_size = move->n_elems;
	int            need_filler;

	assert(cycle_size > MAX_PERMI_SIZE);
	need_filler = (cycle_size % (MAX_PERMI_SIZE - 1) != 1);

	/* In this first step, we place as many Permis of the maximum allowed size as
	 * possible.  We stop as soon as the rest of the permutation needs a
	 * smaller Perm node. */
	for (i = cycle_size - MAX_PERMI_SIZE; i >= 0; i -= MAX_PERMI_SIZE - 1) {
		ir_node *args[MAX_PERMI_SIZE];
		ir_node *ress[MAX_PERMI_SIZE];
		ir_node *permi;
		int pidx;
		int j;
		const int last = MAX_PERMI_SIZE - 1;

		for (j = 0; j < MAX_PERMI_SIZE; ++j) {
			const int in  = j;
			const int out = (in + 1) % MAX_PERMI_SIZE;

			args[j] = get_node_for_in_register(pairs, n_pairs, move->elems[i + in]);
			ress[j] = get_node_for_out_register(pairs, n_pairs, move->elems[i + out]);
		}

		permi = new_bd_sparc_Permi(NULL, block, MAX_PERMI_SIZE, args, MAX_PERMI_SIZE);
		set_Permi_reg_reqs(permi, pairs[i].in_reg);
		DBG((dbg_icore, LEVEL_1, "%+F created smaller Permi node %+F with size %d\n",
		                         irn, permi, MAX_PERMI_SIZE));

		if (need_filler || i >= (MAX_PERMI_SIZE - 1)) {
			/* Cycle is not done yet */
			pidx = get_pairidx_for_in_regidx(pairs, n_pairs, move->elems[i]->index);

			/* Create intermediate proj */
			ress[last] = new_r_Proj(permi, get_irn_mode(ress[last]), last);

			/* Set as in for next Permi */
			pairs[pidx].in_node = ress[last];
		}

		for (j = 0; j < MAX_PERMI_SIZE; ++j) {
			const int out = (j + 1) % MAX_PERMI_SIZE;

			set_Proj_pred(ress[j], permi);
			set_Proj_proj(ress[j], j);
			arch_set_irn_register(ress[j], move->elems[i + out]);
		}

		/* Insert the copy/exchange node in schedule after the magic schedule node
		 * (see comment in lower_perm_node) */
		sched_add_after(skip_Proj(sched_point), permi);
		DB((dbg_icore, LEVEL_1, "replacing %+F with %+F, placed new node after %+F\n", irn, permi, sched_point));

		/* Set the new scheduling point */
		sched_point = ress[last];
	}

	/* Do we have to place another Perm with size < MAX_PERM_SIZE? */
	if (need_filler) {
		const int arity = i + MAX_PERMI_SIZE;
		ir_node *args[MAX_PERMI_SIZE];
		ir_node *ress[MAX_PERMI_SIZE];
		ir_node *permi;
		int j;

		assert(arity < MAX_PERMI_SIZE);

		for (j = 0; j < arity; ++j) {
			const int in  = j;
			const int out = (in + 1) % arity;

			args[j] = get_node_for_in_register(pairs, n_pairs, move->elems[0 + in]);
			ress[j] = get_node_for_out_register(pairs, n_pairs, move->elems[0 + out]);
		}

		permi = new_bd_sparc_Permi(NULL, block, arity, args, arity);
		set_Permi_reg_reqs(permi, pairs[0].in_reg);
		DBG((dbg_icore, LEVEL_1, "%+F created smaller Permi node %+F with size %d\n",
		                         irn, permi, arity));

		for (j = 0; j < arity; ++j) {
			const int out = (j + 1) % arity;

			set_Proj_pred(ress[j], permi);
			set_Proj_proj(ress[j], j);
			arch_set_irn_register(ress[j], move->elems[0 + out]);
		}

		/* Insert the copy/exchange node in schedule after the magic schedule node
		 * (see comment in lower_perm_node) */
		sched_add_after(skip_Proj(sched_point), permi);
		DB((dbg_icore, LEVEL_1, "replacing %+F with %+F, placed new node after %+F\n", irn, permi, sched_point));

		sched_point = permi;
	}
}

static void handle_cycle(ir_node *irn, const perm_move_t *move, reg_pair_t *const pairs, int n_pairs)
{
	const int cycle_size = move->n_elems;
	assert(move->type == PERM_CYCLE);

	if (cycle_size <= MAX_PERMI_SIZE) {
		/* If we have a cycle of size smaller or equal to MAX_CYCLE_SIZE, we
		 * can handle it directly with one iCore instruction. */
		ir_node **args  = ALLOCAN(ir_node*, cycle_size);
		ir_node **ress  = ALLOCAN(ir_node*, cycle_size);
		ir_node  *block = get_nodes_block(irn);
		ir_node  *permi;
		int       i;

		/*
		 * The order of the registers in move->elems describes a cycle
		 * like this:
		 *   Order       r0, r1, r2, r3, r4
		 *   describes   r0->r1->r2->r3->r4->r0.
		 */
		for (i = 0; i < cycle_size; ++i) {
			const int in  = i;
			const int out = (in + 1) % cycle_size;

			args[i] = get_node_for_in_register(pairs, n_pairs, move->elems[in]);
			ress[i] = get_node_for_out_register(pairs, n_pairs, move->elems[out]);
		}

		permi = new_bd_sparc_Permi(NULL, block, cycle_size, args, cycle_size);
		set_Permi_reg_reqs(permi, pairs[0].in_reg);
		DBG((dbg_icore, LEVEL_1, "%+F created smaller permi node %+F with size %d\n",
		                         irn, permi, cycle_size));

		for (i = 0; i < cycle_size; ++i) {
			int                    out  = (i + 1) % cycle_size;
			const arch_register_t *reg  = move->elems[out];
			ir_node               *proj = ress[i];

			set_Proj_pred(proj, permi);
			set_Proj_proj(proj, i);
			DBG((dbg_icore, LEVEL_1, "   setting register for output %d to %s\n", i, reg->name));
			arch_irn_set_register(permi, i, reg);
		}

		sched_add_after(sched_point, permi);
		sched_point = permi;
	} else {
		/* Otherwise, we want to replace the big Perm node with a series
		 * of smaller ones. */

		split_big_cycle(irn, move, pairs, n_pairs);
	}
}

static void combine_cycles(ir_node *irn, reg_pair_t *const pairs, int n_pairs, perm_move_t *cycle2, perm_move_t *cycle3)
{
	const int cycle2_size = cycle2->n_elems;
	const int cycle3_size = cycle3->n_elems;
	const int arity = cycle2_size + cycle3_size;
	ir_node **args  = ALLOCAN(ir_node*, arity);
	ir_node **ress  = ALLOCAN(ir_node*, arity);
	ir_node  *block = get_nodes_block(irn);
	ir_node  *permi23;
	int       i;

	assert(cycle2->type == PERM_CYCLE && cycle2_size == 2);
	/* The second cycle might be a 3-cycle but can be a 2-cycle, too. */
	assert(cycle3->type == PERM_CYCLE && cycle3_size <= 3);

	/* Handle 2-cycle.  Cycle always has size 2. */
	for (i = 0; i < 2; ++i) {
		const int in  = i;
		const int out = (i + 1) % 2;

		args[i] = get_node_for_in_register(pairs, n_pairs, cycle2->elems[in]);
		ress[i] = get_node_for_out_register(pairs, n_pairs, cycle2->elems[out]);
	}

	/* Handle 3-cycle.  Might have size 2 or size 3. */
	for (i = 0; i < cycle3_size; ++i) {
		const int in  = i;
		const int out = (in + 1) % cycle3_size;

		args[2 + i] = get_node_for_in_register(pairs, n_pairs, cycle3->elems[in]);
		ress[2 + i] = get_node_for_out_register(pairs, n_pairs, cycle3->elems[out]);
	}

	permi23 = new_bd_sparc_Permi23(NULL, block, arity, args, arity);
	set_Permi_reg_reqs(permi23, pairs[0].in_reg);
	DBG((dbg_icore, LEVEL_1, "%+F created smaller Permi23 node %+F\n",
							 irn, permi23));

	/* Handle 2-cycle. */
	for (i = 0; i < 2; ++i) {
		int                    out  = (i + 1) % 2;
		const arch_register_t *reg  = cycle2->elems[out];
		ir_node               *proj = ress[i];

		set_Proj_pred(proj, permi23);
		set_Proj_proj(proj, i);
		DBG((dbg_icore, LEVEL_1, "   setting register for output %d to %s\n", i, reg->name));
		arch_irn_set_register(permi23, i, reg);
	}

	/* Handle 3-cycle. */
	for (i = 0; i < cycle3_size; ++i) {
		int                    out  = (i + 1) % cycle3_size;
		const arch_register_t *reg  = cycle3->elems[out];
		ir_node               *proj = ress[2 + i];

		set_Proj_pred(proj, permi23);
		set_Proj_proj(proj, 2 + i);
		DBG((dbg_icore, LEVEL_1, "   setting register for output %d to %s\n", 2 + i, reg->name));
		arch_irn_set_register(permi23, 2 + i, reg);
	}

	sched_add_after(sched_point, permi23);
	sched_point = permi23;
}

static void search_combinable_cycles(ir_node *irn, reg_pair_t *const pairs, int n_pairs, perm_move_t *cycles2, int n_cycles2, perm_move_t *cycles3, int n_cycles3)
{
	int j;
	int n_used3 = 0;
	DBG((dbg_icore, LEVEL_1, "%+F: %d cycles of size 2 and %d cycles of size 3 left.\n", irn, n_cycles2, n_cycles3));

	for (j = 0; j < n_cycles2; ++j) {
		if (n_used3 < n_cycles3) { /* Are there any 3-cycles left? */
			perm_move_t *cycle2 = &cycles2[j];
			perm_move_t *cycle3 = &cycles3[n_used3];

			combine_cycles(irn, pairs, n_pairs, cycle2, cycle3);

			++n_used3;
		} else if (j + 1 < n_cycles2) { /* If not, are there any other 2-cycles left? */
			perm_move_t *cycle2_1 = &cycles2[j];
			perm_move_t *cycle2_2 = &cycles2[j + 1];

			combine_cycles(irn, pairs, n_pairs, cycle2_1, cycle2_2);

			++j;
		} else { /* If not, this must be the last 2-cycle */
			handle_cycle(irn, &cycles2[j], pairs, n_pairs);
		}
	}

	/* There might be 3-cycles left, e.g., if the list of 2-cycles was empty
	 * but the list of 3-cycles was not. */
	for (j = n_used3; j < n_cycles3; ++j) {
		handle_cycle(irn, &cycles3[j], pairs, n_pairs);
	}
}

static void split_chain_into_copies(ir_node *irn, const perm_move_t *move, reg_pair_t *const pairs, int n_pairs)
{
	/* Build copy nodes from back to front */
	const arch_register_class_t *const reg_class   = arch_get_irn_register(get_irn_n(irn, 0))->reg_class;
	ir_node                     *const block       = get_nodes_block(irn);
	int                                i;

	for (i = move->n_elems - 2; i >= 0; --i) {
		const arch_register_t *in_reg  = move->elems[i];
		const arch_register_t *out_reg = move->elems[i + 1];
		ir_node         *arg1    = get_node_for_in_register(pairs, n_pairs, in_reg);
		ir_node         *res2    = get_node_for_out_register(pairs, n_pairs, out_reg);
		ir_node         *cpy;

		cpy = be_new_Copy(reg_class, block, arg1);
		DB((dbg_icore, LEVEL_1, "%+F created copy node %+F to implement (%+F, %s) -> (%+F, %s)\n",
		                        irn, cpy, arg1, in_reg->name, res2, out_reg->name));

		arch_set_irn_register(cpy, out_reg);

		/* Exchange copy node and proj. */
		exchange(res2, cpy);

		/* Insert the copy/exchange node in schedule after the magic schedule node. */
		sched_add_after(skip_Proj(sched_point), cpy);

		/* Set the new scheduling point. */
		sched_point = cpy;
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
	int          const arity     = get_irn_arity(irn);
	reg_pair_t  *const pairs     = ALLOCAN(reg_pair_t, arity);
	perm_move_t *      cycles2   = ALLOCAN(perm_move_t, arity); // TODO:  Maybe use arity / 2?
	perm_move_t *      cycles3   = ALLOCAN(perm_move_t, arity);
	int                n_pairs   = 0;
	int                n_cycles2 = 0;
	int                n_cycles3 = 0;

	assert(be_is_Perm(irn) && "Non-Perm node passed to lower_perm_node");
	assert(arity == get_irn_n_edges(irn) && "perm's in and out numbers different");

	/* Get the schedule predecessor node to the perm.
	 * NOTE: This works with auto-magic. If we insert the new copy/exchange
	 * nodes after this node, everything should be ok. */
	sched_point = sched_prev(irn);
	assert(sched_point && "Perm is not scheduled or has no predecessor");
	DBG((dbg_icore, LEVEL_1, "perm: %+F, sched point is %+F\n", irn, sched_point));

	/* Build the list of register pairs (in, out) */
	build_register_pair_list(pairs, &n_pairs, irn);
	DBG((dbg_icore, LEVEL_1, "%+F has %d unresolved constraints\n", irn, n_pairs));

	/* Collect all cycles and chains */
	while (get_n_unchecked_pairs(pairs, n_pairs) > 0) {
		perm_move_t move;
		int         i;

		/* Go to the first unchecked pair */
		for (i = 0; pairs[i].checked; ++i);

		/* Identifies cycles or chains in the given list of register pairs.
		 * Found cycles/chains are written to move.n_elems, the type (cycle
		 * or chain) is saved in move.type. */
		get_perm_move_info(&move, pairs, n_pairs, i);

		DB((dbg_icore, LEVEL_1, "%+F: following %s found:\n  ", irn, move.type == PERM_CHAIN ? "chain" : "cycle"));
		for (i = 0; i < move.n_elems; ++i) {
			DB((dbg_icore, LEVEL_1, " %s", move.elems[i]->name));
		}
		DB((dbg_icore, LEVEL_1, "\n"));

		if (move.type == PERM_CHAIN) {
			split_chain_into_copies(irn, &move, pairs, n_pairs);
		} else { /* move.type == PERM_CYCLE */
			if (move.n_elems == 2) {
				cycles2[n_cycles2++] = move; /* Might be combinable, save for later. */
			} else if (move.n_elems == 3) {
				cycles3[n_cycles3++] = move; /* Might be combinable, save for later. */
			} else {
				handle_cycle(irn, &move, pairs, n_pairs);
				free(move.elems);
			}
		}
	}

	search_combinable_cycles(irn, pairs, n_pairs, cycles2, n_cycles2, cycles3, n_cycles3);

	/* Clean up. */
	while (n_cycles2) {
		free(cycles2[--n_cycles2].elems);
	}

	while (n_cycles3) {
		free(cycles3[--n_cycles3].elems);
	}

	/* Remove the perm from schedule */
	sched_remove(irn);
	kill_node(irn);
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

	dump_ir_graph(irg, "before_icore_lowering");
	irg_walk_graph(irg, NULL, lower_nodes_after_ra_walker, NULL);
	dump_ir_graph(irg, "after_icore_lowering");
}
