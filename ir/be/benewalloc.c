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
 * @brief       New approach to allocation and copy coalescing
 * @author      Matthias Braun
 * @date        14.2.2009
 * @version     $Id$
 *
 * ... WE NEED A NAME FOR THIS ...
 *
 * Only a proof of concept at this moment...
 *
 * The idea is to allocate registers in 2 passes:
 * 1. A first pass to determine "preferred" registers for live-ranges. This
 *    calculates for each register and each live-range a value indicating
 *    the usefulness. (You can roughly think of the value as the negative
 *    costs needed for copies when the value is in the specific registers...)
 *
 * 2. Walk blocks and assigns registers in a greedy fashion. Preferring
 *    registers with high preferences. When register constraints are not met,
 *    add copies and split live-ranges.
 *
 * TODO:
 *  - make use of free registers in the permutate_values code
 *  - output constraints are not ensured. The algorithm fails to copy values
 *    away, so the registers for constrained outputs are free.
 *  - must_be_different constraint is not respected
 *  - We have to pessimistically construct Phi_0s when not all predecessors
 *    of a block are known.
 *  - Phi color assignment should give bonus points towards registers already
 *    assigned at predecessors.
 *  - think about a smarter sequence of visiting the blocks. Sorted by
 *    execfreq might be good, or looptree from inner to outermost loops going
 *    over blocks in a reverse postorder
 */
#include "config.h"

#include <float.h>

#include "obst.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irgwalk.h"
#include "execfreq.h"

#include "be.h"
#include "bera.h"
#include "belive_t.h"
#include "bemodule.h"
#include "bechordal_t.h"
#include "besched.h"
#include "beirg.h"
#include "benode_t.h"
#include "bespill.h"
#include "bespillutil.h"
#include "beverify.h"

#include "bipartite.h"
#include "hungarian.h"

#define USE_FACTOR       1.0f
#define DEF_FACTOR       1.0f
#define NEIGHBOR_FACTOR  0.2f
#define SHOULD_BE_SAME   1.0f

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static struct obstack               obst;
static be_irg_t                    *birg;
static ir_graph                    *irg;
static const arch_register_class_t *cls;
static be_lv_t                     *lv;
static const ir_exec_freq          *execfreqs;
static unsigned                     n_regs;
static bitset_t                    *ignore_regs;

/** info about the current assignment for a register */
struct assignment_t {
	ir_node *value;            /**< currently assigned value */
};
typedef struct assignment_t assignment_t;

/** currently active assignments (while processing a basic block) */
static assignment_t *assignments;

/**
 * allocation information: last_uses, register preferences
 * the information is per firm-node.
 */
struct allocation_info_t {
	unsigned      last_uses;   /**< bitset indicating last uses (input pos) */
	assignment_t *current_assignment;
	float         prefs[0];    /**< register preferences */
};
typedef struct allocation_info_t allocation_info_t;

/** helper datastructure used when sorting register preferences */
struct reg_pref_t {
	unsigned num;
	float    pref;
};
typedef struct reg_pref_t reg_pref_t;

/** per basic-block information */
struct block_info_t {
	int          processed;       /**< indicate wether block is processed */
	assignment_t assignments[0];  /**< register assignments at end of block */
};
typedef struct block_info_t block_info_t;

/**
 * Get the allocation info for a node.
 * The info is allocated on the first visit of a node.
 */
static allocation_info_t *get_allocation_info(ir_node *node)
{
	allocation_info_t *info;
	if (!irn_visited_else_mark(node)) {
		size_t size = sizeof(info[0]) + n_regs * sizeof(info->prefs[0]);
		info = obstack_alloc(&obst, size);
		memset(info, 0, size);
		set_irn_link(node, info);
	} else {
		info = get_irn_link(node);
	}

	return info;
}

/**
 * Get allocation information for a basic block
 */
static block_info_t *get_block_info(ir_node *block)
{
	block_info_t *info;

	assert(is_Block(block));
	if (!irn_visited_else_mark(block)) {
		size_t size = sizeof(info[0]) + n_regs * sizeof(info->assignments[0]);
		info = obstack_alloc(&obst, size);
		memset(info, 0, size);
		set_irn_link(block, info);
	} else {
		info = get_irn_link(block);
	}

	return info;
}

/**
 * Link the allocation info of a node to a copy.
 * Afterwards, both nodes uses the same allocation info.
 * Copy must not have an allocation info assigned yet.
 *
 * @param copy   the node that gets the allocation info assigned
 * @param value  the original node
 */
static void link_to(ir_node *copy, ir_node *value)
{
	allocation_info_t *info = get_allocation_info(value);
	assert(!irn_visited(copy));
	set_irn_link(copy, info);
	mark_irn_visited(copy);
}

/**
 * Calculate the penalties for every register on a node and its live neighbors.
 *
 * @param live_nodes   the set of live nodes at the current position, may be NULL
 * @param penalty      the penalty to subtract from
 * @param limited      a raw bitset containing the limited set for the node
 * @param node         the node
 */
static void give_penalties_for_limits(const ir_nodeset_t *live_nodes,
                                      float penalty, const unsigned* limited,
									  ir_node *node)
{
	ir_nodeset_iterator_t iter;
	unsigned              r;
	allocation_info_t     *info = get_allocation_info(node);
	ir_node               *neighbor;

	/* give penalty for all forbidden regs */
	for (r = 0; r < n_regs; ++r) {
		if (rbitset_is_set(limited, r))
			continue;

		info->prefs[r] -= penalty;
	}

	/* all other live values should get a penalty for allowed regs */
	if (live_nodes == NULL)
		return;

	/* TODO: reduce penalty if there are multiple allowed registers... */
	penalty *= NEIGHBOR_FACTOR;
	foreach_ir_nodeset(live_nodes, neighbor, iter) {
		allocation_info_t *neighbor_info;

		/* TODO: if op is used on multiple inputs we might not do a
		 * continue here */
		if (neighbor == node)
			continue;

		neighbor_info = get_allocation_info(neighbor);
		for (r = 0; r < n_regs; ++r) {
			if (!rbitset_is_set(limited, r))
				continue;

			neighbor_info->prefs[r] -= penalty;
		}
	}
}

/**
 * Calculate the preferences of a definition for the current register class.
 * If the definition uses a limited set of registers, reduce the preferences
 * for the limited register on the node and its neighbors.
 *
 * @param live_nodes  the set of live nodes at the current node
 * @param weight      the weight
 * @param node        the current node
 */
static void check_defs(const ir_nodeset_t *live_nodes, float weight,
                       ir_node *node)
{
	const arch_register_req_t *req;

	if (get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;
		foreach_out_edge(node, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			check_defs(live_nodes, weight, proj);
		}
		return;
	}

	if (!arch_irn_consider_in_reg_alloc(cls, node))
		return;

	req = arch_get_register_req_out(node);
	if (req->type & arch_register_req_type_limited) {
		const unsigned *limited = req->limited;
		float           penalty = weight * DEF_FACTOR;
		give_penalties_for_limits(live_nodes, penalty, limited, node);
	}

	if (req->type & arch_register_req_type_should_be_same) {
		ir_node           *insn  = skip_Proj(node);
		allocation_info_t *info  = get_allocation_info(node);
		int                arity = get_irn_arity(insn);
		int                i;

		float factor = 1.0f / rbitset_popcnt(&req->other_same, arity);
		for (i = 0; i < arity; ++i) {
			ir_node           *op;
			unsigned           r;
			allocation_info_t *op_info;

			if (!rbitset_is_set(&req->other_same, i))
				continue;

			op      = get_irn_n(insn, i);
			op_info = get_allocation_info(op);
			for (r = 0; r < n_regs; ++r) {
				if (bitset_is_set(ignore_regs, r))
					continue;
				op_info->prefs[r] += info->prefs[r] * factor;
			}
		}
	}
}

/**
 * Walker: Runs an a block calculates the preferences for any
 * node and every register from the considered register class.
 */
static void analyze_block(ir_node *block, void *data)
{
	float         weight = get_block_execfreq(execfreqs, block);
	ir_nodeset_t  live_nodes;
	ir_node      *node;
	(void) data;

	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	sched_foreach_reverse(block, node) {
		allocation_info_t *info;
		int                i, arity;

		if (is_Phi(node)) {
			/* TODO: handle constrained phi-nodes */
			break;
		}

		/* TODO give/take penalties for should_be_same/different) */
		check_defs(&live_nodes, weight, node);

		/* mark last uses */
		arity = get_irn_arity(node);
		/* I was lazy, and only allocated 1 unsigned
		   => maximum of 32 uses per node (rewrite if necessary) */
		assert(arity <= (int) sizeof(unsigned) * 8);

		info = get_allocation_info(node);
		for (i = 0; i < arity; ++i) {
			ir_node *op = get_irn_n(node, i);
			if (!arch_irn_consider_in_reg_alloc(cls, op))
				continue;

			/* last usage of a value? */
			if (!ir_nodeset_contains(&live_nodes, op)) {
				rbitset_set(&info->last_uses, i);
			}
		}

		be_liveness_transfer(cls, node, &live_nodes);

		/* update weights based on usage constraints */
		for (i = 0; i < arity; ++i) {
			const arch_register_req_t *req;
			const unsigned            *limited;
			ir_node                   *op = get_irn_n(node, i);

			if (!arch_irn_consider_in_reg_alloc(cls, op))
				continue;

			req = arch_get_register_req(node, i);
			if (!(req->type & arch_register_req_type_limited))
				continue;

			/* TODO: give penalties to neighbors for precolored nodes! */

			limited = req->limited;
			give_penalties_for_limits(&live_nodes, weight * USE_FACTOR, limited,
			                          op);
		}
	}

	ir_nodeset_destroy(&live_nodes);
}

/**
 * Assign register reg to the given node.
 *
 * @param node  the node
 * @param reg   the register
 */
static void use_reg(ir_node *node, const arch_register_t *reg)
{
	unsigned           r          = arch_register_get_index(reg);
	assignment_t      *assignment = &assignments[r];
	allocation_info_t *info;

	assert(assignment->value == NULL);
	assignment->value = node;

	info = get_allocation_info(node);
	info->current_assignment = assignment;

	arch_set_irn_register(node, reg);
}

/**
 * Compare two register preferences in decreasing order.
 */
static int compare_reg_pref(const void *e1, const void *e2)
{
	const reg_pref_t *rp1 = (const reg_pref_t*) e1;
	const reg_pref_t *rp2 = (const reg_pref_t*) e2;
	if (rp1->pref < rp2->pref)
		return 1;
	if (rp1->pref > rp2->pref)
		return -1;
	return 0;
}

static void fill_sort_candidates(reg_pref_t *regprefs,
                                 const allocation_info_t *info)
{
	unsigned r;

	for (r = 0; r < n_regs; ++r) {
		float pref = info->prefs[r];
		if (bitset_is_set(ignore_regs, r)) {
			pref = -10000;
		}
		regprefs[r].num  = r;
		regprefs[r].pref = pref;
	}
	/* TODO: use a stable sort here to avoid unnecessary register jumping */
	qsort(regprefs, n_regs, sizeof(regprefs[0]), compare_reg_pref);
}

/**
 * Determine and assign a register for node @p node
 */
static void assign_reg(const ir_node *block, ir_node *node)
{
	const arch_register_t     *reg;
	allocation_info_t         *info;
	const arch_register_req_t *req;
	reg_pref_t                *reg_prefs;
	ir_node                   *in_node;
	unsigned                  i;

	assert(arch_irn_consider_in_reg_alloc(cls, node));

	/* preassigned register? */
	reg = arch_get_irn_register(node);
	if (reg != NULL) {
		DB((dbg, LEVEL_2, "Preassignment %+F -> %s\n", node, reg->name));
		use_reg(node, reg);
		return;
	}

	/* give should_be_same boni */
	info = get_allocation_info(node);
	req  = arch_get_register_req_out(node);

	in_node = skip_Proj(node);
	if (req->type & arch_register_req_type_should_be_same) {
		float weight = get_block_execfreq(execfreqs, block);
		int   arity  = get_irn_arity(in_node);
		int   i;

		assert(arity <= (int) sizeof(req->other_same) * 8);
		for (i = 0; i < arity; ++i) {
			ir_node               *in;
			const arch_register_t *reg;
			unsigned               r;
			if (!rbitset_is_set(&req->other_same, i))
				continue;

			in  = get_irn_n(in_node, i);
			reg = arch_get_irn_register(in);
			assert(reg != NULL);
			r = arch_register_get_index(reg);
			if (bitset_is_set(ignore_regs, r))
				continue;
			info->prefs[r] += weight * SHOULD_BE_SAME;
		}
	}

	/* TODO: handle must_be_different */

	/*  */
	DB((dbg, LEVEL_2, "Candidates for %+F:", node));
	reg_prefs = alloca(n_regs * sizeof(reg_prefs[0]));
	fill_sort_candidates(reg_prefs, info);
	for (i = 0; i < n_regs; ++i) {
		unsigned               num = reg_prefs[i].num;
		const arch_register_t *reg = arch_register_for_index(cls, num);
		DB((dbg, LEVEL_2, " %s(%f)", reg->name, reg_prefs[i].pref));
	}
	DB((dbg, LEVEL_2, "\n"));

	for (i = 0; i < n_regs; ++i) {
		unsigned r = reg_prefs[i].num;
		/* ignores should be last and we should have a non-ignore left */
		assert(!bitset_is_set(ignore_regs, r));
		/* already used?
           TODO: It might be better to copy the value occupying the register around here, find out when... */
		if (assignments[r].value != NULL)
			continue;
		reg = arch_register_for_index(cls, r);
		DB((dbg, LEVEL_2, "Assign %+F -> %s\n", node, reg->name));
		use_reg(node, reg);
		break;
	}
}

static void free_reg_of_value(ir_node *node)
{
	allocation_info_t *info;
	assignment_t      *assignment;
	unsigned          r;

	if (!arch_irn_consider_in_reg_alloc(cls, node))
		return;

	info       = get_allocation_info(node);
	assignment = info->current_assignment;

	assert(assignment != NULL);

	r = assignment - assignments;
	DB((dbg, LEVEL_2, "Value %+F ended, freeing %s\n",
		node, arch_register_for_index(cls, r)->name));
	assignment->value        = NULL;
	info->current_assignment = NULL;
}

/**
 * Return the index of the currently assigned register of a node.
 */
static unsigned get_current_reg(ir_node *node)
{
	allocation_info_t *info       = get_allocation_info(node);
	assignment_t      *assignment = info->current_assignment;
	return assignment - assignments;
}

/**
 * Return the currently assigned assignment of a node.
 */
static assignment_t *get_current_assignment(ir_node *node)
{
	allocation_info_t *info = get_allocation_info(node);
	return info->current_assignment;
}

/**
 * Add an permutation in front of a node and change the assignments
 * due to this permutation.
 *
 * To understand this imagine a permutation like this:
 *
 * 1 -> 2
 * 2 -> 3
 * 3 -> 1, 5
 * 4 -> 6
 * 5
 * 6
 * 7 -> 7
 *
 * First we count how many destinations a single value has. At the same time
 * we can be sure that each destination register has at most 1 source register
 * (it can have 0 which means we don't care what value is in it).
 * We ignore all fullfilled permuations (like 7->7)
 * In a first pass we create as much copy instructions as possible as they
 * are generally cheaper than exchanges. We do this by counting into how many
 * destinations a register has to be copied (in the example it's 2 for register
 * 3, or 1 for the registers 1,2,4 and 7).
 * We can then create a copy into every destination register when the usecount
 * of that register is 0 (= noone else needs the value in the register).
 *
 * After this step we should have cycles left. We implement a cyclic permutation
 * of n registers with n-1 transpositions.
 *
 * @param live_nodes   the set of live nodes, updated due to live range split
 * @param before       the node before we add the permutation
 * @param permutation  the permutation array indices are the destination
 *                     registers, the values in the array are the source
 *                     registers.
 */
static void permutate_values(ir_nodeset_t *live_nodes, ir_node *before,
                             unsigned *permutation)
{
	ir_node   *block;
	ir_node  **ins    = ALLOCANZ(ir_node*, n_regs);
	unsigned  *n_used = ALLOCANZ(unsigned, n_regs);
	unsigned   r;

	/* create a list of permutations. Leave out fix points. */
	for (r = 0; r < n_regs; ++r) {
		unsigned      old_reg = permutation[r];
		assignment_t *assignment;
		ir_node      *value;

		/* no need to do anything for a fixpoint */
		if (old_reg == r)
			continue;

		assignment = &assignments[old_reg];
		value      = assignment->value;
		if (value == NULL) {
			/* nothing to do here, reg is not live. Mark it as fixpoint
			 * so we ignore it in the next steps */
			permutation[r] = r;
			continue;
		}

		ins[old_reg] = value;
		++n_used[old_reg];

		/* free occupation infos, we'll add the values back later */
		if (live_nodes != NULL) {
			free_reg_of_value(value);
			ir_nodeset_remove(live_nodes, value);
		}
	}

	block = get_nodes_block(before);

	/* step1: create copies where immediately possible */
	for (r = 0; r < n_regs; /* empty */) {
		ir_node *copy;
		ir_node *src;
		const arch_register_t *reg;
		unsigned               old_r = permutation[r];

		/* - no need to do anything for fixed points.
		   - we can't copy if the value in the dest reg is still needed */
		if (old_r == r || n_used[r] > 0) {
			++r;
			continue;
		}

		/* create a copy */
		src = ins[old_r];
		copy = be_new_Copy(cls, block, src);
		reg = arch_register_for_index(cls, r);
		DB((dbg, LEVEL_2, "Copy %+F (from %+F) -> %s\n", copy, src, reg->name));
		link_to(copy, src);
		use_reg(copy, reg);
		sched_add_before(before, copy);

		/* old register has 1 user less, permutation is resolved */
		assert(arch_register_get_index(arch_get_irn_register(src)) == old_r);
		assert(n_used[old_r] > 0);
		--n_used[old_r];
		permutation[r] = r;

		/* advance or jump back (this copy could have enabled another copy) */
		if (old_r < r && n_used[old_r] == 0) {
			r = old_r;
		} else {
			++r;
		}
	}

	/* at this point we only have "cycles" left which we have to resolve with
	 * perm instructions
	 * TODO: if we have free registers left, then we should really use copy
	 * instructions for any cycle longer than 2 registers...
	 * (this is probably architecture dependent, there might be archs where
	 *  copies are preferable even for 2 cycles)
	 */

	/* create perms with the rest */
	for (r = 0; r < n_regs; /* empty */) {
		const arch_register_t *reg;
		unsigned  old_r = permutation[r];
		unsigned  r2;
		ir_node  *in[2];
		ir_node  *perm;
		ir_node  *proj0;
		ir_node  *proj1;

		if (old_r == r) {
			++r;
			continue;
		}

		/* we shouldn't have copies from 1 value to multiple destinations left*/
		assert(n_used[old_r] == 1);

		/* exchange old_r and r2; after that old_r is a fixed point */
		r2 = permutation[old_r];

		in[0] = ins[r2];
		in[1] = ins[old_r];
		perm = be_new_Perm(cls, block, 2, in);

		proj0 = new_r_Proj(block, perm, get_irn_mode(in[0]), 0);
		link_to(proj0, in[0]);
		reg = arch_register_for_index(cls, old_r);
		use_reg(proj0, reg);

		proj1 = new_r_Proj(block, perm, get_irn_mode(in[1]), 1);

		/* 1 value is now in the correct register */
		permutation[old_r] = old_r;
		/* the source of r changed to r2 */
		permutation[r] = r2;
		ins[r2] = in[1];
		reg = arch_register_for_index(cls, r2);
		if (r == r2) {
			/* if we have reached a fixpoint update data structures */
			link_to(proj1, in[1]);
			use_reg(proj1, reg);
		} else {
			arch_set_irn_register(proj1, reg);
		}
	}

#ifdef DEBUG_libfirm
	/* now we should only have fixpoints left */
	for (r = 0; r < n_regs; ++r) {
		assert(permutation[r] == r);
	}
#endif
}

/**
 * Free regs for values last used.
 *
 * @param live_nodes   set of live nodes, will be updated
 * @param node         the node to consider
 */
static void free_last_uses(ir_nodeset_t *live_nodes, ir_node *node)
{
	allocation_info_t *info  = get_allocation_info(node);
	int                arity = get_irn_arity(node);
	int                i;
	for (i = 0; i < arity; ++i) {
		ir_node *op;

		/* check if one operand is the last use */
		if (!rbitset_is_set(&info->last_uses, i))
			continue;

		op = get_irn_n(node, i);
		free_reg_of_value(op);
		ir_nodeset_remove(live_nodes, op);
	}
}

/**
 * Create a bitset of registers occupied with value living through an
 * instruction
 */
static void determine_live_through_regs(unsigned *bitset, ir_node *node)
{
	const allocation_info_t *info = get_allocation_info(node);
	unsigned r;
	int i;
	int arity;

	/* mark all used registers as potentially live-through */
	for (r = 0; r < n_regs; ++r) {
		const assignment_t *assignment = &assignments[r];
		if (assignment->value == NULL)
			continue;

		rbitset_set(bitset, r);
	}

	/* remove registers of value dying at the instruction */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node               *op;
		const arch_register_t *reg;

		if (!rbitset_is_set(&info->last_uses, i))
			continue;

		op  = get_irn_n(node, i);
		reg = arch_get_irn_register(op);
		rbitset_clear(bitset, arch_register_get_index(reg));
	}
}

/**
 * Enforce constraints at a node by live range splits.
 *
 * @param live_nodes  the set of live nodes, might be changed
 * @param node        the current node
 */
static void enforce_constraints(ir_nodeset_t *live_nodes, ir_node *node)
{
	int arity = get_irn_arity(node);
	int i, dummy, res;
	hungarian_problem_t *bp;
	unsigned l, r, p;
	unsigned *assignment;

	/* see if any use constraints are not met */
	bool good = true;
	for (i = 0; i < arity; ++i) {
		ir_node                   *op = get_irn_n(node, i);
		const arch_register_req_t *req;
		const unsigned            *limited;
		unsigned                  r;

		if (!arch_irn_consider_in_reg_alloc(cls, op))
			continue;

		/* are there any limitations for the i'th operand? */
		req = arch_get_register_req(node, i);
		if (!(req->type & arch_register_req_type_limited))
			continue;

		limited = req->limited;
		r       = get_current_reg(op);
		if (!rbitset_is_set(limited, r)) {
			/* found an assignement outside the limited set */
			good = false;
			break;
		}
	}

	/* construct a list of register occupied by live-through values */
	unsigned *live_through_regs = NULL;
	unsigned *output_regs       = NULL;

	/* is any of the live-throughs using a constrainted output register? */
	if (get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			const arch_register_req_t *req;

			if (!arch_irn_consider_in_reg_alloc(cls, proj))
				continue;

			req = arch_get_register_req_out(proj);
			if (!(req->type & arch_register_req_type_limited))
				continue;

			if (live_through_regs == NULL) {
				rbitset_alloca(live_through_regs, n_regs);
				determine_live_through_regs(live_through_regs, node);

				rbitset_alloca(output_regs, n_regs);
			}

			rbitset_or(output_regs, req->limited, n_regs);
			if (rbitsets_have_common(req->limited, live_through_regs, n_regs)) {
				good = false;
				break;
			}
		}
	} else {
		if (arch_irn_consider_in_reg_alloc(cls, node)) {
			const arch_register_req_t *req = arch_get_register_req_out(node);
			if (req->type & arch_register_req_type_limited) {
				rbitset_alloca(live_through_regs, n_regs);
				determine_live_through_regs(live_through_regs, node);
				if (rbitsets_have_common(req->limited, live_through_regs, n_regs)) {
					good = false;

					rbitset_alloca(output_regs, n_regs);
					rbitset_or(output_regs, req->limited, n_regs);
				}
			}
		}
	}

	if (good)
		return;

	if (live_through_regs == NULL) {
		rbitset_alloca(live_through_regs, n_regs);
		rbitset_alloca(output_regs, n_regs);
	}

	/* swap values around */
	bp = hungarian_new(n_regs, n_regs, HUNGARIAN_MATCH_PERFECT);

	/* add all combinations, then remove not allowed ones */
	for (l = 0; l < n_regs; ++l) {
		if (bitset_is_set(ignore_regs, l)) {
			hungarian_add(bp, l, l, 90);
			continue;
		}

		for (r = 0; r < n_regs; ++r) {
			if (bitset_is_set(ignore_regs, r))
				continue;
			/* livethrough values may not use constrainted output registers */
			if (rbitset_is_set(live_through_regs, l)
					&& rbitset_is_set(output_regs, r))
				continue;

			hungarian_add(bp, l, r, l == r ? 90 : 89);
		}
	}

	for (i = 0; i < arity; ++i) {
		ir_node                   *op = get_irn_n(node, i);
		const arch_register_req_t *req;
		const unsigned            *limited;
		unsigned                  current_reg;

		if (!arch_irn_consider_in_reg_alloc(cls, op))
			continue;

		req = arch_get_register_req(node, i);
		if (!(req->type & arch_register_req_type_limited))
			continue;

		limited     = req->limited;
		current_reg = get_current_reg(op);
		for (r = 0; r < n_regs; ++r) {
			if (rbitset_is_set(limited, r))
				continue;
			hungarian_remv(bp, current_reg, r);
		}
	}

	hungarian_print_costmatrix(bp, 1);
	hungarian_prepare_cost_matrix(bp, HUNGARIAN_MODE_MAXIMIZE_UTIL);

	assignment = ALLOCAN(unsigned, n_regs);
	res = hungarian_solve(bp, (int*) assignment, &dummy, 0);
	assert(res == 0);

	printf("Swap result:");
	for (p = 0; p < n_regs; ++p) {
		printf(" %d", assignment[p]);
	}
	printf("\n");

	hungarian_free(bp);

	permutate_values(live_nodes, node, assignment);
}

/** test wether a node @p n is a copy of the value of node @p of */
static int is_copy_of(ir_node *n, ir_node *of)
{
	allocation_info_t *of_info;

	if (n == NULL)
		return 0;

	if (n == of)
		return 1;

	of_info = get_allocation_info(of);
	if (!irn_visited(n))
		return 0;

	return of_info == get_irn_link(n);
}

/** find a value in the end-assignment of a basic block
 * @returns the index into the assignment array if found
 *          -1 if not found
 */
static int find_value_in_block_info(block_info_t *info, ir_node *value)
{
	unsigned      r;
	assignment_t *assignments = info->assignments;
	for (r = 0; r < n_regs; ++r) {
		const assignment_t *assignment = &assignments[r];
		if (is_copy_of(assignment->value, value))
			return (int) r;
	}

	return -1;
}

/**
 * Create the necessary permutations at the end of a basic block to fullfill
 * the register assignment for phi-nodes in the next block
 */
static void add_phi_permutations(ir_node *block, int p)
{
	unsigned  r;
	unsigned *permutation;
	assignment_t *old_assignments;
	int       need_permutation;
	ir_node  *node;
	ir_node  *pred = get_Block_cfgpred_block(block, p);

	block_info_t *pred_info = get_block_info(pred);

	/* predecessor not processed yet? nothing to do */
	if (!pred_info->processed)
		return;

	permutation = ALLOCAN(unsigned, n_regs);
	for (r = 0; r < n_regs; ++r) {
		permutation[r] = r;
	}

	/* check phi nodes */
	need_permutation = 0;
	node = sched_first(block);
	for ( ; is_Phi(node); node = sched_next(node)) {
		const arch_register_t *reg;
		int                    regn;
		int                    a;
		ir_node               *op;

		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		op = get_Phi_pred(node, p);
		a = find_value_in_block_info(pred_info, op);
		assert(a >= 0);

		reg = arch_get_irn_register(node);
		regn = arch_register_get_index(reg);
		if (regn != a) {
			permutation[regn] = a;
			need_permutation = 1;
		}
	}

	old_assignments = assignments;
	assignments     = pred_info->assignments;
	permutate_values(NULL, be_get_end_of_block_insertion_point(pred),
	                 permutation);
	assignments     = old_assignments;

	node = sched_first(block);
	for ( ; is_Phi(node); node = sched_next(node)) {
		int                    a;
		ir_node               *op;

		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		op = get_Phi_pred(node, p);
		/* TODO: optimize */
		a = find_value_in_block_info(pred_info, op);
		assert(a >= 0);

		op = pred_info->assignments[a].value;
		set_Phi_pred(node, p, op);
	}
}

/**
 * Walker: assign registers to all nodes of a block that
 * need registers from the currently considered register class.
 */
static void allocate_coalesce_block(ir_node *block, void *data)
{
	int                    i;
	unsigned               r;
	ir_nodeset_t           live_nodes;
	ir_nodeset_iterator_t  iter;
	ir_node               *node, *start;
	int                    n_preds;
	block_info_t          *block_info;
	block_info_t         **pred_block_infos;

	(void) data;
	DB((dbg, LEVEL_2, "Allocating in block %+F\n", block));

	/* clear assignments */
	block_info  = get_block_info(block);
	assignments = block_info->assignments;

	for (r = 0; r < n_regs; ++r) {
		assignment_t       *assignment = &assignments[r];
		ir_node            *value      = assignment->value;
		allocation_info_t  *info;

		if (value == NULL)
			continue;

		info                     = get_allocation_info(value);
		info->current_assignment = assignment;
	}

	ir_nodeset_init(&live_nodes);

	/* gather regalloc infos of predecessor blocks */
	n_preds = get_Block_n_cfgpreds(block);
	pred_block_infos = ALLOCAN(block_info_t*, n_preds);
	for (i = 0; i < n_preds; ++i) {
		ir_node *pred = get_Block_cfgpred_block(block, i);
		pred_block_infos[i] = get_block_info(pred);
	}

	/* collect live-in nodes and preassigned values */
	be_lv_foreach(lv, block, be_lv_state_in, i) {
		const arch_register_t *reg;

		node = be_lv_get_irn(lv, block, i);
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		/* remember that this node is live at the beginning of the block */
		ir_nodeset_insert(&live_nodes, node);

		/* if the node already has a register assigned use it */
		reg = arch_get_irn_register(node);
		if (reg != NULL) {
			/* TODO: consult pred-block infos here. The value could be copied
			   away in some/all predecessor blocks. We need to construct
			   phi-nodes in this case.
			   We even need to construct some Phi_0 like constructs in cases
			   where the predecessor allocation is not determined yet. */
			use_reg(node, reg);
		}
	}

	/* handle phis... */
	node = sched_first(block);
	for ( ; is_Phi(node); node = sched_next(node)) {
		const arch_register_t *reg;

		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		/* fill in regs already assigned */
		reg = arch_get_irn_register(node);
		if (reg != NULL) {
			use_reg(node, reg);
		} else {
			/* TODO: give boni for registers already assigned at the
			   predecessors */
			assign_reg(block, node);
		}
	}
	start = node;

	/* assign regs for live-in values */
	foreach_ir_nodeset(&live_nodes, node, iter) {
		const arch_register_t *reg = arch_get_irn_register(node);
		if (reg != NULL)
			continue;

		assign_reg(block, node);
	}

	/* permutate values at end of predecessor blocks in case of phi-nodes */
	if (n_preds > 1) {
		int p;
		for (p = 0; p < n_preds; ++p) {
			add_phi_permutations(block, p);
		}
	}

	/* assign instructions in the block */
	for (node = start; !sched_is_end(node); node = sched_next(node)) {
		int arity = get_irn_arity(node);
		int i;

		/* enforce use constraints */
		enforce_constraints(&live_nodes, node);

		/* exchange values to copied values where needed */
		for (i = 0; i < arity; ++i) {
			ir_node      *op = get_irn_n(node, i);
			assignment_t *assignment;

			if (!arch_irn_consider_in_reg_alloc(cls, op))
				continue;
			assignment = get_current_assignment(op);
			assert(assignment != NULL);
			if (op != assignment->value) {
				set_irn_n(node, i, assignment->value);
			}
		}

		/* free registers of values last used at this instruction */
		free_last_uses(&live_nodes, node);

		/* assign output registers */
		/* TODO: 2 phases: first: pre-assigned ones, 2nd real regs */
		if (get_irn_mode(node) == mode_T) {
			const ir_edge_t *edge;
			foreach_out_edge(node, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(cls, proj))
					continue;
				assign_reg(block, proj);
			}
		} else if (arch_irn_consider_in_reg_alloc(cls, node)) {
			assign_reg(block, node);
		}
	}

	ir_nodeset_destroy(&live_nodes);
	assignments = NULL;

	block_info->processed = 1;

	/* if we have exactly 1 successor then we might be able to produce phi
	   copies now */
	if (get_irn_n_edges_kind(block, EDGE_KIND_BLOCK) == 1) {
		const ir_edge_t *edge
			= get_irn_out_edge_first_kind(block, EDGE_KIND_BLOCK);
		ir_node      *succ      = get_edge_src_irn(edge);
		int           p         = get_edge_src_pos(edge);
		block_info_t *succ_info = get_block_info(succ);

		if (succ_info->processed) {
			add_phi_permutations(succ, p);
		}
	}
}

/**
 * Run the register allocator for the current register class.
 */
static void be_straight_alloc_cls(void)
{
	lv = be_assure_liveness(birg);
	be_liveness_assure_sets(lv);
	be_liveness_assure_chk(lv);

	assignments = NULL;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	DB((dbg, LEVEL_2, "=== Allocating registers of %s ===\n", cls->name));

	irg_block_walk_graph(irg, NULL, analyze_block, NULL);
	irg_block_walk_graph(irg, NULL, allocate_coalesce_block, NULL);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);
}

/**
 * Run the spiller on the current graph.
 */
static void spill(void)
{
	/* make sure all nodes show their real register pressure */
	BE_TIMER_PUSH(t_ra_constr);
	be_pre_spill_prepare_constr(birg, cls);
	BE_TIMER_POP(t_ra_constr);

	/* spill */
	BE_TIMER_PUSH(t_ra_spill);
	be_do_spill(birg, cls);
	BE_TIMER_POP(t_ra_spill);

	BE_TIMER_PUSH(t_ra_spill_apply);
	check_for_memory_operands(irg);
	BE_TIMER_POP(t_ra_spill_apply);
}

/**
 * The straight register allocator for a whole procedure.
 */
static void be_straight_alloc(be_irg_t *new_birg)
{
	const arch_env_t *arch_env = new_birg->main_env->arch_env;
	int   n_cls                = arch_env_get_n_reg_class(arch_env);
	int   c;

	obstack_init(&obst);

	birg      = new_birg;
	irg       = be_get_birg_irg(birg);
	execfreqs = birg->exec_freq;

	/* TODO: extract some of the stuff from bechordal allocator, like
	 * statistics, time measurements, etc. and use them here too */

	for (c = 0; c < n_cls; ++c) {
		cls = arch_env_get_reg_class(arch_env, c);
		if (arch_register_class_flags(cls) & arch_register_class_flag_manual_ra)
			continue;

		stat_ev_ctx_push_str("bestraight_cls", cls->name);

		n_regs      = arch_register_class_n_regs(cls);
		ignore_regs = bitset_malloc(n_regs);
		be_put_ignore_regs(birg, cls, ignore_regs);

		spill();

		/* verify schedule and register pressure */
		BE_TIMER_PUSH(t_verify);
		if (birg->main_env->options->vrfy_option == BE_CH_VRFY_WARN) {
			be_verify_schedule(birg);
			be_verify_register_pressure(birg, cls, irg);
		} else if (birg->main_env->options->vrfy_option == BE_CH_VRFY_ASSERT) {
			assert(be_verify_schedule(birg) && "Schedule verification failed");
			assert(be_verify_register_pressure(birg, cls, irg)
				&& "Register pressure verification failed");
		}
		BE_TIMER_POP(t_verify);

		BE_TIMER_PUSH(t_ra_color);
		be_straight_alloc_cls();
		BE_TIMER_POP(t_ra_color);

		bitset_free(ignore_regs);

		stat_ev_ctx_pop("bestraight_cls");
	}

	BE_TIMER_PUSH(t_verify);
	if (birg->main_env->options->vrfy_option == BE_CH_VRFY_WARN) {
		be_verify_register_allocation(birg);
	} else if(birg->main_env->options->vrfy_option == BE_CH_VRFY_ASSERT) {
		assert(be_verify_register_allocation(birg)
				&& "Register allocation invalid");
	}
	BE_TIMER_POP(t_verify);

	obstack_free(&obst, NULL);
}

/**
 * Initializes this module.
 */
void be_init_straight_alloc(void)
{
	static be_ra_t be_ra_straight = {
		be_straight_alloc,
	};

	FIRM_DBG_REGISTER(dbg, "firm.be.straightalloc");

	be_register_allocator("straight", &be_ra_straight);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_straight_alloc);
