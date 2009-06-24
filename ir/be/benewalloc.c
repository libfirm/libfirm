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
 * 2. Walk blocks and assigns registers in a greedy fashion. Preferring registers
 *    with high preferences. When register constraints are not met, add copies
 *    and split live-ranges.
 *
 * TODO:
 *  - output constraints are not ensured. The algorithm fails to copy values
 *    away, so the registers for constrained outputs are free.
 *  - must_be_different constraint is not respected
 *  - No parallel copies at basic block borders are created, no additional phis
 *    created after copies have been inserted.
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
#include "besched_t.h"
#include "beirg_t.h"
#include "benode_t.h"
#include "bespilloptions.h"
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

typedef struct assignment_t assignment_t;
struct assignment_t {
	ir_node *value;            /**< currently assigned value */
};

static assignment_t *assignments;

typedef struct allocation_info_t allocation_info_t;
struct allocation_info_t {
	unsigned      last_uses;   /**< bitset indicating last uses (input pos) */
	assignment_t *current_assignment;
	float         prefs[0];    /**< register preferences */
};

typedef struct reg_pref_t reg_pref_t;
struct reg_pref_t {
	unsigned num;
	float    pref;
};

/**
 * Get the allocation info for a node.
 * The info is allocated on the first visit of a node.
 */
static allocation_info_t *get_allocation_info(ir_node *node)
{
	allocation_info_t *info;
	if (!irn_visited(node)) {
		size_t size = sizeof(info[0]) + n_regs * sizeof(float);
		info = obstack_alloc(&obst, size);
		memset(info, 0, size);
		set_irn_link(node, info);
		mark_irn_visited(node);
	} else {
		info = get_irn_link(node);
	}

	return info;
}

/**
 * Link the allocation info of a node to a copy.
 * Afterwards, both nodes uses the same allocation info.
 * Copy must not have an allocation info assigned yet.
 *
 * @param copy   the node the gets the allocation info assigned
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
			unsigned          r;
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
		int               i, arity;

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
			if ((req->type & arch_register_req_type_limited) == 0)
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
	unsigned      r          = arch_register_get_index(reg);
	assignment_t *assignment = &assignments[r];
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
		/* already used? TODO: It might be better to copy the value occupying the register around here, find out when... */
		if (assignments[r].value != NULL)
			continue;
		use_reg(node, arch_register_for_index(cls, r));
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
 * @param live_nodes   the set of live nodes, updated due to live range split
 * @param before       the node before we add the permutation
 * @param permutation  the permutation array (map indexes to indexes)
 */
static void permutate_values(ir_nodeset_t *live_nodes, ir_node *before,
                             unsigned *permutation)
{
	ir_node *block, *perm;
	ir_node **in = ALLOCAN(ir_node*, n_regs);
	size_t    r;

	int i = 0;
	for (r = 0; r < n_regs; ++r) {
		unsigned     new_reg = permutation[r];
		assignment_t *assignment;
		ir_node      *value;

		if (new_reg == r)
			continue;

		assignment = &assignments[r];
		value      = assignment->value;
		if (value == NULL) {
			/* nothing to do here, reg is not live */
			permutation[r] = r;
			continue;
		}

		in[i++] = value;

		free_reg_of_value(value);
		ir_nodeset_remove(live_nodes, value);
	}

	block = get_nodes_block(before);
	perm  = be_new_Perm(cls, irg, block, i, in);

	sched_add_before(before, perm);

	i = 0;
	for (r = 0; r < n_regs; ++r) {
		unsigned new_reg = permutation[r];
		ir_node  *value;
		ir_mode  *mode;
		ir_node  *proj;
		const arch_register_t *reg;

		if (new_reg == r)
			continue;

		value = in[i];
		mode  = get_irn_mode(value);
		proj  = new_r_Proj(irg, block, perm, mode, i);

		reg = arch_register_for_index(cls, new_reg);

		link_to(proj, value);
		use_reg(proj, reg);
		ir_nodeset_insert(live_nodes, proj);

		++i;
	}
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
		if ((req->type & arch_register_req_type_limited) == 0)
			continue;

		limited = req->limited;
		r       = get_current_reg(op);
		if (!rbitset_is_set(limited, r)) {
			/* found an assignement outside the limited set */
			good = false;
			break;
		}
	}

	if (good)
		return;

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
		if ((req->type & arch_register_req_type_limited) == 0)
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

/**
 * Walker: assign registers to all nodes of a block that
 * needs registers from the currently considered register class.
 */
static void allocate_coalesce_block(ir_node *block, void *data)
{
	int                   i;
	ir_nodeset_t          live_nodes;
	ir_nodeset_iterator_t iter;
	ir_node               *node, *start;

	/* clear assignments */
	memset(assignments, 0, n_regs * sizeof(assignments[0]));

	(void) data;

	/* collect live-in nodes and preassigned values */
	ir_nodeset_init(&live_nodes);
	be_lv_foreach(lv, block, be_lv_state_in, i) {
		const arch_register_t *reg;

		node = be_lv_get_irn(lv, block, i);
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		ir_nodeset_insert(&live_nodes, node);

		/* fill in regs already assigned */
		reg = arch_get_irn_register(node);
		if (reg != NULL) {
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
		}
	}
	start = node;

	/* assign regs for live-in values */
	foreach_ir_nodeset(&live_nodes, node, iter) {
		assign_reg(block, node);
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

	foreach_ir_nodeset(&live_nodes, node, iter) {
		free_reg_of_value(node);
	}

	ir_nodeset_destroy(&live_nodes);
}

/**
 * Run the register allocator for the current register class.
 */
static void be_straight_alloc_cls(void)
{
	n_regs         = arch_register_class_n_regs(cls);
	lv             = be_assure_liveness(birg);
	be_liveness_assure_sets(lv);
	be_liveness_assure_chk(lv);

	assignments = obstack_alloc(&obst, n_regs * sizeof(assignments[0]));

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	irg_block_walk_graph(irg, analyze_block, NULL, NULL);
	irg_block_walk_graph(irg, allocate_coalesce_block, NULL, NULL);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);
}

/**
 * Run the spiller on the current graph.
 */
static void spill(void)
{
	/* TODO: rewrite pre_spill_prepare to work without chordal_env... */
	be_chordal_env_t  cenv;
	memset(&cenv, 0, sizeof(cenv));
	cenv.obst          = &obst;
	cenv.irg           = irg;
	cenv.birg          = birg;
	cenv.cls           = cls;
	cenv.ignore_colors = ignore_regs;

	/* make sure all nodes show their real register pressure */
	be_pre_spill_prepare_constr(&cenv);

	/* spill */
	be_do_spill(birg, cls);

	check_for_memory_operands(irg);
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

		n_regs      = cls->n_regs;
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

		/* TODO: dump intermediate results */

		stat_ev_ctx_pop("bestraight_cls");
	}

	obstack_free(&obst, NULL);
}

static be_ra_t be_ra_straight = {
	be_straight_alloc,
};

/**
 * Initializes this module.
 */
void be_init_straight_alloc(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.straightalloc");

	be_register_allocator("straight", &be_ra_straight);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_straight_alloc);
