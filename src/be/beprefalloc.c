/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Preference Guided Register Assignment
 * @author      Matthias Braun
 * @date        14.2.2009
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
 *  - make use of free registers in the permute_values code
 */
#include "be.h"
#include "bechordal_t.h"
#include "beirg.h"
#include "belive.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "besched.h"
#include "bespill.h"
#include "bespillutil.h"
#include "bestack.h"
#include "beutil.h"
#include "beverify.h"
#include "debug.h"
#include "execfreq.h"
#include "hungarian.h"
#include "ircons.h"
#include "irdom.h"
#include "irdump.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irtools.h"
#include "lpp.h"
#include "obst.h"
#include "panic.h"
#include "pdeq.h"
#include "raw_bitset.h"
#include "statev.h"
#include "target_t.h"
#include "unionfind.h"
#include "util.h"
#include <float.h>
#include <math.h>
#include <stdbool.h>

#define USE_FACTOR                     1.0f
#define DEF_FACTOR                     1.0f
#define NEIGHBOR_FACTOR                0.2f
#define AFF_SHOULD_BE_SAME             0.5f
#define AFF_PHI                        1.0f
#define SPLIT_DELTA                    1.0f
#define MAX_OPTIMISTIC_SPLIT_RECURSION 0

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static struct obstack               obst;
static ir_graph                    *irg;
static const arch_register_class_t *cls;
static be_lv_t                     *lv;
static unsigned                     n_regs;
static unsigned                    *normal_regs;
static int                         *congruence_classes;
static ir_node                    **block_order;
static size_t                       n_block_order;

/** currently active assignments (while processing a basic block)
 * maps registers to values(their current copies) */
static ir_node **assignments;

/**
 * allocation information: last_uses, register preferences
 * the information is per firm-node.
 */
struct allocation_info_t {
	unsigned  last_uses[2];   /**< bitset indicating last uses (input pos) */
	ir_node  *current_value;  /**< copy of the value that should be used */
	ir_node  *original_value; /**< for copies point to original value */
	float     prefs[];        /**< register preferences */
};
typedef struct allocation_info_t allocation_info_t;

/** helper data structure used when sorting register preferences */
struct reg_pref_t {
	unsigned num;
	float    pref;
};
typedef struct reg_pref_t reg_pref_t;

/** per basic-block information */
struct block_info_t {
	bool     processed;       /**< indicate whether block is processed */
	ir_node *assignments[];   /**< register assignments at end of block */
};
typedef struct block_info_t block_info_t;

/**
 * Get the allocation info for a node.
 * The info is allocated on the first visit of a node.
 */
static allocation_info_t *get_allocation_info(ir_node *node)
{
	allocation_info_t *info = (allocation_info_t*)get_irn_link(node);
	if (info == NULL) {
		info = OALLOCFZ(&obst, allocation_info_t, prefs, n_regs);
		info->current_value  = node;
		info->original_value = node;
		set_irn_link(node, info);
	}

	return info;
}

static allocation_info_t *try_get_allocation_info(const ir_node *node)
{
	return (allocation_info_t*) get_irn_link(node);
}

/**
 * Get allocation information for a basic block
 */
static block_info_t *get_block_info(ir_node *block)
{
	block_info_t *info = (block_info_t*)get_irn_link(block);

	assert(is_Block(block));
	if (info == NULL) {
		info = OALLOCFZ(&obst, block_info_t, assignments, n_regs);
		set_irn_link(block, info);
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
static void mark_as_copy_of(ir_node *copy, ir_node *value)
{
	allocation_info_t *info      = get_allocation_info(value);
	allocation_info_t *copy_info = get_allocation_info(copy);

	/* find original value */
	ir_node *original = info->original_value;
	if (original != value) {
		info = get_allocation_info(original);
	}

	assert(info->original_value == original);
	info->current_value = copy;

	/* the copy should not be linked to something else yet */
	assert(copy_info->original_value == copy);
	copy_info->original_value = original;

	/* copy over allocation preferences */
	MEMCPY(copy_info->prefs, info->prefs, n_regs);
}

/**
 * Calculate the penalties for every register on a node and its live neighbors.
 *
 * @param live_nodes  the set of live nodes at the current position, may be NULL
 * @param penalty     the penalty to subtract from
 * @param limited     a raw bitset containing the limited set for the node
 * @param node        the node
 */
static void give_penalties_for_limits(const ir_nodeset_t *live_nodes,
                                      float penalty, const unsigned* limited,
                                      ir_node *node)
{
	allocation_info_t *info = get_allocation_info(node);

	/* give penalty for all forbidden regs */
	for (unsigned r = 0; r < n_regs; ++r) {
		if (rbitset_is_set(limited, r))
			continue;

		info->prefs[r] -= penalty;
	}

	/* all other live values should get a penalty for allowed regs */
	if (live_nodes == NULL)
		return;

	penalty   *= NEIGHBOR_FACTOR;
	size_t n_allowed = rbitset_popcount(limited, n_regs);
	if (n_allowed > 1) {
		/* only create a very weak penalty if multiple regs are allowed */
		penalty = (penalty * 0.8f) / n_allowed;
	}
	foreach_ir_nodeset(live_nodes, neighbor, iter) {
		allocation_info_t *neighbor_info;

		/* TODO: if op is used on multiple inputs we might not do a
		 * continue here */
		if (neighbor == node)
			continue;

		neighbor_info = get_allocation_info(neighbor);
		for (unsigned r = 0; r < n_regs; ++r) {
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
static void check_defs(ir_nodeset_t const *const live_nodes, float const weight, ir_node *const node, arch_register_req_t const *const req)
{
	if (req->limited != NULL) {
		const unsigned *limited = req->limited;
		float           penalty = weight * DEF_FACTOR;
		give_penalties_for_limits(live_nodes, penalty, limited, node);
	}

	if (req->should_be_same != 0) {
		ir_node           *insn  = skip_Proj(node);
		allocation_info_t *info  = get_allocation_info(node);
		int                arity = get_irn_arity(insn);

		float factor = 1.0f / rbitset_popcount(&req->should_be_same, arity);
		foreach_irn_in(insn, i, op) {
			if (!rbitset_is_set(&req->should_be_same, i))
				continue;

			/* if we the value at the should_be_same input doesn't die at the
			 * node, then it is no use to propagate the constraints (since a
			 * copy will emerge anyway) */
			if (ir_nodeset_contains(live_nodes, op))
				continue;

			allocation_info_t *op_info = get_allocation_info(op);
			for (unsigned r = 0; r < n_regs; ++r) {
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
	float        weight = (float)get_block_execfreq(block);
	ir_nodeset_t live_nodes;
	(void) data;

	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	sched_foreach_non_phi_reverse(block, node) {
		be_foreach_definition(node, cls, value, req,
			check_defs(&live_nodes, weight, value, req);
		);

		allocation_info_t *info = get_allocation_info(node);
		if (get_irn_arity(node) >= (int)sizeof(info->last_uses) * 8) {
			panic("node with more than %d inputs not supported yet",
			      (int) sizeof(info->last_uses) * 8);
		}

		/* mark last uses */
		foreach_irn_in(node, i, op) {
			const arch_register_req_t *req = arch_get_irn_register_req(op);
			if (req->cls != cls)
				continue;

			/* last usage of a value? */
			if (!ir_nodeset_contains(&live_nodes, op)) {
				rbitset_set(info->last_uses, i);
			}
		}

		be_liveness_transfer(cls, node, &live_nodes);

		/* update weights based on usage constraints */
		be_foreach_use(node, cls, req, op, op_req,
			if (req->limited == NULL)
				continue;

			give_penalties_for_limits(&live_nodes, weight * USE_FACTOR, req->limited, op);
		);
	}

	ir_nodeset_destroy(&live_nodes);
}

static void congruence_def(ir_nodeset_t *const live_nodes, ir_node const *const node, arch_register_req_t const *const req)
{
	/* should be same constraint? */
	if (req->should_be_same != 0) {
		const ir_node *insn     = skip_Proj_const(node);
		unsigned       node_idx = get_irn_idx(node);
		node_idx = uf_find(congruence_classes, node_idx);

		foreach_irn_in(insn, i, op) {
			if (!rbitset_is_set(&req->should_be_same, i))
				continue;

			int op_idx = get_irn_idx(op);
			op_idx = uf_find(congruence_classes, op_idx);

			/* do we interfere with the value */
			bool interferes = false;
			foreach_ir_nodeset(live_nodes, live, iter) {
				int lv_idx = get_irn_idx(live);
				lv_idx     = uf_find(congruence_classes, lv_idx);
				if (lv_idx == op_idx) {
					interferes = true;
					break;
				}
			}
			/* don't put in same affinity class if we interfere */
			if (interferes)
				continue;

			uf_union(congruence_classes, node_idx, op_idx);
			DB((dbg, LEVEL_3, "Merge %+F and %+F congruence classes\n",
			    node, op));
			/* one should_be_same is enough... */
			break;
		}
	}
}

static void create_congruence_class(ir_node *block, void *data)
{
	ir_nodeset_t live_nodes;

	(void) data;
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	/* check should be same constraints */
	sched_foreach_non_phi_reverse(block, node) {
		be_foreach_definition(node, cls, value, req,
			congruence_def(&live_nodes, value, req);
		);
		be_liveness_transfer(cls, node, &live_nodes);
	}

	/* check phi congruence classes */
	sched_foreach_phi(block, phi) {
		if (!arch_irn_consider_in_reg_alloc(cls, phi))
			continue;

		int node_idx = get_irn_idx(phi);
		node_idx = uf_find(congruence_classes, node_idx);

		int arity = get_irn_arity(phi);
		for (int i = 0; i < arity; ++i) {
			ir_node *op     = get_Phi_pred(phi, i);
			int      op_idx = get_irn_idx(op);
			op_idx = uf_find(congruence_classes, op_idx);

			/* do we interfere with the value */
			bool interferes = false;
			foreach_ir_nodeset(&live_nodes, live, iter) {
				int lv_idx = get_irn_idx(live);
				lv_idx     = uf_find(congruence_classes, lv_idx);
				if (lv_idx == op_idx) {
					interferes = true;
					break;
				}
			}
			/* don't put in same affinity class if we interfere */
			if (interferes)
				continue;
			/* any other phi has the same input? */
			sched_foreach_phi(block, phi) {
				ir_node *oop;
				int      oop_idx;
				if (!arch_irn_consider_in_reg_alloc(cls, phi))
					continue;
				oop = get_Phi_pred(phi, i);
				if (oop == op)
					continue;
				oop_idx = get_irn_idx(oop);
				oop_idx = uf_find(congruence_classes, oop_idx);
				if (oop_idx == op_idx) {
					interferes = true;
					break;
				}
			}
			if (interferes)
				continue;

			/* merge the 2 congruence classes and sum up their preferences */
			int old_node_idx = node_idx;
			node_idx = uf_union(congruence_classes, node_idx, op_idx);
			DB((dbg, LEVEL_3, "Merge %+F and %+F congruence classes\n",
			    phi, op));

			old_node_idx = node_idx == old_node_idx ? op_idx : old_node_idx;
			allocation_info_t *head_info
				= get_allocation_info(get_idx_irn(irg, node_idx));
			allocation_info_t *other_info
				= get_allocation_info(get_idx_irn(irg, old_node_idx));
			for (unsigned r = 0; r < n_regs; ++r) {
				head_info->prefs[r] += other_info->prefs[r];
			}
		}
	}
	ir_nodeset_destroy(&live_nodes);
}

static void set_congruence_prefs(ir_node *node, void *data)
{
	(void) data;
	unsigned node_idx = get_irn_idx(node);
	unsigned node_set = uf_find(congruence_classes, node_idx);

	/* head of congruence class or not in any class */
	if (node_set == node_idx)
		return;

	if (!arch_irn_consider_in_reg_alloc(cls, node))
		return;

	ir_node *head = get_idx_irn(irg, node_set);
	allocation_info_t *head_info = get_allocation_info(head);
	allocation_info_t *info      = get_allocation_info(node);

	MEMCPY(info->prefs, head_info->prefs, n_regs);
}

static void combine_congruence_classes(void)
{
	size_t n = get_irg_last_idx(irg);
	congruence_classes = XMALLOCN(int, n);
	uf_init(congruence_classes, n);

	/* create congruence classes */
	irg_block_walk_graph(irg, create_congruence_class, NULL, NULL);
	/* merge preferences */
	irg_walk_graph(irg, set_congruence_prefs, NULL, NULL);
	free(congruence_classes);
}


static void register_assignment(ir_node *const node, unsigned const reg_idx, unsigned const width)
{
	for (unsigned r0 = reg_idx; r0 < reg_idx + width; ++r0)
		assignments[r0] = node;
}

/**
 * Assign register reg to the given node.
 *
 * @param node  the node
 * @param reg   the register
 */
static void use_reg(ir_node *node, const arch_register_t *reg, unsigned width)
{
	register_assignment(node, reg->index, width);
	arch_set_irn_register(node, reg);
}

static void use_reg_idx(ir_node *const node, unsigned const reg_idx, unsigned const width)
{
	register_assignment(node, reg_idx, width);
	arch_set_irn_register_idx(node, reg_idx);
}

static void free_reg_of_value(ir_node *node)
{
	if (!arch_irn_consider_in_reg_alloc(cls, node))
		return;

	const arch_register_t     *reg = arch_get_irn_register(node);
	const arch_register_req_t *req = arch_get_irn_register_req(node);
	unsigned                   r   = reg->index;
	/* assignment->value may be NULL if a value is used at 2 inputs
	 * so it gets freed twice. */
	for (unsigned r0 = r; r0 < r + req->width; ++r0) {
		assert(assignments[r0] == node || assignments[r0] == NULL);
		assignments[r0] = NULL;
	}
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
	for (unsigned r = 0; r < n_regs; ++r) {
		float pref = info->prefs[r];
		regprefs[r].num  = r;
		regprefs[r].pref = pref;
	}
	/* TODO: use a stable sort here to avoid unnecessary register jumping */
	QSORT(regprefs, n_regs, compare_reg_pref);
}

static bool try_optimistic_split(ir_node *to_split, ir_node *before,
                                 float pref, float pref_delta,
                                 unsigned *forbidden_regs, int recursion)
{
	(void) pref;
	unsigned           r = 0;
	allocation_info_t *info = get_allocation_info(to_split);
	float              delta = 0;

	/* stupid hack: don't optimistically split don't spill nodes...
	 * (so we don't split away the values produced because of
	 *  must_be_different constraints) */
	ir_node *original_insn = skip_Proj(info->original_value);
	if (arch_get_irn_flags(original_insn) & arch_irn_flag_dont_spill)
		return false;

	const arch_register_t *from_reg        = arch_get_irn_register(to_split);
	unsigned               from_r          = from_reg->index;
	ir_node               *block           = get_nodes_block(before);
	float split_threshold = (float)get_block_execfreq(block) * SPLIT_DELTA;

	if (pref_delta < split_threshold*0.5)
		return false;

	/* find the best free position where we could move to */
	reg_pref_t *prefs = ALLOCAN(reg_pref_t, n_regs);
	fill_sort_candidates(prefs, info);
	unsigned i;
	for (i = 0; i < n_regs; ++i) {
		/* we need a normal register which is not an output register
		   an different from the current register of to_split */
		r = prefs[i].num;
		if (!rbitset_is_set(normal_regs, r))
			continue;
		if (rbitset_is_set(forbidden_regs, r))
			continue;
		if (r == from_r)
			continue;

		/* is the split worth it? */
		delta = pref_delta + prefs[i].pref;
		if (delta < split_threshold) {
			DB((dbg, LEVEL_3, "Not doing optimistical split of %+F (depth %d), win %f too low\n",
			    to_split, recursion, delta));
			return false;
		}

		/* if the register is free then we can do the split */
		if (assignments[r] == NULL)
			break;

		/* otherwise we might try recursively calling optimistic_split */
		if (recursion+1 > MAX_OPTIMISTIC_SPLIT_RECURSION)
			continue;

		float apref        = prefs[i].pref;
		float apref_delta  = i+1 < n_regs ? apref - prefs[i+1].pref : 0;
		apref_delta += pref_delta - split_threshold;

		/* our source register isn't a useful destination for recursive
		   splits */
		bool old_source_state = rbitset_is_set(forbidden_regs, from_r);
		rbitset_set(forbidden_regs, from_r);
		/* try recursive split */
		bool res = try_optimistic_split(assignments[r], before, apref,
		                              apref_delta, forbidden_regs, recursion+1);
		/* restore our destination */
		if (old_source_state) {
			rbitset_set(forbidden_regs, from_r);
		} else {
			rbitset_clear(forbidden_regs, from_r);
		}

		if (res)
			break;
	}
	if (i >= n_regs)
		return false;

	ir_node *const copy  = be_new_Copy(block, to_split);
	unsigned const width = 1;
	mark_as_copy_of(copy, to_split);
	/* hacky, but correct here */
	if (assignments[from_reg->index] == to_split)
		free_reg_of_value(to_split);
	use_reg_idx(copy, r, width);
	sched_add_before(before, copy);

	DB((dbg, LEVEL_3,
	    "Optimistic live-range split %+F move %+F(%s) -> %s before %+F (win %f, depth %d)\n",
	    copy, to_split, from_reg->name, arch_get_irn_register(copy)->name, before, delta, recursion));
	return true;
}

/**
 * Determine and assign a register for node @p node
 */
static void assign_reg(ir_node const *const block, ir_node *const node, arch_register_req_t const *const req, unsigned *const forbidden_regs)
{
	assert(!is_Phi(node));
	/* preassigned register? */
	arch_register_t const *const final_reg = arch_get_irn_register(node);
	unsigned               const width     = req->width;
	if (final_reg != NULL) {
		DB((dbg, LEVEL_2, "Preassignment %+F -> %s\n", node, final_reg->name));
		use_reg(node, final_reg, width);
		return;
	}

	/* ignore reqs must be preassigned */
	assert(!req->ignore);

	/* give should_be_same boni */
	allocation_info_t *info    = get_allocation_info(node);
	ir_node           *in_node = skip_Proj(node);
	if (req->should_be_same != 0) {
		float weight = (float)get_block_execfreq(block);

		assert(get_irn_arity(in_node) <= (int)sizeof(req->should_be_same) * 8);
		foreach_irn_in(in_node, i, in) {
			if (!rbitset_is_set(&req->should_be_same, i))
				continue;

			const arch_register_t *reg       = arch_get_irn_register(in);
			unsigned               reg_index = reg->index;

			/* if the value didn't die here then we should not propagate the
			 * should_be_same info */
			if (assignments[reg_index] == in)
				continue;

			info->prefs[reg_index] += weight * AFF_SHOULD_BE_SAME;
		}
	}

	/* create list of register candidates and sort by their preference */
	DB((dbg, LEVEL_2, "Candidates for %+F:", node));
	reg_pref_t *reg_prefs = ALLOCAN(reg_pref_t, n_regs);
	fill_sort_candidates(reg_prefs, info);
	for (unsigned r = 0; r < n_regs; ++r) {
		unsigned num = reg_prefs[r].num;
		if (!rbitset_is_set(normal_regs, num))
			continue;
		DEBUG_ONLY(const arch_register_t *reg = arch_register_for_index(cls, num);)
		DB((dbg, LEVEL_2, " %s(%f)", reg->name, reg_prefs[r].pref));
	}
	DB((dbg, LEVEL_2, "\n"));

	const unsigned *allowed_regs = normal_regs;
	if (req->limited != NULL) {
		allowed_regs = req->limited;
	}

	unsigned final_reg_index = 0;
	unsigned r;
	for (r = 0; r < n_regs; ++r) {
		final_reg_index = reg_prefs[r].num;
		if (!rbitset_is_set(allowed_regs, final_reg_index))
			continue;
		/* alignment constraint? */
		if (width > 1) {
			if (final_reg_index % width != 0)
				continue;
			bool fine = true;
			for (unsigned r0 = r+1; r0 < r+width; ++r0) {
				if (assignments[r0] != NULL)
					fine = false;
			}
			/* TODO: attempt optimistic split here */
			if (!fine)
				continue;
		}

		if (assignments[final_reg_index] == NULL)
			break;
		float    pref   = reg_prefs[r].pref;
		float    delta  = r+1 < n_regs ? pref - reg_prefs[r+1].pref : 0;
		ir_node *before = skip_Proj(node);
		bool     res
			= try_optimistic_split(assignments[final_reg_index], before, pref,
			                       delta, forbidden_regs, 0);
		if (res)
			break;
	}
	if (r >= n_regs) {
		/* the common reason to hit this panic is when 1 of your nodes is not
		 * register pressure faithful */
		panic("no register left for %+F", node);
	}

	use_reg_idx(node, final_reg_index, width);
	DB((dbg, LEVEL_2, "Assign %+F -> %s\n", node, arch_get_irn_register(node)->name));
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
 * We ignore all fulfilled permuations (like 7->7)
 * In a first pass we create as much copy instructions as possible as they
 * are generally cheaper than exchanges. We do this by counting into how many
 * destinations a register has to be copied (in the example it's 2 for register
 * 3, or 1 for the registers 1,2,4 and 7).
 * We can then create a copy into every destination register when the usecount
 * of that register is 0 (= noone else needs the value in the register).
 *
 * After this step we should only have cycles left. We implement a cyclic
 * permutation of n registers with n-1 transpositions.
 *
 * @param live_nodes   the set of live nodes, updated due to live range split
 * @param before       the node before we add the permutation
 * @param permutation  the permutation array indices are the destination
 *                     registers, the values in the array are the source
 *                     registers.
 */
static void permute_values(ir_nodeset_t *live_nodes, ir_node *before,
                           unsigned *permutation)
{
	unsigned *n_used = ALLOCANZ(unsigned, n_regs);

	/* determine how often each source register needs to be read */
	for (unsigned r = 0; r < n_regs; ++r) {
		unsigned  old_reg = permutation[r];
		ir_node  *value;

		value = assignments[old_reg];
		if (value == NULL) {
			/* nothing to do here, reg is not live. Mark it as fixpoint
			 * so we ignore it in the next steps */
			permutation[r] = r;
			continue;
		}

		++n_used[old_reg];
	}

	ir_node *block = get_nodes_block(before);

	/* step1: create copies where immediately possible */
	for (unsigned r = 0; r < n_regs; /* empty */) {
		unsigned old_r = permutation[r];

		/* - no need to do anything for fixed points.
		   - we can't copy if the value in the dest reg is still needed */
		if (old_r == r || n_used[r] > 0) {
			++r;
			continue;
		}

		/* create a copy */
		ir_node *src  = assignments[old_r];
		ir_node *copy = be_new_Copy(block, src);
		sched_add_before(before, copy);
		mark_as_copy_of(copy, src);
		unsigned width = 1; /* TODO */
		use_reg_idx(copy, r, width);
		DB((dbg, LEVEL_2, "Copy %+F (from %+F, before %+F) -> %s\n", copy, src, before, arch_get_irn_register(copy)->name));

		if (live_nodes != NULL) {
			ir_nodeset_insert(live_nodes, copy);
		}

		/* old register has 1 user less, permutation is resolved */
		assert(arch_get_irn_register(src)->index == old_r);
		permutation[r] = r;

		assert(n_used[old_r] > 0);
		--n_used[old_r];
		if (n_used[old_r] == 0) {
			if (live_nodes != NULL) {
				ir_nodeset_remove(live_nodes, src);
			}
			free_reg_of_value(src);
		}

		/* advance or jump back (if this copy enabled another copy) */
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
	 *  copies are preferable even for 2-cycles) */

	/* create perms with the rest */
	for (unsigned r = 0; r < n_regs; /* empty */) {
		unsigned old_r = permutation[r];

		if (old_r == r) {
			++r;
			continue;
		}

		/* we shouldn't have copies from 1 value to multiple destinations left*/
		assert(n_used[old_r] == 1);

		/* exchange old_r and r2; after that old_r is a fixed point */
		unsigned r2 = permutation[old_r];

		ir_node *const in[] = { assignments[r2], assignments[old_r] };
		ir_node *const perm = be_new_Perm(block, ARRAY_SIZE(in), in);
		sched_add_before(before, perm);
		DB((dbg, LEVEL_2, "Perm %+F (perm %+F,%+F, before %+F)\n",
		    perm, in[0], in[1], before));

		unsigned width = 1; /* TODO */

		ir_node *const proj0 = be_new_Proj(perm, 0);
		mark_as_copy_of(proj0, in[0]);
		use_reg_idx(proj0, old_r, width);

		ir_node *const proj1 = be_new_Proj(perm, 1);
		mark_as_copy_of(proj1, in[1]);
		use_reg_idx(proj1, r2, width);

		/* 1 value is now in the correct register */
		permutation[old_r] = old_r;
		/* the source of r changed to r2 */
		permutation[r] = r2;

		/* if we have reached a fixpoint update data structures */
		if (live_nodes != NULL) {
			ir_nodeset_remove(live_nodes, in[0]);
			ir_nodeset_remove(live_nodes, in[1]);
			ir_nodeset_remove(live_nodes, proj0);
			ir_nodeset_insert(live_nodes, proj1);
		}
	}

#ifndef NDEBUG
	/* now we should only have fixpoints left */
	for (unsigned r = 0; r < n_regs; ++r) {
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
	allocation_info_t *info      = get_allocation_info(node);
	const unsigned    *last_uses = info->last_uses;

	foreach_irn_in(node, i, op) {
		/* check if one operand is the last use */
		if (!rbitset_is_set(last_uses, i))
			continue;

		free_reg_of_value(op);
		ir_nodeset_remove(live_nodes, op);
	}
}

/**
 * change inputs of a node to the current value (copies/perms)
 */
static void rewire_inputs(ir_node *node)
{
	foreach_irn_in(node, i, op) {
		allocation_info_t *info = try_get_allocation_info(op);

		if (info == NULL)
			continue;

		info = get_allocation_info(info->original_value);
		if (info->current_value != op) {
			set_irn_n(node, i, info->current_value);
		}
	}
}

/**
 * Create a bitset of registers occupied with value living through an
 * instruction
 */
static void determine_live_through_regs(unsigned *bitset, ir_node *node)
{
	const allocation_info_t *info = get_allocation_info(node);

	/* mark all used registers as potentially live-through */
	for (unsigned r = 0; r < n_regs; ++r) {
		if (assignments[r] == NULL)
			continue;
		if (!rbitset_is_set(normal_regs, r))
			continue;

		rbitset_set(bitset, r);
	}

	/* remove registers of value dying at the instruction */
	foreach_irn_in(node, i, op) {
		if (!rbitset_is_set(info->last_uses, i))
			continue;

		const arch_register_t *reg = arch_get_irn_register(op);
		rbitset_clear(bitset, reg->index);
	}
}

static void solve_lpp(ir_nodeset_t *live_nodes, ir_node *node,
                      unsigned *forbidden_regs, unsigned *live_through_regs)
{
	unsigned *forbidden_edges = rbitset_malloc(n_regs * n_regs);
	int      *lpp_vars        = XMALLOCNZ(int, n_regs*n_regs);

	lpp_t *lpp = lpp_new("prefalloc", lpp_minimize);
	//lpp_set_time_limit(lpp, 20);
	lpp_set_log(lpp, stdout);

	/** mark some edges as forbidden */
	be_foreach_use(node, cls, req, op, op_req,
		if (req->limited == NULL)
			continue;

		const unsigned        *limited     = req->limited;
		const arch_register_t *reg         = arch_get_irn_register(op);
		unsigned               current_reg = reg->index;
		for (unsigned r = 0; r < n_regs; ++r) {
			if (rbitset_is_set(limited, r))
				continue;

			rbitset_set(forbidden_edges, current_reg*n_regs + r);
		}
	);

	/* add all combinations, except for not allowed ones */
	for (unsigned l = 0; l < n_regs; ++l) {
		if (!rbitset_is_set(normal_regs, l)) {
			char name[15];
			snprintf(name, sizeof(name), "%u_to_%u", l, l);
			lpp_vars[l*n_regs+l] = lpp_add_var(lpp, name, lpp_binary, 1);
			continue;
		}

		for (unsigned r = 0; r < n_regs; ++r) {
			if (!rbitset_is_set(normal_regs, r))
				continue;
			if (rbitset_is_set(forbidden_edges, l*n_regs + r))
				continue;
			/* livethrough values may not use constrained output registers */
			if (rbitset_is_set(live_through_regs, l)
			    && rbitset_is_set(forbidden_regs, r))
				continue;

			char name[15];
			snprintf(name, sizeof(name), "%u_to_%u", l, r);

			double costs = l==r ? 9 : 8;
			lpp_vars[l*n_regs+r]
				= lpp_add_var(lpp, name, lpp_binary, costs);
			assert(lpp_vars[l*n_regs+r] > 0);
		}
	}
	free(forbidden_edges);

	/* add constraints */
	for (unsigned l = 0; l < n_regs; ++l) {
		/* only 1 destination per register */
		int constraint = -1;
		for (unsigned r = 0; r < n_regs; ++r) {
			int var = lpp_vars[l*n_regs+r];
			if (var == 0)
				continue;
			if (constraint < 0) {
				char name[64];
				snprintf(name, sizeof(name), "%u_to_dest", l);
				constraint = lpp_add_cst(lpp, name, lpp_equal, 1);
			}
			lpp_set_factor_fast(lpp, constraint, var, 1);
		}
		/* each destination used by at most 1 value */
		constraint = -1;
		for (unsigned r = 0; r < n_regs; ++r) {
			int var = lpp_vars[r*n_regs+l];
			if (var == 0)
				continue;
			if (constraint < 0) {
				char name[64];
				snprintf(name, sizeof(name), "one_to_%u", l);
				constraint = lpp_add_cst(lpp, name, lpp_less_equal, 1);
			}
			lpp_set_factor_fast(lpp, constraint, var, 1);
		}
	}

	FILE *out = fopen("lppdump.txt", "w");
	if (out == NULL)
		panic("couldn't open lppdump.txt");
	lpp_dump_plain(lpp, out);
	fclose(out);

	/* solve lpp */
	lpp_solve(lpp, be_options.ilp_solver);
	if (!lpp_is_sol_valid(lpp))
		panic("ilp solution not valid");

	unsigned *assignment = ALLOCAN(unsigned, n_regs);
	for (unsigned l = 0; l < n_regs; ++l) {
		unsigned dest_reg = (unsigned)-1;
		for (unsigned r = 0; r < n_regs; ++r) {
			int var = lpp_vars[l*n_regs+r];
			if (var == 0)
				continue;
			double val = lpp_get_var_sol(lpp, var);
			if (val == 1) {
				assert(dest_reg == (unsigned)-1);
				dest_reg = r;
			}
		}
		assert(dest_reg != (unsigned)-1);
		assignment[dest_reg] = l;
	}
	free(lpp_vars);

	fprintf(stderr, "Assignment: ");
	for (unsigned l = 0; l < n_regs; ++l) {
		fprintf(stderr, "%u ", assignment[l]);
	}
	fprintf(stderr, "\n");
	fflush(stdout);
	permute_values(live_nodes, node, assignment);
	lpp_free(lpp);
}

static bool is_aligned(unsigned num, unsigned alignment)
{
	unsigned mask = alignment-1;
	assert(is_po2_or_zero(alignment));
	return (num&mask) == 0;
}

/**
 * Enforce constraints at a node by live range splits.
 *
 * @param  live_nodes  the set of live nodes, might be changed
 * @param  node        the current node
 */
static void enforce_constraints(ir_nodeset_t *live_nodes, ir_node *node,
                                unsigned *forbidden_regs)
{
	/* see if any use constraints are not met and whether double-width
	 * values are involved */
	bool double_width = false;
	bool good = true;
	be_foreach_use(node, cls, req, op, op_req,
		/* are there any limitations for the i'th operand? */
		const arch_register_t *reg       = arch_get_irn_register(op);
		unsigned               reg_index = reg->index;
		if (req->width != 1) {
			double_width = true;
			if (!is_aligned(reg_index, req->width)) {
				good = false;
				continue;
			}
		}
		if (req->limited == NULL)
			continue;

		const unsigned *limited = req->limited;
		if (!rbitset_is_set(limited, reg_index)) {
			/* found an assignment outside the limited set */
			good = false;
			continue;
		}
	);

	/* is any of the live-throughs using a constrained output register? */
	unsigned *live_through_regs = NULL;
	be_foreach_definition(node, cls, value, req,
		(void)value;
		if (req->width > 1)
			double_width = true;
		if (req->limited == NULL)
			continue;
		if (live_through_regs == NULL) {
			live_through_regs = rbitset_alloca(n_regs);
			determine_live_through_regs(live_through_regs, node);
		}
		rbitset_or(forbidden_regs, req->limited, n_regs);
		if (rbitsets_have_common(req->limited, live_through_regs, n_regs))
			good = false;
	);

	if (good)
		return;

	/* create these arrays if we haven't yet */
	if (live_through_regs == NULL) {
		live_through_regs = rbitset_alloca(n_regs);
	}

	if (double_width) {
		/* only the ILP variant can solve this yet */
		solve_lpp(live_nodes, node, forbidden_regs, live_through_regs);
		return;
	}

	/* at this point we have to construct a bipartite matching problem to see
	 * which values should go to which registers
	 * Note: We're building the matrix in "reverse" - source registers are
	 *       right, destinations left because this will produce the solution
	 *       in the format required for permute_values.
	 */
	hungarian_problem_t *bp
		= hungarian_new(n_regs, n_regs, HUNGARIAN_MATCH_PERFECT);

	/* add all combinations, then remove not allowed ones */
	for (unsigned l = 0; l < n_regs; ++l) {
		if (!rbitset_is_set(normal_regs, l)) {
			hungarian_add(bp, l, l, 1);
			continue;
		}

		for (unsigned r = 0; r < n_regs; ++r) {
			if (!rbitset_is_set(normal_regs, r))
				continue;
			/* livethrough values may not use constrainted output registers */
			if (rbitset_is_set(live_through_regs, l)
			    && rbitset_is_set(forbidden_regs, r))
				continue;

			hungarian_add(bp, r, l, l == r ? 9 : 8);
		}
	}

	be_foreach_use(node, cls, req, op, op_req,
		if (req->limited == NULL)
			continue;

		const unsigned        *limited     = req->limited;
		const arch_register_t *reg         = arch_get_irn_register(op);
		unsigned               current_reg = reg->index;
		for (unsigned r = 0; r < n_regs; ++r) {
			if (rbitset_is_set(limited, r))
				continue;
			hungarian_remove(bp, r, current_reg);
		}
	);

	//hungarian_print_cost_matrix(bp, 1);
	hungarian_prepare_cost_matrix(bp, HUNGARIAN_MODE_MAXIMIZE_UTIL);

	unsigned *assignment = ALLOCAN(unsigned, n_regs);
	int       res        = hungarian_solve(bp, assignment, NULL, 0);
	(void)res;
	assert(res == 0);

	hungarian_free(bp);

	permute_values(live_nodes, node, assignment);
}

/** test whether a node @p n is a copy of the value of node @p of */
static bool is_copy_of(ir_node *value, ir_node *test_value)
{
	if (value == test_value)
		return true;

	allocation_info_t *info      = get_allocation_info(value);
	allocation_info_t *test_info = get_allocation_info(test_value);
	return test_info->original_value == info->original_value;
}

/**
 * find a value in the end-assignment of a basic block
 * @returns the index into the assignment array if found
 *          -1 if not found
 */
static int find_value_in_block_info(block_info_t *info, ir_node *value)
{
	ir_node  **end_assignments = info->assignments;
	for (unsigned r = 0; r < n_regs; ++r) {
		ir_node *a_value = end_assignments[r];

		if (a_value == NULL)
			continue;
		if (is_copy_of(a_value, value))
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
	ir_node      *pred      = get_Block_cfgpred_block(block, p);
	block_info_t *pred_info = get_block_info(pred);

	/* predecessor not processed yet? nothing to do */
	if (!pred_info->processed)
		return;

	unsigned *permutation = ALLOCAN(unsigned, n_regs);
	for (unsigned r = 0; r < n_regs; ++r) {
		permutation[r] = r;
	}

	/* check phi nodes */
	bool need_permutation = false;
	sched_foreach_phi(block, phi) {
		if (!arch_irn_consider_in_reg_alloc(cls, phi))
			continue;

		ir_node *phi_pred = get_Phi_pred(phi, p);
		int      a        = find_value_in_block_info(pred_info, phi_pred);
		assert(a >= 0);

		const arch_register_t *reg  = arch_get_irn_register(phi);
		int                    regn = reg->index;
		/* same register? nothing to do */
		if (regn == a)
			continue;

		ir_node               *op     = pred_info->assignments[a];
		const arch_register_t *op_reg = arch_get_irn_register(op);
		/* Virtual registers are ok, too. */
		if (op_reg->is_virtual)
			continue;

		permutation[regn] = a;
		need_permutation  = true;
	}

	if (need_permutation) {
		/* permute values at end of predecessor */
		ir_node **old_assignments = assignments;
		assignments     = pred_info->assignments;
		permute_values(NULL, be_get_end_of_block_insertion_point(pred),
		               permutation);
		assignments = old_assignments;
	}

	/* change phi nodes to use the copied values */
	sched_foreach_phi(block, phi) {
		if (!arch_irn_consider_in_reg_alloc(cls, phi))
			continue;

		/* we have permuted all values into the correct registers so we can
		   simply query which value occupies the phis register in the
		   predecessor */
		int      a  = arch_get_irn_register(phi)->index;
		ir_node *op = pred_info->assignments[a];
		set_Phi_pred(phi, p, op);
	}
}

/**
 * Set preferences for a phis register based on the registers used on the
 * phi inputs.
 */
static void adapt_phi_prefs(ir_node *phi)
{
	ir_node           *block = get_nodes_block(phi);
	allocation_info_t *info  = get_allocation_info(phi);

	foreach_irn_in(phi, i, op) {
		const arch_register_t *reg = arch_get_irn_register(op);

		if (reg == NULL)
			continue;
		/* we only give the bonus if the predecessor already has registers
		 * assigned, otherwise we only see a dummy value
		 * and any conclusions about its register are useless */
		ir_node      *pred_block      = get_Block_cfgpred_block(block, i);
		block_info_t *pred_block_info = get_block_info(pred_block);
		if (!pred_block_info->processed)
			continue;

		/* give bonus for already assigned register */
		float weight = (float)get_block_execfreq(pred_block);
		info->prefs[reg->index] += weight * AFF_PHI;
	}
}

/**
 * After a phi has been assigned a register propagate preference inputs
 * to the phi inputs.
 */
static void propagate_phi_register(ir_node *phi, unsigned assigned_r)
{
	ir_node *block = get_nodes_block(phi);

	int arity = get_irn_arity(phi);
	for (int i = 0; i < arity; ++i) {
		ir_node           *op         = get_Phi_pred(phi, i);
		allocation_info_t *info       = get_allocation_info(op);
		ir_node           *pred_block = get_Block_cfgpred_block(block, i);
		float              weight
			= (float)get_block_execfreq(pred_block) * AFF_PHI;

		if (info->prefs[assigned_r] >= weight)
			continue;

		/* promote the prefered register */
		for (unsigned r = 0; r < n_regs; ++r) {
			if (info->prefs[r] > -weight) {
				info->prefs[r] = -weight;
			}
		}
		info->prefs[assigned_r] = weight;

		if (is_Phi(op))
			propagate_phi_register(op, assigned_r);
	}
}

static void assign_phi_registers(ir_node *block)
{
	/* count phi nodes */
	int n_phis = 0;
	sched_foreach_phi(block, node) {
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;
		++n_phis;
	}

	if (n_phis == 0)
		return;

	/* build a bipartite matching problem for all phi nodes */
	hungarian_problem_t *bp
		= hungarian_new(n_phis, n_regs, HUNGARIAN_MATCH_PERFECT);
	int n = 0;
	sched_foreach_phi(block, node) {
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		/* give boni for predecessor colorings */
		adapt_phi_prefs(node);
		/* add stuff to bipartite problem */
		allocation_info_t *info = get_allocation_info(node);
		DB((dbg, LEVEL_3, "Prefs for %+F: ", node));
		for (unsigned r = 0; r < n_regs; ++r) {
			if (!rbitset_is_set(normal_regs, r))
				continue;

			float costs = info->prefs[r];
			costs = costs < 0 ? -logf(-costs+1) : logf(costs+1);
			costs *= 100;
			costs += 10000;
			hungarian_add(bp, n, r, (int)costs);
			DB((dbg, LEVEL_3, " %s(%f)", arch_register_for_index(cls, r)->name,
			    info->prefs[r]));
		}
		DB((dbg, LEVEL_3, "\n"));
		++n;
	}

	//hungarian_print_cost_matrix(bp, 7);
	hungarian_prepare_cost_matrix(bp, HUNGARIAN_MODE_MAXIMIZE_UTIL);

	unsigned *assignment = ALLOCAN(unsigned, n_regs);
	int       res        = hungarian_solve(bp, assignment, NULL, 0);
	(void)res;
	assert(res == 0);

	/* apply results */
	n = 0;
	sched_foreach_phi(block, node) {
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;
		const arch_register_req_t *req
			= arch_get_irn_register_req(node);

		unsigned r = assignment[n++];
		assert(rbitset_is_set(normal_regs, r));
		use_reg_idx(node, r, req->width);
		DB((dbg, LEVEL_2, "Assign %+F -> %s\n", node, arch_get_irn_register(node)->name));

		/* adapt preferences for phi inputs */
		propagate_phi_register(node, r);
	}
}

/**
 * Walker: assign registers to all nodes of a block that
 * need registers from the currently considered register class.
 */
static void allocate_coalesce_block(ir_node *block, void *data)
{
	(void) data;
	DB((dbg, LEVEL_2, "* Block %+F\n", block));

	/* clear assignments */
	block_info_t *block_info  = get_block_info(block);
	assignments = block_info->assignments;

	ir_nodeset_t live_nodes;
	ir_nodeset_init(&live_nodes);

	/* gather regalloc infos of predecessor blocks */
	int            n_preds          = get_Block_n_cfgpreds(block);
	block_info_t **pred_block_infos = ALLOCAN(block_info_t*, n_preds);
	for (int i = 0; i < n_preds; ++i) {
		ir_node      *pred      = get_Block_cfgpred_block(block, i);
		block_info_t *pred_info = get_block_info(pred);
		pred_block_infos[i]     = pred_info;
	}

	ir_node **phi_ins = ALLOCAN(ir_node*, n_preds);

	/* collect live-in nodes and preassigned values */
	be_lv_foreach(lv, block, be_lv_state_in, node) {
		const arch_register_req_t *req = arch_get_irn_register_req(node);
		if (req->cls != cls)
			continue;

		if (req->ignore) {
			allocation_info_t *info = get_allocation_info(node);
			info->current_value = node;

			const arch_register_t *reg = arch_get_irn_register(node);
			assert(reg != NULL); /* ignore values must be preassigned */
			use_reg(node, reg, req->width);
			continue;
		}

		/* check all predecessors for this value, if it is not everywhere the
		   same or unknown then we have to construct a phi
		   (we collect the potential phi inputs here) */
		bool need_phi = false;
		for (int p = 0; p < n_preds; ++p) {
			block_info_t *pred_info = pred_block_infos[p];

			if (!pred_info->processed) {
				/* use node for now, it will get fixed later */
				phi_ins[p] = node;
				need_phi   = true;
			} else {
				int a = find_value_in_block_info(pred_info, node);

				/* must live out of predecessor */
				assert(a >= 0);
				phi_ins[p] = pred_info->assignments[a];
				/* different value from last time? then we need a phi */
				if (p > 0 && phi_ins[p-1] != phi_ins[p]) {
					need_phi = true;
				}
			}
		}

		if (need_phi) {
			const arch_register_req_t *phi_req = cls->class_req;
			if (req->width > 1)
				phi_req = be_create_cls_req(irg, cls, req->width);
			ir_node *const phi = be_new_Phi(block, n_preds, phi_ins, phi_req);

			DB((dbg, LEVEL_3, "Create Phi %+F (for %+F) -", phi, node));
#ifdef DEBUG_libfirm
			for (int pi = 0; pi < n_preds; ++pi) {
				DB((dbg, LEVEL_3, " %+F", phi_ins[pi]));
			}
			DB((dbg, LEVEL_3, "\n"));
#endif
			mark_as_copy_of(phi, node);
			sched_add_after(block, phi);

			node = phi;
		} else {
			allocation_info_t *info = get_allocation_info(node);
			info->current_value = phi_ins[0];

			/* Grab 1 of the inputs we constructed (might not be the same as
			 * "node" as we could see the same copy of the value in all
			 * predecessors */
			node = phi_ins[0];
		}

		/* if the node already has a register assigned use it */
		const arch_register_t *reg = arch_get_irn_register(node);
		if (reg != NULL) {
			use_reg(node, reg, req->width);
		}

		/* remember that this node is live at the beginning of the block */
		ir_nodeset_insert(&live_nodes, node);
	}

	/** Collects registers which must not be used for optimistic splits. */
	unsigned *const forbidden_regs = rbitset_alloca(n_regs);

	/* handle phis... */
	assign_phi_registers(block);

	/* all live-ins must have a register */
#ifndef NDEBUG
	foreach_ir_nodeset(&live_nodes, node, iter) {
		const arch_register_t *reg = arch_get_irn_register(node);
		assert(reg != NULL);
	}
#endif

	/* assign instructions in the block, phis are already assigned */
	sched_foreach_non_phi(block, node) {
		rewire_inputs(node);

		/* enforce use constraints */
		rbitset_clear_all(forbidden_regs, n_regs);
		enforce_constraints(&live_nodes, node, forbidden_regs);

		rewire_inputs(node);

		/* we may not use registers used for inputs for optimistic splits */
		be_foreach_use(node, cls, in_req, op, op_req,
			const arch_register_t *reg = arch_get_irn_register(op);
			rbitset_set(forbidden_regs, reg->index);
		);

		/* free registers of values last used at this instruction */
		free_last_uses(&live_nodes, node);

		/* assign output registers */
		be_foreach_definition_(node, cls, value, req,
			assign_reg(block, value, req, forbidden_regs);
		);
	}

	ir_nodeset_destroy(&live_nodes);
	assignments = NULL;

	block_info->processed = true;

	/* permute values at end of predecessor blocks in case of phi-nodes */
	if (n_preds > 1) {
		for (int p = 0; p < n_preds; ++p) {
			add_phi_permutations(block, p);
		}
	}

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

typedef struct block_costs_t block_costs_t;
struct block_costs_t {
	float costs;   /**< costs of the block */
	int   dfs_num; /**< depth first search number (to detect backedges) */
};

static int cmp_block_costs(const void *d1, const void *d2)
{
	const ir_node       * const *block1 = (const ir_node**)d1;
	const ir_node       * const *block2 = (const ir_node**)d2;
	const block_costs_t *info1  = (const block_costs_t*)get_irn_link(*block1);
	const block_costs_t *info2  = (const block_costs_t*)get_irn_link(*block2);
	return QSORT_CMP(info2->costs, info1->costs);
}

static void determine_block_order(void)
{
	ir_node **blocklist = be_get_cfgpostorder(irg);
	size_t    n_blocks  = ARR_LEN(blocklist);
	int       dfs_num   = 0;
	ir_node **order     = XMALLOCN(ir_node*, n_blocks);
	size_t    order_p   = 0;
	deq_t     worklist;
	deq_init(&worklist);

	/* clear block links... */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	for (size_t p = 0; p < n_blocks; ++p) {
		ir_node *block = blocklist[p];
		set_irn_link(block, NULL);
	}

	/* walk blocks in reverse postorder, the costs for each block are the
	 * sum of the costs of its predecessors (excluding the costs on backedges
	 * which we can't determine) */
	for (size_t p = n_blocks; p > 0;) {
		block_costs_t *cost_info;
		ir_node *block = blocklist[--p];

		float execfreq   = (float)get_block_execfreq(block);
		float costs      = execfreq;
		int   n_cfgpreds = get_Block_n_cfgpreds(block);
		for (int p2 = 0; p2 < n_cfgpreds; ++p2) {
			ir_node       *pred_block = get_Block_cfgpred_block(block, p2);
			block_costs_t *pred_costs = (block_costs_t*)get_irn_link(pred_block);
			/* we don't have any info for backedges */
			if (pred_costs == NULL)
				continue;
			costs += pred_costs->costs;
		}

		cost_info          = OALLOCZ(&obst, block_costs_t);
		cost_info->costs   = costs;
		cost_info->dfs_num = dfs_num++;
		set_irn_link(block, cost_info);
	}

	/* sort array by block costs */
	QSORT_ARR(blocklist, cmp_block_costs);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);

	for (size_t p = 0; p < n_blocks; ++p) {
		ir_node *block = blocklist[p];
		if (Block_block_visited(block))
			continue;

		/* continually add predecessors with highest costs to worklist
		 * (without using backedges) */
		do {
			block_costs_t *info       = (block_costs_t*)get_irn_link(block);
			ir_node       *best_pred  = NULL;
			float          best_costs = -1;
			int            n_cfgpred  = get_Block_n_cfgpreds(block);

			deq_push_pointer_right(&worklist, block);
			mark_Block_block_visited(block);
			for (int i = 0; i < n_cfgpred; ++i) {
				ir_node       *pred_block = get_Block_cfgpred_block(block, i);
				block_costs_t *pred_info  = (block_costs_t*)get_irn_link(pred_block);

				/* ignore backedges */
				if (pred_info->dfs_num > info->dfs_num)
					continue;

				if (info->costs > best_costs) {
					best_costs = info->costs;
					best_pred  = pred_block;
				}
			}
			block = best_pred;
		} while (block != NULL && !Block_block_visited(block));

		/* now put all nodes in the worklist in our final order */
		while (!deq_empty(&worklist)) {
			ir_node *pblock = deq_pop_pointer_right(ir_node, &worklist);
			assert(order_p < n_blocks);
			order[order_p++] = pblock;
		}
	}
	assert(order_p == n_blocks);
	deq_free(&worklist);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_BLOCK_VISITED);

	DEL_ARR_F(blocklist);

	obstack_free(&obst, NULL);
	obstack_init(&obst);

	block_order   = order;
	n_block_order = n_blocks;
}

static void free_block_order(void)
{
	free(block_order);
}

/**
 * Run the register allocator for the current register class.
 */
static void be_pref_alloc_cls(void)
{
	be_assure_live_sets(irg);
	lv = be_get_irg_liveness(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	DB((dbg, LEVEL_2, "=== Allocating registers of %s ===\n", cls->name));

	irg_walk_graph(irg, firm_clear_link, NULL, NULL);

	irg_block_walk_graph(irg, NULL, analyze_block, NULL);
	combine_congruence_classes();

	for (size_t i = 0; i < n_block_order; ++i) {
		ir_node *block = block_order[i];
		allocate_coalesce_block(block, NULL);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

/**
 * Run the spiller on the current graph.
 */
static void spill(const regalloc_if_t *regif)
{
	/* spill */
	be_timer_push(T_RA_SPILL);
	be_do_spill(irg, cls, regif);
	be_timer_pop(T_RA_SPILL);

	be_timer_push(T_RA_SPILL_APPLY);
	check_for_memory_operands(irg, regif);
	be_timer_pop(T_RA_SPILL_APPLY);

	be_dump(DUMP_RA, irg, "spill");
}

/**
 * The pref register allocator for a whole procedure.
 */
static void be_pref_alloc(ir_graph *new_irg, const regalloc_if_t *regif)
{
	/* disable optimization callbacks as we cannot deal with same-input phis
	 * getting optimized away. */
	int last_opt_state = get_optimize();
	set_optimize(0);

	irg = new_irg;
	obstack_init(&obst);

	be_spill_prepare_for_constraints(irg);

	/* determine a good coloring order */
	determine_block_order();

	arch_register_class_t const *const reg_classes
		= ir_target.isa->register_classes;
	for (int c = 0, n_cls = ir_target.isa->n_register_classes; c < n_cls; ++c) {
		cls = &reg_classes[c];
		if (cls->manual_ra)
			continue;

		stat_ev_ctx_push_str("regcls", cls->name);

		n_regs      = cls->n_regs;
		normal_regs = rbitset_malloc(n_regs);
		be_get_allocatable_regs(irg, cls, normal_regs);

		spill(regif);

		/* verify schedule and register pressure */
		if (be_options.do_verify) {
			be_timer_push(T_VERIFY);
			bool check_schedule = be_verify_schedule(irg);
			be_check_verify_result(check_schedule, irg);
			bool check_pressure = be_verify_register_pressure(irg, cls);
			be_check_verify_result(check_pressure, irg);
			be_timer_pop(T_VERIFY);
		}

		be_timer_push(T_RA_COLOR);
		be_pref_alloc_cls();
		be_timer_pop(T_RA_COLOR);

		/* we most probably constructed new Phis so liveness info is invalid
		 * now */
		be_invalidate_live_sets(irg);
		free(normal_regs);

		stat_ev_ctx_pop("regcls");
	}

	free_block_order();
	obstack_free(&obst, NULL);

	set_optimize(last_opt_state);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_pref_alloc)
void be_init_pref_alloc(void)
{
	be_register_allocator("pref", be_pref_alloc);
	FIRM_DBG_REGISTER(dbg, "firm.be.prefalloc");
}
