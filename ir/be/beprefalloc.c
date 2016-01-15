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
#include "config.h"

#include <float.h>
#include <stdbool.h>
#include <math.h>
#include "lpp.h"

#include "error.h"
#include "execfreq.h"
#include "ircons.h"
#include "irdom.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "irdump.h"
#include "irtools.h"
#include "util.h"
#include "obst.h"
#include "raw_bitset.h"
#include "unionfind.h"
#include "pdeq.h"
#include "hungarian.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"

#include "beabi.h"
#include "bechordal_t.h"
#include "be.h"
#include "beirg.h"
#include "belive_t.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "besched.h"
#include "bespill.h"
#include "bespillutil.h"
#include "beverify.h"
#include "beutil.h"
#include "bestack.h"

#define USE_FACTOR                     1.0f
#define DEF_FACTOR                     1.0f
#define NEIGHBOR_FACTOR                0.2f
#define AFF_SHOULD_BE_SAME             0.5f
#define AFF_PHI                        1.0f
#define SPLIT_DELTA                    1.0f
#define MAX_OPTIMISTIC_SPLIT_RECURSION 0

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)
DEBUG_ONLY(static firm_dbg_module_t *dbg_icore = NULL;)

static struct obstack               obst;
static ir_graph                    *irg;
static const arch_register_class_t *cls;
static be_lv_t                     *lv;
static unsigned                     n_regs;
static unsigned                    *normal_regs;
static int                         *congruence_classes;
static ir_node                    **block_order;
static size_t                       n_block_order;
static int                          create_preferences        = true;
static int                          create_congruence_classes = true;
static int                          propagate_phi_registers   = true;
static int                          perm_building_mode        = 0;

enum {
	PREFALLOC_PERM_DIRECT,
	PREFALLOC_PERM_NORMAL,
	PREFALLOC_PERM_ICORE,
};

static const lc_opt_enum_int_items_t perm_items[] = {
	{ "direct", PREFALLOC_PERM_DIRECT },
	{ "normal", PREFALLOC_PERM_NORMAL },
	{ "icore",  PREFALLOC_PERM_ICORE },
	{ NULL,     0 },
};

static lc_opt_enum_int_var_t perm_building_mode_var = {
	&perm_building_mode, perm_items
};

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_BOOL("prefs", "use preference based coloring", &create_preferences),
	LC_OPT_ENT_BOOL("congruences", "create congruence classes", &create_congruence_classes),
	LC_OPT_ENT_BOOL("prop_phi", "propagate phi registers", &propagate_phi_registers),
	LC_OPT_ENT_ENUM_INT("perm_mode", "select perm build mode", &perm_building_mode_var),
	LC_OPT_LAST
};

/** currently active assignments (while processing a basic block)
 * maps registers to values(their current copies) */
static ir_node **assignments;

/**
 * allocation information: last_uses, register preferences
 * the information is per firm-node.
 */
struct allocation_info_t {
	unsigned  last_uses[4];   /**< bitset indicating last uses (input pos) */
	ir_node  *current_value;  /**< copy of the value that should be used */
	ir_node  *original_value; /**< for copies point to original value */
	float     prefs[];        /**< register preferences */
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
	memcpy(copy_info->prefs, info->prefs, n_regs * sizeof(copy_info->prefs[0]));
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
static void check_defs(const ir_nodeset_t *live_nodes, float weight,
                       ir_node *node)
{
	const arch_register_req_t *req = arch_get_irn_register_req(node);
	if (req->type & arch_register_req_type_limited) {
		const unsigned *limited = req->limited;
		float           penalty = weight * DEF_FACTOR;
		give_penalties_for_limits(live_nodes, penalty, limited, node);
	}

	if (req->type & arch_register_req_type_should_be_same) {
		ir_node           *insn  = skip_Proj(node);
		allocation_info_t *info  = get_allocation_info(node);
		int                arity = get_irn_arity(insn);

		float factor = 1.0f / rbitset_popcount(&req->other_same, arity);
		for (int i = 0; i < arity; ++i) {
			if (!rbitset_is_set(&req->other_same, i))
				continue;

			ir_node *op = get_irn_n(insn, i);

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

	sched_foreach_reverse(block, node) {
		if (is_Phi(node))
			break;

		if (create_preferences) {
			be_foreach_definition(node, cls, value,
				check_defs(&live_nodes, weight, value);
			);
		}

		/* mark last uses */
		int arity = get_irn_arity(node);

		allocation_info_t *info = get_allocation_info(node);
		if (arity >= (int) sizeof(info->last_uses) * 8) {
			panic("Node with more than %d inputs not supported yet",
					(int) sizeof(info->last_uses) * 8);
		}

		for (int i = 0; i < arity; ++i) {
			ir_node                   *op  = get_irn_n(node, i);
			const arch_register_req_t *req = arch_get_irn_register_req(op);
			if (req->cls != cls)
				continue;

			/* last usage of a value? */
			if (!ir_nodeset_contains(&live_nodes, op)) {
				rbitset_set(info->last_uses, i);
			}
		}

		be_liveness_transfer(cls, node, &live_nodes);

		if (create_preferences) {
			/* update weights based on usage constraints */
			for (int i = 0; i < arity; ++i) {
				ir_node *op = get_irn_n(node, i);
				if (!arch_irn_consider_in_reg_alloc(cls, op))
					continue;

				const arch_register_req_t *req
					= arch_get_irn_register_req_in(node, i);
				if (!(req->type & arch_register_req_type_limited))
					continue;

				const unsigned *limited = req->limited;
				give_penalties_for_limits(&live_nodes, weight * USE_FACTOR,
										  limited, op);
			}
		}
	}

	ir_nodeset_destroy(&live_nodes);
}

static void congruence_def(ir_nodeset_t *live_nodes, const ir_node *node)
{
	const arch_register_req_t *req = arch_get_irn_register_req(node);

	/* should be same constraint? */
	if (req->type & arch_register_req_type_should_be_same) {
		const ir_node *insn     = skip_Proj_const(node);
		int            arity    = get_irn_arity(insn);
		unsigned       node_idx = get_irn_idx(node);
		node_idx = uf_find(congruence_classes, node_idx);

		for (int i = 0; i < arity; ++i) {
			if (!rbitset_is_set(&req->other_same, i))
				continue;

			ir_node *op     = get_irn_n(insn, i);
			int      op_idx = get_irn_idx(op);
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
	ir_node *last_phi = NULL;
	sched_foreach_reverse(block, node) {
		if (is_Phi(node)) {
			last_phi = node;
			break;
		}

		be_foreach_definition(node, cls, value,
			congruence_def(&live_nodes, value);
		);
		be_liveness_transfer(cls, node, &live_nodes);
	}
	if (!last_phi) {
		ir_nodeset_destroy(&live_nodes);
		return;
	}

	/* check phi congruence classes */
	sched_foreach_reverse_from(last_phi, phi) {
		assert(is_Phi(phi));

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
			sched_foreach(block, phi) {
				ir_node *oop;
				int      oop_idx;
				if (!is_Phi(phi))
					break;
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

	memcpy(info->prefs, head_info->prefs, n_regs * sizeof(info->prefs[0]));
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



/**
 * Assign register reg to the given node.
 *
 * @param node  the node
 * @param reg   the register
 */
static void use_reg(ir_node *node, const arch_register_t *reg, unsigned width)
{
	unsigned r = reg->index;
	for (unsigned r0 = r; r0 < r + width; ++r0)
		assignments[r0] = node;
	arch_set_irn_register(node, reg);
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
	qsort(regprefs, n_regs, sizeof(regprefs[0]), compare_reg_pref);
}

static bool try_optimistic_split(ir_node *to_split, ir_node *before,
                                 float pref, float pref_delta,
                                 unsigned *forbidden_regs, int recursion)
{
	(void) pref;
	unsigned           r = 0;
	allocation_info_t *info = get_allocation_info(to_split);
	float              delta = 0;

	/* stupid hack: don't optimisticallt split don't spill nodes...
	 * (so we don't split away the values produced because of
	 *  must_be_different constraints) */
	ir_node *original_insn = skip_Proj(info->original_value);
	if (arch_get_irn_flags(original_insn) & arch_irn_flags_dont_spill)
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

	const arch_register_t *reg   = arch_register_for_index(cls, r);
	ir_node               *copy  = be_new_Copy(block, to_split);
	unsigned               width = 1;
	mark_as_copy_of(copy, to_split);
	/* hacky, but correct here */
	if (assignments[from_reg->index] == to_split)
		free_reg_of_value(to_split);
	use_reg(copy, reg, width);
	sched_add_before(before, copy);

	DB((dbg, LEVEL_3,
	    "Optimistic live-range split %+F move %+F(%s) -> %s before %+F (win %f, depth %d)\n",
	    copy, to_split, from_reg->name, reg->name, before, delta, recursion));
	return true;
}

/**
 * Determine and assign a register for node @p node
 */
static void assign_reg(const ir_node *block, ir_node *node,
                       unsigned *forbidden_regs)
{
	assert(!is_Phi(node));
	/* preassigned register? */
	const arch_register_t     *final_reg = arch_get_irn_register(node);
	const arch_register_req_t *req       = arch_get_irn_register_req(node);
	unsigned                   width     = req->width;
	if (final_reg != NULL) {
		DB((dbg, LEVEL_2, "Preassignment %+F -> %s\n", node, final_reg->name));
		use_reg(node, final_reg, width);
		return;
	}

	/* ignore reqs must be preassigned */
	assert (! (req->type & arch_register_req_type_ignore));

	/* give should_be_same boni */
	allocation_info_t *info    = get_allocation_info(node);
	ir_node           *in_node = skip_Proj(node);
	if (req->type & arch_register_req_type_should_be_same) {
		float weight = (float)get_block_execfreq(block);
		int   arity  = get_irn_arity(in_node);

		assert(arity <= (int) sizeof(req->other_same) * 8);
		for (int i = 0; i < arity; ++i) {
			if (!rbitset_is_set(&req->other_same, i))
				continue;

			ir_node               *in        = get_irn_n(in_node, i);
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
		const arch_register_t *reg = arch_register_for_index(cls, num);
		DB((dbg, LEVEL_2, " %s(%f)", reg->name, reg_prefs[r].pref));
	}
	DB((dbg, LEVEL_2, "\n"));

	const unsigned *allowed_regs = normal_regs;
	if (req->type & arch_register_req_type_limited) {
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
			if ((req->type & arch_register_req_type_aligned)
				&& (final_reg_index % width) != 0)
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
		panic("No register left for %+F\n", node);
	}

	final_reg = arch_register_for_index(cls, final_reg_index);
	DB((dbg, LEVEL_2, "Assign %+F -> %s\n", node, final_reg->name));
	use_reg(node, final_reg, width);
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
static void permute_values_direct(ir_nodeset_t *live_nodes, ir_node *before,
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
		const arch_register_t *reg = arch_register_for_index(cls, r);
		DB((dbg, LEVEL_2, "Copy %+F (from %+F, before %+F) -> %s\n",
		    copy, src, before, reg->name));
		mark_as_copy_of(copy, src);
		unsigned width = 1; /* TODO */
		use_reg(copy, reg, width);

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

		ir_node *in[2] = { assignments[r2], assignments[old_r] };
		ir_node *perm = be_new_Perm(cls, block, 2, in);
		sched_add_before(before, perm);
		DB((dbg, LEVEL_2, "Perm %+F (perm %+F,%+F, before %+F)\n",
		    perm, in[0], in[1], before));

		unsigned width = 1; /* TODO */

		ir_node *proj0 = new_r_Proj(perm, get_irn_mode(in[0]), 0);
		mark_as_copy_of(proj0, in[0]);
		const arch_register_t *reg0 = arch_register_for_index(cls, old_r);
		use_reg(proj0, reg0, width);

		ir_node *proj1 = new_r_Proj(perm, get_irn_mode(in[1]), 1);
		mark_as_copy_of(proj1, in[1]);
		const arch_register_t *reg1 = arch_register_for_index(cls, r2);
		use_reg(proj1, reg1, width);

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

static void permute_values_normal(ir_nodeset_t *live_nodes, ir_node *before,
                                  unsigned *parcopy)
{
	unsigned *n_used = ALLOCANZ(unsigned, n_regs);

	/* determine how often each source register needs to be read */
	for (unsigned r = 0; r < n_regs; ++r) {
		unsigned  old_reg = parcopy[r];
		ir_node  *value;

		value = assignments[old_reg];
		if (value == NULL) {
			/* nothing to do here, reg is not live. Mark it as fixpoint
			 * so we ignore it in the next steps */
			parcopy[r] = r;
			continue;
		}

		++n_used[old_reg];
	}

	ir_node  *block      = get_nodes_block(before);
	unsigned  num_copies = 0;

	/* step1: create copies where immediately possible */
	for (unsigned r = 0; r < n_regs; /* empty */) {
		unsigned old_r = parcopy[r];

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
		++num_copies;
		const arch_register_t *reg = arch_register_for_index(cls, r);
		DB((dbg, LEVEL_2, "Copy %+F (from %+F, before %+F) -> %s\n",
		    copy, src, before, reg->name));
		mark_as_copy_of(copy, src);
		unsigned width = 1; /* TODO */
		use_reg(copy, reg, width);

		if (live_nodes != NULL) {
			ir_nodeset_insert(live_nodes, copy);
		}

		/* old register has 1 user less, permutation is resolved */
		assert(arch_get_irn_register(src)->index == old_r);
		parcopy[r] = r;

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

	/* at this point we only have cycles left which we resolve with
	 * one perm instruction. */
	unsigned srcs[n_regs];
	unsigned dsts[n_regs];
	unsigned arity = 0;

	for (unsigned r = 0; r < n_regs; /* empty */) {
		unsigned old_r = parcopy[r];

		if (old_r == r) {
			++r;
			continue;
		}

		/* we shouldn't have copies from 1 value to multiple destinations left*/
		assert(n_used[old_r] == 1);

		srcs[arity] = old_r;
		dsts[arity] = r;
		++arity;

		/* mark as fixed point */
		parcopy[r] = r;
	}

	if (arity > 0) {
		ir_node *ins[arity];
		for (unsigned i = 0; i < arity; ++i) {
			ins[i] = assignments[srcs[i]];
		}
		ir_node *perm = be_new_Perm(cls, block, arity, ins);
		sched_add_before(before, perm);

		unsigned width = 1; /* TODO */

		for (unsigned i = 0; i < arity; ++i) {
			ir_node *proj = new_r_Proj(perm, get_irn_mode(ins[i]), i);
			mark_as_copy_of(proj, ins[i]);
			const arch_register_t *reg = arch_register_for_index(cls, dsts[i]);
			use_reg(proj, reg, width);

			if (live_nodes != NULL) {
				ir_nodeset_remove(live_nodes, ins[i]);
				ir_nodeset_insert(live_nodes, proj);
			}
		}
	}

#ifdef DEBUG_libfirm
	stat_ev_int("bessadestr_copies", num_copies);
#endif

#ifndef NDEBUG
	/* now we should only have fixpoints left */
	for (unsigned r = 0; r < n_regs; ++r) {
		assert(parcopy[r] == r);
	}
#endif
}


static const char *get_reg_name(unsigned reg_index)
{
	return arch_register_for_index(cls, reg_index)->name;
}

#ifdef DEBUG_libfirm
static void print_parcopy(unsigned *parcopy_orig, unsigned *n_used_orig)
{
	unsigned parcopy[n_regs];
	unsigned n_used[n_regs];
	memcpy(parcopy, parcopy_orig, sizeof(unsigned) * n_regs);
	memcpy(n_used, n_used_orig, sizeof(unsigned) * n_regs);

	for (unsigned i = 0; i < n_regs; ++i)
		if (n_used_orig[i] != 0)
			DB((dbg_icore, LEVEL_2, "#users[%s(%u)] = %u\n", get_reg_name(i), i, n_used_orig[i]));

	unsigned comp[n_regs];
	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r || n_used[r] > 0) {
			++r;
			continue;
		}

		/* Perfect, end of a chain. */
		unsigned len = 0;
		comp[len++] = r;
		unsigned s = r;
		while (n_used[s] == 0 && parcopy[s] != s) {
			unsigned src = parcopy[s];
			parcopy[s] = s;
			comp[len++] = src;
			assert(n_used[src] > 0);
			--n_used[src];
			s = src;
		}

		/* Reverse. */
		for (unsigned i = 0; i < len / 2; ++i) {
			unsigned t = comp[i];
			comp[i] = comp[len - i - 1];
			comp[len - i - 1] = t;
		}

		for (unsigned i = 0; i + 1 < len; ++i)
			DB((dbg_icore, LEVEL_2, "%s(%u) -> ", get_reg_name(comp[i]), comp[i]));
		DB((dbg_icore, LEVEL_2, "%s(%i)\n", get_reg_name(comp[len - 1]), comp[len - 1]));
	}

	/* Only cycles left. */
	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r) {
			++r;
			continue;
		}

		assert(n_used[r] == 1);

		unsigned len = 0;
		unsigned s = r;
		while (parcopy[s] != s) {
			unsigned src = parcopy[s];
			comp[len++] = s;
			parcopy[s] = s;
			s = src;
		}

		for (unsigned i = 0; i < len / 2; ++i) {
			unsigned t = comp[i];
			comp[i] = comp[len - i - 1];
			comp[len - i - 1] = t;
		}

		for (unsigned i = 0; i < len; ++i)
			DB((dbg_icore, LEVEL_2, "%s(%u) -> ", get_reg_name(comp[i]), comp[i]));
		DB((dbg_icore, LEVEL_2, "%s(%u)\n", get_reg_name(comp[0]), comp[0]));
	}
}
#endif

static void mark_cycle_parts(bool *part_of_cycle, unsigned *parcopy_orig,
                             unsigned *n_used_orig)
{
	unsigned parcopy[n_regs];
	unsigned n_used[n_regs];
	memcpy(parcopy, parcopy_orig, sizeof(unsigned) * n_regs);
	memcpy(n_used, n_used_orig, sizeof(unsigned) * n_regs);
	memset(part_of_cycle, 0, sizeof(bool) * n_regs);

	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r || n_used[r] > 0) {
			++r;
			continue;
		}

		/* Perfect, end of a chain. */
		unsigned s = r;
		while (n_used[s] == 0 && parcopy[s] != s) {
			part_of_cycle[s] = false;

			unsigned src = parcopy[s];
			parcopy[s] = s;
			assert(n_used[src] > 0);
			--n_used[src];
			s = src;
		}
	}

	/* Only cycles left. */
	for (unsigned r = 0; r < n_regs; ) {
		if (parcopy[r] == r) {
			if (n_used[r] > 0)
				part_of_cycle[r] = true;
			++r;
			continue;
		}

		assert(n_used[r] == 1);

		unsigned s = r;
		while (parcopy[s] != s) {
			part_of_cycle[s] = true;
			unsigned src = parcopy[s];
			parcopy[s] = s;
			s = src;
		}
	}
}

static unsigned find_longest_chain(unsigned *parcopy, unsigned *n_used,
                                   unsigned fork_reg)
{
	/* fork_reg must be a fork. */
	assert(n_used[fork_reg] > 1);

	DB((dbg_icore, LEVEL_2, "  Searching for longest chain starting at %s\n", get_reg_name(fork_reg)));

	/* Search the longest chain starting from r. */
	unsigned max_len = 0;
	unsigned max_dst = (unsigned) -1;

	for (unsigned to_reg = 0; to_reg < n_regs; /* empty */) {
		unsigned from_reg = parcopy[to_reg];

		if (from_reg == to_reg || n_used[to_reg] > 0) {
			++to_reg;
			continue;
		}

		DB((dbg_icore, LEVEL_2, "  Found candidate ending in %s\n", get_reg_name(to_reg)));
		unsigned r   = to_reg;
		unsigned len = 0;
		while (r != parcopy[r]) {
			unsigned src = parcopy[r];
			++len;
			if (src == fork_reg && len > max_len) {
				DB((dbg_icore, LEVEL_2, "  Chain starts in %s, continues via %s, length %u\n", get_reg_name(fork_reg), get_reg_name(r), len));
				max_len = len;
				max_dst = r;
				break;
			}
			r = src;
		}

		++to_reg;
	}

	return max_dst;
}

#ifdef DEBUG_libfirm
static void emit_rtg_stats(unsigned *parcopy)
{
	bool *part_of_rtg = ALLOCANZ(bool, n_regs);
	for (unsigned i = 0; i < n_regs; ++i) {
		if (parcopy[i] != i) {
			part_of_rtg[i] = true;
			part_of_rtg[parcopy[i]] = true;
		}
	}
	unsigned num_rtg_nodes = 0;
	for (unsigned i = 0; i < n_regs; ++i) {
		if (part_of_rtg[i])
			++num_rtg_nodes;
	}
	stat_ev_int("bessadestr_num_rtg_nodes", num_rtg_nodes);
}
#endif

unsigned find_costs_general(unsigned *rtg, unsigned *numUsed, unsigned numRegs, bool dump);

static void permute_values_icore(ir_nodeset_t *live_nodes, ir_node *before,
                                 unsigned *parcopy)
{
	unsigned *n_used = ALLOCANZ(unsigned, n_regs);

	/* Determine how often each source register needs to be read */
	for (unsigned r = 0; r < n_regs; ++r) {
		unsigned  old_reg = parcopy[r];
		ir_node  *value;

		value = assignments[old_reg];
		if (value == NULL) {
			/* Nothing to do here, reg is not live. Mark it as fixpoint,
			 * so we ignore it in the next steps. */
			parcopy[r] = r;
			continue;
		}

		++n_used[old_reg];
	}

#ifdef DEBUG_libfirm
	print_parcopy(parcopy, n_used);
#endif
	unsigned opt_costs;
	{
		unsigned raw_rtg[n_regs];
		unsigned raw_n_used[n_regs];

		for (unsigned i = 0; i < n_regs; ++i) {
			if (parcopy[i] != i)
				raw_rtg[i] = parcopy[i];
			else if (parcopy[i] == i && n_used[i] > 1)
				raw_rtg[i] = i;
			else
				raw_rtg[i] = n_regs;

			raw_n_used[i] = n_used[i];
		}
		opt_costs = find_costs_general(raw_rtg, raw_n_used, n_regs, false);
	}

	ir_node *block = get_nodes_block(before);

	unsigned restore_srcs[n_regs];
	unsigned restore_dsts[n_regs];
	unsigned num_restores = 0;

	DB((dbg_icore, LEVEL_2, "Searching for out-of-cycle propagations.\n"));
	bool is_part_of_cycle[n_regs];
	mark_cycle_parts(is_part_of_cycle, parcopy, n_used);
	for (unsigned to_reg = 0; to_reg < n_regs; /* empty */) {
		unsigned from_reg = parcopy[to_reg];

		if (from_reg == to_reg) {
			++to_reg;
			continue;
		}

		if (is_part_of_cycle[from_reg] && !is_part_of_cycle[to_reg]) {
			DB((dbg_icore, LEVEL_2, "  Found out-of-cycle propagation %s -> %s\n", get_reg_name(from_reg), get_reg_name(to_reg)));
			unsigned new_src = (unsigned) -1;
			for (unsigned src = 0; src < n_regs; ++src) {
				if (parcopy[src] == from_reg && is_part_of_cycle[src]) {
					/* new_src must be unambiguous. */
					new_src = src;
					break;
				}
			}
			assert((new_src != ((unsigned) -1)) && "Could not find new source for out-of-cycle propagation");

			restore_srcs[num_restores] = new_src;
			restore_dsts[num_restores] = to_reg;
			++num_restores;
			DB((dbg_icore, LEVEL_2, "  Added restore %s -> %s\n", get_reg_name(new_src), get_reg_name(to_reg)));
			--n_used[from_reg];
			parcopy[to_reg] = to_reg;
		}

		++to_reg;
	}
	DB((dbg_icore, LEVEL_2, "Finished search for out-of-cycle propagation.\n"));

	DB((dbg_icore, LEVEL_2, "Searching for forks.\n"));
	for (unsigned to_reg = 0; to_reg < n_regs; /* empty */) {
		unsigned from_reg = parcopy[to_reg];

		if (from_reg == to_reg || n_used[to_reg] > 0) {
			++to_reg;
			continue;
		}

		/* Found the end of a chain, follow it. */
		unsigned r = to_reg;
		while (r != parcopy[r]) {
			r = parcopy[r];
			if (n_used[r] > 1) {
				/* Found a fork. */
				DB((dbg_icore, LEVEL_2, "  Found a fork at %s\n", get_reg_name(r)));
				unsigned longest_next = find_longest_chain(parcopy, n_used, r);
				DB((dbg_icore, LEVEL_2, "  Longest chain from %s via %s\n", get_reg_name(r), get_reg_name(longest_next)));

				/* Reroute all others. */
				for (unsigned dst = 0; dst < n_regs; ++dst) {
					if (dst != longest_next && dst != r && parcopy[dst] == r) {
						restore_srcs[num_restores] = longest_next;
						restore_dsts[num_restores] = dst;
						++num_restores;
						DB((dbg_icore, LEVEL_2, "  Added restore %s -> %s\n", get_reg_name(longest_next), get_reg_name(dst)));
						--n_used[r];
						parcopy[dst] = dst;
					}
				}
			}
		}

		++to_reg;
	}
	DB((dbg_icore, LEVEL_2, "Finished searching for forks.\n"));

	DB((dbg_icore, LEVEL_2, "Current parallel copy:\n"));
#ifdef DEBUG_libfirm
	print_parcopy(parcopy, n_used);
#endif

	/* Step 3: The remaining parcopy must be suitable for a Perm. */
	unsigned perm_size = 0;
	ir_node *ins[n_regs];
	for (unsigned r = 0; r < n_regs; ++r) {
		unsigned src = parcopy[r];
		if (src != r)
			ins[perm_size++] = assignments[src];
	}

	ir_node *perm = NULL;
	if (perm_size > 0) {
		DB((dbg_icore, LEVEL_2, "Creating Perm using parcopy.\n"));

		perm = be_new_Perm(cls, block, perm_size, ins);
		sched_add_before(before, perm);
		unsigned input = 0;
		for (unsigned r = 0; r < n_regs; ++r) {
			unsigned src = parcopy[r];
			if (src != r) {
				ir_node *proj = new_r_Proj(perm, get_irn_mode(ins[input]), input);
				mark_as_copy_of(proj, ins[input]);

				const arch_register_t *reg = arch_register_for_index(cls, r);
				use_reg(proj, reg, /* width = */ 1);

				assert(n_used[src] == 1);
				if (parcopy[src] == src) {
					DB((dbg_icore, LEVEL_2, "Perm: Freeing register %s of value %+F\n", get_reg_name(src), ins[input]));
					free_reg_of_value(assignments[src]);
				}

				if (live_nodes != NULL) {
					ir_nodeset_remove(live_nodes, ins[input]);
					ir_nodeset_insert(live_nodes, proj);
				}
				++input;
			}
		}

		for (unsigned r = 0; r < n_regs; ++r)
			parcopy[r] = r;
	}

#ifndef NDEBUG
	/* now we should only have fixpoints left */
	for (unsigned r = 0; r < n_regs; ++r) {
		assert(parcopy[r] == r);
	}
#endif

#ifdef DEBUG_libfirm
	/* Emit statistics. */
	if (perm != NULL) {
		stat_ev_ctx_push_fmt("perm_stats", "%ld", get_irn_node_nr(perm));
		stat_ev_int("perm_num_restores", num_restores);
		stat_ev_int("perm_opt_costs", opt_costs);
		stat_ev_ctx_pop("perm_stats");
		const int already_in_prtg_form = num_restores == 0;
		stat_ev_int("bessadestr_already_in_prtg_form", already_in_prtg_form);
	} else if (num_restores > 0) {
		stat_ev_int("bessadestr_copies", num_restores);
		stat_ev_int("bessadestr_opt_costs", opt_costs);
		stat_ev_int("bessadestr_already_in_prtg_form", 0);
	}
#endif

	if (num_restores > 0) {
		/* Step 4: Place restore movs. */
		DB((dbg_icore, LEVEL_2, "Placing restore movs.\n"));
		for (unsigned i = 0; i < num_restores; ++i) {
			unsigned src_reg = restore_srcs[i];
			unsigned dst_reg = restore_dsts[i];
			ir_node *src     = assignments[src_reg];
			ir_node *copy    = be_new_Copy(block, src);
			sched_add_before(before, copy);

			DB((dbg_icore, LEVEL_2, "Inserted restore copy %+F %s -> %s\n", copy, get_reg_name(src_reg), get_reg_name(dst_reg)));
			mark_as_copy_of(copy, src);
			const arch_register_t *reg = arch_register_for_index(cls, dst_reg);
			use_reg(copy, reg, /* width = */ 1);

			if (live_nodes != NULL) {
				ir_nodeset_remove(live_nodes, src);
				ir_nodeset_insert(live_nodes, copy);
			}
		}
		DB((dbg_icore, LEVEL_2, "Finished placing restore movs.\n"));
	}
}

static void permute_values(ir_nodeset_t *live_nodes, ir_node *before,
                           unsigned *permutation)
{
	DB((dbg_icore, LEVEL_2, "permute_values: before:\n"));
	for (unsigned i = 0; i < n_regs; ++i)
		DB((dbg_icore, LEVEL_2, "  %u -> %+F\n", i, assignments[i]));

#ifdef DEBUG_libfirm
	emit_rtg_stats(permutation);
#endif

	switch (perm_building_mode) {
	case PREFALLOC_PERM_DIRECT:
		permute_values_direct(live_nodes, before, permutation);
		break;
	case PREFALLOC_PERM_NORMAL:
		permute_values_normal(live_nodes, before, permutation);
		break;
	case PREFALLOC_PERM_ICORE:
		permute_values_icore(live_nodes, before, permutation);
		break;
	}

	DB((dbg_icore, LEVEL_2, "permute_values: after:\n"));
	for (unsigned i = 0; i < n_regs; ++i)
		DB((dbg_icore, LEVEL_2, "  %u -> %+F\n", i, assignments[i]));
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
	int                arity     = get_irn_arity(node);

	for (int i = 0; i < arity; ++i) {
		/* check if one operand is the last use */
		if (!rbitset_is_set(last_uses, i))
			continue;

		ir_node *op = get_irn_n(node, i);
		free_reg_of_value(op);
		ir_nodeset_remove(live_nodes, op);
	}
}

/**
 * change inputs of a node to the current value (copies/perms)
 */
static void rewire_inputs(ir_node *node)
{
	int arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		ir_node           *op = get_irn_n(node, i);
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
	int arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		if (!rbitset_is_set(info->last_uses, i))
			continue;

		ir_node               *op  = get_irn_n(node, i);
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
	int arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		ir_node *op = get_irn_n(node, i);
		if (!arch_irn_consider_in_reg_alloc(cls, op))
			continue;

		const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
		if (!(req->type & arch_register_req_type_limited))
			continue;

		const unsigned        *limited     = req->limited;
		const arch_register_t *reg         = arch_get_irn_register(op);
		unsigned               current_reg = reg->index;
		for (unsigned r = 0; r < n_regs; ++r) {
			if (rbitset_is_set(limited, r))
				continue;

			rbitset_set(forbidden_edges, current_reg*n_regs + r);
		}
	}

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

	lpp_dump_plain(lpp, fopen("lppdump.txt", "w"));

	/* solve lpp */
	lpp_solve(lpp, be_options.ilp_server, be_options.ilp_solver);
	if (!lpp_is_sol_valid(lpp))
		panic("ilp solution not valid!");

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
	assert(is_po2(alignment));
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
	int  arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		ir_node *op = get_irn_n(node, i);
		if (!arch_irn_consider_in_reg_alloc(cls, op))
			continue;

		/* are there any limitations for the i'th operand? */
		const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
		if (req->width > 1)
			double_width = true;
		const arch_register_t *reg       = arch_get_irn_register(op);
		unsigned               reg_index = reg->index;
		if (req->type & arch_register_req_type_aligned) {
			if (!is_aligned(reg_index, req->width)) {
				good = false;
				continue;
			}
		}
		if (!(req->type & arch_register_req_type_limited))
			continue;

		const unsigned *limited = req->limited;
		if (!rbitset_is_set(limited, reg_index)) {
			/* found an assignment outside the limited set */
			good = false;
			continue;
		}
	}

	/* is any of the live-throughs using a constrained output register? */
	unsigned *live_through_regs = NULL;
	be_foreach_definition(node, cls, value,
		(void)value;
		if (req_->width > 1)
			double_width = true;
		if (! (req_->type & arch_register_req_type_limited))
			continue;
		if (live_through_regs == NULL) {
			rbitset_alloca(live_through_regs, n_regs);
			determine_live_through_regs(live_through_regs, node);
		}
		rbitset_or(forbidden_regs, req_->limited, n_regs);
		if (rbitsets_have_common(req_->limited, live_through_regs, n_regs))
			good = false;
	);

	if (good)
		return;

	/* create these arrays if we haven't yet */
	if (live_through_regs == NULL) {
		rbitset_alloca(live_through_regs, n_regs);
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

	for (int i = 0; i < arity; ++i) {
		ir_node *op = get_irn_n(node, i);
		if (!arch_irn_consider_in_reg_alloc(cls, op))
			continue;

		const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
		if (!(req->type & arch_register_req_type_limited))
			continue;

		const unsigned        *limited     = req->limited;
		const arch_register_t *reg         = arch_get_irn_register(op);
		unsigned               current_reg = reg->index;
		for (unsigned r = 0; r < n_regs; ++r) {
			if (rbitset_is_set(limited, r))
				continue;
			hungarian_remove(bp, r, current_reg);
		}
	}

	//hungarian_print_cost_matrix(bp, 1);
	hungarian_prepare_cost_matrix(bp, HUNGARIAN_MODE_MAXIMIZE_UTIL);

	unsigned *assignment = ALLOCAN(unsigned, n_regs);
	int res = hungarian_solve(bp, assignment, NULL, 0);
	assert(res == 0);

#if 0
	fprintf(stderr, "Swap result:");
	for (i = 0; i < (int) n_regs; ++i) {
		fprintf(stderr, " %d", assignment[i]);
	}
	fprintf(stderr, "\n");
#endif

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
	bool     need_permutation = false;
	ir_node *phi              = sched_first(block);
	for ( ; is_Phi(phi); phi = sched_next(phi)) {
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
		/* virtual or joker registers are ok too */
		if ((op_reg->type & arch_register_type_joker)
				|| (op_reg->type & arch_register_type_virtual))
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
	phi = sched_first(block);
	for ( ; is_Phi(phi); phi = sched_next(phi)) {
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

	int arity = get_irn_arity(phi);
	for (int i = 0; i < arity; ++i) {
		ir_node               *op  = get_irn_n(phi, i);
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
	sched_foreach(block, node) {
		if (!is_Phi(node))
			break;
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
	sched_foreach(block, node) {
		if (!is_Phi(node))
			break;
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
	assert(res == 0);

	/* apply results */
	n = 0;
	sched_foreach(block, node) {
		if (!is_Phi(node))
			break;
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;
		const arch_register_req_t *req
			= arch_get_irn_register_req(node);

		unsigned r = assignment[n++];
		assert(rbitset_is_set(normal_regs, r));
		const arch_register_t *reg = arch_register_for_index(cls, r);
		DB((dbg, LEVEL_2, "Assign %+F -> %s\n", node, reg->name));
		use_reg(node, reg, req->width);

		/* adapt preferences for phi inputs */
		propagate_phi_register(node, r);
	}
}

static arch_register_req_t *allocate_reg_req(ir_graph *irg)
{
	struct obstack *obst = be_get_be_obst(irg);
	arch_register_req_t *req = OALLOCZ(obst, arch_register_req_t);
	return req;
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

		if (req->type & arch_register_req_type_ignore) {
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
			ir_mode *mode = get_irn_mode(node);
			const arch_register_req_t *phi_req = cls->class_req;
			if (req->width > 1) {
				arch_register_req_t *new_req = allocate_reg_req(irg);
				new_req->cls   = cls;
				new_req->type  = req->type & arch_register_req_type_aligned;
				new_req->width = req->width;
				phi_req = new_req;
			}
			ir_node *phi  = be_new_Phi(block, n_preds, phi_ins, mode,
			                           phi_req);

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

	unsigned *forbidden_regs; /**< collects registers which must
	                               not be used for optimistic splits */
	rbitset_alloca(forbidden_regs, n_regs);

	/* handle phis... */
	assign_phi_registers(block);

	/* all live-ins must have a register */
#ifndef NDEBUG
	foreach_ir_nodeset(&live_nodes, node, iter) {
		const arch_register_t *reg = arch_get_irn_register(node);
		assert(reg != NULL);
	}
#endif

	/* assign instructions in the block */
	sched_foreach(block, node) {
		/* phis are already assigned */
		if (is_Phi(node))
			continue;

		rewire_inputs(node);

		/* enforce use constraints */
		rbitset_clear_all(forbidden_regs, n_regs);
		enforce_constraints(&live_nodes, node, forbidden_regs);

		rewire_inputs(node);

		/* we may not use registers used for inputs for optimistic splits */
		int arity = get_irn_arity(node);
		for (int i = 0; i < arity; ++i) {
			ir_node *op = get_irn_n(node, i);
			if (!arch_irn_consider_in_reg_alloc(cls, op))
				continue;

			const arch_register_t *reg = arch_get_irn_register(op);
			rbitset_set(forbidden_regs, reg->index);
		}

		/* free registers of values last used at this instruction */
		free_last_uses(&live_nodes, node);

		/* assign output registers */
		be_foreach_definition_(node, cls, value,
			assign_reg(block, value, forbidden_regs);
		);
	}

	ir_nodeset_destroy(&live_nodes);
	assignments = NULL;

	block_info->processed = true;

	be_timer_push(T_RA_SSA);
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
	be_timer_pop(T_RA_SSA);
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
	pdeq     *worklist  = new_pdeq();
	ir_node **order     = XMALLOCN(ir_node*, n_blocks);
	size_t    order_p   = 0;

	/* clear block links... */
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
	qsort(blocklist, n_blocks, sizeof(blocklist[0]), cmp_block_costs);

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

			pdeq_putr(worklist, block);
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
		while (!pdeq_empty(worklist)) {
			ir_node *pblock = (ir_node*)pdeq_getr(worklist);
			assert(order_p < n_blocks);
			order[order_p++] = pblock;
		}
	}
	assert(order_p == n_blocks);
	del_pdeq(worklist);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);

	DEL_ARR_F(blocklist);

	obstack_free(&obst, NULL);
	obstack_init(&obst);

	block_order   = order;
	n_block_order = n_blocks;
}

static void free_block_order(void)
{
	xfree(block_order);
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

	be_clear_links(irg);

	irg_block_walk_graph(irg, NULL, analyze_block, NULL);
	combine_congruence_classes();

	for (size_t i = 0; i < n_block_order; ++i) {
		ir_node *block = block_order[i];
		allocate_coalesce_block(block, NULL);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

static void dump(int mask, ir_graph *irg, const char *suffix)
{
	if (be_options.dump_flags & mask)
		dump_ir_graph(irg, suffix);
}

/**
 * Run the spiller on the current graph.
 */
static void spill(void)
{
	/* make sure all nodes show their real register pressure */
	be_timer_push(T_RA_CONSTR);
	be_pre_spill_prepare_constr(irg, cls);
	be_timer_pop(T_RA_CONSTR);

	dump(DUMP_RA, irg, "spillprepare");

	/* spill */
	be_timer_push(T_RA_SPILL);
	be_do_spill(irg, cls);
	be_timer_pop(T_RA_SPILL);

	be_timer_push(T_RA_SPILL_APPLY);
	check_for_memory_operands(irg);
	be_timer_pop(T_RA_SPILL_APPLY);

	dump(DUMP_RA, irg, "spill");
}

/**
 * The pref register allocator for a whole procedure.
 */
static void be_pref_alloc(ir_graph *new_irg)
{
	obstack_init(&obst);

	irg = new_irg;

	/* determine a good coloring order */
	determine_block_order();

	const arch_env_t *arch_env = be_get_irg_arch_env(new_irg);
	int               n_cls    = arch_env->n_register_classes;
	for (int c = 0; c < n_cls; ++c) {
		cls = &arch_env->register_classes[c];
		if (arch_register_class_flags(cls) & arch_register_class_flag_manual_ra)
			continue;

		stat_ev_ctx_push_str("regcls", cls->name);

		n_regs      = arch_register_class_n_regs(cls);
		normal_regs = rbitset_malloc(n_regs);
		be_set_allocatable_regs(irg, cls, normal_regs);

		spill();

		/* verify schedule and register pressure */
		be_timer_push(T_VERIFY);
		if (be_options.verify_option == BE_VERIFY_WARN) {
			be_verify_schedule(irg);
			be_verify_register_pressure(irg, cls);
		} else if (be_options.verify_option == BE_VERIFY_ASSERT) {
			assert(be_verify_schedule(irg) && "Schedule verification failed");
			assert(be_verify_register_pressure(irg, cls)
				&& "Register pressure verification failed");
		}
		be_timer_pop(T_VERIFY);

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

	be_timer_push(T_RA_SPILL_APPLY);
	be_abi_fix_stack_nodes(irg);
	be_timer_pop(T_RA_SPILL_APPLY);

	be_timer_push(T_VERIFY);
	if (be_options.verify_option == BE_VERIFY_WARN) {
		be_verify_register_allocation(irg);
	} else if (be_options.verify_option == BE_VERIFY_ASSERT) {
		assert(be_verify_register_allocation(irg)
		       && "Register allocation invalid");
	}
	be_timer_pop(T_VERIFY);

	obstack_free(&obst, NULL);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_pref_alloc)
void be_init_pref_alloc(void)
{
	static be_ra_t be_ra_pref = { be_pref_alloc };
	be_register_allocator("pref", &be_ra_pref);

	lc_opt_entry_t *be_grp          = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *prefalloc_group = lc_opt_get_grp(be_grp, "prefalloc");
	lc_opt_add_table(prefalloc_group, options);

	FIRM_DBG_REGISTER(dbg, "firm.be.prefalloc");
	FIRM_DBG_REGISTER(dbg_icore, "firm.be.prefalloc.icore");
}
