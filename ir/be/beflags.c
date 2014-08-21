/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       modifies schedule so flags dependencies are respected.
 * @author      Matthias Braun, Christoph Mallon
 *
 * Fixup schedule to respect flag constraints by moving and rematerialisation of
 * nodes.
 *
 * Flags are modeled as register classes with ignore registers. However to avoid
 * bloating the graph, only flag-consumer -> producer dependencies are
 * explicitely modeled in the graph. Nodes that just change the flags are only
 * marked with the arch_irn_flag_modify_flags flag.
 *
 * Flags are usually a limited resource that can't (or at least shouldn't) be
 * spilled. So in some situations (for example 2 adc-nodes that use the flags of
 * a single add node on x86) operations have to be repeated to work correctly.
 */
#include <stdbool.h>

#include "irgwalk.h"
#include "irnode_t.h"
#include "irtools.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irouts_t.h"
#include "panic.h"

#include "beflags.h"
#include "bearch.h"
#include "beirg.h"
#include "beirgmod.h"
#include "besched.h"
#include "benode.h"
#include "belive.h"
#include "beabihelper.h"

static const arch_register_class_t *flag_class;
static const arch_register_t       *flags_reg;
static func_rematerialize           remat;
static check_modifies_flags         check_modify;
static try_replace_flags            try_replace;
static bool                         changed;

static ir_node *default_remat(ir_node *node, ir_node *after)
{
	ir_node *block = is_Block(after) ? after : get_nodes_block(after);
	ir_node *copy  = exact_copy(node);
	set_nodes_block(copy, block);
	sched_add_after(after, copy);
	return copy;
}

static bool default_check_modifies(const ir_node *node)
{
	return arch_irn_is(node, modify_flags);
}

static bool default_try_replace(ir_node *consumers, ir_node *flags, ir_node *available, int pn)
{
	(void)consumers;
	(void)flags;
	(void)available;
	(void)pn;
	return false;
}

/**
 * tests whether we can legally move node node after node after
 * (only works for nodes in same block)
 */
static bool can_move(ir_node *node, ir_node *after)
{
	ir_node *node_block = get_nodes_block(node);
	assert(node_block == get_nodes_block(after));

	/* TODO respect dep edges */
	assert(get_irn_n_edges_kind(node, EDGE_KIND_DEP) == 0);

	/** all users have to be after the after node */
	foreach_out_edge(node, edge) {
		ir_node *out = get_edge_src_irn(edge);
		if (get_nodes_block(out) != node_block)
			continue;
		/* phi represents a usage at block end */
		if (is_Phi(out))
			continue;
		if (arch_is_irn_not_scheduled(out)) {
			if (!can_move(out, after))
				return false;
		} else {
			if (sched_get_time_step(out) <= sched_get_time_step(after))
				return false;
		}
	}

	return true;
}

/**
 * After node has been rematerialized as copy, this checks all
 * other uses of node to see if they can use copy instead (this
 * reduces the lifetime of node's results).
 */
static void move_other_uses(ir_node *node, ir_node *copy)
{
	/* copy_prev already has its visited flag set, but is still
	 * scheduled before copy. */
	ir_node *copy_prev = get_irn_sched_info(copy)->prev;

	foreach_out_edge(node, out) {
		ir_node *proj = get_edge_src_irn(out);
		if (get_irn_mode(proj) == flag_class->mode) {
			continue;
		}

		ir_node *new_proj = NULL;
		foreach_out_edge_safe(proj, edge) {
			ir_node *succ = get_edge_src_irn(edge);
			if (irn_visited(succ) && succ != copy_prev &&
			    value_strictly_dominates(copy, succ)) {
				ir_graph *irg = get_irn_irg(succ);
				be_lv_t *lv = be_get_irg_liveness(irg);
				if (new_proj == NULL) {
					ir_mode *proj_mode = get_irn_mode(proj);
					int      pn        = get_Proj_proj(proj);
					new_proj = new_r_Proj(copy, proj_mode, pn);
				}
				int n = get_edge_src_pos(edge);
				set_irn_n(succ, n, new_proj);
				be_liveness_update(lv, proj);
				be_liveness_update(lv, new_proj);
			}
		}
	}
}

/**
 * Tries the following solutions in order:
 * 1. Move flags_needed behind node
 * 2. Modify flag_consumers to use available_flags instead of flags_needed
 * 3. Rematerialize flags_needed behind node
 *
 * Returns true, if flag_consumers now use available_flags.
 */
static bool rematerialize_or_move(ir_node *flags_needed, ir_node *node,
                                  ir_node *flag_consumers, int pn, ir_node *available_flags)
{
	if (!is_Block(node)
	  && get_nodes_block(flags_needed) == get_nodes_block(node)
	  && can_move(flags_needed, node)) {
		/* move it */
		sched_remove(flags_needed);
		sched_add_after(node, flags_needed);
		/* No need to update liveness, the node stays in the same block */
		return false;
	}

	/* Try to use the flags available at this point. */
	if (available_flags != NULL &&
	    try_replace(flag_consumers, flags_needed, available_flags, pn)) {
		return true;
	}

	changed = true;
	ir_node *copy = remat(flags_needed, node);
	ir_node *value;
	if (get_irn_mode(copy) == mode_T) {
		move_other_uses(flags_needed, copy);
		ir_mode *mode = flag_class->mode;
		value = new_r_Proj(copy, mode, pn);
		be_add_missing_keeps_node(copy);
	} else {
		value = copy;
	}

	ir_mode *value_mode = get_irn_mode(value);
	ir_node *n          = flag_consumers;
	do {
		foreach_irn_in(n, i, in) {
			/* Assume that each node has at most one flag
			 * input. */
			if (get_irn_mode(in) == value_mode) {
				set_irn_n(n, i, value);
				break;
			}
		}
		n = (ir_node*)get_irn_link(n);
	} while (n != NULL);

	/* No need to introduce the copy, because it only lives in this block, but
	 * we have to update the liveness of all operands */
	if (is_Block(node)
	 || get_nodes_block(node) != get_nodes_block(flags_needed)) {
		ir_graph *irg = get_irn_irg(node);
		be_lv_t  *lv  = be_get_irg_liveness(irg);
		if (lv != NULL) {
			foreach_irn_in_r(copy, i, pred) {
				be_liveness_update(lv, pred);
			}
		}
	}

	return false;
}

/**
 * walks up the schedule and makes sure there are no flag-destroying nodes
 * between a flag-consumer -> flag-producer chain. Fixes problematic situations
 * by moving and/or rematerialisation of the flag-producers.
 * (This can be extended in the future to do some register allocation on targets
 *  like ppc32 where we conceptually have 8 flag registers)
 */
static void fix_flags_walker(ir_node *block, void *env)
{
	(void)env;
	ir_node *flags_needed   = NULL;
	ir_node *flag_consumers = NULL;
	int      pn             = -1;
	ir_node *place          = block;
	sched_foreach_reverse(block, node) {
		mark_irn_visited(node);

		if (is_Phi(node)) {
			place = node;
			break;
		}

		if (node == flags_needed) {
			/* all ok */
			flags_needed   = NULL;
			flag_consumers = NULL;
		}

		/* test whether node destroys the flags */
		ir_node *test = node;
		if (be_is_Keep(test))
			test = sched_prev(test);

		if (flags_needed != NULL && check_modify(test)) {
			/* rematerialize */
			rematerialize_or_move(flags_needed, node, flag_consumers, pn, test);
			flags_needed   = NULL;
			flag_consumers = NULL;
		}

		/* test whether the current node needs flags */
		ir_node *new_flags_needed = NULL;
		foreach_irn_in(node, i, pred) {
			const arch_register_req_t *req
				= arch_get_irn_register_req_in(node, i);
			if (req->cls == flag_class) {
				assert(new_flags_needed == NULL);
				new_flags_needed = pred;
			}
		}

		if (new_flags_needed == NULL)
			continue;

		/* spiller can't (correctly) remat flag consumers at the moment */
		assert(!arch_irn_is(node, rematerializable));

		if (skip_Proj(new_flags_needed) != flags_needed) {
			if (flags_needed != NULL) {
				/* rematerialize node */
				bool use_new_flags = rematerialize_or_move(
					flags_needed, node, flag_consumers, pn, new_flags_needed);
				/* We are only done with
				 * flag_consumers, if flags_needed has
				 * actually been rematerialized. */
				if (!use_new_flags) {
					flag_consumers = NULL;
				}
			}

			flags_needed = new_flags_needed;
			arch_set_irn_register(flags_needed, flags_reg);
			if (is_Proj(flags_needed)) {
				pn           = get_Proj_proj(flags_needed);
				flags_needed = get_Proj_pred(flags_needed);
			}
			flag_consumers = node;
			set_irn_link(flag_consumers, NULL);
			assert(arch_irn_is(flags_needed, rematerializable));
		} else {
			/* link all consumers in a list */
			set_irn_link(node, flag_consumers);
			flag_consumers = node;
		}
	}

	if (flags_needed != NULL) {
		assert(get_nodes_block(flags_needed) != block);
		rematerialize_or_move(flags_needed, place, flag_consumers, pn, NULL);
		flags_needed   = NULL;
		flag_consumers = NULL;
	}

	assert(flags_needed   == NULL);
	assert(flag_consumers == NULL);
}

void be_sched_fix_flags(ir_graph *irg, const arch_register_class_t *flag_cls,
                        func_rematerialize remat_func,
                        check_modifies_flags check_modifies_flags_func,
                        try_replace_flags try_replace_flags_func)
{
	flag_class   = flag_cls;
	flags_reg    = &flag_class->regs[0];
	remat        = remat_func;
	check_modify = check_modifies_flags_func;
	try_replace  = try_replace_flags_func;
	changed      = false;
	if (remat == NULL)
		remat = &default_remat;
	if (check_modify == NULL)
		check_modify = &default_check_modifies;
	if (try_replace == NULL)
		try_replace = &default_try_replace;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK |
	                          IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	irg_block_walk_graph(irg, fix_flags_walker, NULL, NULL);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK |
		               IR_RESOURCE_IRN_VISITED);

	if (changed) {
		be_remove_dead_nodes_from_schedule(irg);
	}
}
