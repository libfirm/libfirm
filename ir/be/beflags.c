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
#include "error.h"

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
static int                          changed;

static ir_node *default_remat(ir_node *node, ir_node *after)
{
	ir_node *block, *copy;
	if (is_Block(after))
		block = after;
	else
		block = get_nodes_block(after);

	copy = exact_copy(node);
	set_nodes_block(copy, block);
	sched_add_after(after, copy);

	return copy;
}

static bool default_check_modifies(const ir_node *node)
{
	return arch_irn_is(node, modify_flags);
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
		if (is_Proj(out)) {
			assert(get_irn_n_edges_kind(out, EDGE_KIND_DEP) == 0);
			foreach_out_edge(out, edge2) {
				ir_node *out2 = get_edge_src_irn(edge2);
				if (get_nodes_block(out2) != node_block)
					continue;
				/* Phi or End represents a usage at block end. */
				if (is_Phi(out2) || is_End(out2))
					continue;
				if (is_Sync(out2)) {
					foreach_out_edge(out2, edge3) {
						ir_node *out3 = get_edge_src_irn(edge3);
						/* Phi or End represents a usage at block end. */
						if (is_Phi(out3) || is_End(out3))
							continue;
						assert(!is_Sync(out3));
						if (sched_get_time_step(out3) <= sched_get_time_step(after)) {
							return false;
						}
					}
				} else if (sched_get_time_step(out2) <= sched_get_time_step(after)) {
					return false;
				}
			}
		} else {
			if (get_nodes_block(out) != node_block)
				continue;
			/* phi represents a usage at block end */
			if (is_Phi(out))
				continue;
			if (sched_get_time_step(out) <= sched_get_time_step(after)) {
				return false;
			}
		}
	}

	return true;
}

static void rematerialize_or_move(ir_node *flags_needed, ir_node *node,
                                  ir_node *flag_consumers, int pn)
{
	ir_node *n;
	ir_node *copy;
	ir_node *value;

	if (!is_Block(node) &&
			get_nodes_block(flags_needed) == get_nodes_block(node) &&
			can_move(flags_needed, node)) {
		/* move it */
		sched_remove(flags_needed);
		sched_add_after(node, flags_needed);
		/* No need to update liveness, because the node stays in the same block */
		return;
	}

	changed = 1;
	copy    = remat(flags_needed, node);

	if (get_irn_mode(copy) == mode_T) {
		ir_mode *mode = flag_class->mode;
		value = new_rd_Proj(NULL, copy, mode, pn);
		be_add_missing_keeps_node(copy);
	} else {
		value = copy;
	}

	n = flag_consumers;
	do {
		int i;
		int arity = get_irn_arity(n);
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(n, i);
			in = skip_Proj(in);
			if (in == flags_needed) {
				set_irn_n(n, i, value);
				break;
			}
		}
		n = (ir_node*)get_irn_link(n);
	} while (n != NULL);

	/* No need to introduce the copy, because it only lives in this block, but
	 * we have to update the liveness of all operands */
	if (is_Block(node) ||
			get_nodes_block(node) != get_nodes_block(flags_needed)) {
		ir_graph *irg = get_irn_irg(node);
		be_lv_t  *lv  = be_get_irg_liveness(irg);
		int       i;

		if (lv != NULL) {
			for (i = get_irn_arity(copy) - 1; i >= 0; --i) {
				be_liveness_update(lv, get_irn_n(copy, i));
			}
		}
	}
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
	ir_node *flags_needed   = NULL;
	ir_node *flag_consumers = NULL;
	int      pn = -1;
	(void) env;

	ir_node *place = block;
	sched_foreach_reverse(block, node) {
		int i, arity;
		ir_node *new_flags_needed = NULL;
		ir_node *test;

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
		test = node;
		if (be_is_Keep(test))
			test = sched_prev(test);

		if (flags_needed != NULL && check_modify(test)) {
			/* rematerialize */
			rematerialize_or_move(flags_needed, node, flag_consumers, pn);
			flags_needed   = NULL;
			flag_consumers = NULL;
		}

		/* test whether the current node needs flags */
		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			const arch_register_req_t *req
				= arch_get_irn_register_req_in(node, i);
			if (req->cls == flag_class) {
				assert(new_flags_needed == NULL);
				new_flags_needed = get_irn_n(node, i);
			}
		}

		if (new_flags_needed == NULL)
			continue;

		/* spiller can't (correctly) remat flag consumers at the moment */
		assert(!arch_irn_is(node, rematerializable));

		if (skip_Proj(new_flags_needed) != flags_needed) {
			if (flags_needed != NULL) {
				/* rematerialize node */
				rematerialize_or_move(flags_needed, node, flag_consumers, pn);
				flags_needed   = NULL;
				flag_consumers = NULL;
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
		rematerialize_or_move(flags_needed, place, flag_consumers, pn);
		flags_needed   = NULL;
		flag_consumers = NULL;
	}

	assert(flags_needed   == NULL);
	assert(flag_consumers == NULL);
}

void be_sched_fix_flags(ir_graph *irg, const arch_register_class_t *flag_cls,
                        func_rematerialize remat_func,
                        check_modifies_flags check_modifies_flags_func)
{
	flag_class   = flag_cls;
	flags_reg    = & flag_class->regs[0];
	remat        = remat_func;
	check_modify = check_modifies_flags_func;
	changed      = 0;
	if (remat == NULL)
		remat = &default_remat;
	if (check_modify == NULL)
		check_modify = &default_check_modifies;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, fix_flags_walker, NULL, NULL);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	if (changed) {
		be_remove_dead_nodes_from_schedule(irg);
	}
}
