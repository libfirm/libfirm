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
 * @brief       modifies schedule so flags dependencies are respected.
 * @author      Matthias Braun, Christoph Mallon
 * @version     $Id: besched.h 14693 2007-06-21 15:35:49Z beck $
 *
 * Fixup schedule to respect flag constraints by moving and rematerialisation of
 * nodes.
 *
 * Flags are modeled as register classes with ignore registers. However to avoid
 * bloating the graph, only flag-consumer -> producer dependencies are
 * explicitely modeled in the graph. Nodes that just change the flags are only
 * marked with the arch_irn_flags_modify_flags flag.
 *
 * Flags are usually a limited resource that can't (or at least shouldn't) be
 * spilled. So in some situations (for example 2 adc-nodes that use the flags of
 * a single add node on x86) operations have to be repeated to work correctly.
 */
#include "config.h"

#include "irgwalk.h"
#include "irnode_t.h"
#include "irtools.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irprintf.h"
#include "error.h"

#include "beflags.h"
#include "bearch.h"
#include "beirg.h"
#include "besched.h"
#include "benode_t.h"
#include "belive.h"

static const arch_register_class_t *flag_class = NULL;
static const arch_register_t       *flags_reg  = NULL;
static func_rematerialize           remat      = NULL;
static int                          changed;

static ir_node *default_remat(ir_node *node, ir_node *after)
{
	ir_node *block, *copy;
	if(is_Block(after))
		block = after;
	else
		block = get_nodes_block(after);

	copy = exact_copy(node);
	set_nodes_block(copy, block);
	sched_add_after(after, copy);

	return copy;
}

/**
 * tests whether we can legally move node node after node after
 * (only works for nodes in same block)
 */
static int can_move(ir_node *node, ir_node *after)
{
	const ir_edge_t *edge;
	assert(get_nodes_block(node) == get_nodes_block(after));

	/* TODO respect dep edges */
	assert(get_irn_n_edges_kind(node, EDGE_KIND_DEP) == 0);

	/** all users have to be after the after node */
	foreach_out_edge(node, edge) {
		ir_node *out = get_edge_src_irn(edge);
		if(is_Proj(out)) {
			const ir_edge_t *edge2;
			assert(get_irn_n_edges_kind(out, EDGE_KIND_DEP) == 0);
			foreach_out_edge(out, edge2) {
				ir_node *out2 = get_edge_src_irn(edge2);
				/* Phi or End represents a usage at block end. */
				if(is_Phi(out2) || is_End(out2))
					continue;
				if(is_Sync(out2)) {
					const ir_edge_t *edge3;
					foreach_out_edge(out2, edge3) {
						ir_node *out3 = get_edge_src_irn(edge3);
						/* Phi or End represents a usage at block end. */
						if(is_Phi(out3) || is_End(out3))
							continue;
						assert(!is_Sync(out3));
						if(sched_get_time_step(out3) <= sched_get_time_step(after)) {
							return 0;
						}
					}
				} else if(sched_get_time_step(out2) <= sched_get_time_step(after)) {
					return 0;
				}
			}
		} else {
			/* phi represents a usage at block end */
			if(is_Phi(out))
				continue;
			if(sched_get_time_step(out) <= sched_get_time_step(after)) {
				return 0;
			}
		}
	}

	return 1;
}

static void rematerialize_or_move(ir_node *flags_needed, ir_node *node,
                                  ir_node *flag_consumers, int pn, be_lv_t *lv)
{
	ir_node *n;
	ir_node *copy;
	ir_node *value;

	if(!is_Block(node) &&
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

	if(get_irn_mode(copy) == mode_T) {
		ir_node *block = get_nodes_block(copy);
		ir_mode *mode  = flag_class->mode;
		value = new_rd_Proj(NULL, block, copy, mode, pn);
	} else {
		value = copy;
	}

	n = flag_consumers;
	do {
		int i;
		int arity = get_irn_arity(n);
		for(i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(n, i);
			in = skip_Proj(in);
			if(in == flags_needed) {
				set_irn_n(n, i, value);
				break;
			}
		}
		n = get_irn_link(n);
	} while(n != NULL);

	/* No need to introduce the copy, because it only lives in this block, but
	 * we have to update the liveness of all operands */
	if (is_Block(node) ||
			get_nodes_block(node) != get_nodes_block(flags_needed)) {
		int i;

		for (i = get_irn_arity(copy) - 1; i >= 0; --i) {
			be_liveness_update(lv, get_irn_n(copy, i));
		}
	}
}

static int is_modify_flags(ir_node *node) {
	int i, arity;

	if (arch_irn_is(node, modify_flags))
		return 1;
	if(!be_is_Keep(node))
		return 0;

	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		ir_node *in = get_irn_n(node, i);
		in = skip_Proj(in);
		if (arch_irn_is(in, modify_flags))
			return 1;
	}

	return 0;
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
	ir_node *node;
	ir_node *flags_needed   = NULL;
	ir_node *flag_consumers = NULL;
	int      pn = -1;

	sched_foreach_reverse(block, node) {
		int i, arity;
		ir_node *new_flags_needed = NULL;

		if(is_Phi(node))
			break;

		if(node == flags_needed) {
			/* all ok */
			flags_needed   = NULL;
			flag_consumers = NULL;
		}

		/* test whether node destroys the flags */
		if(flags_needed != NULL && is_modify_flags(node)) {
			/* rematerialize */
			rematerialize_or_move(flags_needed, node, flag_consumers, pn, env);
			flags_needed   = NULL;
			flag_consumers = NULL;
		}

		/* test whether the current node needs flags */
		arity = get_irn_arity(node);
		for(i = 0; i < arity; ++i) {
			const arch_register_class_t *cls = arch_get_irn_reg_class(node, i);
			if(cls == flag_class) {
				assert(new_flags_needed == NULL);
				new_flags_needed = get_irn_n(node, i);
			}
		}

		if(new_flags_needed == NULL)
			continue;

		/* spiller can't (correctly) remat flag consumers at the moment */
		assert(!arch_irn_is(node, rematerializable));

		if(skip_Proj(new_flags_needed) != flags_needed) {
			if(flags_needed != NULL) {
				/* rematerialize node */
				rematerialize_or_move(flags_needed, node, flag_consumers, pn, env);
				flags_needed   = NULL;
				flag_consumers = NULL;
			}

			flags_needed = new_flags_needed;
			arch_set_irn_register(flags_needed, flags_reg);
			if(is_Proj(flags_needed)) {
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

	if(flags_needed != NULL) {
		assert(get_nodes_block(flags_needed) != block);
		rematerialize_or_move(flags_needed, node, flag_consumers, pn, env);
		flags_needed   = NULL;
		flag_consumers = NULL;
	}

	assert(flags_needed   == NULL);
	assert(flag_consumers == NULL);
}

void be_sched_fix_flags(be_irg_t *birg, const arch_register_class_t *flag_cls,
                        func_rematerialize remat_func)
{
	ir_graph *irg = be_get_birg_irg(birg);

	flag_class = flag_cls;
	flags_reg  = & flag_class->regs[0];
	remat      = remat_func;
	changed    = 0;
	if(remat == NULL)
		remat = &default_remat;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, fix_flags_walker, NULL, birg->lv);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	if(changed) {
		be_remove_dead_nodes_from_schedule(birg);
	}
}
