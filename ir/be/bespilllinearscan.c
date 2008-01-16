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
 * @brief       Linear Scan Spill algorithm
 * @author      Matthias Braun
 * @date        20.09.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode_t.h"
#include "irprintf.h"
#include "iredges_t.h"

#include "bemodule.h"
#include "besched_t.h"
#include "bespilloptions.h"
#include "bespill.h"
#include "benode_t.h"
#include "be_t.h"
#include "belive_t.h"

/* a place in the program */
typedef struct place_t {
	unsigned  block_nr;
	int       timestep;
} place_t;

typedef struct interval_t {
	ir_node *value;
	double   spill_costs;
	place_t  begin;
	place_t  end;
} interval_t;

static interval_t **intervals;

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static struct obstack               obst;
static const arch_env_t            *arch_env;
static const arch_register_class_t *cls;
static spill_env_t                 *spill_env;
static unsigned                     n_regs;
static const be_lv_t               *lv;

static double get_spill_costs(ir_node *node)
{
	const ir_edge_t *edge;
	ir_node         *spill_place = skip_Proj(node);
	double           costs       = be_get_spill_costs(spill_env, node,
	                                                  spill_place);
	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);

		/* keeps should be directly below the node */
		if(be_is_Keep(use)) {
			continue;
		}

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			costs += be_get_reload_costs_on_edge(spill_env, node, block, in);
		} else {
			costs += be_get_reload_costs(spill_env, node, use);
		}
	}

	return costs;
}

/**
 * spills a node by placing a reload before each usage
 */
static void spill_node(ir_node *node)
{
	const ir_edge_t *edge;

	DBG((dbg, LEVEL_3, "\tspilling %+F\n", node));

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);
		if(is_Anchor(use))
			continue;
		if(be_is_Keep(use))
			continue;

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			be_add_reload_on_edge(spill_env, node, block, in, cls, 1);
		} else {
			be_add_reload(spill_env, node, use, cls, 1);
		}
	}
}


static int place_less(const place_t *place1, const place_t *place2)
{
	if(place1->block_nr < place2->block_nr)
		return 1;
	if(place1->block_nr > place2->block_nr)
		return 0;

	return place1->timestep < place2->timestep;
}

static int place_equal(const place_t *place1, const place_t *place2)
{
	return place1->block_nr == place2->block_nr
		&& place1->timestep == place2->timestep;
}

static void extend_interval(ir_node *value, const place_t *place)
{
	interval_t *interval;

	if(!irn_visited(value)) {
		interval = obstack_alloc(&obst, sizeof(interval[0]));
		interval->begin       = *place;
		interval->end         = *place;
		interval->value       = value;
		interval->spill_costs = 0;

		ARR_APP1(interval_t*, intervals, interval);

		set_irn_link(value, interval);
		mark_irn_visited(value);
	} else {
		interval = get_irn_link(value);

		if(place_less(place, &interval->begin)) {
			interval->begin = *place;
		}
		if(place_less(&interval->end, place)) {
			interval->end = *place;
		}
	}
}

/**
 * link live intervals to values, put all intervals into a list,
 * sort the list. We process the blocks in a toplogical order (by ignoring
 * backedges).
 */
static void calculate_liveness_intervals(ir_node *block, unsigned block_nr)
{
	ir_node *node;
	int      i, arity;
	place_t  place;

	set_irn_link(block, INT_TO_PTR(block_nr));

	place.block_nr = block_nr;
	place.timestep = 0;

	be_lv_foreach(lv, block, be_lv_state_in, i) {
		ir_node *node = be_lv_get_irn(lv, block, i);
		if(!arch_irn_consider_in_reg_alloc(arch_env, cls, node))
			continue;

		extend_interval(node, &place);
	}

	sched_foreach_reverse(block, node) {

		if(is_Phi(node))
			break;

		place.timestep = sched_get_time_step(node);

		if(get_irn_mode(node) == mode_T) {
			const ir_edge_t *edge;

			foreach_out_edge(node, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if(arch_irn_consider_in_reg_alloc(arch_env, cls, proj)) {
					extend_interval(proj, &place);
				}
			}
		} else if(arch_irn_consider_in_reg_alloc(arch_env, cls, node)) {
			extend_interval(node, &place);
		}

		arity = get_irn_arity(node);
		for(i = 0; i < arity; ++i) {
			ir_node *op = get_irn_n(node, i);

			if(arch_irn_consider_in_reg_alloc(arch_env, cls, op)) {
				extend_interval(op, &place);
			}
		}
	}

	place.timestep++;
	be_lv_foreach(lv, block, be_lv_state_end, i) {
		ir_node *node = be_lv_get_irn(lv, block, i);
		if(!arch_irn_consider_in_reg_alloc(arch_env, cls, node))
			continue;

		extend_interval(node, &place);
	}

	ir_printf("processing block %+F(%u)\n", block, block_nr);
}

static unsigned next_block_nr;

/**
 * process blocks in a toplogical order (we ignore backedges and create a
 * topological order from the remaining edges)
 */
static void process_block(ir_node *block)
{
	unsigned block_nr;
	int      n_preds;
	int      i;

	if(irn_visited(block))
		return;
	mark_irn_visited(block);

	n_preds = get_Block_n_cfgpreds(block);
	for(i = 0; i < n_preds; ++i) {
		ir_node *pred_block;

		if(is_backedge(block, i))
			continue;

		pred_block = get_Block_cfgpred_block(block, i);
		process_block(pred_block);
	}

	block_nr = next_block_nr;
	next_block_nr++;
	calculate_liveness_intervals(block, block_nr);
}

static void print_interval(const interval_t *interval)
{
	ir_fprintf(stderr, "%+F [%u,%d] -> [%u,%d]\n", interval->value,
			interval->begin.block_nr, interval->begin.timestep,
			interval->end.block_nr,	interval->end.timestep);
}

static int compare_spill_costs(const void *d1, const void *d2)
{
	const interval_t *interval1 = *((const interval_t**)d1);
	const interval_t *interval2 = *((const interval_t**)d2);
	if (interval2->spill_costs < interval1->spill_costs)
		return -1;
	return 1;
}

static void do_spilling(void)
{
	interval_t **live_intervals;
	unsigned   n_live_intervals;
	interval_t **intervals_to_allocate;
	unsigned   n_intervals_to_allocate;
	int        i, len;
	unsigned   a;

	live_intervals   = alloca(n_regs * sizeof(live_intervals[0]));
	n_live_intervals = 0;
	intervals_to_allocate = alloca(n_regs * sizeof(intervals_to_allocate[0]));

	len = ARR_LEN(intervals);
	for (i = 0; i < len; ) {
		const place_t place = intervals[i]->begin;
		int           spills_needed;

		n_intervals_to_allocate = 0;
		do {
			interval_t *interval = intervals[i];

			print_interval(interval);

			intervals_to_allocate[n_intervals_to_allocate] = intervals[i];
			++n_intervals_to_allocate;
			++i;
		} while (i < len && place_equal(&intervals[i]->begin, &place));

		spills_needed = n_live_intervals + n_intervals_to_allocate - n_regs;

		/* first expire intervals whose endpoint is above our current place */
		if (spills_needed > 0) {
			unsigned a;

			for (a = 0; a < n_live_intervals; ) {
				interval_t *live_interval = live_intervals[a];
				if(place_less(&place, &live_interval->end)) {
					++a;
				} else {
					fprintf(stderr, "expired: ");
					print_interval(live_interval);
					live_intervals[a] = live_intervals[n_live_intervals-1];
					--n_live_intervals;
				}
			}

			spills_needed = n_live_intervals + n_intervals_to_allocate - n_regs;
		}
		/* spill intervals */
		if (spills_needed > 0) {
			ir_fprintf(stderr, "need to spill %d values at %u,%d\n",
			           spills_needed, place.block_nr, place.timestep);

			for(a = 0; a < n_live_intervals; ++a) {
				interval_t *live_interval = live_intervals[a];
				if(live_interval->spill_costs == 0) {
					ir_node *value             = live_interval->value;
					live_interval->spill_costs = get_spill_costs(value);
					ir_fprintf(stderr, "spillcosts for %+F: %f\n", value,
					           live_interval->spill_costs);
				}
			}

			qsort(live_intervals, n_live_intervals, sizeof(live_intervals[0]),
			      compare_spill_costs);

			a = n_live_intervals - spills_needed;
			for ( ; a < n_live_intervals; ++a) {
				const interval_t *live_interval = live_intervals[a];
				ir_node          *value         = live_interval->value;

				ir_fprintf(stderr, "spilling %+F (%f)\n", value, live_interval->spill_costs);
				spill_node(value);
			}
			n_live_intervals -= spills_needed;
		}

		assert(n_regs - n_live_intervals >= n_intervals_to_allocate);

		for (a = 0; a < n_intervals_to_allocate; ++a) {
			live_intervals[n_live_intervals] = intervals_to_allocate[a];
			++n_live_intervals;
		}
		assert(n_live_intervals <= n_regs);
	}
}

static int cmp_interval(const void *d1, const void *d2)
{
	const interval_t *interval1 = *((const interval_t**) d1);
	const interval_t *interval2 = *((const interval_t**) d2);

	return !place_less(&interval1->begin, &interval2->begin);
}

static void be_spill_linearscan(be_irg_t *birg,
                                const arch_register_class_t *new_cls)
{
	size_t    n_intervals;
	ir_node  *end_block;
	ir_graph *irg = be_get_birg_irg(birg);

	be_liveness_assure_sets(be_assure_liveness(birg));

	arch_env  = be_get_birg_arch_env(birg);
	cls       = new_cls;
	intervals = NEW_ARR_F(interval_t*, 0);
	spill_env = be_new_spill_env(birg);
	lv        = be_get_birg_liveness(birg);
	n_regs    = cls->n_regs - be_put_ignore_regs(birg, new_cls, NULL);

	obstack_init(&obst);

	set_using_irn_visited(irg);
	set_using_irn_link(irg);
	inc_irg_visited(irg);

	next_block_nr = 0;

	/* use toposort for liveness analysis */
	end_block = get_irg_end_block(irg);
	process_block(end_block);

	assert(irn_visited(get_irg_start_block(irg)));

	n_intervals = ARR_LEN(intervals);
	qsort(intervals, n_intervals, sizeof(intervals[0]), cmp_interval);

	do_spilling();

	clear_using_irn_visited(irg);
	clear_using_irn_link(irg);

	DEL_ARR_F(intervals);
	obstack_free(&obst, NULL);

	/* Insert spill/reload nodes into the graph and fix usages */
	be_insert_spills_reloads(spill_env);

	be_delete_spill_env(spill_env);
	spill_env = NULL;
}

void be_init_spilllinearscan(void)
{
	static be_spiller_t spiller = {
		be_spill_linearscan
	};

	be_register_spiller("linearscan", &spiller);
	FIRM_DBG_REGISTER(dbg, "firm.be.spill.linearscan");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spilllinearscan);
