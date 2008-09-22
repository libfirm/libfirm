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
 * @brief       MIN-algorithm with some twists (aka belady spiller v3)
 * @author      Matthias Braun
 * @date        15.01.2008
 * @version     $Id$
 *
 * TODO:
 *   - handle phis correctly, decide whether we should spill them ~ok
 *   - merge multiple start worksets of blocks ~ok
 *   - how to and when to do the tentative phase...
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdbool.h>

#include "debug.h"
#include "list.h"
#include "pdeq.h"

#include "irnode_t.h"
#include "irprintf.h"
#include "iredges_t.h"
#include "irloop_t.h"
#include "irgwalk.h"
#include "execfreq.h"

#include "bemodule.h"
#include "bespill.h"
#include "beutil.h"
#include "bespilloptions.h"
#include "besched_t.h"
#include "be_t.h"

#define EXPENSIVE_CHECKS

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct loop_edge_t loop_edge_t;
struct loop_edge_t {
	ir_node     *block;
	int          pos;
	loop_edge_t *next;
};

typedef struct loop_info_t loop_info_t;
struct loop_info_t {
	ir_loop     *loop;
	loop_edge_t *entry_edges;
	loop_edge_t *exit_edges;
	size_t       max_register_pressure;
};

typedef struct worklist_entry_t worklist_entry_t;
struct worklist_entry_t {
	struct list_head  head;
	ir_node          *value;
	ir_node          *reload_point;
	ir_loop          *unused_livethrough_loop;
};

typedef struct worklist_t worklist_t;
struct worklist_t {
	struct list_head  live_values;
	size_t            n_live_values;
	ir_visited_t      visited;
};

typedef struct block_info_t block_info_t;
struct block_info_t {
	worklist_t *start_worklist;
	worklist_t *end_worklist;
};

static const arch_env_t            *arch_env;
static const arch_register_class_t *cls;
static struct obstack               obst;
static spill_env_t                 *senv;
static size_t                       n_regs;
static size_t                       max_register_pressure;
static bool                         tentative_mode;
static bool                         should_have_reached_fixpoint;
static bool                         do_push_unused_livethroughs;
static ir_exec_freq                *exec_freq;
static ir_visited_t                 worklist_visited;

static worklist_t *new_worklist(void)
{
	worklist_t *worklist = obstack_alloc(&obst, sizeof(worklist[0]));
	memset(worklist, 0, sizeof(worklist[0]));

	INIT_LIST_HEAD(&worklist->live_values);
	worklist->n_live_values    = 0;

	return worklist;
}

static void mark_irn_not_visited(ir_node *node)
{
	set_irn_visited(node, get_irg_visited(current_ir_graph) - 1);
}

static bool worklist_contains(const ir_node *node)
{
	return irn_visited(node);
}

static block_info_t *get_block_info(ir_node *block)
{
	block_info_t *info = get_irn_link(block);
	if (info != NULL)
		return info;

	info = obstack_alloc(&obst, sizeof(info[0]));
	memset(info, 0, sizeof(info[0]));
	set_irn_link(block, info);
	return info;
}

static void deactivate_worklist(const worklist_t *worklist)
{
	struct list_head *entry;

	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);
		assert(worklist_contains(wl_entry->value));
		mark_irn_not_visited(wl_entry->value);
		set_irn_link(wl_entry->value, NULL);
	}
}

static void activate_worklist(const worklist_t *worklist)
{
	struct list_head *entry;

	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);
		assert(!worklist_contains(wl_entry->value));
		mark_irn_visited(wl_entry->value);
		set_irn_link(wl_entry->value, wl_entry);
	}
}

/**
 * duplicate a worklist and directly activates it
 */
static void fill_and_activate_worklist(worklist_t *new_worklist,
		const worklist_t *worklist, ir_node *block, ir_node *succ_block,
		int succ_pos)
{
	ir_node          *reload_point  = NULL;
	size_t            n_live_values = 0;
	struct list_head *entry;

	if (succ_block != NULL &&
			(get_Block_n_cfgpreds(succ_block) > 1
			 || get_irn_n_edges_kind(block, EDGE_KIND_BLOCK) > 1)) {
		reload_point = be_get_end_of_block_insertion_point(block);
	}

	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry  = list_entry(entry, worklist_entry_t, head);
		ir_node          *value     = wl_entry->value;
		worklist_entry_t *new_entry;

		if (new_worklist->n_live_values >= n_regs)
			break;

		if (is_Phi(value) && get_nodes_block(value) == succ_block) {
			value = get_Phi_pred(value, succ_pos);

			/* can happen for unknown phi preds */
			if (!arch_irn_consider_in_reg_alloc(arch_env, cls, value))
				continue;
		}

		if (irn_visited(value))
			continue;

		new_entry = obstack_alloc(&obst, sizeof(new_entry[0]));
		memset(new_entry, 0, sizeof(new_entry[0]));

		new_entry->value = value;
		if (reload_point != NULL) {
			new_entry->reload_point = reload_point;
		} else {
			new_entry->reload_point = wl_entry->reload_point;
		}

		list_add_tail(&new_entry->head, &new_worklist->live_values);
		++n_live_values;

		mark_irn_visited(value);
		set_irn_link(value, new_entry);
		new_worklist->n_live_values++;
	}
}

static worklist_t *duplicate_worklist(const worklist_t *worklist)
{
	worklist_t       *new_worklist;
	struct list_head *entry;

	new_worklist = obstack_alloc(&obst, sizeof(new_worklist[0]));
	memset(new_worklist, 0, sizeof(new_worklist[0]));
	INIT_LIST_HEAD(&new_worklist->live_values);
	new_worklist->n_live_values = worklist->n_live_values;

	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry  = list_entry(entry, worklist_entry_t, head);
		worklist_entry_t *new_entry
			= obstack_alloc(&obst, sizeof(new_entry[0]));

		memcpy(new_entry, wl_entry, sizeof(new_entry[0]));
		list_add_tail(&new_entry->head, &new_worklist->live_values);
	}

	return new_worklist;
}

#ifdef DEBUG_libfirm
static void print_worklist(const worklist_t *worklist, int level)
{
	struct list_head *entry;

	DB((dbg, level, "%d values: ", worklist->n_live_values));
	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);

		DB((dbg, level, "%+F ", wl_entry->value));
	}
}
#endif



static void clear_loop_info(ir_loop *loop)
{
	int n_elements = get_loop_n_elements(loop);
	int i;

	loop->link = NULL;
	for (i = 0; i < n_elements; ++i) {
		loop_element element = get_loop_element(loop, i);
		if (*element.kind != k_ir_loop)
			continue;

		clear_loop_info(element.son);
	}
}

static loop_info_t *get_loop_info(ir_loop *loop)
{
	loop_info_t *info = loop->link;
	if (info != NULL)
		return info;

	info = obstack_alloc(&obst, sizeof(info[0]));
	memset(info, 0, sizeof(info[0]));
	info->loop = loop;
	loop->link = info;
	return info;
}

/**
 * constructs loop in+out edges when called in a block walker
 */
static void construct_loop_edges(ir_node *block, void *data)
{
	int      n_cfgpreds = get_Block_n_cfgpreds(block);
	ir_loop *loop       = get_irn_loop(block);
	int      i;

	(void) data;

	for (i = 0; i < n_cfgpreds; ++i) {
		ir_node     *cfgpred_block = get_Block_cfgpred_block(block, i);
		ir_loop     *cfgpred_loop  = get_irn_loop(cfgpred_block);
		loop_edge_t *edge;
		bool        is_exit_edge;
		ir_loop     *l, *goal;

		if (cfgpred_loop == loop)
			continue;

		/* critical edges are splitted, so we can't jump from 1 loop
		 * directly into another without going out of it */
		assert(get_loop_depth(cfgpred_loop) != get_loop_depth(loop));

		/* edge out of loop */
		if (get_loop_depth(cfgpred_loop) > get_loop_depth(loop)) {
			is_exit_edge = true;
			l            = cfgpred_loop;
			goal         = loop;
		} else {
			is_exit_edge = false;
			l            = loop;
			goal         = cfgpred_loop;
		}

		/* this might be a jump out/in multiple loops */
		do {
			loop_info_t *l_info = get_loop_info(l);

			edge = obstack_alloc(&obst, sizeof(edge[0]));
			memset(edge, 0, sizeof(edge[0]));
			edge->block = block;
			edge->pos   = i;

			if (is_exit_edge) {
				edge->next         = l_info->exit_edges;
				l_info->exit_edges = edge;
				assert(get_loop_depth(loop) < get_loop_depth(l));
			} else {
				edge->next          = l_info->entry_edges;
				l_info->entry_edges = edge;
				assert(get_loop_depth(cfgpred_loop) < get_loop_depth(l));
			}
			l = get_loop_outer_loop(l);
		} while(l != goal);
	}
}




static void place_reload(worklist_entry_t *entry)
{
	if (tentative_mode)
		return;

	DB((dbg, LEVEL_1, "reload of %+F before %+F\n", entry->value,
		entry->reload_point));
	assert(entry->reload_point != NULL);
	be_add_reload(senv, entry->value, entry->reload_point, cls, 1);
	entry->reload_point = NULL;
}

/**
 * makes sure the worklist contains not more than n_regs - room_needed entries
 */
static void make_room(worklist_t *worklist, size_t room_needed)
{
	int               i;
	int               spills_needed;
	struct list_head *entry;

	spills_needed = worklist->n_live_values + room_needed - n_regs;
	if (spills_needed <= 0)
		return;

	entry = worklist->live_values.next;
	for(i = spills_needed; i > 0; --i) {
		struct list_head *next = entry->next;
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);

		assert(worklist_contains(wl_entry->value));
		mark_irn_not_visited(wl_entry->value);
		place_reload(wl_entry);
		list_del(entry);

		entry = next;
	}
	worklist->n_live_values -= spills_needed;
}

/**
 * a value was used, so bring it to the back of the worklist (which might
 * result in a spill of another value).
 */
static void val_used(worklist_t *worklist, ir_node *value, ir_node *sched_point)
{
	/* already in the worklist? move around, otherwise add at back */
	worklist_entry_t *entry = get_irn_link(value);

	assert(arch_irn_consider_in_reg_alloc(arch_env, cls, value));

	if (worklist_contains(value)) {
		assert(entry != NULL);

		list_del(&entry->head);
	} else {
		if (entry == NULL) {
			entry        = obstack_alloc(&obst, sizeof(entry[0]));
			memset(entry, 0, sizeof(entry[0]));

			entry->value = value;
			set_irn_link(value, entry);
		}

		++worklist->n_live_values;
		mark_irn_visited(value);
	}

	entry->reload_point = sched_point;
	list_add_tail(&entry->head, &worklist->live_values);
}

static void worklist_remove(worklist_t *worklist, ir_node *value)
{
	worklist_entry_t *entry = get_irn_link(value);
	assert(entry != NULL);
	list_del(&entry->head);
	--worklist->n_live_values;

	assert(worklist_contains(value));
	mark_irn_not_visited(value);
}

static void update_max_pressure(worklist_t *worklist)
{
	if (worklist->n_live_values > max_register_pressure)
		max_register_pressure = worklist->n_live_values;
}

static void do_spilling(ir_node *block, worklist_t *worklist)
{
	ir_node *node;

	assert(worklist != NULL);

	sched_foreach_reverse(block, node) {
		int    i, arity;
		size_t n_defs = 0;

		DB((dbg, LEVEL_2, "\t%+F... ", node));
		update_max_pressure(worklist);

		if (is_Phi(node)) {
			ir_node *node2;
			/* TODO: if we have some free registers, then we could decide to
			 * not spill some phis (but not for phis where at least 1 input is
			 * themselfes) */

			/* we have to spill all phis that are not live */
			sched_foreach_reverse_from(node, node2) {
				assert(is_Phi(node2));

				if (worklist_contains(node2))
					continue;
				if (!arch_irn_consider_in_reg_alloc(arch_env, cls, node2))
					continue;

				if (!tentative_mode)
					be_spill_phi(senv, node2);
			}
			DB((dbg, LEVEL_2, "\n"));
			break;
		}

		/* remove values defined by this instruction from the workset. Values
		 * defined but not in the workset need free registers */
		if (get_irn_mode(node) == mode_T) {
			const ir_edge_t *edge;

			foreach_out_edge(node, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if (!arch_irn_consider_in_reg_alloc(arch_env, cls, proj))
					continue;
				if (worklist_contains(proj)) {
					worklist_remove(worklist, proj);
				} else {
					++n_defs;
				}
			}
		} else if (arch_irn_consider_in_reg_alloc(arch_env, cls, node)) {
			if (worklist_contains(node)) {
				worklist_remove(worklist, node);
			} else {
				n_defs = 1;
			}
		}

		/* make sure we have enough free registers for the spills */
		make_room(worklist, n_defs);

		/* put all values used by the instruction into the workset */
		arity = get_irn_arity(node);
		for(i = 0; i < arity; ++i) {
			ir_node *use = get_irn_n(node, i);

			if (!arch_irn_consider_in_reg_alloc(arch_env, cls, use))
				continue;

			val_used(worklist, use, node);
		}

		/* we might have too many values in the worklist now and need to spill
		 * some */
		make_room(worklist, 0);

#ifdef DEBUG_libfirm
		print_worklist(worklist, LEVEL_2);
		DB((dbg, LEVEL_2, "\n"));
#endif
	}

	update_max_pressure(worklist);

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_1, "worklist at begin of %+F:", block));
	print_worklist(worklist, LEVEL_1);
	DB((dbg, LEVEL_1, "\n"));
#endif
}

static bool worklists_equal(const worklist_t *wl1, const worklist_t *wl2)
{
	const struct list_head *i1 = &wl1->live_values;
	const struct list_head *i2 = &wl2->live_values;

	for ( ; i1 != &wl1->live_values && i2 != &wl2->live_values;
			i1 = i1->next, i2 = i2->next) {
		worklist_entry_t *entry1 = list_entry(i1, worklist_entry_t, head);
		worklist_entry_t *entry2 = list_entry(i2, worklist_entry_t, head);

		if (entry1->value != entry2->value)
			return false;
	}
	/* both lists at the end */
	if (i1 != &wl1->live_values || i2 != &wl2->live_values)
		return false;

	return true;
}

static bool fill_start_worklist(worklist_t *new_worklist, ir_node *block)
{
	double           best_execfreq   = -1;
	worklist_t      *best_worklist   = NULL;
	ir_node         *best_succ_block = NULL;
	int              best_pos;
	const ir_edge_t *edge;

	/* construct worklist */
	foreach_block_succ(block, edge) {
		ir_node      *succ_block = get_edge_src_irn(edge);
		double       execfreq    = get_block_execfreq(exec_freq, succ_block);
		block_info_t *block_info;
		worklist_t   *succ_worklist;

		if (execfreq < best_execfreq)
			continue;

		block_info    = get_block_info(succ_block);
		succ_worklist = block_info->start_worklist;

		if (succ_worklist == NULL || succ_worklist->visited >= worklist_visited)
			continue;

		best_execfreq   = execfreq;
		best_worklist   = succ_worklist;
		best_succ_block = succ_block;
		best_pos        = get_edge_src_pos(edge);
	}

	if (best_worklist == NULL)
		return false;
	best_worklist->visited = worklist_visited;

	fill_and_activate_worklist(new_worklist, best_worklist, block,
			best_succ_block, best_pos);
	return true;
}

static worklist_t *construct_start_worklist(ir_node *block)
{
	worklist_t *worklist = new_worklist();

	++worklist_visited;

	while(fill_start_worklist(worklist, block)) {
		if (worklist->n_live_values >= n_regs)
			break;
	}

	return worklist;
}

static void process_block(ir_node *block, void *env)
{
	block_info_t    *block_info;
	worklist_t      *worklist;
	int              n_preds;

	(void) env;
	DB((dbg, LEVEL_1, "Processing %+F\n", block));

	worklist   = construct_start_worklist(block);
	block_info = get_block_info(block);

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_1, "worklist at end of %+F:", block));
	print_worklist(worklist, LEVEL_1);
	DB((dbg, LEVEL_1, "\n"));

	if (should_have_reached_fixpoint) {
		assert(worklists_equal(block_info->end_worklist, worklist));
	}
#endif
	block_info->end_worklist = duplicate_worklist(worklist);

	do_spilling(block, worklist);
	deactivate_worklist(worklist);

#ifdef DEBUG_libfirm
	if (should_have_reached_fixpoint) {
		assert(worklists_equal(block_info->start_worklist, worklist));
	}
#endif
	block_info->start_worklist = worklist;

	/* we shouldn't have any live values left at the start block */
	n_preds = get_Block_n_cfgpreds(block);
	//assert(n_preds != 0 || worklist->n_live_values == 0);
}

typedef struct block_or_loop_t block_or_loop_t;
struct block_or_loop_t {
	union {
		ir_node *block;
		ir_loop *loop;
	} v;
	bool is_loop;
};

static block_or_loop_t *loop_blocks;
static ir_loop         *current_loop;

static void find_blocks(ir_node *block);

static void find_in_loop(ir_loop *loop, ir_node *entry)
{
	loop_info_t     *loop_info = get_loop_info(loop);
	block_or_loop_t block_or_loop;
	loop_edge_t     *edge;

	/* simply mark 1 block in the loop to indicate that the loop was already
	 * processed */
	ir_node *some_block = loop_info->entry_edges->block;
	if (Block_block_visited(some_block))
		return;

	block_or_loop.v.loop  = loop;
	block_or_loop.is_loop = true;
	ARR_APP1(block_or_loop_t, loop_blocks, block_or_loop);

#ifndef NDEBUG
	{
		/* we should be 1 of the entry blocks */
		loop_edge_t *edge  = loop_info->entry_edges;
		bool         found = false;
		for ( ; edge != NULL; edge = edge->next) {
			if (edge->block == entry)
				found = true;
		}
		assert(found);
	}
#endif
	/* check all loop successors */
	for (edge = loop_info->exit_edges; edge != NULL; edge = edge->next) {
		ir_node *succ      = edge->block;
		ir_loop *succ_loop = get_irn_loop(succ);

		if (succ_loop == current_loop) {
			find_blocks(succ);
		} else {
			assert(get_loop_depth(succ_loop) < get_loop_depth(current_loop));
		}
	}
}

static void find_blocks(ir_node *block)
{
	const ir_edge_t *edge;
	block_or_loop_t block_or_loop;

	if (Block_block_visited(block))
		return;

	block_or_loop.v.block = block;
	block_or_loop.is_loop = false;

	ARR_APP1(block_or_loop_t, loop_blocks, block_or_loop);
	mark_Block_block_visited(block);

	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		/* is it part of the current loop? */
		ir_loop *loop = get_irn_loop(succ);
		if (loop != current_loop) {
			/* a sub-loop? */
			if (get_loop_depth(loop) > get_loop_depth(current_loop)) {
				find_in_loop(loop, succ);
			} else {
				/* parent loop: we're not interested in the block */
			}
		} else {
			find_blocks(succ);
		}
	}
}

/**
 * append an entry to a worklist. WARNING: The entry must not already be in the
 * worklist.
 */
static void worklist_append(worklist_t *worklist, ir_node *value,
                            ir_node *reload_point,
							ir_loop *unused_livethrough_loop)
{
	worklist_entry_t *entry     = obstack_alloc(&obst, sizeof(entry[0]));
	memset(entry, 0, sizeof(entry[0]));

#ifdef EXPENSIVE_CHECKS
	{
		struct list_head *entry;
		list_for_each(entry, &worklist->live_values) {
			worklist_entry_t *wl_entry
				= list_entry(entry, worklist_entry_t, head);
			assert(wl_entry->value != value);
		}
	}
#endif

	entry->value                   = value;
	entry->reload_point            = reload_point;
	entry->unused_livethrough_loop = unused_livethrough_loop;
	list_add_tail(&entry->head, &worklist->live_values);
	++worklist->n_live_values;
	assert(worklist->n_live_values <= n_regs);
}

static void push_unused_livethrough(loop_info_t *loop_info, ir_node *value)
{
	loop_edge_t *edge;
	++worklist_visited;

	/* add the value to all loop exit and entry blocks */
	for (edge = loop_info->exit_edges; edge != NULL; edge = edge->next) {
		ir_node            *block
			= get_Block_cfgpred_block(edge->block, edge->pos);
		const block_info_t *info     = get_block_info(block);
		worklist_t         *worklist = info->end_worklist;
		ir_node            *reload_point = NULL;

		if (worklist->visited >= worklist_visited)
			continue;
		worklist->visited = worklist_visited;

		/* TODO: we need a smarter mechanism here, that makes the reloader place
		 * reload nodes on all loop exits... */

		worklist_append(worklist, value, reload_point, loop_info->loop);
	}
	edge = loop_info->entry_edges;
	for ( ; edge != NULL; edge = edge->next) {
		ir_node            *entry_block = edge->block;
		const block_info_t *info        = get_block_info(entry_block);
		worklist_t         *worklist    = info->start_worklist;
		ir_node            *pred_block;
		ir_node            *reload_point;

		if (worklist->visited >= worklist_visited)
			continue;
		worklist->visited = worklist_visited;

		pred_block   = get_Block_cfgpred_block(entry_block, edge->pos);
		reload_point = be_get_end_of_block_insertion_point(pred_block);

		worklist_append(worklist, value, reload_point, loop_info->loop);
	}

	set_irn_link(value, NULL);
	++loop_info->max_register_pressure;
}

static void push_unused_livethroughs(loop_info_t *loop_info)
{
	loop_edge_t *edge;

	/* we can only push unused livethroughs if register pressure inside the loop
	 * was low enough */
	if (loop_info->max_register_pressure >= n_regs)
		return;

	/* find unused livethroughs: register pressure in the loop was low enough
	 * which means that we had no spills which implies that at every point in
	 * the loop all*/
	for (edge = loop_info->exit_edges; edge != NULL; edge = edge->next) {
		ir_node            *block          = edge->block;
		const block_info_t *info           = get_block_info(block);
		worklist_t         *start_worklist = info->start_worklist;
		ir_node            *exit_block;
		const block_info_t *exit_info;
		worklist_t         *end_worklist;
		struct list_head   *entry;

		if (start_worklist == NULL)
			continue;

		exit_block   = get_Block_cfgpred_block(edge->block, edge->pos);
		exit_info    = get_block_info(exit_block);
		end_worklist = exit_info->end_worklist;

		activate_worklist(end_worklist);
		/* all values contained in the start_worklist, which are not available
		 * in the end_worklist, must be unused livethroughs */

		list_for_each(entry, &start_worklist->live_values) {
			worklist_entry_t *wl_entry
				= list_entry(entry, worklist_entry_t, head);
			ir_node *value = wl_entry->value;

			if (loop_info->max_register_pressure >= n_regs)
				break;


			if (!worklist_contains(value)) {
				/* add it to all loop exits */
				DB((dbg, LEVEL_2, "Found unused livethrough: %+F (regpressure in loop %d)\n", value, loop_info->max_register_pressure));
				push_unused_livethrough(loop_info, value);
				/* the value should now be part of end_worklist, so mark it */
				mark_irn_visited(value);
			}
		}

		deactivate_worklist(end_worklist);
	}
}

static void process_block_or_loop(const block_or_loop_t *block_or_loop)
{
	if (block_or_loop->is_loop) {
		loop_info_t *loop_info = get_loop_info(block_or_loop->v.loop);

		if (do_push_unused_livethroughs)
			push_unused_livethroughs(loop_info);

		if (loop_info->max_register_pressure > max_register_pressure)
			max_register_pressure = loop_info->max_register_pressure;

		return;
	}
	process_block(block_or_loop->v.block, NULL);
}

static void process_loop(ir_loop *loop)
{
	int         n_elements = get_loop_n_elements(loop);
	int         i, len;
	loop_info_t *loop_info;
	loop_edge_t *edge;
	ir_node     *some_block;

	/* first handle all sub-loops */
	for (i = 0; i < n_elements; ++i) {
		loop_element element = get_loop_element(loop, i);
		if (*element.kind != k_ir_loop)
			continue;

		process_loop(element.son);
	}

	/* create a postorder of the blocks */
	loop_info = get_loop_info(loop);
	edge      = loop_info->entry_edges;
	if (edge != NULL) {
		some_block = edge->block;
	} else {
		assert(loop == get_irg_loop(current_ir_graph));
		some_block = get_irg_start_block(current_ir_graph);
	}

	loop_blocks  = NEW_ARR_F(block_or_loop_t,0);
	current_loop = loop;

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(current_ir_graph);
	find_blocks(some_block);
	/* for endless loops the end-block might be unreachable */
	if (loop == get_irg_loop(current_ir_graph)) {
		find_blocks(get_irg_end_block(current_ir_graph));
	}
	ir_free_resources(current_ir_graph, IR_RESOURCE_BLOCK_VISITED);

	DB((dbg, LEVEL_3, "Block List for loop %p\n", loop));
	len = ARR_LEN(loop_blocks);
	for (i = 0; i < len; ++i) {
		block_or_loop_t *block_or_loop = &loop_blocks[i];
		if (block_or_loop->is_loop) {
			DB((dbg, LEVEL_3, " L-%p", block_or_loop->v.loop));
		} else {
			DB((dbg, LEVEL_3, " B-%+F", block_or_loop->v.block));
		}
	}
	DB((dbg, LEVEL_3, "\n"));

	max_register_pressure = 0;

	/* run1: tentative phase */
	tentative_mode = true;
	for (i = len-1; i >= 0; --i) {
		process_block_or_loop(&loop_blocks[i]);
	}

	/* run2: tentative phase - should reach fixpoint */
	tentative_mode = true;
	for (i = len-1; i >= 0; --i) {
		process_block_or_loop(&loop_blocks[i]);
	}

#ifndef NDEBUG
	/* run3: tentative phase - check fixpoint */
	tentative_mode               = true;
	should_have_reached_fixpoint = true;
	for (i = len-1; i >= 0; --i) {
		process_block_or_loop(&loop_blocks[i]);
	}
	should_have_reached_fixpoint = false;
#endif

	/* run4: add spills/reloads */
	tentative_mode              = false;
	do_push_unused_livethroughs = true;
	for (i = len-1; i >= 0; --i) {
		process_block_or_loop(&loop_blocks[i]);
	}
	do_push_unused_livethroughs = false;

	loop_info->max_register_pressure = max_register_pressure;
	DB((dbg, LEVEL_2, "Regpressure in loop %p: %u\n", loop,
				(unsigned) max_register_pressure));

	DEL_ARR_F(loop_blocks);
}

static void fix_block_borders(ir_node *block, void *data)
{
	block_info_t *block_info     = get_block_info(block);
	worklist_t   *start_worklist = block_info->start_worklist;
	int           n_cfgpreds     = get_Block_n_cfgpreds(block);
	ir_loop      *loop           = get_irn_loop(block);
	int           i;

	(void) data;
	assert(start_worklist != NULL);

	for (i = 0; i < n_cfgpreds; ++i) {
		ir_node      *pred_block      = get_Block_cfgpred_block(block, i);
		block_info_t *pred_block_info = get_block_info(pred_block);
		worklist_t   *end_worklist    = pred_block_info->end_worklist;
		ir_loop      *pred_loop       = get_irn_loop(pred_block);
		bool          is_loop_entry   = false;
		struct list_head *entry;

		assert(end_worklist != NULL);

		if (get_loop_depth(pred_loop) < get_loop_depth(loop)) {
			is_loop_entry = true;
		}

		/* reload missing values */
		activate_worklist(end_worklist);

		list_for_each(entry, &start_worklist->live_values) {
			worklist_entry_t *wl_entry
				= list_entry(entry, worklist_entry_t, head);
			ir_node          *value = wl_entry->value;

			if (is_Phi(value) && get_nodes_block(value) == block) {
				value = get_irn_n(value, i);

				/* we might have unknowns as argument for the phi */
				if (!arch_irn_consider_in_reg_alloc(arch_env, cls, value))
					continue;
			}

			if (worklist_contains(value))
				continue;
			if (wl_entry->unused_livethrough_loop != NULL && !is_loop_entry)
				continue;

			be_add_reload_on_edge(senv, value, block, i, cls, 1);
		}

		deactivate_worklist(end_worklist);
	}
}

static void be_spill_belady3(be_irg_t *birg, const arch_register_class_t *ncls)
{
	ir_graph *irg = be_get_birg_irg(birg);

	cls    = ncls;
	n_regs = cls->n_regs - be_put_ignore_regs(birg, cls, NULL);

	/* shortcut for register classes with ignore regs only */
	if (n_regs == 0)
		return;

	worklist_visited = 0;
	arch_env         = be_get_birg_arch_env(birg);
	exec_freq        = be_get_birg_exec_freq(birg);

	be_clear_links(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED | IR_RESOURCE_IRN_LINK
			| IR_RESOURCE_LOOP_LINK);
	inc_irg_visited(irg);

	obstack_init(&obst);
	senv = be_new_spill_env(birg);

	assure_cf_loop(irg);
	clear_loop_info(get_irg_loop(irg));
	irg_block_walk_graph(irg, construct_loop_edges, NULL, NULL);

	process_loop(get_irg_loop(current_ir_graph));

	irg_block_walk_graph(irg, fix_block_borders, NULL, NULL);

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED | IR_RESOURCE_IRN_LINK
			| IR_RESOURCE_LOOP_LINK);

	be_insert_spills_reloads(senv);

	obstack_free(&obst, NULL);

	/* clean up */
	be_delete_spill_env(senv);
}

void be_init_spillbelady3(void)
{
	static be_spiller_t belady3_spiller = {
		be_spill_belady3
	};

	be_register_spiller("belady3", &belady3_spiller);
	FIRM_DBG_REGISTER(dbg, "firm.be.spill.belady3");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillbelady3);
