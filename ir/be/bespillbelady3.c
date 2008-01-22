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
 *   - handle phis correctly, decide wether we should spill them
 *   - merge multiple start worksets of blocks
 *   - how to and when to do the tentative phase...
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "debug.h"
#include "list.h"
#include "pdeq.h"

#include "irnode_t.h"
#include "irprintf.h"
#include "iredges_t.h"
#include "execfreq.h"

#include "bemodule.h"
#include "bespill.h"
#include "beutil.h"
#include "bespilloptions.h"
#include "besched_t.h"
#include "be_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct worklist_entry_t worklist_entry_t;
struct worklist_entry_t {
	struct list_head  head;
	ir_node          *value;
	unsigned          timestep;
	ir_node          *reload_point;
};

typedef struct worklist_t worklist_t;
struct worklist_t {
	struct list_head  live_values;
	size_t            n_live_values;
	unsigned          current_timestep;
};

static const arch_env_t            *arch_env;
static const arch_register_class_t *cls;
static struct obstack               obst;
static spill_env_t                 *senv;
static size_t                       n_regs;
static int                          tentative_mode;
static ir_exec_freq                *exec_freq;

static void init_worklist(worklist_t *worklist, unsigned timestep)
{
	INIT_LIST_HEAD(&worklist->live_values);
	worklist->n_live_values    = 0;
	worklist->current_timestep = timestep;
}

static void mark_irn_not_visited(ir_node *node)
{
	set_irn_visited(node, get_irg_visited(current_ir_graph) - 1);
}

static void deactivate_worklist(const worklist_t *worklist)
{
	struct list_head *entry;

	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);
		assert(irn_visited(wl_entry->value));
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
		ir_node          *value = wl_entry->value;

		assert(irn_not_visited(value));
		mark_irn_visited(value);
		set_irn_link(value, wl_entry);
	}
}

static worklist_t *duplicate_worklist(const worklist_t *worklist,
                                      ir_node *block,
                                      ir_node *succ_block, int succ_pos)
{
	ir_node          *reload_point = NULL;
	struct list_head *entry;
	worklist_t       *new_worklist = obstack_alloc(&obst, sizeof(new_worklist[0]));

	INIT_LIST_HEAD(&new_worklist->live_values);

	if(succ_block != NULL && get_Block_n_cfgpreds(succ_block) > 1) {
		reload_point = be_get_end_of_block_insertion_point(block);
	}

	new_worklist->current_timestep = worklist->current_timestep;
	new_worklist->n_live_values    = worklist->n_live_values;

	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);
		worklist_entry_t *new_entry
			= obstack_alloc(&obst, sizeof(new_entry[0]));
		ir_node          *value = wl_entry->value;

		if(is_Phi(value) && get_nodes_block(value) == succ_block) {
			value = get_Phi_pred(value, succ_pos);
		}

		new_entry->value        = value;
		new_entry->timestep     = wl_entry->timestep;
		if(reload_point != NULL) {
			new_entry->reload_point = reload_point;
		} else {
			new_entry->reload_point = wl_entry->reload_point;
		}

		list_add_tail(&new_entry->head, &new_worklist->live_values);
	}

	return new_worklist;
}

#ifdef DEBUG_libfirm
static void print_worklist(const worklist_t *worklist, int level)
{
	struct list_head *entry;

	DB((dbg, level, "%d values (TS %u): ", worklist->n_live_values,
	    worklist->current_timestep));
	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);

		//DB((dbg, level, "%+F(%+F) ", wl_entry->value,
		//    wl_entry->reload_point));
		DB((dbg, level, "%+F ", wl_entry->value));
	}
}
#endif

static void place_reload(worklist_entry_t *entry)
{
	if(tentative_mode)
		return;
	DB((dbg, LEVEL_1, "reload of %+F before %+F", entry->value,
	    entry->reload_point));
	be_add_reload(senv, entry->value, entry->reload_point, cls, 1);
}

static void spill_non_live_nodes(const worklist_t *worklist)
{
	struct list_head *entry;

	list_for_each(entry, &worklist->live_values) {
		worklist_entry_t *wl_entry
			= list_entry(entry, worklist_entry_t, head);
		ir_node          *value = wl_entry->value;

		if(irn_visited(value))
			continue;

		place_reload(wl_entry);
	}
}

/**
 * makes sure the worklist contains not more than n_regs - room_needed entries
 */
static void make_room(worklist_t *worklist, size_t room_needed)
{
	int spills_needed = worklist->n_live_values + room_needed - n_regs;
	if(spills_needed > 0) {
		int               i     = spills_needed;
		struct list_head *entry = worklist->live_values.next;
		for(i = spills_needed; i > 0; --i) {
			struct list_head *next = entry->next;
			worklist_entry_t *wl_entry
				= list_entry(entry, worklist_entry_t, head);
			assert(irn_visited(wl_entry->value));
			mark_irn_not_visited(wl_entry->value);
			place_reload(wl_entry);
			list_del(entry);

			entry = next;
		}
		worklist->n_live_values -= spills_needed;
	}
}

/**
 * a value was used, so bring it to the back of the worklist (which might
 * result in a spill of another value).
 */
static void val_used(worklist_t *worklist, ir_node *value, ir_node *sched_point)
{
	/* is the node in the worklist already? */
	worklist_entry_t *entry = get_irn_link(value);
	if(irn_visited(value)) {
		assert(entry != NULL);

		assert(irn_visited(value));
		list_del(&entry->head);
	} else {
		if(entry == NULL) {
			entry        = obstack_alloc(&obst, sizeof(entry[0]));
			entry->value = value;
			set_irn_link(value, entry);
		}

		++worklist->n_live_values;
		mark_irn_visited(value);
	}

	entry->timestep     = worklist->current_timestep;
	entry->reload_point = sched_point;
	list_add_tail(&entry->head, &worklist->live_values);
}

static void worklist_remove(worklist_t *worklist, ir_node *value)
{
	worklist_entry_t *entry = get_irn_link(value);
	assert(entry != NULL);
	list_del(&entry->head);
	--worklist->n_live_values;

	assert(irn_visited(value));
	mark_irn_not_visited(value);
}

static void do_spilling(ir_node *block, worklist_t *worklist)
{
	ir_node *node;

	assert(worklist != NULL);

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_1, "worklist at end of %+F:", block));
	print_worklist(worklist, LEVEL_1);
	DB((dbg, LEVEL_1, "\n"));
#endif

	sched_foreach_reverse(block, node) {
		int    i, arity;
		size_t n_defs = 0;

		DB((dbg, LEVEL_2, "\t%+F... ", node));

		if(is_Phi(node)) {
			ir_node *node2;
			/* TODO: if we have some free registers, then we could decide to
			 * not spill some phis (but not for phis where at least 1 input is
			 * themselfes) */

			/* we have to spill all phis that are not live */
			sched_foreach_reverse_from(node, node2) {
				assert(is_Phi(node2));

				if(irn_visited(node2))
					continue;
				if(!arch_irn_consider_in_reg_alloc(arch_env, cls, node2))
					continue;

				be_spill_phi(senv, node2);
			}
			break;
		}

		/* remove values defined by this instruction from the workset. Values
		 * defined but not in the workset need free registers */
		if(get_irn_mode(node) == mode_T) {
			const ir_edge_t *edge;

			foreach_out_edge(node, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				if(!arch_irn_consider_in_reg_alloc(arch_env, cls, proj))
					continue;
				if(irn_visited(proj)) {
					worklist_remove(worklist, proj);
				} else {
					++n_defs;
				}
			}
		} else if(arch_irn_consider_in_reg_alloc(arch_env, cls, node)) {
			if(irn_visited(node)) {
				worklist_remove(worklist, node);
			} else {
				n_defs = 1;
			}
		}

		/* make sure we have enough free registers for the spills */
		make_room(worklist, n_defs);

		/* put all values by the instruction into the workset */
		arity = get_irn_arity(node);
		for(i = 0; i < arity; ++i) {
			ir_node *use = get_irn_n(node, i);

			if(!arch_irn_consider_in_reg_alloc(arch_env, cls, use))
				continue;

			val_used(worklist, use, node);
		}

		/* we might have too many values in the worklist now and need to spill
		 * some */
		make_room(worklist, 0);

		++worklist->current_timestep;

#ifdef DEBUG_libfirm
		print_worklist(worklist, LEVEL_2);
		DB((dbg, LEVEL_2, "\n"));
#endif
	}

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_1, "worklist at begin of %+F:", block));
	print_worklist(worklist, LEVEL_1);
	DB((dbg, LEVEL_1, "\n"));
#endif
}

static void process_block(ir_node *block, void *env)
{
	int              n_preds;
	const ir_edge_t *edge;
	worklist_t      *worklist        = NULL;
	double           best_execfreq   = -1;
	ir_node         *best_succ_block = NULL;
	int              best_pos        = -1;

	(void) env;
	DB((dbg, LEVEL_1, "Processing %+F\n", block));

	/* construct worklist */
	foreach_block_succ(block, edge) {
		ir_node *succ_block = get_edge_src_irn(edge);
		double   execfreq   = get_block_execfreq(exec_freq, succ_block);

		if(execfreq > best_execfreq) {
			worklist_t *succ_worklist = get_irn_link(succ_block);
			if(succ_worklist != NULL) {
				best_execfreq   = execfreq;
				worklist        = succ_worklist;
				best_succ_block = succ_block;
				best_pos        = get_edge_src_pos(edge);
			}
		}
	}
	if(worklist == NULL) {
		/* only the end-block has 0 successors */
		assert(block == get_irg_end_block(get_irn_irg(block)));

		worklist = obstack_alloc(&obst, sizeof(worklist[0]));
		init_worklist(worklist, 0);
	} else {
		worklist = duplicate_worklist(worklist, block, best_succ_block,
		                              best_pos);
		activate_worklist(worklist);

		/* now we could have live values in the succ worklists that are not
		 * live anymore in the worklist we picked. We need reloads for them.
		 */
		if(!tentative_mode) {
			foreach_block_succ(block, edge) {
				ir_node      *succ_block    = get_edge_src_irn(edge);
				worklist_t   *succ_worklist = get_irn_link(succ_block);

				spill_non_live_nodes(succ_worklist);
			}
		}
	}

	do_spilling(block, worklist);
	deactivate_worklist(worklist);

	set_irn_link(block, worklist);

	/* we shouldn't have any live values left at the start block */
	n_preds = get_Block_n_cfgpreds(block);
	assert(n_preds != 0 || worklist->n_live_values == 0);
}

static void be_spill_belady3(be_irg_t *birg, const arch_register_class_t *ncls)
{
	ir_graph *irg = be_get_birg_irg(birg);

	cls       = ncls;
	n_regs    = cls->n_regs - be_put_ignore_regs(birg, cls, NULL);

	if(n_regs == 0)
		return;

	arch_env       = be_get_birg_arch_env(birg);
	exec_freq      = be_get_birg_exec_freq(birg);
	tentative_mode = 0;

	be_clear_links(irg);
	set_using_irn_link(irg);
	set_using_irn_visited(irg);
	inc_irg_visited(irg);

	obstack_init(&obst);
	senv = be_new_spill_env(birg);

	/* do a post-order walk over the CFG to make sure we have a maximum number
	 * of preds processed before entering a block */
	irg_block_edges_walk(get_irg_start_block(irg), NULL, process_block, NULL);

	clear_using_irn_link(irg);
	clear_using_irn_visited(irg);

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
