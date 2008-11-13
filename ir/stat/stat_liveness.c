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
 * @brief   Statistics for Firm, register pressure estimation.
 * @author  Michael Beck
 * @version $Id$
 */
#include "config.h"
#include "irgwalk.h"
#include "irbitset.h"
#include "irtools.h"
#include "array_t.h"
#include "irnode_t.h"

typedef struct block_entry_t block_entry_t;
typedef struct environment_t environment_t;

/** A Block entry. */
struct block_entry_t {
	ir_node **live_ins;   /**< The array of live ins. */
	ir_node **live_outs;  /**< The array of live outs. */
	ir_node *block;       /**< The associated block. */
	block_entry_t *next;  /**< Links to the next block entry. */
};

struct environment_t {
	struct obstack obst;
	block_entry_t *entries;  /**< List of all allocated block entries. */
	void          *visited;  /**< a Bitset to mark visited nodes */
};

static environment_t *env;

/**
 * Get the block entry or allocate one if not yet assigned.
 */
static block_entry_t *get_block_entry(ir_node *block) {
	block_entry_t *entry = get_irn_link(block);

	if (entry == NULL) {
		entry = obstack_alloc(&env->obst, sizeof(*entry));

		entry->live_ins  = NEW_ARR_F(ir_node *, 0);
		entry->live_outs = NEW_ARR_F(ir_node *, 0);

		entry->next  = env->entries;
		env->entries = entry;
	}
	return entry;
}

static void add_entry(ir_node ***arr, ir_node *irn) {
	ir_node **list = *arr;
	int i;

	for (i = ARR_LEN(list) - 1; i >= 0; --i) {
		if (list[i] == irn) {
			/* already there */
			return;
		}
	}
	ARR_APP1(ir_node *, *arr, irn);
}

static void add_live_in(ir_node *block, ir_node *irn) {
	block_entry_t *entry = get_block_entry(block);

	add_entry(&entry->live_ins, irn);
}

static void add_live_out(ir_node *block, ir_node *irn) {
	block_entry_t *entry = get_block_entry(block);

	add_entry(&entry->live_outs, irn);
}

/**
 * Mark a node (value) live out at a certain block. Do this also
 * transitively, i.e. if the block is not the block of the value's
 * definition, all predecessors are also marked live.
 *
 * @param def      The node (value).
 * @param block    The block to mark the value live out of.
 */
static void live_end_at_block(ir_node *def, ir_node *block) {
	add_live_out(block, def);

	if (is_irn_constlike(def)) {
		/* do NOT follow Constants further, assume they are
		   part of instruction or can easily
		   be rematerialized */
		return;
	}

	if (! bitset_contains_irn(env->visited, block)) {
		bitset_add_irn(env->visited, block);

		/*
		 * If this block is not the definition block, we have to go up
		 * further.
		 */
		if (get_nodes_block(def) != block) {
			int i;

			add_live_in(block, def);

			for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i)
				live_end_at_block(def, get_Block_cfgpred_block(block, i));
		}
	}
}

/**
 * Walker: finds live-outs and calculate live-ins from that.
 */
static void find_live_outs(ir_node *irn, void *ctx)
{
	ir_mode *mode = get_irn_mode(irn);
	ir_node *block, *use_block;
	int i;

	(void)ctx;

	/* only data nodes */
	if (! mode_is_datab(mode))
		return;

	block = get_nodes_block(irn);

	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(irn, i);

		if (mode_is_datab(get_irn_mode(pred))) {
			ir_node *def_block = get_nodes_block(pred);

			if (is_Phi(irn)) {
				use_block = get_Block_cfgpred_block(block, i);

				/* even if pred is a Const, it MUST be
				   in a register on block output */
				bitset_clear_all(env->visited);
				live_end_at_block(pred, use_block);

				/* The Block has only one live in, the Phi itself */
				add_live_in(block, irn);
			}
			else if (def_block != use_block) {
				/* pred is a live in of our block */
				if (is_irn_constlike(pred)) {
					/* ignore Constants, assume they are
					   part of instruction or can easily
					   be rematerialized */
					continue;
				}

				use_block = block;
				add_live_in(use_block, pred);

				bitset_clear_all(env->visited);
				for (i = get_Block_n_cfgpreds(use_block); i >= 0; --i) {
					ir_node *pred_block = get_Block_cfgpred_block(use_block, i);
					live_end_at_block(irn, pred_block);
				}
			}
		}
	}
}

/**
 * Calculate the live-in and live out of blocks for datab nodes.
 * Use it to estimate register pressure.
 */
void stat_liveness(ir_graph *irg) {
	environment_t genv;
	block_entry_t *p;

	env = &genv;

	obstack_init(&env->obst);
	env->entries = NULL;
	env->visited = bitset_obstack_alloc(&env->obst, get_irg_last_idx(irg));

	irg_block_walk_graph(irg, NULL, firm_clear_link, NULL);
	irg_walk_graph(irg, NULL, find_live_outs, NULL);

	for (p = env->entries; p != NULL; p = p->next) {
		DEL_ARR_F(p->live_ins);
		DEL_ARR_F(p->live_outs);
	}
	obstack_free(&env->obst, NULL);
	env = NULL;
}
