/*
 * This file is part of libFirm.
 * Copyright (C) 2014 Karlsruhe Institute of Technology
 */

/**
 * @file
 * @brief   Control flow optimizations.
 * @author  Goetz Lindenmaier, Michael Beck, Sebastian Hack, Matthias Braun
 *
 * Removes Bad control flow predecessors, merges blocks with jumps and
 * transforms pointless conditional jumps into undonciditonal ones.
 */
#include "debug.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irverify.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <stdbool.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** Set or reset the removable property of a block. */
static void set_Block_removable(ir_node *block, bool removable)
{
	set_Block_mark(block, removable);
}

/** Check if a block has the removable property set. */
static bool is_Block_removable(const ir_node *block)
{
	return get_Block_mark(block);
}

/** Walker: clear link fields and mark all blocks as removable. */
static void clear_link_and_mark_blocks_removable(ir_node *node, void *ctx)
{
	(void)ctx;
	if (is_Block(node)) {
		set_Block_removable(node, true);
		set_Block_phis(node, NULL);
	} else if (is_Switch(node)) {
		set_irn_link(node, INT_TO_PTR(0));
	} else if (is_Phi(node)) {
		set_Phi_next(node, NULL);
	}
}

/**
 * Collects all Phi nodes in link list of Block.
 * Marks all blocks "non_removable" if they contain a node other
 * than Jmp (and Proj).
 * Links all Proj nodes to their predecessors.
 * Collects all switch-Conds in a list.
 */
static void collect_nodes(ir_node *n, void *ctx)
{
	(void)ctx;
	if (is_Phi(n)) {
		ir_node *block = get_nodes_block(n);
		add_Block_phi(block, n);
		return;
	}
	if (is_Block(n)) {
		/* do not merge blocks with Jump labels for now (we cannot currently
		 * have multiple labels on a block) */
		if (get_Block_entity(n) != NULL)
			set_Block_removable(n, false);
		return;
	}
	/* these nodes are fine and still allow us to potentially remove the
	 * block (Switch will be decided based on its ProjX nodes) */
	if (is_Bad(n) || is_Jmp(n) || is_Switch(n))
		return;
	if (is_Proj(n)) {
		ir_node *pred = get_Proj_pred(n);
		if (is_Switch(pred)) {
			/* Switch with just default Proj is fine too */
			if (get_Proj_num(n) == pn_Switch_default)
				return;
			/* mark switch as having a non-default Proj */
			set_irn_link(pred, INT_TO_PTR(1));
		}
	}
	/* Any other node leads to the block not being removable. */
	ir_node *block = get_nodes_block(n);
	set_Block_removable(block, false);
}

/**
 * Before setting new block predecessors check if there are pairs of
 * predecessors from the same Cond node. If there are and all Phi inputs
 * are equivalent, then the Cond has no noticable effect and can be replaced
 * with a simple Jmp.
 */
static unsigned optimize_pointless_forks(ir_node *block, unsigned n_cfgpreds,
                                         ir_node **cfgpreds)
{
	ir_node **new_cfgpreds = NULL;
	if (cfgpreds == NULL) {
		cfgpreds     = get_Block_cfgpred_arr(block);
		new_cfgpreds = NULL;
	} else {
		new_cfgpreds = cfgpreds;
	}

	unsigned new_n_cfgpreds = n_cfgpreds;
	for (unsigned i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred0 = cfgpreds[i];
		if (new_cfgpreds != NULL && new_cfgpreds[i] == NULL)
			continue;
		if (!is_Proj(pred0))
			continue;
		ir_node *cfop = get_Proj_pred(pred0);
		if (!is_Cond(cfop) && !is_Switch(cfop))
			continue;

		for (unsigned j = i+1; j < n_cfgpreds; ++j) {
			ir_node *pred1 = cfgpreds[j];
			if (new_cfgpreds != NULL && new_cfgpreds[j] == NULL)
				continue;
			if (!is_Proj(pred1))
				continue;
			if (get_Proj_pred(pred1) != cfop)
				continue;
			/* Both variants jump into the same block, check if all
			 * Phi nodes have the same inputs */
			bool phis_ok = true;
			for (ir_node *phi = get_Block_phis(block); phi != NULL;
			     phi = get_Phi_next(phi)) {
				ir_node *phi_pred0 = get_Phi_pred(phi, i);
				ir_node *phi_pred1 = get_Phi_pred(phi, j);
				if (phi_pred0 != phi_pred1) {
					phis_ok = false;
					break;
				}
			}
			if (!phis_ok)
				continue;

			if (new_cfgpreds == NULL) {
				new_cfgpreds = XMALLOCN(ir_node*, n_cfgpreds);
				MEMCPY(new_cfgpreds, cfgpreds, n_cfgpreds);
			}
			if (is_Cond(cfop)) {
				/* replace Cond with Jmp */
				ir_node *predb1 = get_nodes_block(pred1);
				ir_node *jmp    = new_r_Jmp(predb1);

				new_cfgpreds[i] = jmp;
				new_cfgpreds[j] = NULL;
				--new_n_cfgpreds;
				DB((dbg, LEVEL_1, "Pointless fork %+F to %+F => replace %+F with %+F\n",
					predb1, block, cfop, jmp));
				break;
			} else {
				/* merge Switch table->Proj mapping entries */
				assert(is_Switch(cfop));

				unsigned pn0 = get_Proj_num(pred0);
				unsigned pn1 = get_Proj_num(pred1);
				/* we merge into pn0, make sure we always merge into the default
				 * case and switch if necessary */
				if (pn1 == pn_Switch_default) {
					unsigned t = pn0;
					pn0 = pn1;
					pn1 = t;
					ir_node *tp = pred0;
					pred0 = pred1;
					pred1 = tp;
					new_cfgpreds[i] = pred0;
				}
				/* remove 2nd ProjX */
				new_cfgpreds[j] = NULL;
				--new_n_cfgpreds;

				ir_switch_table *table = get_Switch_table(cfop);
				for (size_t i = 0, n = ir_switch_table_get_n_entries(table);
				     i < n; ++i) {
					if (ir_switch_table_get_pn(table, i) == pn1) {
						ir_tarval *min = ir_switch_table_get_min(table, i);
						ir_tarval *max = ir_switch_table_get_max(table, i);
						ir_switch_table_set(table, i, min, max, pn0);
					}
				}
				DB((dbg, LEVEL_1,
				    "Merge switch %+F table entry for %+F, %+F (pn %u into %u)\n",
				    cfop, pred1, pred0, pn1, pn0));
			}
		}
	}

	if (new_n_cfgpreds != n_cfgpreds) {
		ir_node **in = XMALLOCN(ir_node*, new_n_cfgpreds);
		for (ir_node *phi = get_Block_phis(block); phi != NULL;
		     phi = get_Phi_next(phi)) {
			unsigned new_n_phi_preds = 0;
			for (unsigned i = 0; i < n_cfgpreds; ++i) {
				if (new_cfgpreds[i] == NULL)
					continue;
				ir_node *pred = get_Phi_pred(phi, i);
				in[new_n_phi_preds++] = pred;
			}
			assert(new_n_phi_preds == new_n_cfgpreds);
			set_irn_in(phi, new_n_phi_preds, in);
		}
		unsigned n = 0;
		for (unsigned i = 0; i < n_cfgpreds; ++i) {
			ir_node *pred = new_cfgpreds[i];
			if (pred == NULL)
				continue;
			new_cfgpreds[n++] = pred;
		}
		assert(n == new_n_cfgpreds);
	}
	if (new_cfgpreds != cfgpreds && new_cfgpreds != NULL)
		set_irn_in(block, new_n_cfgpreds, new_cfgpreds);
	return new_n_cfgpreds;
}

static void exchange_phi(ir_node *old, ir_node *new)
{
	if (get_Phi_loop(old)) {
		remove_keep_alive(old);
		set_Phi_loop(old, false);
	}
	exchange(old, new);
}

#ifndef NDEBUG
static bool is_default_switch(const ir_node *node)
{
	ir_switch_table *table = get_Switch_table(node);
	for (size_t i = 0, n_entries = ir_switch_table_get_n_entries(table);
	     i < n_entries; ++i) {
		if (ir_switch_table_get_pn(table, i) != pn_Switch_default)
			return false;
	}
	return true;
}
#endif

/** Merge the single predecessor of @p block at position @p pred_pos.
 * The predecessor has to end with a Jmp for this to be legal. */
static bool try_merge_blocks(ir_node *block, unsigned pred_pos)
{
	if (get_Block_entity(block) != NULL)
		return false;
	ir_node *pred = get_Block_cfgpred(block, pred_pos);
	if (is_Proj(pred)) {
		ir_node *cfop = get_Proj_pred(pred);
		/* is it a switch with just a default Proj? */
		if (!is_Switch(cfop) || get_irn_link(cfop) != INT_TO_PTR(0))
			return false;
		assert(is_default_switch(cfop));
	} else if (!is_Jmp(pred)) {
		return false;
	}

	/* We can simply remove the Phi nodes and replace them by their single
	 * real input. */
	for (ir_node *phi = get_Block_phis(block), *next_phi; phi != NULL;
	     phi = next_phi) {
		next_phi = get_Phi_next(phi);
		ir_node *val = get_Phi_pred(phi, pred_pos);
		exchange_phi(phi, val);
	}
	ir_node *pred_block = get_Block_cfgpred_block(block, pred_pos);
	DB((dbg, LEVEL_1, "Replace block %+F with %+F\n", block, pred_block));
	if (!is_Block_removable(block))
		set_Block_removable(pred_block, false);
	assert(get_Block_entity(block) == NULL);
	exchange(block, pred_block);
	return true;
}

/**
 * Merge empty predecessor blocks with block and remove Bad block inputs.
 *
 * For each predecessor p of a Block b there are three cases:
 *  - The predecessor p is a Bad node: just skip it. The in array of b shrinks
 *    by one.
 *  - The predecessor p is empty. Remove p. All predecessors of p are now
 *    predecessors of b.
 *  - The predecessor p is a block containing useful code. Just keep p as is.
 *
 * The most complicated case we handle looks like this:
 * @verbatim
 *    then_b     else_b                              then_b  else_b
 *       \      /                                      \      /
 *        \    /                                        |    /
 *        pred_b                                        |   /
 *         |   ____                                     |  /  ____
 *         |  |    |                                    |  | |    |
 *         |  |    |       === optimized to ===>        \  | |    |
 *        loop_b   |                                     loop_b   |
 *         |  |    |                                      |  |    |
 *         |  |____|                                      |  |____|
 *         |                                              |
 * @endverbatim
 *
 * We have to adapt the Phi nodes in the loop_b and pred_b blocks. Note that
 * for the additional new inputs of the Phi nodes in pred_b we use the Phi node
 * itself (self-loop). This is because pred_b either does not dominate loop_b
 * and the Phi node isn't used except for Phis inside loop_b which are changed
 * anyway. The only case where pred_b dominated loop_b is in case of a loop
 * header then using self-loops for the additional Phi inputs is correct.
 */
static void merge_empty_predecessors(ir_node *block, unsigned new_n_cfgpreds)
{
	unsigned  n_cfgpreds = get_Block_n_cfgpreds(block);
	ir_node **in         = XMALLOCN(ir_node*, new_n_cfgpreds);

	/* Fix the Phi nodes of the current block */
	for (ir_node *phi = get_Block_phis(block), *next; phi != NULL; phi = next) {
		next = get_Phi_next(phi);

		/* Find the new predecessors for the Phi */
		unsigned new_n_phi_preds = 0;
		for (unsigned i = 0; i < n_cfgpreds; ++i) {
			ir_node *pred = get_Block_cfgpred(block, i);
			if (is_Bad(pred))
				continue;

			/* case1: keep the predecessor */
			ir_node *predb = get_nodes_block(pred);
			if (predb == block || !is_Block_removable(predb)) {
				in[new_n_phi_preds++] = get_Phi_pred(phi, i);
				continue;
			}

			/* case2: merge with empty predecessor block */
			ir_node *phi_pred = get_Phi_pred(phi, i);
			for (unsigned j = 0, n_pred_cfgpreds = get_Block_n_cfgpreds(predb);
			     j < n_pred_cfgpreds; ++j) {
				ir_node *pred_pred = get_Block_cfgpred(predb, j);
				if (is_Bad(pred_pred))
					continue;

				ir_node *new_val = phi_pred;
				if (get_nodes_block(phi_pred) == predb) {
					assert(is_Phi(phi_pred));
					new_val = get_Phi_pred(phi_pred, j);
				}
				in[new_n_phi_preds++] = new_val;
			}
		}
		assert(new_n_phi_preds == new_n_cfgpreds);

		/* Fix the node */
		if (new_n_phi_preds == 1) {
			exchange_phi(phi, in[0]);
		} else {
			set_irn_in(phi, new_n_phi_preds, in);
		}
	}
	/* all phis have been removed if we only have 1 pred => clear phi list */
	if (new_n_cfgpreds == 1)
		set_Block_phis(block, NULL);

	/* Fix Phi nodes of predecessor blocks which we merge. This is necessary
	 * when we merge between loop backedge and single loop entry. */
	for (unsigned i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred = get_Block_cfgpred(block, i);
		if (is_Bad(pred))
			continue;
		ir_node *predb = get_nodes_block(pred);
		if (predb == block || !is_Block_removable(predb))
			continue;

		/* Adapt Phis of predecessor block */
		for (ir_node *phi = get_Block_phis(predb), *next_phi; phi != NULL;
			 phi = next_phi) {
			next_phi = get_Phi_next(phi);

			unsigned new_n_phi_preds = 0;
			for (unsigned j = 0; j < n_cfgpreds; ++j) {
				ir_node *pred2 = get_Block_cfgpred(block, j);
				if (is_Bad(pred2))
					continue;
				ir_node *predb2 = get_nodes_block(pred2);
				if (predb2 == block || !is_Block_removable(predb2)) {
					in[new_n_phi_preds++] = phi;
					continue;
				}

				for (unsigned k = 0,
				     n_predb2_cfgpreds = get_Block_n_cfgpreds(predb2);
				     k < n_predb2_cfgpreds; ++k) {
				    ir_node *predpred = get_Block_cfgpred(predb2, k);
				    if (is_Bad(predpred))
						continue;
					ir_node *new_phi_pred;
					if (j == i) {
						new_phi_pred = get_Phi_pred(phi, k);
					} else {
						new_phi_pred = phi;
					}
					in[new_n_phi_preds++] = new_phi_pred;
				}
			}
			assert(new_n_phi_preds == new_n_cfgpreds);

			/* kill Phi if only 1 input left else update preds and move */
			if (new_n_phi_preds == 1) {
				exchange_phi(phi, in[0]);
			} else {
				set_irn_in(phi, new_n_phi_preds, in);
				/* move Phi from predecessor into block and update Phi list */
				set_nodes_block(phi, block);
				set_Phi_next(phi, get_Block_phis(block));
				set_Block_phis(block, phi);

				/* We have two possibilities here:
				 *  1) predb does not dominate block -> the Phi is unused
				 *     -> it doesn't matter what we do
				 *  2) predb dominates block -> we merge into a loop which must
				 *     already contain another PhiM[Loop] so no need to create
				 *     a 2nd one.
				 */
				if (get_Phi_loop(phi)) {
					remove_keep_alive(phi);
					set_Phi_loop(phi, false);
				}
			}
		}
		/* all phis are removed if we only have 1 pred => clear phi list */
		if (new_n_cfgpreds == 1)
			set_Block_phis(block, NULL);
	}

	/* Calculate new block inputs */
	unsigned n = 0;
	for (unsigned i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred = get_Block_cfgpred(block, i);
		if (is_Bad(pred))
			continue;
		ir_node *predb = get_nodes_block(pred);

		/* case1: keep predecessor */
		if (predb == block || !is_Block_removable(predb)) {
			in[n++] = pred;
			continue;
		}
		DB((dbg, LEVEL_1, "Merge block %+F into %+F\n", predb, block));
		/* case2: merge predecessor */
		for (unsigned j = 0, n_pred_cfgpreds = get_Block_n_cfgpreds(predb);
			 j < n_pred_cfgpreds; ++j) {
			ir_node *predpred = get_Block_cfgpred(predb, j);
			if (is_Bad(predpred))
				continue;
			in[n++] = predpred;
		}
		/* Merge blocks to preserve keep alive edges. */
		exchange(predb, block);
	}
	assert(n == new_n_cfgpreds);

	n = optimize_pointless_forks(block, n, in);

	DB((dbg, LEVEL_1, "Set new inputs for %+F\n", block));
	set_irn_in(block, n, in);
	free(in);
}

/**
 * Optimize control flow leading into a basic block.
 */
static bool optimize_block(ir_node *block, bool *changed)
{
	if (irn_visited_else_mark(block))
		return false;

again:;
	/* Count the number of predecessor if this block is merged with pred blocks
	   that are empty. */
	unsigned n_cfgpreds      = get_Block_n_cfgpreds(block);
	unsigned real_preds      = 0;
	/** number of inputs when removing Bads and merging empty predecessors */
	unsigned new_n_cfgpreds  = 0;
	unsigned single_pred_pos = (unsigned)-1;
	bool     can_opt         = false;
	for (unsigned i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred = get_Block_cfgpred(block, i);
		if (is_Bad(pred)) {
			can_opt = true;
			continue;
		}

		/* make sure predecessor is optimized */
		ir_node *predb = get_nodes_block(pred);
		bool bail_out = optimize_block(predb, changed);
		/* bail out if recursion changed our current block (may happen in
		 * endless loops only reachable by keep-alive edges) */
		if (bail_out || is_Id(block) || is_Id(predb))
			return false;

		++real_preds;
		single_pred_pos = i;
		if (predb != block && is_Block_removable(predb)) {
			can_opt = true;
			for (int j = 0, n_pred_cfgpreds = get_Block_n_cfgpreds(predb);
			     j < n_pred_cfgpreds; ++j) {
				ir_node *predpred = get_Block_cfgpred(predb, j);
				if (is_Bad(predpred))
					continue;
				++new_n_cfgpreds;
			}
		} else {
			++new_n_cfgpreds;
		}
	}

	/* If we only have a single predecessor which jumps into this block,
	 * then we can simply merge the blocks even if they are not empty. */
	if (real_preds == 1 && try_merge_blocks(block, single_pred_pos)) {
		*changed = true;
		return true;
	}

	if (!can_opt) {
		/* attempt conditional->unconditional jump optimization */
		unsigned new_n_cfgpreds
			= optimize_pointless_forks(block, n_cfgpreds, NULL);
		if (new_n_cfgpreds != n_cfgpreds) {
			*changed = true;
			goto again;
		}
		return false;
	}

	merge_empty_predecessors(block, new_n_cfgpreds);
	*changed = true;
	return false;
}

void optimize_cf(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
	                         | IR_GRAPH_PROPERTY_ONE_RETURN);
	/* we have some hacky is_Id() checks here so exchange must not use Deleted
	 * nodes because out edges are active. */
	edges_deactivate(irg);
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST
	                        | IR_RESOURCE_IRN_LINK);

	FIRM_DBG_REGISTER(dbg, "firm.opt.controlflow");
	DB((dbg, LEVEL_1, "===> Performing control flow opt on %+F\n", irg));

	ir_node *end            = get_irg_end(irg);
	ir_node *end_block      = get_irg_end_block(irg);
	bool     global_changed = false;
	bool     changed;
	do {
		/* Analysis: Create basic block phi lists and marks blocks which only
		 * contain Jmp and Phi nodes. */
		irg_walk_graph(irg, clear_link_and_mark_blocks_removable, collect_nodes,
		               NULL);

		/* Transformation. Calls recursive opt function starting from end block
		 * and blocks which are kept alive. */
		changed = false;
		ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
		inc_irg_visited(irg);
		optimize_block(end_block, &changed);
		foreach_irn_in(end, i, kept) {
			if (is_Block(kept))
				optimize_block(kept, &changed);
		}
		ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
		global_changed |= changed;
	} while(changed);

	remove_End_Bads_and_doublets(end);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST
	                     | IR_RESOURCE_IRN_LINK);
	confirm_irg_properties(irg, global_changed ? IR_GRAPH_PROPERTIES_NONE
	                                           : IR_GRAPH_PROPERTIES_ALL);
}
