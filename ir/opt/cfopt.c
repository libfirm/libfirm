/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Control flow optimizations.
 * @author  Goetz Lindenmaier, Michael Beck, Sebastian Hack
 *
 * Removes Bad control flow predecessors and empty blocks.  A block is empty
 * if it contains only a Jmp node. Blocks can only be removed if they are not
 * needed for the semantics of Phi nodes. Further, we NEVER remove labeled
 * blocks (even if we could move the label).
 */
#include "iroptimize.h"

#include <assert.h>
#include <stdbool.h>

#include "array.h"
#include "firmstat.h"
#include "irbackedge_t.h"
#include "ircons.h"
#include "irdump.h"
#include "iredges.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irnode_t.h"
#include "iropt_dbg.h"
#include "iropt_t.h"
#include "irouts_t.h"
#include "irprog_t.h"
#include "irverify.h"
#include "xmalloc.h"

/** An environment for merge_blocks and collect nodes. */
typedef struct merge_env {
	bool changed;      /**< Set if the graph was changed. */
	bool phis_moved;   /**< Set if Phi nodes were moved. */
} merge_env;

/** set or reset the removable property of a block. */
static void set_Block_removable(ir_node *block, bool removable)
{
	set_Block_mark(block, removable);
}

/** check if a block has the removable property set. */
static bool is_Block_removable(const ir_node *block)
{
	return get_Block_mark(block);
}

/** Walker: clear link fields and mark all blocks as removable. */
static void clear_link_and_mark_blocks_removable(ir_node *node, void *ctx)
{
	(void) ctx;
	set_irn_link(node, NULL);
	if (is_Block(node)) {
		set_Block_removable(node, true);
		set_Block_phis(node, NULL);
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
	(void) ctx;
	if (is_Phi(n)) {
		/* Collect Phi nodes to compact ins along with block's ins. */
		ir_node *block = get_nodes_block(n);
		add_Block_phi(block, n);
	} else if (is_Block(n)) {
		if (get_Block_entity(n) != NULL) {
			/* block with a jump label attached cannot be removed. */
			set_Block_removable(n, false);
		}
	} else if (is_Bad(n) || is_Jmp(n)) {
		/* ignore these */
		return;
	} else {
		/* Check for non-empty block. */
		ir_node *block = get_nodes_block(n);

		set_Block_removable(block, false);

		if (is_Proj(n)) {
			/* link Proj nodes */
			ir_node *pred = get_Proj_pred(n);
			set_irn_link(n, get_irn_link(pred));
			set_irn_link(pred, n);
		}
	}
}

/** Returns true if pred is predecessor of block b. */
static bool is_pred_of(const ir_node *pred, const ir_node *b)
{
	int i;

	for (i = get_Block_n_cfgpreds(b) - 1; i >= 0; --i) {
		ir_node *b_pred = get_Block_cfgpred_block(b, i);
		if (b_pred == pred)
			return true;
	}
	return false;
}

/** Test whether we can optimize away pred block pos of b.
 *
 *  @param  b    A block node.
 *  @param  pos  The position of the predecessor block to judge about.
 *
 *  @returns     The number of predecessors
 *
 *  The test is rather tricky.
 *
 *  The situation is something like the following:
 *  @verbatim
 *                 if-block
 *                  /   \
 *              then-b  else-b
 *                  \   /
 *                    b
 *  @endverbatim
 *
 *  b merges the control flow of an if-then-else.  We may not remove
 *  the 'then' _and_ the 'else' block of an 'if' if there is a Phi
 *  node in b, even if both are empty.  The destruction of this Phi
 *  requires that a copy is added before the merge.  We have to
 *  keep one of the case blocks to place the copies in.
 *
 *  To perform the test for pos, we must regard predecessors before pos
 *  as already removed.
 **/
static unsigned test_whether_dispensable(const ir_node *b, int pos)
{
	ir_node *pred  = get_Block_cfgpred(b, pos);
	ir_node *predb = get_nodes_block(pred);
	if (is_Bad(pred) || !is_Block_removable(predb))
		return 1;

	/* can't remove self-loops */
	if (predb == b)
		goto non_dispensable;
	if (is_unknown_jump(pred))
		goto non_dispensable;

	/* Seems to be empty. At least we detected this in collect_nodes. */
	if (get_Block_phis(b) != NULL) {
		/* there are Phi nodes */

		/* b's pred blocks and pred's pred blocks must be pairwise disjunct.
		 * Handle all pred blocks with preds < pos as if they were already
		 * removed. */
		for (int i = 0; i < pos; ++i) {
			ir_node *other_pred  = get_Block_cfgpred(b, i);
			ir_node *other_predb = get_nodes_block(other_pred);
			if (is_Bad(other_pred))
				continue;
			if (is_Block_removable(other_predb)
			    && !Block_block_visited(other_predb)) {
				for (int j = get_Block_n_cfgpreds(other_predb); j-- > 0; ) {
					ir_node *other_predpred
						= get_Block_cfgpred_block(other_predb, j);
					if (is_pred_of(other_predpred, predb))
						goto non_dispensable;
				}
			} else if (is_pred_of(other_predb, predb)) {
				goto non_dispensable;
			}
		}
		for (int i = pos+1, n_cfgpreds = get_Block_n_cfgpreds(b);
		     i < n_cfgpreds; ++i) {
			ir_node *other_predb = get_Block_cfgpred_block(b, i);
			if (is_pred_of(other_predb, predb))
				goto non_dispensable;
		}
	}
	/* we will not dispense already visited blocks */
	if (Block_block_visited(predb))
		return 1;
	/* if we get here, the block is dispensable, count useful preds */
	return get_irn_arity(predb);

non_dispensable:
	set_Block_removable(predb, false);
	return 1;
}

/**
 * This method merges blocks. A block is applicable to be merged, if it
 * has only one predecessor with an unconditional jump to this block;
 * and if this block does not contain any phis.
 */
static void merge_blocks(ir_node *b, void *env)
{
	(void) env;

	if (get_Block_n_cfgpreds(b) == 1) {
		ir_node* pred = get_Block_cfgpred(b, 0);
		if (is_Jmp(pred)) {
			ir_node* pred_block = get_nodes_block(pred);
			if (get_Block_phis(b) == NULL) {
				exchange(b, pred_block);
			}
		}
	}
}

/**
 * This method removes empty blocks.  A block is empty if it only contains Phi
 * and Jmp nodes.
 *
 * We first adapt Phi nodes, then Block nodes, as we need the old ins
 * of the Block to adapt the Phi nodes.  We do this by computing new
 * in arrays, and then replacing the old ones.  So far we compute new in arrays
 * for all nodes, not regarding whether there is a possibility for optimization.
 *
 * For each predecessor p of a Block b there are three cases:
 *  - The predecessor p is a Bad node: just skip it. The in array of b shrinks
 *    by one.
 *  - The predecessor p is empty. Remove p. All predecessors of p are now
 *    predecessors of b.
 *  - The predecessor p is a block containing useful code. Just keep p as is.
 *
 * For Phi nodes f we have to check the conditions at the Block of f.
 * For cases 1 and 3 we proceed as for Blocks.  For case 2 we can have two
 * cases:
 *  -2a: The old predecessor of the Phi f is a Phi pred_f IN THE BLOCK REMOVED.
 *       In this case we proceed as for blocks. We remove pred_f.  All
 *       predecessors of pred_f now are predecessors of f.
 *  -2b: The old predecessor of f is NOT in the block removed. It might be a Phi
 *       too. We have to replicate f for each predecessor of the removed block.
 *       Or, with other words, the removed predecessor block has exactly one
 *       predecessor.
 *
 * Further there is a special case for self referencing blocks:
 * @verbatim
 *
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
 * If there is a Phi in pred_b, but we remove pred_b, we have to generate a
 * Phi in loop_b, that has the ins of the Phi in pred_b and a self referencing
 * backedge.
 */
static void optimize_blocks(ir_node *b, void *ctx)
{
	if (get_Block_dom_depth(b) < 0) {
		/* ignore unreachable blocks */
		return;
	}

	/* Count the number of predecessor if this block is merged with pred blocks
	   that are empty. */
	int max_preds = 0;
	for (int i = 0, k = get_Block_n_cfgpreds(b); i < k; ++i) {
		max_preds += test_whether_dispensable(b, i);
	}
	ir_node **in = XMALLOCN(ir_node*, max_preds);

	int        p_preds = -1;
	merge_env *env     = (merge_env*)ctx;

	/*- Fix the Phi nodes of the current block -*/
	for (ir_node *phi = get_Block_phis(b), *next; phi != NULL; phi = next) {
		next = get_Phi_next(phi);

		/* Find the new predecessors for the Phi */
		p_preds = 0;
		for (int i = 0, n = get_Block_n_cfgpreds(b); i < n; ++i) {
			ir_graph *irg   = get_irn_irg(b);
			ir_node  *predx = get_Block_cfgpred(b, i);

			/* case Phi 1: maintain Bads, as somebody else is responsible to
			 * remove them */
			if (is_Bad(predx)) {
				in[p_preds++] = new_r_Bad(irg, get_irn_mode(phi));
				continue;
			}

			ir_node *pred = get_nodes_block(predx);

			/* case Phi 2: It's an empty block and not yet visited. */
			if (is_Block_removable(pred) && !Block_block_visited(pred)) {
				ir_node *phi_pred = get_Phi_pred(phi, i);

				for (int j = 0, k = get_Block_n_cfgpreds(pred); j < k; j++) {
					ir_node *pred_pred = get_Block_cfgpred(pred, j);
					if (is_Bad(pred_pred)) {
						in[p_preds++] = new_r_Bad(irg, get_irn_mode(phi));
						continue;
					}

					if (get_nodes_block(phi_pred) == pred) {
						/* case Phi 2a: */
						in[p_preds++] = get_Phi_pred(phi_pred, j);
					} else {
						/* case Phi 2b: */
						in[p_preds++] = phi_pred;
					}
				}
			} else {
				/* case Phi 3: */
				in[p_preds++] = get_Phi_pred(phi, i);
			}
		}
		assert(p_preds == max_preds);

		/* Fix the node */
		if (p_preds == 1) {
			exchange(phi, in[0]);
		} else {
			set_irn_in(phi, p_preds, in);
		}
		env->changed = true;
	}

	/*- This happens only if merge between loop backedge and single loop entry.
	    Moreover, it is only needed if predb is the direct dominator of b,
	    else there can be no uses of the Phi's in predb ... -*/
	for (int k = 0, n = get_Block_n_cfgpreds(b); k < n; ++k) {
		ir_node *pred  = get_Block_cfgpred(b, k);
		ir_node *predb = get_nodes_block(pred);
		if (is_Bad(pred))
			continue;

		if (is_Block_removable(predb) && !Block_block_visited(predb)) {
			/* we found a predecessor block at position k that will be removed */
			for (ir_node *phi = get_Block_phis(predb), *next_phi; phi != NULL;
			     phi = next_phi) {
				int q_preds = 0;
				next_phi = get_Phi_next(phi);

				if (get_Block_idom(b) != predb) {
					/* predb is not the dominator. There can't be uses of
					 * pred's Phi nodes, kill them .*/
					ir_graph *irg  = get_irn_irg(b);
					ir_mode  *mode = get_irn_mode(phi);
					exchange(phi, new_r_Bad(irg, mode));
				} else {
					/* predb is the direct dominator of b. There might be uses
					 * of the Phi nodes from predb in further block, so move
					 * this phi from the predecessor into the block b */
					set_nodes_block(phi, b);
					set_Phi_next(phi, get_Block_phis(b));
					set_Block_phis(b, phi);
					env->phis_moved = true;

					/* first, copy all 0..k-1 predecessors */
					for (int i = 0; i < k; i++) {
						ir_node *predx = get_Block_cfgpred(b, i);
						if (is_Bad(predx)) {
							ir_graph *irg  = get_irn_irg(b);
							ir_mode  *mode = get_irn_mode(phi);
							in[q_preds++] = new_r_Bad(irg, mode);
							continue;
						}

						ir_node *pred_block = get_nodes_block(predx);
						if (is_Block_removable(pred_block)
						    && !Block_block_visited(pred_block)) {
							/* It's an empty block and not yet visited. */
							for (int j = 0,
							     n_cfgpreds = get_Block_n_cfgpreds(pred_block);
							     j < n_cfgpreds; ++j) {
								if (!is_Bad(get_Block_cfgpred(pred_block, j))) {
									in[q_preds++] = phi;
								} else {
									ir_graph *irg  = get_irn_irg(b);
									ir_mode  *mode = get_irn_mode(phi);
									in[q_preds++] = new_r_Bad(irg, mode);
								}
							}
						} else {
							in[q_preds++] = phi;
						}
					}

					/* now we are at k, copy the phi predecessors */
					for (int i = 0, n = get_Phi_n_preds(phi); i < n; ++i) {
						in[q_preds++] = get_Phi_pred(phi, i);
					}

					/* and now all the rest */
					for (int i = k+1, n = get_Block_n_cfgpreds(b); i < n; ++i) {
						ir_node *phi_pred = get_Block_cfgpred_block(b, i);

						if (phi_pred == NULL) {
							ir_graph *irg  = get_irn_irg(b);
							ir_mode  *mode = get_irn_mode(phi);
							in[q_preds++] = new_r_Bad(irg, mode);
						} else if (is_Block_removable(phi_pred) && !Block_block_visited(phi_pred)) {
							/* It's an empty block and not yet visited. */
							for (int j = 0, n = get_Block_n_cfgpreds(phi_pred);
							     j < n; ++j) {
								if (!is_Bad(get_Block_cfgpred(phi_pred, j))) {
									in[q_preds++] = phi;
								} else {
									ir_graph *irg  = get_irn_irg(b);
									ir_mode  *mode = get_irn_mode(phi);
									in[q_preds++] = new_r_Bad(irg, mode);
								}
							}
						} else {
							in[q_preds++] = phi;
						}
					}

					/* Fix the node */
					if (q_preds == 1)
						exchange(phi, in[0]);
					else
						set_irn_in(phi, q_preds, in);
					env->changed = true;

					assert(q_preds <= max_preds);
				}
			}
		}
	}

	/*- Fix the block -*/
	int n_preds = 0;
	for (int i = 0, n = get_Block_n_cfgpreds(b); i < n; ++i) {
		ir_node  *pred  = get_Block_cfgpred(b, i);
		ir_node  *predb = get_nodes_block(pred);
		ir_graph *irg   = get_irn_irg(pred);

		/* case 1: Bad predecessor */
		if (is_Bad(pred)) {
			in[n_preds++] = new_r_Bad(irg, mode_X);
			continue;
		}
		if (is_Block_removable(predb) && !Block_block_visited(predb)) {
			/* case 2: It's an empty block and not yet visited. */
			for (int j = 0, n = get_Block_n_cfgpreds(predb); j < n; ++j) {
				ir_node *predpred = get_Block_cfgpred(predb, j);
				if (is_Bad(predpred)) {
					in[n_preds++] = new_r_Bad(irg, mode_X);
					continue;
				}

				in[n_preds++] = predpred;
			}
			/* Remove jump and merge blocks as they might be kept alive. */
			exchange(pred, new_r_Bad(get_irn_irg(b), mode_X));
			exchange(predb, b);
		} else {
			/* case 3: */
			in[n_preds++] = pred;
		}
	}
	assert(n_preds == max_preds);

	set_irn_in(b, n_preds, in);
	env->changed = true;

	/* see if phi-fix was correct */
	assert(get_Block_phis(b) == NULL || p_preds == -1 || (n_preds == p_preds));
	free(in);
}

/**
 * Optimize boolean Conds, where true and false jump to the same block into a Jmp
 * Block must contain no Phi nodes.
 *
 *        Cond
 *       /    \
 *  projA      projB   =>   Jmp     Bad
 *       \    /                \   /
 *       block                 block
 */
static bool optimize_pred_cond(ir_node *block, int i, int j)
{
	assert(i != j);
	ir_node *projA = get_Block_cfgpred(block, i);
	if (!is_Proj(projA))
		return false;
	ir_node *projB = get_Block_cfgpred(block, j);
	if (!is_Proj(projB))
		return false;
	ir_node *cond  = get_Proj_pred(projA);
	if (!is_Cond(cond))
		return false;
	if (cond != get_Proj_pred(projB))
		return false;

	/* cond should actually be a Jmp */
	ir_node *pred_block = get_nodes_block(cond);
	ir_node *jmp        = new_r_Jmp(pred_block);
	ir_node *bad        = new_r_Bad(get_irn_irg(block), mode_X);

	assert(projA != projB);
	exchange(projA, jmp);
	exchange(projB, bad);
	return true;
}

typedef enum block_flags_t {
	BF_HAS_OPERATIONS         = 1 << 0,
	BF_HAS_PHIS               = 1 << 1,
	BF_IS_UNKNOWN_JUMP_TARGET = 1 << 2,
} block_flags_t;

static bool get_block_flag(const ir_nodehashmap_t *infos, const ir_node *block,
                           int flag)
{
	return PTR_TO_INT(ir_nodehashmap_get(void, infos, block)) & flag;
}

static void set_block_flag(ir_nodehashmap_t *infos, ir_node *block,
                           block_flags_t flag)
{
	int data = PTR_TO_INT(ir_nodehashmap_get(void, infos, block));
	data |= flag;
	ir_nodehashmap_insert(infos, block, INT_TO_PTR(data));
}

static void clear_block_flag(ir_nodehashmap_t *infos, const ir_node *block)
{
	ir_nodehashmap_remove(infos, block);
}

static bool has_operations(ir_nodehashmap_t *infos, const ir_node *block)
{
	return get_block_flag(infos, block, BF_HAS_OPERATIONS);
}

static void set_has_operations(ir_nodehashmap_t *infos, ir_node *block)
{
	set_block_flag(infos, block, BF_HAS_OPERATIONS);
}

static bool has_phis(ir_nodehashmap_t *infos, const ir_node *block)
{
	return get_block_flag(infos, block, BF_HAS_PHIS);
}

static void set_has_phis(ir_nodehashmap_t *infos, ir_node *block)
{
	set_block_flag(infos, block, BF_HAS_PHIS);
}

static bool is_unknown_jump_target(ir_nodehashmap_t *infos, const ir_node *block)
{
	return get_block_flag(infos, block, BF_IS_UNKNOWN_JUMP_TARGET);
}

static void set_is_unknown_jump_target(ir_nodehashmap_t *infos, ir_node *block)
{
	set_block_flag(infos, block, BF_IS_UNKNOWN_JUMP_TARGET);
}

/**
 * Pre-Walker: fill block info information.
 */
static void compute_block_info(ir_node *n, void *x)
{
	ir_nodehashmap_t *infos = (ir_nodehashmap_t*)x;

	if (is_Block(n)) {
		for (int i = 0, n_cfgpreds = get_Block_n_cfgpreds(n); i < n_cfgpreds;
		     ++i) {
			ir_node *pred = get_Block_cfgpred(n,i);
			if (is_unknown_jump(pred)) {
				set_is_unknown_jump_target(infos, n);
			}
		}
	} else if (is_Phi(n)) {
		ir_node *block = get_nodes_block(n);
		set_has_phis(infos, block);
	} else if (is_Jmp(n) || is_Cond(n) || is_Proj(n)) {
		/* ignore */
	} else {
		ir_node *block = get_nodes_block(n);
		set_has_operations(infos, block);
	}
}

static void clear_block_info(ir_node *block, void *x)
{
	ir_nodehashmap_t *infos = (ir_nodehashmap_t*)x;
	clear_block_flag(infos, block);
}

typedef struct skip_env {
	bool             changed;
	ir_nodehashmap_t block_infos;
} skip_env;

/**
 * Post-Block-walker: Optimize useless if's (boolean Cond nodes
 * with same true/false target) away.
 */
static void optimize_ifs(ir_node *block, void *x)
{
	skip_env *env = (skip_env*)x;
	if (has_phis(&env->block_infos, block))
		return;

	/* optimize Cond predecessors (might produce Bad predecessors) */
	for (int i = 0, n_preds = get_Block_n_cfgpreds(block); i < n_preds; ++i) {
		for (int j = i+1; j < n_preds; ++j) {
			optimize_pred_cond(block, i, j);
		}
	}
}

/**
 * Pre-Block walker: remove empty blocks (only contain a Jmp)
 * that are control flow predecessors of the current block.
 */
static void remove_empty_blocks(ir_node *block, void *x)
{
	skip_env *env = (skip_env*)x;
	for (int i = 0, n_preds = get_Block_n_cfgpreds(block); i < n_preds; ++i) {
		ir_node *jmp = get_Block_cfgpred(block, i);
		if (!is_Jmp(jmp))
			continue;
		ir_node *jmp_block = get_nodes_block(jmp);
		if (jmp_block == block)
			continue; /* this infinite loop cannot be optimized any further */
		if (is_unknown_jump_target(&env->block_infos, jmp_block))
			continue; /* unknown jump target must not be optimized */
		if (has_phis(&env->block_infos,jmp_block))
			continue; /* this block contains Phis and is not skipped */
		if (Block_block_visited(jmp_block)) {
			continue;
			/* otherwise we could break the walker,
			 * if block was reached via
			 *     KeepAlive edge -> jmp_block -> A ---> block,
			 * because the walker cannot handle Id nodes.
			 *
			 *   A      B
			 *    \    /
			 *   jmp_block
			 *    /    \
			 * block    End
			 */
		}

		/* jmp_block is an empty block and can be optimized! */

		int n_jpreds = get_Block_n_cfgpreds(jmp_block);
		/**
		 * If the jmp block has only one predecessor this is straightforward.
		 * However, if there are more predecessors, we only handle this,
		 * if block has no Phis.
		 */
		if (n_jpreds == 1) {
			ir_node *pred        = get_Block_cfgpred(jmp_block, 0);
			ir_node *pred_block  = get_nodes_block(pred);
			if (has_operations(&env->block_infos,jmp_block)) {
				if (!is_Jmp(pred))
					continue; /* must not create partially dead code, especially when it is mode_M */
			}

			/* skip jmp block by rerouting its predecessor to block
			 *
			 *     A              A
			 *     |              |
			 *  jmp_block   =>    |
			 *     |              |
			 *   block          block
			 */
			exchange(jmp, pred);

			/* cleanup: jmp_block might have a Keep edge! */
			exchange(jmp_block, pred_block);
			env->changed = true;
		} else if (!has_phis(&env->block_infos, block) &&
		           !has_operations(&env->block_infos,jmp_block)) {
			/* all predecessors can skip the jmp block, so block gets some new
			 * predecessors
			 *
			 *  A     B                 A  B
			 *   \   /                  |  |
			 * jmp_block  C  =>  Bad  C |  |
			 *      \    /          \ | | /
			 *      block            block
			 */
			ir_node **ins = ALLOCAN(ir_node*, n_preds+n_jpreds);
			/* first copy the old predecessors, because the outer loop (i)
			 * still walks over them */
			for (int j = 0; j < n_preds; ++j) {
				ins[j] = get_Block_cfgpred(block, j);
			}
			/* now append the new predecessors */
			for (int j = 0; j < n_jpreds; ++j) {
				ir_node *pred = get_Block_cfgpred(jmp_block, j);
				ins[n_preds+j] = pred;
			}
			set_irn_in(block, n_preds+n_jpreds, ins);
			/* convert the jmp_block to Bad */
			ir_graph *irg = get_irn_irg(block);
			exchange(jmp_block, new_r_Bad(irg, mode_BB));
			exchange(jmp, new_r_Bad(irg, mode_X));
			/* let the outer loop walk over the new predecessors as well */
			n_preds += n_jpreds;
			env->changed = true;
			// TODO What if jmp_block had a KeepAlive edge?
		} else {
			/* This would involve Phis ... */
		}
	}
}

/*
 * All cfg optimizations, which do not touch Phi nodes.
 *
 * Note that this might create critical edges.
 */
static void cfgopt_ignoring_phis(ir_graph *irg)
{
	skip_env env;
	env.changed = true;
	ir_nodehashmap_init(&env.block_infos);

	while (env.changed) {
		irg_walk_graph(irg, compute_block_info, NULL, &env.block_infos);
		env.changed = false;

		/* Remove blocks, which only consist of a Jmp */
		irg_block_walk_graph(irg, remove_empty_blocks, NULL, &env);

		/* Optimize Cond->Jmp, where then- and else-block are the same. */
		irg_block_walk_graph(irg, NULL, optimize_ifs, &env);

		if (env.changed) {
			confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
			/* clear block info, because it must be recomputed */
			irg_block_walk_graph(irg, clear_block_info, NULL, &env.block_infos);
			/* Removing blocks and Conds might enable more optimizations */
			continue;
		} else {
			confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
			break;
		}
	}

	ir_nodehashmap_destroy(&env.block_infos);
}

/* Optimizations of the control flow that also require changes of Phi nodes.  */
void optimize_cf(ir_graph *irg)
{
	merge_env env;
	env.changed    = false;
	env.phis_moved = false;

	/* if the graph is not pinned, we cannot determine empty blocks */
	assert(get_irg_pinned(irg) != op_pin_state_floats);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);

	/* First the "simple" optimizations, which do not touch Phis */
	cfgopt_ignoring_phis(irg);

	/* we use the mark flag to mark removable blocks */
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_IRN_LINK
	                     | IR_RESOURCE_PHI_LIST);

	/*
	 * This pass collects all Phi nodes in a link list in the block
	 * nodes.  Further it performs simple control flow optimizations.
	 * Finally it marks all blocks that do not contain useful
	 * computations, i.e., these blocks might be removed.
	 */
	ir_node *end = get_irg_end(irg);
	irg_walk(end, clear_link_and_mark_blocks_removable, collect_nodes, NULL);

	/* assert due to collect_nodes:
	 * 1. removable blocks are now marked as such
	 * 2. phi lists are up to date
	 */

	/* Optimize the standard code.
	 * It walks only over block nodes and adapts these and the Phi nodes in
	 * these blocks, which it finds in a linked list computed before.
	 */
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	irg_block_walk_graph(irg, optimize_blocks, merge_blocks, &env);

	ir_node *new_end = optimize_in_place(end);
	if (new_end != end) {
		set_irg_end(irg, new_end);
		end = new_end;
		env.changed = true;
	}
	remove_End_Bads_and_doublets(end);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_IRN_LINK
	                  | IR_RESOURCE_PHI_LIST);

	if (env.phis_moved) {
		/* Bad: when we moved Phi's, we might produce dead Phi nodes
		   that are kept-alive.
		   Some other phases cannot copy with this, so kill them.
		 */
		int n = get_End_n_keepalives(end);
		if (n > 0) {
			ir_node **in = ALLOCAN(ir_node*, n);
			assure_irg_outs(irg);

			int j = 0;
			for (int i = 0; i < n; ++i) {
				ir_node *ka = get_End_keepalive(end, i);
				if (is_Phi(ka)) {
					foreach_irn_out_r(ka, k, user) {
						if (user != ka && user != end) {
							/* Is it a real user or just a self loop ? */
							in[j++] = ka;
							break;
						}
					}
				} else {
					in[j++] = ka;
				}
			}
			if (j != n) {
				set_End_keepalives(end, j, in);
				env.changed = true;
			}
		}
	}

	confirm_irg_properties(irg,
		env.changed ? IR_GRAPH_PROPERTIES_NONE : IR_GRAPH_PROPERTIES_ALL);
}
